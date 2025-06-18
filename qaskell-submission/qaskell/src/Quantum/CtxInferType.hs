{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Quantum.CtxInferType
  where

import Control.Monad
import Control.Monad.Identity

import Data.Bifunctor
import Data.Coerce

import Data.Maybe

import Data.List

import Quantum.Program hiding (Var, var)
import Quantum.DistinctDepthN

infixr :->
data Type = IntType | Type :-> Type
  deriving (Show, Eq, Ord)

data Expr f a
  = Var String (TypeInCtx a)
  | Num Int (TypeInCtx a)
  | App (f (Expr f a, Expr f a)) (TypeInCtx a)
  | Lambda String Type (f (Expr f a)) (TypeInCtx a)
  deriving (Functor, Foldable, Traversable)

deriving instance (Show a, Show (f (Expr f a)), Show (f (Expr f a, Expr f a))) =>
  Show (Expr f a)

deriving instance (Eq a, Eq (f (Expr f a)), Eq (f (Expr f a, Expr f a))) =>
  Eq (Expr f a)

deriving instance (Ord a, Ord (f (Expr f a)), Ord (f (Expr f a, Expr f a))) =>
  Ord (Expr f a)

mmorphExpr :: Functor f =>
  (forall x. f x -> g x) ->
  Expr f a ->
  Expr g a
mmorphExpr _ (Var x ann) = Var x ann
mmorphExpr _ (Num n ann) = Num n ann
mmorphExpr alpha (App children ann) =
  App (alpha $ fmap (both (mmorphExpr alpha)) children) ann
mmorphExpr alpha (Lambda x ty' body ann) =
  Lambda x ty' (alpha (fmap (mmorphExpr alpha) body)) ann

maybeExpr :: Expr Identity a -> Maybe (Expr Maybe a)
maybeExpr = Just . mmorphExpr (Just . runIdentity)

newtype MaybeExpr a = MaybeExpr { unMaybeExpr :: Maybe (Expr Maybe a) }
  deriving (Show, Functor, Foldable, Traversable, Eq, Ord)

-- pattern EmptyM = MaybeExpr Nothing
-- pattern VarM x ann = MaybeExpr (Just (Var x ann))
-- pattern NumM x ann = MaybeExpr (Just (Num x ann))
-- pattern AppM children ann = MaybeExpr (Just (App children ann))
-- pattern LambdaM x ty body ann = MaybeExpr (Just (Lambda x ty body ann))
pattern EmptyM :: MaybeExpr a
pattern EmptyM = MaybeExpr Nothing

pattern VarM :: String -> TypeInCtx a -> MaybeExpr a
pattern VarM x ann = MaybeExpr (Just (Var x ann))

pattern NumM :: Int -> TypeInCtx a -> MaybeExpr a
pattern NumM x ann = MaybeExpr (Just (Num x ann))

pattern AppM :: Maybe (Expr Maybe a, Expr Maybe a) -> TypeInCtx a -> MaybeExpr a
pattern AppM children ann = MaybeExpr (Just (App children ann))

pattern LambdaM :: String -> Type -> Maybe (Expr Maybe a) -> TypeInCtx a -> MaybeExpr a
pattern LambdaM x ty body ann = MaybeExpr (Just (Lambda x ty body ann))



var :: String -> MaybeExpr ()
var x = VarM x emptyTyInCtx

num :: Int -> MaybeExpr ()
num x = NumM x emptyTyInCtx

app :: MaybeExpr () -> MaybeExpr () -> MaybeExpr ()
app a b = MaybeExpr $ do
  a' <- unMaybeExpr a
  b' <- unMaybeExpr b
  unMaybeExpr $ AppM (Just (a', b')) emptyTyInCtx

lambda :: String -> Type -> MaybeExpr () -> MaybeExpr ()
lambda x ty' body = MaybeExpr $ do
  body' <- unMaybeExpr body
  unMaybeExpr $ LambdaM x ty' (Just body') emptyTyInCtx

expr1 :: MaybeExpr ()
expr1 = lambda "x" IntType (var "x")

exprFull :: MaybeExpr ()
exprFull = app expr1 (var "y")

instance Part (MaybeExpr a) where
  immediateChildren = coerce (immediateChildren :: (Maybe (Expr Maybe a) -> [Maybe (Expr Maybe a)]))
  truncateHere = coerce (truncateHere :: Int -> Maybe (Expr Maybe a) -> Maybe (Maybe (Expr Maybe a)))

instance Part (Maybe (Expr Maybe a)) where
  immediateChildren Nothing = []
  immediateChildren (Just (Var {})) = []
  immediateChildren (Just (Num {})) = []
  immediateChildren (Just (App (Just (left, right)) _ann)) = [Just left, Just right]
  immediateChildren (Just (App Nothing _ann)) = []
  immediateChildren (Just (Lambda _x _ty body _ann)) = map Just $ maybeToList body

  truncateHere 0 _ = Just Nothing
  truncateHere _ Nothing = Just Nothing
  truncateHere _ (Just t@(Var {})) = Just $ Just t
  truncateHere _ (Just t@(Num {})) = Just $ Just t
  truncateHere _ (Just (App Nothing ann)) =
    Just $ Just $ App Nothing ann
  truncateHere n (Just (App (Just (left, right)) ann)) =
    Just $ Just $
    App
      (liftA2 (,)
              (join (truncateHere (n-1) (Just left)))
              (join (truncateHere (n-1) (Just right))))
      ann
  truncateHere n (Just (Lambda x ty' body ann)) =
    Just $ Just $
    Lambda x
           ty'
           (join $ (truncateHere (n-1)) $ body)
           ann

getAnn :: Expr f a -> TypeInCtx a
getAnn (Var _ ann) = ann
getAnn (Num _ ann) = ann
getAnn (App _ ann) = ann
getAnn (Lambda _ _ _ ann) = ann

getFreeVars :: Foldable f => Expr f a -> [String]
getFreeVars = nub . go
  where
    go (Var x _) = [x]
    go (Num {}) = []
    go (App children _) =
      foldMap (\(a, b) -> go a ++ go b) children
    go (Lambda x _ body _) =
        foldMap (\a -> go a \\ [x]) body

type Ctx a = [(String, a)]

data TypeInCtx a =
  TypeInCtx
  { ctx :: Ctx a
  , ty :: a
  }
  deriving (Show, Functor, Foldable, Traversable, Eq, Ord)

emptyTyInCtx :: TypeInCtx ()
emptyTyInCtx = TypeInCtx [] ()

makeBlankTypeInCtx :: [String] -> TypeInCtx ()
makeBlankTypeInCtx fvs =
  TypeInCtx (map (, ()) fvs) ()

-- | Fill the typing contexts with blanks for the in-scope free variables
makeBlankExpr :: MaybeExpr () -> MaybeExpr ()
makeBlankExpr = go []
  where
    go :: [String] -> MaybeExpr () -> MaybeExpr ()
    go ctx' = \case
      VarM x _ -> VarM x (makeBlankTypeInCtx ctx')
      NumM i _ -> NumM i (makeBlankTypeInCtx ctx')

      AppM children _ ->
        AppM (go2 ctx' children) (makeBlankTypeInCtx ctx')

      LambdaM x ty' body _ ->
        LambdaM x ty' (go1 (x:ctx') body) (makeBlankTypeInCtx ctx')

      _ -> error "makeBlankExpr: Unsupported expression type"

    go1 :: [String] -> Maybe (Expr Maybe ()) -> Maybe (Expr Maybe ())
    go1 _ Nothing = Nothing
    go1 ctx' (Just a) =
      unMaybeExpr $ go ctx' (MaybeExpr (Just a))

    go2 :: [String] -> Maybe (Expr Maybe (), Expr Maybe ()) -> Maybe (Expr Maybe (), Expr Maybe ())
    go2 _ Nothing = Nothing
    go2 ctx' (Just (a, b)) = do
      a' <- unMaybeExpr $ go ctx' (MaybeExpr (Just a))
      b' <- unMaybeExpr $ go ctx' (MaybeExpr (Just b))
      pure (a', b')

maybeToEnergy :: Maybe a -> Int
maybeToEnergy Nothing = 1
maybeToEnergy (Just _) = 0

inferType :: MaybeExpr () -> Program MaybeExpr () Type Int
inferType expr =
  Program
    { choices = map nAryIntType [0..length expr-1]
    , struct = makeBlankExpr expr
    , view = 2
    , constraints = maybeToEnergy .
        \case
          EmptyM -> Nothing
          VarM x tyInCtx -> do
            let aTy = case lookup x (ctx tyInCtx) of
                        Just (_, ty') -> ty'
                        Nothing -> IntType

            guard (aTy == snd (ty tyInCtx))
            pure aTy

          NumM _ tyInCtx -> do
            let ((), aTy) = ty tyInCtx
            guard (aTy == IntType)
            pure aTy

          AppM childrenM tyInCtx -> do
            let ((), overallTy) = ty tyInCtx

            (a, b) <- childrenM
            let aTyInCtx = getAnn a
                bTyInCtx = getAnn b

            -- TODO: Do we need to ensure that all three typing contexts
            -- are the same here with a guard?

            let ((), aTy) = ty aTyInCtx
                ((), bTy) = ty bTyInCtx

            case aTy of
              srcTy :-> tgtTy -> do
                guard (srcTy == bTy)
                guard (tgtTy == overallTy)
                pure overallTy
              _ -> Nothing

          LambdaM x paramTy bodyM tyInCtx -> do
            let ((), overallTy) = ty tyInCtx

            body <- bodyM

            let bodyTyInCtx = getAnn body
                ((), bodyTy) = ty bodyTyInCtx

            ((), xTy) <- lookup x (ctx bodyTyInCtx)
            guard (xTy == paramTy)

            case overallTy of
              srcTy :-> tgtTy -> do
                guard (xTy == srcTy)
                guard (tgtTy == bodyTy)
                pure overallTy

              _ -> Nothing
          _ -> undefined
    }

nAryIntType :: Int -> Type
nAryIntType 0 = IntType
nAryIntType n = IntType :-> nAryIntType (n-1)

-- | Get the nodes from an adjacency list
getNodes :: Eq a => [(a, a)] -> [a]
getNodes = nub . concatMap (\(x, y) -> [x, y])

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f
