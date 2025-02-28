{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Quantum.Examples where

import Control.Monad
import Control.Monad.Identity

import Data.Bifunctor
import Data.Coerce

import Data.Maybe

import Data.List

import Quantum.Program hiding (Var, var)
import Quantum.ExampleData
import Quantum.DistinctDepthN

-- ghci> solveQuantum (eqSum [1, 2])
-- ...
eqSum ::
  [Int] -> Program [] Int Int Int
eqSum inputList =
  Program
    { choices = [-1, 1]
    , struct = inputList
    , view = 2
    , constraints = \[(a, choiceA), (b, choiceB)] ->
        (a * choiceA)
          *
        (b * choiceB)
    }

-- ghci> solveQuantum (graphColoring 2 graph1)
-- ...
graphColoring ::
  Int -> [(Int, Int)] -> Program [] Int Int Int
graphColoring colorCount edges = 
  Program
    { choices = [0..colorCount-1]
    , struct = getNodes edges
    , view = 2
    , constraints = go
    }
  where
    go :: [((Int, Int))] -> Int
    go [(a, choiceA), (b, choiceB)] =
      if (a, b) `elem` edges && choiceA == choiceB
      then 1
      else 0

cliqueFinding ::
  Int -> AdjList Int -> Program [] Int Int Int
cliqueFinding cliqueSize edges =
  Program
    { choices = [0, 1]
    , struct = getNodes edges
    , view = 2
    , constraints = \[(a, choiceA), (b, choiceB)] ->
        if (a, b) `elem` edges
        then 0
        else choiceA * choiceB
    }

data Cover a = MkCover { vars :: [a], valuation :: [a] -> Bool }

exactCover :: Cover Int -> Program [] Int Int Int
exactCover cover =
  Program
    { choices = [0, 1],
      struct = vars cover,
      view = 3,
      constraints =
        \[(a, choiceA), (b, choiceB), (c, choiceC)] ->
          if valuation cover [a, b, c] && choiceA + choiceB + choiceC == 1
          then 0
          else 1
    }

infixr :->
data Type = IntType | Type :-> Type
  deriving (Show, Eq)

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

mmorphExpr :: Functor f =>
  (forall a. f a -> g a) ->
  Expr f a ->
  Expr g a
mmorphExpr alpha (Var x ann) = Var x ann
mmorphExpr alpha (Num n ann) = Num n ann
mmorphExpr alpha (App children ann) =
  App (alpha $ fmap (both (mmorphExpr alpha)) children) ann
mmorphExpr alpha (Lambda x ty body ann) =
  Lambda x ty (alpha (fmap (mmorphExpr alpha) body)) ann

maybeExpr :: Expr Identity a -> Maybe (Expr Maybe a)
maybeExpr = Just . mmorphExpr (Just . runIdentity)

newtype MaybeExpr a = MaybeExpr { unMaybeExpr :: Maybe (Expr Maybe a) }
  deriving (Functor, Foldable, Traversable, Eq)

pattern EmptyM = MaybeExpr Nothing
pattern VarM x ann = MaybeExpr (Just (Var x ann))
pattern NumM x ann = MaybeExpr (Just (Num x ann))
pattern AppM children ann = MaybeExpr (Just (App children ann))
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
lambda x ty body = MaybeExpr $ do
  body' <- unMaybeExpr body
  unMaybeExpr $ LambdaM x ty (Just body') emptyTyInCtx

expr1 :: MaybeExpr ()
expr1 = lambda "x" IntType (var "x")

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

  truncateHere 0 t = Just Nothing
  truncateHere n Nothing = Just Nothing
  truncateHere n (Just t@(Var {})) = Just $ Just t
  truncateHere n (Just t@(Num {})) = Just $ Just t
  truncateHere n (Just (App Nothing ann)) =
    Just $ Just $ App Nothing ann
  truncateHere n (Just (App (Just (left, right)) ann)) =
    Just $ Just $
    App
      (liftA2 (,)
              (join (truncateHere (n-1) (Just left)))
              (join (truncateHere (n-1) (Just right))))
      ann
  truncateHere n (Just (Lambda x ty body ann)) =
    Just $ Just $
    Lambda x
           ty
           (join $ (truncateHere (n-1)) $ body)
           ann


-- exprDepth :: Expr a -> Int
-- exprDepth Var{} = 1
-- exprDepth Num{} = 1
-- exprDepth (App x y _) = 1 + max (exprDepth x) (exprDepth y)
-- exprDepth (Lambda _ _ x _) = 1 + exprDepth x

-- -- instance DistinctTuples Expr where
-- --   distinctNTuples n = undefined

-- instance Comonad Expr where
--   extract (Var _ x) = x
--   extract (Num _ x) = x
--   extract (App _ _ x) = x
--   extract (Lambda _ _ _ x) = x

--   duplicate (Var v x) = Var v (Var v x)
--   duplicate (Num n x) = Num n (Num n x)
--   duplicate (App a b x) = App (duplicate a) (duplicate b) (App a b x)
--   duplicate (Lambda v ty body x) = Lambda v ty (duplicate body) (Lambda v ty body x)

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
  deriving (Show, Functor, Foldable, Traversable, Eq)

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
    go ctx = \case
      VarM x _ -> VarM x (makeBlankTypeInCtx ctx)
      NumM i _ -> NumM i (makeBlankTypeInCtx ctx)

      AppM children _ ->
        AppM (go2 ctx children) (makeBlankTypeInCtx ctx)

      LambdaM x ty body _ ->
        LambdaM x ty (go1 (x:ctx) body) (makeBlankTypeInCtx ctx)

    go1 :: [String] -> Maybe (Expr Maybe ()) -> Maybe (Expr Maybe ())
    go1 ctx Nothing = Nothing
    go1 ctx (Just a) =
      unMaybeExpr $ go ctx (MaybeExpr (Just a))

    go2 :: [String] -> Maybe (Expr Maybe (), Expr Maybe ()) -> Maybe (Expr Maybe (), Expr Maybe ())
    go2 ctx Nothing = Nothing
    go2 ctx (Just (a, b)) = do
      a' <- unMaybeExpr $ go ctx (MaybeExpr (Just a))
      b' <- unMaybeExpr $ go ctx (MaybeExpr (Just b))
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
            ((), aTy) <- lookup x (ctx tyInCtx)
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

            ((), xTy) <- lookup x (ctx bodyTyInCtx)
            guard (xTy == paramTy)

            case overallTy of
              srcTy :-> tgtTy -> do
                guard (xTy == srcTy)
                pure overallTy

              _ -> Nothing

          --   case aTy of
          --     srcTy :-> tgtTy -> do
          --       guard (srcTy == bTy)
          --       guard (tgtTy == ty)
          --       pure ty
          --     _ -> Nothing

          -- LambdaM x paramTy bodyM ((), ty) -> do
          --   undefined

        -- case e of
        --   Var str () -> Just ty
        --   Num _ () -> do
        --     guard (ty == IntType)
        --     Just ty
        --   App _ _ () -> Just ty
        --   Lambda x (FnType {}) body () -> Just ty
        --   Lambda {} -> Nothing
    }
  where
    nAryIntType 0 = IntType
    nAryIntType n = IntType :-> nAryIntType (n-1)

-- | Get the nodes from an adjacency list
getNodes :: Eq a => [(a, a)] -> [a]
getNodes = nub . concatMap (\(x, y) -> [x, y])

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

