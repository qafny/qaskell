{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeExample
  where

import DSL.Syntax -- hiding (Expr (..))
-- import qualified DSL.Syntax as DSL

import Control.Applicative
import Control.Monad
import Data.Foldable

infixr :->
data Type = IntTy | Type :-> Type
  deriving (Show, Eq, Ord)

-- data Expr a where
--   Var :: VarId -> a -> Expr a
--   Lit :: Int -> Expr a
--   App :: Expr a -> Expr a -> a -> Expr a
--   Lam :: VarId -> Type -> Expr a -> a -> Expr a
--   deriving (Show, Functor, Foldable, Eq, Ord)

-- instance Traversable Expr where
--   traverse f (Var x tyAnn) = Var x <$> f tyAnn
--   traverse _ (Lit i) = pure (Lit i)
--   traverse f (App a b tyAnn) =
--     liftA3 App
--            (traverse f a)
--            (traverse f b)
--            (f tyAnn)

--   traverse f (Lam x xTy body tyAnn) =
--     liftA2 (Lam x xTy)
--            (traverse f body)
--            (f tyAnn)

-- types :: [Type]
-- types =
--   [IntTy, IntTy :-> IntTy, IntTy :-> IntTy :-> IntTy]

-- infer :: Expr () -> Super _ _
-- infer expr = do
--   choice <- fmap snd <$> genChoicesQuantum types expr

--   -- let actualChoice = makeActualChoice types choice
--   undefined

data Program t a b =
  Program
    { choices :: [a]
    , struct :: t b
    , view :: Int
    , constraints :: [((a, Expr b), a)] -> Expr b
    }

solve :: forall b c. (Eq c, Num c, GetBit c) => Super c (Expr c) -> [c]
solve act = do
  let (choices, expr) = runSuper act
      freeVars = getFreeVars expr

      sbsts = mkVarSubst freeVars choices

  sbst <- sbsts
  pure $ eval sbst expr

solveProgram :: (Ord a, Num a, GetBit a, Traversable t) =>
  Program t a a ->
  [a]
solveProgram prog = solve $ do
  choice <- genChoicesQuantum (choices prog) (struct prog)

  let pairs = distinctNTuples (view prog) (toList choice)
      actualPairs = makeActualChoice (choices prog) pairs

  pure $ sum $ map (constraints prog) actualPairs

  -- pure $ 

  -- let expr = undefined
  --     freeVars = getFreeVars expr

  --     sbsts = mkVarSubst freeVars choices

  -- sbst <- sbsts
  -- pure $ eval sbst expr





-- -- distinctNTuples :: Int -> [a] -> [[a]]
-- distinctNTuples n xs =
--   filter ((== n) . length) $ filterM (const [False, True]) xs

-- data TypeConstraint = Equal Type Type

-- type TypeError = String

-- type GenConstraint = WriterT [TypeConstraint] (Either TypeError)

-- typeError :: TypeError -> GenConstraint a
-- typeError msg = lift (Left msg)

-- generateConstraintsInfer :: Expr a -> GenConstraint Type
-- generateConstraintsInfer (Var x) = undefined

-- generateConstraintsCheck :: Expr a -> Type -> GenConstraint ()
-- generateConstraintsCheck = undefined

-- -- generateConstraints :: Expr a -> [TypeConstraint]
-- -- generateConstraints = undefined

