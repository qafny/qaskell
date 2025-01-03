{-# LANGUAGE GADTs #-}

module DSL.LowLevel
  where

import Prettyprinter

import DSL.Utils
import DSL.Super.Simulated
import DSL.Tensor

data Operator where
  Keep1 :: Scalar -> Operator -- | Keep1 x   <--->   a*(x) ∘ a(x)
  Keep0 :: Scalar -> Operator -- | Keep0 x   <--->   a(x) ∘ a*(x)
  deriving (Show)

data Scalar where
  Lit :: Int -> Scalar
  Add :: Scalar -> Scalar -> Scalar
  Sub :: Scalar -> Scalar -> Scalar
  Mul :: Scalar -> Scalar -> Scalar
  deriving (Show)

instance Num Scalar where
  fromInteger = Lit . fromInteger
  (+) = Add
  (-) = Sub
  (*) = Mul

instance Pretty Operator where
  pretty (Keep1 x) = pretty "Keep1" <> parens (pretty x)
  pretty (Keep0 x) = pretty "Keep0" <> parens (pretty x)

-- TODO
scalarAddPrec, scalarSubPrec, scalarMulPrec :: Int
scalarAddPrec = 7
scalarSubPrec = 7
scalarMulPrec = 8

instance Pretty Scalar where
  pretty (Lit i) = pretty i
  pretty (Add x y) = pretty x <+> pretty "+" <+> pretty y
  pretty (Sub x y) = pretty x <+> pretty "-" <+> pretty y
  pretty (Mul x y) = pretty x <+> pretty "*" <+> pretty y

instance Nesting Operator where prettyNested _ = pretty

instance Nesting Scalar where
  prettyNested _    e@Lit{} = pretty e
  prettyNested prec e@Add{} = parensWhen (prec < scalarAddPrec) $ pretty e
  prettyNested prec e@Sub{} = parensWhen (prec < scalarSubPrec) $ pretty e
  prettyNested prec e@Mul{} = parensWhen (prec < scalarMulPrec) $ pretty e

computeEnergy ::
     (Scalar -> Scalar) ->
     Super (Tensor Operator) ->
     Super (Scalar, Tensor Operator)
computeEnergy energy = fmap go
  where
    go tensor =
      let y = tensorPlaces tensor
          energies = fmap tensorItem y
          totalEnergy = sum energies
      in
      (totalEnergy, tensor)

    tensorItem (input, Keep1 _) = -energy (Lit input)
    tensorItem (input, Keep0 _) = energy (Lit input)





-- data Operator a where
--   deriving (Show)

-- instance Pretty a => Pretty (Operator a) where
--   pretty (Keep1 x) = pretty "Keep1" <> parens (pretty x)
--   pretty (Keep0 x) = pretty "Keep2" <> parens (pretty x)

-- data Expr a where
--   Lit :: Int -> Expr
--   Keep1 :: a -> Expr -- | Keep1 x   <--->   a*(x) ∘ a(x)
--   Keep0 :: a -> Expr -- | Keep0 x   <--->   a(x) ∘ a*(x)
--   deriving (Show)

-- instance Pretty Expr where pretty = pretty . show

-- Tensor (Super (Operator Expr)) -> Super (Tensor (Operator Expr))

