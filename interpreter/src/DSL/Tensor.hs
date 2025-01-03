{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}

module DSL.Tensor
  where

import Data.List

import Prettyprinter
import Data.Coerce

import DSL.Utils
import DSL.Super.Simulated

newtype Tensor a = Tensor [a]
  deriving (Show, Foldable, Functor, Applicative)

tensorPrecedence :: Int
tensorPrecedence = 2

tensorPlaces :: Tensor a -> Tensor (Int, a)
tensorPlaces (Tensor xs) = Tensor (zip [0..] xs)

tensorZipWith :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
tensorZipWith f (Tensor xs) (Tensor ys) = Tensor (zipWith f xs ys)

instance Nesting a => Pretty (Tensor a) where
  pretty (Tensor []) = pretty "[]"
  pretty (Tensor xs) =
    hsep $ intersperse (pretty "⊗") $ map (prettyNested tensorPrecedence) xs

instance Nesting a => Nesting (Tensor a) where
  prettyNested p = parensWhen (p < tensorPrecedence) . pretty

