{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
  where

import Control.Applicative
import Control.Monad

import Data.Ord
import Data.Foldable

import Data.Complex

-- TODO: Probability amplitudes
newtype Choice a = Choice [a]
  deriving (Show, Semigroup, Monoid, Functor, Applicative, Monad, Alternative)

newtype TensorPower a = TensorPower [a]
  deriving (Show, Semigroup, Monoid, Functor)

choice :: [a] -> Choice a
choice = Choice

runChoice :: Choice a -> [a]
runChoice (Choice xs) = xs

-- cartesianProduct :: [Tensor a] -> [Tensor a]
-- cartesianProduct = undefined

type Basic = Complex Double

type Env m = m (TensorPower Basic)

tensorIndex :: TensorPower a -> Int -> a
tensorIndex (TensorPower xs) j = xs !! j

singletonTensor :: a -> TensorPower a
singletonTensor x = TensorPower [x]

tensorZipWith :: (a -> b -> c) -> TensorPower a -> TensorPower b -> TensorPower c
tensorZipWith f (TensorPower xs) (TensorPower ys) =
  TensorPower (zipWith f xs ys)

quadrance :: TensorPower Basic -> Double
quadrance (TensorPower xs) = toDouble (sum (map sqrMagnitude xs))
  where
    toDouble (r :+ _) = r

    sqrMagnitude x = x * conjugate x

tensorMagnitude :: TensorPower Basic -> Double
tensorMagnitude = sqrt . quadrance

getMin :: (MonadPlus m, Foldable m) => Env m -> TensorPower Basic
getMin = minimumBy (comparing tensorMagnitude)

