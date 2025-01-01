module Main (main) where

import Lib
import Eval

import Data.Complex

import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Gen

main :: IO ()
main = pure ()

-- genComplex :: Gen (Complex Double)
-- genComplex = liftA2 (:+) genDouble genDouble

-- genTensor :: Gen (TensorPower Basic)
-- genTensor = _ $ do
--   n <- choose (2, 6)
--   replicateM n genDouble

-- prop_equalSum :: Property
-- prop_equalSum = do
--   tensor <- genTensor
--   undefined

