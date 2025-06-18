{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Woverlapping-patterns -Wincomplete-patterns #-}

module Quantum.Program
  where

import Control.Monad.State
import Control.Monad

import Data.Functor
import Data.Coerce

import Data.Foldable
import Data.List (partition, intersperse)

import Numeric.LinearAlgebra hiding ((<>), toList, scale, add)
import Data.Bifunctor (first, second)

import Data.Bits (testBit)

import Quantum.DistinctDepthN

import qualified Data.Map.Strict as M

debugSolver :: Bool
debugSolver = False

type VarId = Int

data PauliExpr = I VarId | Z VarId
  deriving (Eq, Ord)

data Scaled a = Scale (Complex Double) a
  deriving (Functor, Eq)

newtype Tensor a = Tensor [a]
  deriving (Functor, Eq, Foldable, Traversable, Ord)

newtype Summed a = Summed [a]
  deriving (Functor, Applicative)

instance Eq a => Eq (Summed a) where
  Summed xs == Summed ys =
      all (\x -> count x xs == count x ys) xs
    where
      count x = length . filter (== x)

type ScaledPauli = Scaled PauliExpr
type ScaledTensor a = Scaled (Tensor a)

parens :: String -> String
parens x = "(" ++ x ++ ")"

instance ShowParens a => Show (Summed a) where
  show (Summed []) = "0"
  show (Summed xs) = unwords $ intersperse "+" (map show xs)

instance ShowParens a => Show (Tensor a) where
  show (Tensor []) = "EmptyTensor"
  show (Tensor xs) = unwords $ intersperse "@" (map show xs)

class Show a => ShowParens a where
  showParens :: a -> String

instance ShowParens PauliExpr where
  showParens = show

instance ShowParens a => ShowParens (Summed a) where
  showParens = parens . show

instance ShowParens a => ShowParens (Tensor a) where
  showParens = parens . show

instance ShowParens a => Show (Scaled a) where
  show (Scale k x) = prettyShow k ++ " " ++ showParens x
    where
      prettyShow (a :+ 0) = show a
      prettyShow (0 :+ b) = show b ++ "i"
      prettyShow (a :+ b) = parens (show a ++ " + " ++ show b ++ "i")

instance ShowParens a => ShowParens (Scaled a) where
  showParens = show

instance Show PauliExpr where
  show (I i) = "I(" ++ [['a'..'z'] !! i] ++ ")"
  show (Z i) = "Z(" ++ [['a'..'z'] !! i] ++ ")"

data Var a = Var a VarId
  deriving (Show, Eq, Ord, Functor)

choice :: Var a -> a
choice (Var x _) = x

var :: Var a -> VarId
var (Var _ i) = i

data Program t a b c =
  Program
    { choices :: [b]
    , struct :: t a
    , view :: Int
    , constraints :: t (a, b) -> c
    }

genChoices :: Traversable t =>
  t a -> Fresh (t (Var a))
genChoices = traverse (\x -> Var x <$> fresh)

minimumsFst :: Ord a => [(a, b)] -> [(a, b)]
minimumsFst [] = []
minimumsFst xs = filter ((==) minfst . fst) xs
    where minfst = minimum (map fst xs)

solveClassical :: forall t a b c. (Eq (t a), Ord (t (Var a)), Part (t (Var a)), Eq a, Eq b, Real c, Traversable t) =>
  Program t a b c ->
  [(c, t (a, b))]
solveClassical prog =
  let
     varStruct = runFresh (genChoices (struct prog))

     tuples = distinctNTuples (view prog) varStruct

     actualTuples = assignChoices (choices prog) tuples

     encodedChoices = createChoices (choices prog) varStruct

     results =
          minimumsFst $ encodedChoices <&>
                  (\ aChoice -> (sum $ actualTuples <&>
                        (\ aTuple -> if isSubList aTuple (toList aChoice)
                                     then (constraints prog (fmap (first choice) aTuple))
                                     else 0)
                                  ,fmap (first choice) aChoice) )
               where isSubList xs ys = all (`elem` ys) xs
  in results

solveQuantum :: forall t a b c. (Ord (t (Var a)), Part (t (Var a)), Eq a, Eq b, Real c, Traversable t) =>
  Program t a b c ->
  Summed (Scaled (Tensor PauliExpr))
solveQuantum prog =
   let
      varStruct :: t (Var a)
      varStruct = runFresh (genChoices (struct prog))

      pairs :: [t (Var a)]
      pairs = distinctNTuples (view prog)
                              varStruct

      actualTuples :: [t (Var a, b)]
      actualTuples = assignChoices (choices prog)
                                  pairs

      encodedChoices = encodeChoices (choices prog)

      decode :: (Var a, b) -> Tensor (Summed ScaledPauli)
      decode (x, c) =
        decodeChoice encodedChoices c (var x)
      
      optimize :: forall x. (ShowParens x, Ord x) => Summed (Scaled x) -> Summed (Scaled x)
      optimize x =
        let y = combine x
            y' = combine' x
        in
        if debugSolver && y /= y'
          then error $ "combine incorrect: " ++ show (y, y')
          else clean y

      constraintResults :: [(c, t (Var a, b))]
      constraintResults =
        map (\x -> (constraints prog (fmap (first choice) x), x))
            actualTuples

      combineSums ::
        [Summed (Scaled (Tensor PauliExpr))] ->
        Summed (Scaled (Tensor PauliExpr))
      combineSums = joinSummed . Summed

      applyScaling ::
        [(Complex Double, Summed (Scaled (Tensor PauliExpr)))] ->
        [Summed (Scaled (Tensor PauliExpr))]
      applyScaling = map (\(k, x) -> fmap (scale k) x)

      coeffsToComplex ::
        [(c, Summed (Scaled (Tensor PauliExpr)))] ->
        [(Complex Double, Summed (Scaled (Tensor PauliExpr)))]
      coeffsToComplex = map (first toComplex')

      commuteTensorScaling ::
        [(c, Summed (Tensor (Scaled (Tensor PauliExpr))))] ->
        [(c, Summed (Scaled (Tensor PauliExpr)))]
      commuteTensorScaling = map (second (fmap commuteScaledTensor))

      buildTensor ::
        [(c, t (Summed (Scaled (Tensor PauliExpr))))] ->
        [(c, Tensor (Summed (Scaled (Tensor PauliExpr))))]
      buildTensor = map (second (Tensor . toList))

      decodeAndDistribute ::
        [(c, t (Var a, b))] ->
        [(c, t (Summed (Scaled (Tensor PauliExpr))))]
      decodeAndDistribute = 
        fmap (\(x, varChoices) ->
                (x, fmap (fmap floatScalars . distr . decode) varChoices))
      
      distributeSummedTensor ::
        [(c, Tensor (Summed (Scaled (Tensor PauliExpr))))] ->
        [(c, Summed (Tensor (Scaled (Tensor PauliExpr))))]
      distributeSummedTensor = map (second distr)

      compiled :: Summed (Scaled (Tensor PauliExpr))
      compiled =
        optimize $
        combineSums $
        applyScaling $
        coeffsToComplex $
        commuteTensorScaling $
        distributeSummedTensor $
        buildTensor $
        decodeAndDistribute $
        constraintResults
   in
   compiled
   where
    toComplex' :: c -> Complex Double
    toComplex' = fromRational . toRational

showChoices :: Show a => [(a, VarId -> Tensor (Summed ScaledPauli))] -> String
showChoices = unlines . zipWith go [0..]
  where
    go x (a, f) =
      "(" ++ show a ++ ", " ++ show (f x) ++ ")"

clean :: Summed (Scaled a) -> Summed (Scaled a)
clean (Summed xs) = Summed $ filter nonZero xs
  where
    nonZero (Scale 0 _) = False
    nonZero _ = True

combine :: (Ord a) => Summed (Scaled a) -> Summed (Scaled a)
-- combine = combine'
combine (Summed xs) =
  Summed
    [ Scale k x
    | (x,k) <- M.toList (foldl' add' M.empty xs)
    , k /= 0
    ]
  where
    add' m (Scale k x) = M.insertWith (+) x k m

combine' :: forall a. Eq a => Summed (Scaled a) -> Summed (Scaled a)
combine' (Summed xs0) = Summed $ go xs0
  where
    isLike :: Scaled a -> Scaled a -> Bool
    isLike (Scale _ x) (Scale _ y) = x == y

    -- | Precondition: the second item of the Scale should be the same for
    -- both arguments
    combineGo :: Scaled a -> Scaled a -> Scaled a
    combineGo (Scale k x) (Scale k' _) = Scale (k + k') x

    combineList :: Scaled a -> [Scaled a] -> Scaled a
    combineList = foldr combineGo

    go :: [Scaled a] -> [Scaled a]
    go [] = []
    go (x:xs) =
      let (likes, notLikes) = partition (isLike x) xs
          newX = combineList x likes
      in
      newX : go notLikes

commuteScaledTensor :: Tensor (Scaled (Tensor a)) -> Scaled (Tensor a)
commuteScaledTensor = {-# SCC commuteScaledTensor #-}
  fmap joinTensor . floatScalars
{-# INLINE commuteScaledTensor #-}

joinSummed :: forall a. Summed (Summed a) -> Summed a
joinSummed xs = {-# SCC joinSummed #-}
  coerce (concat (coerce xs :: [[a]]))
{-# INLINE joinSummed #-}

joinTensor :: forall a. Tensor (Tensor a) -> Tensor a
joinTensor xs = {-# SCC joinTensor #-}
  coerce (concat (coerce xs :: [[a]]))
{-# INLINE joinTensor #-}

distr :: Tensor (Summed a) -> Summed (Tensor a)
distr = sequenceA

encodeChoices :: [a] -> [(a, VarId -> Tensor (Summed ScaledPauli))]
encodeChoices choices' = {-# SCC encodeChoices #-}
    zipWith (\choice' i ->
                                  (choice', toPauli choiceCount i))
                                choices'
                                [0..]
  where
    choiceCount = length choices'
{-# INLINE encodeChoices #-}

decodeChoice :: Eq a => [(a, VarId -> Tensor (Summed ScaledPauli))] -> a -> VarId -> Tensor (Summed ScaledPauli)
decodeChoice encodedChoices choice' x =
  case lookup choice' encodedChoices of
    Just pauliFn -> pauliFn x
    Nothing -> error "decodeChoice"

scale :: Complex Double -> Scaled a -> Scaled a
scale k (Scale k' x) = Scale (k * k') x

scaleSummed :: Complex Double -> Summed (Scaled a) -> Summed (Scaled a)
scaleSummed k = fmap (scale k)

tensor :: [Scaled a] -> Scaled (Tensor a)
tensor xs = {-# SCC tensor #-}
    Scale (product (map getScalar xs)) (Tensor (map getVec xs))
  where
    getScalar (Scale k _) = k
    {-# INLINE getScalar #-}
    getVec (Scale _ x) = x
    {-# INLINE getVec #-}
{-# INLINE tensor #-}

floatScalars :: Tensor (Scaled a) -> Scaled (Tensor a)
floatScalars = {-# SCC floatScalars #-}
  tensor . coerce
{-# INLINE floatScalars #-}

add :: ScaledPauli -> ScaledPauli -> Summed ScaledPauli
add x y = Summed [x, y]

sub :: ScaledPauli -> ScaledPauli -> Summed ScaledPauli
sub x y = add x (scale (-1) y)

pauliZ :: VarId -> ScaledPauli
pauliZ x = Scale 1 (Z x)

pauliI :: VarId -> ScaledPauli
pauliI x = Scale 1 (I x)

-- Helper to convert an integer to a big-endian list of bits
toBits :: Int -> Int -> [Int]
toBits d n = [ if testBit n i then 1 else 0 | i <- reverse [0..d-1] ]

toPauli :: Int -> Int -> VarId -> Tensor (Summed ScaledPauli)
toPauli totalChoiceCount i = \x ->
  let
      d = neededBitSize totalChoiceCount
      bits = toBits d i
      qubitIds = map (\j -> x * d + j) [0..d-1]
      pauliOps = zipWith (\bit qubitId -> if bit == 1 then pos qubitId else neg qubitId) bits qubitIds
  in Tensor pauliOps
  where
    pos v = scaleSummed (0.5) (sub (pauliI v) (pauliZ v)) -- |1> projector
    neg v = scaleSummed (0.5) (add (pauliI v) (pauliZ v)) -- |0> projector

neededBitSize :: Int -> Int
neededBitSize n = ceiling (logBase 2 (fromIntegral n :: Double))

strength :: Functor g => (a, g b) -> g (a, b)
strength (x, gy) = fmap (\y -> (x, y)) gy

createChoices :: (Traversable t, Applicative f) =>
  f b -> t a -> f (t (a, b))
createChoices ds struct' =
    traverse (\a -> strength (a, ds)) struct'

assignChoices :: Traversable t => [b] -> [t a] -> [t (a, b)]
assignChoices choices' xss = do
  xs <- xss
  ys <- replicateM (length xs) choices'
  pure (fillTraversablePairs ys xs)

fillTraversablePairs :: Traversable t => [a] -> t b -> t (b, a)
fillTraversablePairs xs t = evalState (traverse makeStatePair t) xs
  where
    makeStatePair b = state $ \case
      [] -> error "Not enough elements in the list"
      (a:as) -> ((b, a), as)

newtype Fresh a = Fresh (State VarId a)
  deriving (Functor, Applicative, Monad)

runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m 0

fresh :: Fresh VarId
fresh = do
  x <- Fresh get
  Fresh $ modify (+1)
  pure x

