{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Woverlapping-patterns -Wincomplete-patterns #-}

module Quantum.Program
  where

import Control.Monad.State
import Control.Monad

import Data.Functor
import Data.Coerce

import Data.Foldable
import Data.List (nub, partition, intersperse, intercalate)

import Numeric.LinearAlgebra hiding ((<>), toList, scale, add, sub)
import qualified Numeric.LinearAlgebra as Matrix
import Data.Complex
import Data.Bifunctor (first, second)

import Quantum.DistinctDepthN

type VarId = Int

data PauliExpr = I VarId | Z VarId
  deriving (Eq)

data Scaled a = Scale (Complex Double) a
  deriving (Functor)

newtype Tensor a = Tensor [a]
  deriving (Functor, Eq, Foldable, Traversable)

newtype Summed a = Summed [a]
  deriving (Functor, Applicative)

type ScaledPauli = Scaled PauliExpr
type ScaledTensor a = Scaled (Tensor a)

parens :: String -> String
parens x = "(" ++ x ++ ")"

instance ShowParens a => Show (Summed a) where
  show (Summed []) = "0"
  show (Summed xs) = unwords $ intersperse "+" (map show xs)

instance ShowParens a => Show (Tensor a) where
  show (Tensor []) = "EmptyTensor"
  show (Tensor xs) = unwords $ intersperse "⊗" (map show xs)

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
  deriving (Show, Eq, Ord)

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

-- newtype Classical a = Classical a
-- newtype Quantum a = Quantum a

-- class Solve r where
--   solveProgram :: forall t a b c. (Eq (t a), Eq (t (Var a)), Part (t (Var a)), Eq a, Eq b, Real c, Traversable t) =>
--     Program t a b c ->
--     r

-- instance Solve 

-- instance Solve (Summed (Scaled (Tensor PauliExpr))) where
--   solveProgram = solveProgramQuantum

-- createChoices :: Traversable t => [b] -> t a -> [(t (Var (a, b)))]
-- createChoices ds = traverse (\a -> msum (map (go a) ds))
--   where
--     go (Var a x) b = return (Var (a, b) x)
    
minimumsFst :: Ord a => [(a, b)] -> [(a, b)]
minimumsFst [] = []
minimumsFst xs = filter ((==) minfst . fst) xs
    where minfst = minimum (map fst xs)

solveClassical :: (Part (t (Var a)), Traversable t, Ord c, Num c, Eq a, Eq b, Eq (t (Var a))) =>
   Program t a b c ->
   [(c, t (a, b))]
solveClassical prog =
  let
     -- varStruct :: t (Var a)
     varStruct = runFresh (genChoices (struct prog))

     -- tuples :: [t (Var a)]
     tuples = distinctNTuples (view prog) varStruct

     -- actualTuples :: [t (Var a, b)]
     actualTuples = assignChoices (choices prog) tuples

     -- encodedChoices :: [t (Var a, b)]
     encodedChoices = createChoices (choices prog) varStruct

     -- results :: [(a, t (a, b))]
     results =
          minimumsFst $
          encodedChoices <&>
                  (\ aChoice ->
                    (sum $ actualTuples <&>
                                 (\ aTuple -> if isSubList aTuple (toList aChoice)
                                              then (constraints prog (fmap (first choice) aTuple))
                                              else 0
                                              )
                    ,fmap (first choice) aChoice)
                    )
               where isSubList xs ys = all (`elem` ys) xs
  in results


-- solveProgramClassical :: forall a b. (Eq a, Eq b, Ord a, Real a) =>
--   Program [] a b a ->
--   a
-- solveProgramClassical prog =
--   let
--       varStruct :: [Var a]
--       varStruct = runFresh (genChoices (struct prog))

--       pairs :: [[Var a]]
--       pairs = distinctNTuples (view prog)
--                               (varStruct)

--       isHit :: [Var a] -> Bool
--       isHit = (`isSubListOf` struct prog) . map choice

--       pairHits :: [[Var a]]
--       pairHits = filter isHit pairs

--       actualPairs :: [[(Var a, b)]]
--       actualPairs = assignChoices (choices prog)
--                                   pairHits

--       results :: [a]
--       results = map (\x -> (constraints prog (map (first choice) x)))
--                     actualPairs
--   in
--   minimum results
--   where
--     isSubListOf xs ys = all (`elem` ys) xs

-- solveProgramClassical :: forall a b. (Eq a, Eq b, Ord a, Real a) =>
--   Program [] a b a ->
--   a
-- solveProgramClassical prog =
--   let
--       varStruct :: [Var a]
--       varStruct = runFresh (genChoices (struct prog))

--       pairs :: [[Var a]]
--       pairs = distinctNTuples (view prog)
--                               (varStruct)

--       isHit :: [Var a] -> Bool
--       isHit = (`isSubListOf` struct prog) . map choice

--       pairHits :: [[Var a]]
--       pairHits = filter isHit pairs

--       actualPairs :: [[(Var a, b)]]
--       actualPairs = assignChoices (choices prog)
--                                   pairHits

--       results :: [a]
--       results = map (\x -> (constraints prog (map (first choice) x)))
--                     actualPairs
--   in
--   minimum results
--   where
--     isSubListOf xs ys = all (`elem` ys) xs

solveQuantum :: forall t a b c. (Eq (t a), Eq (t (Var a)), Part (t (Var a)), Eq a, Eq b, Real c, Traversable t) =>
  Program t a b c ->
  Summed (Scaled (Tensor PauliExpr))
solveQuantum prog =
  let
      varStruct :: t (Var a)
      varStruct = runFresh (genChoices (struct prog))

      pairs :: [t (Var a)]
      pairs = distinctNTuples (view prog)
                              varStruct
      actualPairs :: [t (Var a, b)]
      actualPairs = assignChoices (choices prog)
                                  pairs

      encodedChoices :: [(b, VarId -> Tensor (Summed ScaledPauli))]
      encodedChoices = encodeChoices (choices prog)

      results :: [(c, t (Var a, b))]
      results = map (\x -> (constraints prog (fmap (first choice) x), x))
                    actualPairs

      decode :: (Var a, b) -> Tensor (Summed ScaledPauli)
      decode (x, choice) = decodeChoice encodedChoices choice (var x)

      decodeAndDistribute :: (Var a, b) -> Summed (Scaled (Tensor PauliExpr))
      decodeAndDistribute = fmap floatScalars . distribute . decode

      results2 :: [(c, Tensor (Summed (Scaled (Tensor PauliExpr))))]
      results2 =
        map (second (Tensor . toList)) $ -- TODO: Does toList just make this work?
        fmap (\(x, varChoices) ->
                (x, fmap decodeAndDistribute varChoices))
             results

      results3 :: [(c, Summed (Scaled (Tensor PauliExpr)))]
      results3 = map (second (fmap commuteScaledTensor . distribute)) results2

      results4 :: [(Complex Double, Summed (Scaled (Tensor PauliExpr)))]
      results4 = map (first toComplex) results3

      results5 :: [Summed (Scaled (Tensor PauliExpr))]
      results5 = map (\(k, x) -> fmap (scale k) x) results4

      results6 :: Summed (Scaled (Tensor PauliExpr))
      results6 = joinSummed $ Summed results5
  in
  eliminateZeroes $ collectLikes results6
  where
    toComplex :: c -> Complex Double
    toComplex = fromRational . toRational

eliminateZeroes :: Summed (Scaled a) -> Summed (Scaled a)
eliminateZeroes (Summed xs) = Summed $ filter nonZero xs
  where
    nonZero (Scale 0 _) = False
    nonZero _ = True

collectLikes :: forall a. Eq a => Summed (Scaled a) -> Summed (Scaled a)
collectLikes (Summed xs0) = Summed $ go xs0
  where
    isLike :: Scaled a -> Scaled a -> Bool
    isLike (Scale _ x) (Scale _ y) = x == y

    -- | Precondition: the second item of the Scale should be the same for
    -- both arguments
    combine :: Scaled a -> Scaled a -> Scaled a
    combine (Scale k x) (Scale k' _) = Scale (k + k') x

    combineList :: Scaled a -> [Scaled a] -> Scaled a
    combineList = foldr combine

    go :: [Scaled a] -> [Scaled a]
    go [] = []
    go (x:xs) =
      let (likes, notLikes) = partition (isLike x) xs
          newX = combineList x likes
      in
      newX : go notLikes

commuteScaledTensor :: Tensor (Scaled (Tensor a)) -> Scaled (Tensor a)
commuteScaledTensor = fmap joinTensor . floatScalars

joinSummed :: forall a. Summed (Summed a) -> Summed a
joinSummed xs = coerce (concat (coerce xs :: [[a]]))

joinTensor :: forall a. Tensor (Tensor a) -> Tensor a
joinTensor xs = coerce (concat (coerce xs :: [[a]]))

distribute :: Tensor (Summed a) -> Summed (Tensor a)
distribute = sequenceA

encodeChoices :: [a] -> [(a, VarId -> Tensor (Summed ScaledPauli))]
encodeChoices choices = zipWith (\choice i -> (choice, toPauli choiceCount i)) choices [0..]
  where
    choiceCount = length choices

decodeChoice :: Eq a => [(a, VarId -> Tensor (Summed ScaledPauli))] -> a -> VarId -> Tensor (Summed ScaledPauli)
decodeChoice encodedChoices choice x =
  case lookup choice encodedChoices of
    Just pauliFn -> pauliFn x
    Nothing -> error "decodeChoice"

scale :: Complex Double -> Scaled a -> Scaled a
scale k (Scale k' x) = Scale (k * k') x

scaleSummed :: Complex Double -> Summed (Scaled a) -> Summed (Scaled a)
scaleSummed k = fmap (scale k)

tensor :: [Scaled a] -> Scaled (Tensor a)
tensor xs = Scale (product (map getScalar xs)) (Tensor (map getVec xs))
  where
    getScalar (Scale k _) = k
    getVec (Scale _ x) = x

floatScalars :: Tensor (Scaled a) -> Scaled (Tensor a)
floatScalars = tensor . coerce

add :: ScaledPauli -> ScaledPauli -> Summed ScaledPauli
add x y = Summed [x, y]

sub :: ScaledPauli -> ScaledPauli -> Summed ScaledPauli
sub x y = add x (scale (-1) y)

pauliZ :: VarId -> ScaledPauli
pauliZ x = Scale 1 (Z x)

pauliI :: VarId -> ScaledPauli
pauliI x = Scale 1 (I x)

toPauli :: Int -> Int -> (VarId -> Tensor (Summed ScaledPauli))
toPauli totalChoiceCount i
  | i > totalChoiceCount = error "toPauli: i > totalChoiceCount"
  | i >= length allBitStrings = error "toPauli: i >= length allBitStrings"
  | otherwise = \x -> Tensor $ map ($ x) (allBitStrings !! i)
  where
    pos, neg :: VarId -> Summed ScaledPauli
    pos x = scaleSummed (1/2) (sub (pauliI x) (pauliZ x))
    neg x = scaleSummed (1/2) (add (pauliI x) (pauliZ x))

    allBitStrings = replicateM bitSize [pos, neg]

    bitSize = neededBitSize totalChoiceCount

neededBitSize :: Int -> Int
neededBitSize = ceiling . logBase 2 . fromIntegral

strength :: Functor g => (a, g b) -> g (a, b)
strength (x, gy) = fmap (\y -> (x, y)) gy

createChoices :: (Traversable t, Applicative f) =>
  f b -> t a -> f (t (a, b))
createChoices ds struct =
    traverse (\a -> strength (a, ds)) struct
  where
    go a d = return (a, d)

assignChoices :: Traversable t => [b] -> [t a] -> [t (a, b)]
assignChoices choices xss = do
  xs <- xss
  ys <- replicateM (length xs) choices
  pure (fillTraversablePairs ys xs)

fillTraversablePairs :: Traversable t => [a] -> t b -> t (b, a)
fillTraversablePairs xs t = evalState (traverse makeStatePair t) xs
  where
    makeStatePair b = state $ \case
      [] -> error "Not enough elements in the list"
      (a:as) -> ((b, a), as)

-- -- instance DistinctTuples [] where
-- distinctNTuples n xs =
--   filter ((== n) . length) $ filterM (const [False, True]) xs

newtype Fresh a = Fresh (State VarId a)
  deriving (Functor, Applicative, Monad)

runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m 0

fresh :: Fresh VarId
fresh = do
  x <- Fresh get
  Fresh $ modify (+1)
  pure x

