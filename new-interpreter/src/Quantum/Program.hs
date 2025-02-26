{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

getVarPayload :: Var a -> a
getVarPayload (Var x _) = x

getVarId :: Var a -> VarId
getVarId (Var _ i) = i

data Program t a b =
  Program
    { choices :: [b]
    , struct :: t a
    , view :: Int
    , constraints :: [(a, b)] -> a
    }

generateVars :: Traversable t =>
  t a -> Fresh (t (Var a))
generateVars = traverse (\x -> Var x <$> fresh)

solveProgramClassical :: forall a b. (Eq a, Eq b, Ord a, Real a) =>
  Program [] a b ->
  a
solveProgramClassical prog =
  let
      varStruct :: [Var a]
      varStruct = runFresh (generateVars (struct prog))

      pairs :: [[Var a]]
      pairs = distinctNTuples (view prog)
                              (toList varStruct)

      isHit :: [Var a] -> Bool
      isHit = (`isSubListOf` struct prog) . map getVarPayload

      pairHits :: [[Var a]]
      pairHits = filter isHit pairs

      actualPairs :: [[(Var a, b)]]
      actualPairs = makeActualChoice (choices prog)
                                     pairHits

      results :: [a]
      results = map (\x -> (constraints prog (map (first getVarPayload) x)))
                    actualPairs
  in
  minimum results
  where
    isSubListOf xs ys = all (`elem` ys) xs

solveProgram :: forall t a b. (Eq a, Eq b, Real a, Traversable t) =>
  Program t a b ->
  Summed (Scaled (Tensor PauliExpr))
solveProgram prog =
  let
      varStruct :: t (Var a)
      varStruct = runFresh (generateVars (struct prog))

      pairs :: [[Var a]]
      pairs = distinctNTuples (view prog)
                              (toList varStruct)

      actualPairs :: [[(Var a, b)]]
      actualPairs = makeActualChoice (choices prog)
                                     pairs

      encodedChoices :: [(b, VarId -> Tensor (Summed ScaledPauli))]
      encodedChoices = encodeChoices (choices prog)

      results :: [(a, [(Var a, b)])]
      results = map (\x -> (constraints prog (map (first getVarPayload) x), x))
                    actualPairs

      decode :: (Var a, b) -> Tensor (Summed ScaledPauli)
      decode (var, choice) = decodeChoice encodedChoices choice (getVarId var)

      decodeAndDistribute :: (Var a, b) -> Summed (Scaled (Tensor PauliExpr))
      decodeAndDistribute = fmap floatScalars . distribute . decode

      results2 :: [(a, Tensor (Summed (Scaled (Tensor PauliExpr))))]
      results2 = map (second Tensor) $ map (\(x, varChoices) -> (x, map decodeAndDistribute varChoices)) results

      results3 :: [(a, Summed (Scaled (Tensor PauliExpr)))]
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
    toComplex :: a -> Complex Double
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
decodeChoice encodedChoices choice var =
  case lookup choice encodedChoices of
    Just pauliFn -> pauliFn var
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
  | otherwise = \var -> Tensor $ map ($ var) (allBitStrings !! i)
  where
    pos, neg :: VarId -> Summed ScaledPauli
    pos x = scaleSummed (1/2) (sub (pauliI x) (pauliZ x))
    neg x = scaleSummed (1/2) (add (pauliI x) (pauliZ x))

    allBitStrings = replicateM bitSize [pos, neg]

    bitSize = neededBitSize totalChoiceCount

neededBitSize :: Int -> Int
neededBitSize = ceiling . logBase 2 . fromIntegral

makeActualChoice :: [b] -> [[a]] -> [[(a, b)]]
makeActualChoice choices xss = do
  xs <- xss
  ys <- replicateM (length xs) choices
  let zs = zip xs ys
  pure zs

-- | Postcondition: In the result of `distinctNTuples n xs`, the sublists
-- all have length `n`.
distinctNTuples :: Int -> [a] -> [[a]]
distinctNTuples n xs =
  filter ((== n) . length) $ filterM (const [False, True]) xs

newtype Fresh a = Fresh (State VarId a)
  deriving (Functor, Applicative, Monad)

runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m 0

fresh :: Fresh VarId
fresh = do
  x <- Fresh get
  Fresh $ modify (+1)
  pure x

