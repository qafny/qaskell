{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Woverlapping-patterns -Wincomplete-patterns #-}

module Quantum.Program
  -- (Program (..)
  -- ,Var
  -- ,getVarPayload
  -- ,solveProgram
  -- ,simplify
  -- )
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

solveProgramClassical :: forall t a b. (Eq a, Ord a, Traversable t) =>
  Program t a b ->
  a
solveProgramClassical prog =
  let
      varStruct :: t (Var a)
      varStruct = runFresh (generateVars (struct prog))

      tuples :: [[Var a]]
      tuples = distinctNTuples (view prog) (toList varStruct)

      actualTuples :: [[(Var a, b)]]
      actualTuples = makeActualChoice (choices prog) tuples

      encodedChoices :: [(t (a,Var a))]
      encodedChoices = undefined

      constraintsApplied :: [([(Var a, b)], a)]
      constraintsApplied = map (\x -> (x, constraints prog (map (first getVarPayload) x))) actualTuples

      results :: [t (a,a)]
      results = undefined
  in
  undefined

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

      decodeAndDistribute :: (Var a, b) -> Summed (Tensor ScaledPauli) 
      decodeAndDistribute = distribute . decode

      results2 :: [(a, Tensor (Summed (Tensor ScaledPauli)))]
      results2 = map (second Tensor) $ map (\(x, varChoices) -> (x, map decodeAndDistribute varChoices)) results

      results3 :: [(a, Summed (Tensor ScaledPauli))]
      results3 = map (second (fmap joinTensor . distribute)) results2

      results4 :: [(Complex Double, Summed (Tensor ScaledPauli))]
      results4 = map (first toComplex) results3

      results5 :: [Summed (Tensor ScaledPauli)]
      results5 = map (uncurry scale) results4

      results6 :: Summed (Tensor ScaledPauli)
      results6 = joinSummed $ Summed results5

      results7 :: Summed (Scaled (Tensor PauliExpr))
      results7 = fmap floatScalars results6
  in
  eliminateZeroes $ collectLikes results7
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
    combine (Scale k x) (Scale k' _) = Scale (k * k') x

    combineList :: Scaled a -> [Scaled a] -> Scaled a
    combineList = foldr combine

    go :: [Scaled a] -> [Scaled a]
    go [] = []
    go (x:xs) =
      let (likes, notLikes) = partition (isLike x) xs
          newX = combineList x likes
      in
      newX : go notLikes

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

-- decodeChoice :: Eq a => [(a, s -> t)] -> a -> s -> t
-- decodeChoice encodedChoices choice var =
--   case lookup choice encodedChoices of
--     Just pauliFn -> pauliFn var
--     Nothing -> error "decodeChoice"

-- type Pauli = Matrix (Complex Double)

-- varsToPauli :: forall a. Eq a => [(a, [Var a])] -> [(a, [PauliExpr])]
-- varsToPauli xs =
--   let 
--       freeVars :: [Var a]
--       freeVars = nub $ concatMap snd xs -- Collect Vars and remove duplicates

--       totalVarCount = length freeVars
--   in
--   map (second (map (toPauli totalVarCount))) xs

-- scale :: ... -> Scaled a -> Scaled a
-- scale :: ... -> Sum a -> Sum (Scaled a)

class Scalable a b where
  scale :: Complex Double -> a -> b

instance Scalable (Scaled PauliExpr) (Scaled PauliExpr) where
  scale k (Scale k' x) = scale (k * k') x

instance Scalable a (Scaled a) => Scalable (Scaled (Summed a)) (Summed (Scaled a)) where
  scale k (Scale k' x) = scale (k * k') x

instance Scalable PauliExpr (Scaled PauliExpr) where
  scale = Scale

instance Scalable a b => Scalable (Summed a) (Summed b) where
  scale k (Summed xs) = Summed $ map (scale k) xs

instance Scalable (Tensor a) (Scaled (Tensor a)) where
  scale = Scale

instance Scalable (Scaled (Tensor a)) (Scaled (Tensor a)) where
  scale k (Scale k' x) = Scale (k * k') x

instance Scalable (Scaled a) (Scaled a) => Scalable (Tensor (Scaled a)) (Tensor (Scaled a)) where
  scale k (Tensor xs) = Tensor $ map (scale k) xs

tensor :: [ScaledPauli] -> Scaled (Tensor PauliExpr)
tensor xs = scale (product (map getScalars xs)) (Tensor (map getVec xs))
  where
    getScalars (Scale k _) = k
    getVec (Scale _ x) = x

floatScalars :: Tensor ScaledPauli -> Scaled (Tensor PauliExpr)
floatScalars = tensor . coerce

-- instance Scalable a b => Scalable (Tensor a) (Tensor b) where
--   scale _ (Tensor []) = Tensor []
--   scale k (Tensor (x:xs)) = Tensor (scale k x : map (scale 1) xs)

-- scale :: ScaledPauli -> ScaledPauli
-- scale = undefined

add :: ScaledPauli -> ScaledPauli -> Summed ScaledPauli
add x y = Summed [x, y]

sub :: ScaledPauli -> ScaledPauli -> Summed ScaledPauli
sub x y = add x (scale (-1) y)

pauliZ :: VarId -> ScaledPauli
pauliZ x = scale 1 (Z x)

pauliI :: VarId -> ScaledPauli
pauliI x = scale 1 (I x)

toPauli :: Int -> Int -> (VarId -> Tensor (Summed ScaledPauli))
toPauli totalChoiceCount i
  | i > totalChoiceCount = error "toPauli: i > totalChoiceCount"
  | i >= length allBitStrings = error "toPauli: i >= length allBitStrings"
  | otherwise = \var -> Tensor $ map ($ var) (allBitStrings !! i)
  where
    -- tensorBitString = foldr1 (liftA2 Tensor)

    pos, neg :: VarId -> Summed ScaledPauli
    pos x = scale (1/2) (sub (pauliZ x) (pauliI x)) --(Sub (Z x) (I x))
    neg x = scale (1/2) (add (pauliZ x) (pauliI x)) --(Add (Z x) (I x))

    allBitStrings = replicateM bitSize [pos, neg]

    bitSize = neededBitSize totalChoiceCount

-- pauliZ :: Matrix (Complex Double)
-- pauliZ =
--   (2><2)
--   [ 1, 0
--   , 0, -1
--   ]

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

