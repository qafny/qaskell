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

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

import qualified Data.Map.Strict as M

debugSolver :: Bool
debugSolver = False

type VarId = Int

data PauliExpr = I VarId | X VarId | Y VarId | Z VarId
  deriving (Eq, Ord)

data Scaled a = Scale (Complex Double) a
  deriving (Functor, Eq)

newtype Tensor a = Tensor [a]
  deriving (Functor, Eq, Foldable, Traversable, Ord)

newtype Summed a = Summed [a]
  deriving (Functor, Applicative)

instance Eq a => Eq (Summed a) where
  Summed xs == Summed ys =
      all (\x -> count' x xs == count' x ys) xs
    where
      count' x = length . filter (== x)

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
  show (X i) = "X(" ++ [['a'..'z'] !! i] ++ ")"
  show (Y i) = "Y(" ++ [['a'..'z'] !! i] ++ ")"

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

-- data Constraint t a b c = 
--     -- Z axis (H_P)
--     Stay { choices :: [b]
--          , struct :: t a
--          , view :: Int
--          , constraints :: t (a, b) -> c
--          } 
--     -- X axis (H_B)
--   | Swap { choices :: [b]    -- Needed so the compiler knows the bit-width of the registers
--          , struct :: t a     -- The variables that are allowed to swap (e.g., all cities)
--          , view :: Int       -- Usually 2, meaning "swap pairs of variables"
--          , rate :: c         -- The coefficient of the swap (usually -1.0)
--          }

-- -- A Program is now just a collection of the stay/swap constraints
-- newtype Program t a b c = Program [Constraint t a b c]

genChoices :: Traversable t =>
  t a -> Fresh (t (Var a))
genChoices = traverse (\x -> Var x <$> fresh)

minimumsFst :: Ord a => [(a, b)] -> [(a, b)]
minimumsFst [] = []
minimumsFst xs = filter ((==) minfst . fst) xs
    where minfst = minimum (map fst xs)

count :: (Functor t, Foldable t) =>
  (a -> Bool) ->
  t a ->
  Int
count p = sum . fmap go
  where
    go x =
      if p x
      then 1
      else 0

hasNOnes :: (Num b, Eq b, Functor t, Foldable t) =>
  Int ->
  t (a, b) ->
  Bool
hasNOnes n s = count go s == n
  where
    go (_, x) = x == 1

solveClassical :: forall t a b c. (Eq (t a), Ord (t (Var a)), Part (t (Var a)), Eq a, Eq b, Real c, Traversable t) =>
  (t (a, b) -> Bool) ->
  Program t a b c ->
  [(c, t (a, b))]
solveClassical p prog =
  let
     varStruct = runFresh (genChoices (struct prog))

     tuples = distinctNTuples (view prog) varStruct

     actualTuples = assignChoices (choices prog) tuples

     encodedChoices = createChoices (choices prog) varStruct

     results =
          minimumsFst $ filter (p . snd) $ encodedChoices <&>
                  (\ aChoice -> (sum $ actualTuples <&>
                        (\ aTuple -> if isSubList aTuple (toList aChoice)
                                     then (constraints prog (fmap (first choice) aTuple))
                                     else 0)
                                  ,fmap (first choice) aChoice) )
               where isSubList xs ys = all (`elem` ys) xs
  in results

-- solveClassical :: forall t a b c. (Eq (t a), Ord (t (Var a)), Part (t (Var a)), Eq a, Eq b, Real c, Traversable t, Foldable t) =>
--   (t (a, b) -> Bool) ->
--   Program t a b c ->
--   [(c, t (a, b))]
-- solveClassical p (Program constraintsList) =
--   let
--      -- Extract classical constraints
--      stays = [ c | c@(Stay {}) <- constraintsList ]
--      baseStay = head stays
     
--      varStruct = runFresh (genChoices (struct baseStay))
--      tuples = distinctNTuples (view baseStay) varStruct
--      actualTuples = assignChoices (choices baseStay) tuples
--      encodedChoices = createChoices (choices baseStay) varStruct

--      -- Evaluate a single constraint block
--      evalStay prog' aChoice =
--        sum $ actualTuples <&> (\aTuple -> 
--          if isSubList aTuple (toList aChoice)
--          then constraints prog' (fmap (first choice) aTuple)
--          else 0)

--      isSubList xs ys = all (`elem` ys) xs

--      results =
--           minimumsFst $ filter (p . snd) $ encodedChoices <&>
--                   (\ aChoice -> ( sum (map (`evalStay` aChoice) stays)
--                                 , fmap (first choice) aChoice) )
--   in results

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

-- solveQuantum :: forall t a b c. (Ord (t (Var a)), Part (t (Var a)), Eq a, Eq b, Real c, Traversable t, Foldable t) =>
--   Program t a b c ->
--   Summed (Scaled (Tensor PauliExpr))
-- solveQuantum (Program constraintsList) =
--    let
--       toComplex' :: c -> Complex Double
--       toComplex' = fromRational . toRational

--       combineSums :: [Summed (Scaled (Tensor PauliExpr))] -> Summed (Scaled (Tensor PauliExpr))
--       combineSums = joinSummed . Summed

--       -- 1. Compile Z-Basis Penalties (H_P)
--       compileConstraint :: Constraint t a b c -> Summed (Scaled (Tensor PauliExpr))
--       compileConstraint prog@(Stay {}) =
--          let
--             varStruct = runFresh (genChoices (struct prog))
--             pairs = distinctNTuples (view prog) varStruct
--             actualTuples = assignChoices (choices prog) pairs
--             encodedChoices = encodeChoices (choices prog)
--             decode (x, c) = decodeChoice encodedChoices c (var x)
            
--             constraintResults = map (\x -> (constraints prog (fmap (first choice) x), x)) actualTuples
            
--             decodedAndDistributed = fmap (\(x, varChoices) -> (x, fmap (fmap floatScalars . distr . decode) varChoices)) constraintResults
--             builtTensor = map (second (Tensor . toList)) decodedAndDistributed
--             distributedSummedTensor = map (second distr) builtTensor
--             commuted = map (second (fmap commuteScaledTensor)) distributedSummedTensor
--             complexCoeffs = map (first toComplex') commuted
--             scaled = map (\(k, x) -> fmap (scale k) x) complexCoeffs
--          in combineSums scaled

--       -- 2. Compile X/Y stuff (H_B)
--       compileConstraint prog@(Swap {}) =
--          let
--             varStruct = runFresh (genChoices (struct prog))
--             pairs = distinctNTuples (view prog) varStruct
--             d = neededBitSize (length (choices prog))
--             r = toComplex' (rate prog)

--             buildSwap tuple =
--               let vars = toList tuple
--               in if length vars == 2
--                  then let qA = var (vars !! 0) * d
--                           qB = var (vars !! 1) * d
--                       in fmap (scale r) (generalizedTransferOp d qA qB 1)
--                  else error "Swap constraint requires view = 2"
--          in combineSums (map buildSwap pairs)

--    in optimize $ combineSums (map compileConstraint constraintsList)
--    where
--      -- Maintain the original optimize helper block inside the where clause
--      optimize :: forall x. (ShowParens x, Ord x) => Summed (Scaled x) -> Summed (Scaled x)
--      optimize x =
--        let y = combine x
--            y' = combine' x
--        in if debugSolver && y /= y'
--             then error $ "combine incorrect: " ++ show (y, y')
--             else clean y

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

pos :: VarId -> Summed ScaledPauli
pos v = scaleSummed 0.5 (sub (pauliI v) (pauliZ v)) -- |1> projector

neg :: VarId -> Summed ScaledPauli
neg v = scaleSummed 0.5 (add (pauliI v) (pauliZ v)) -- |0> projector

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

pauliX :: VarId -> ScaledPauli
pauliX x = Scale 1 (X x)

pauliY :: VarId -> ScaledPauli
pauliY x = Scale 1 (Y x)

-- a = 0.5(X + iY)
anni :: VarId -> Summed ScaledPauli
anni q = Summed [Scale 0.5 (X q), Scale (0 :+ 0.5) (Y q)]

-- a* = 0.5(X - iY)
crea :: VarId -> Summed ScaledPauli
crea q = Summed [Scale 0.5 (X q), Scale (0 :+ (-0.5)) (Y q)]

toBinString :: Int -> Int -> String
toBinString len val =
  let bs  = showIntAtBase 2 intToDigit val ""
      pad = replicate (len - length bs) '0'
  in pad ++ bs

buildTransition :: VarId -> String -> String -> Tensor (Summed ScaledPauli)
buildTransition startQ xStr yStr =
  Tensor $ zipWith (\i (bx, by) -> opForBit (startQ + i) bx by) [0..] (zip xStr yStr)
  where
    opForBit q '0' '0' = neg q
    opForBit q '1' '1' = pos q
    opForBit q '0' '1' = crea q
    opForBit q '1' '0' = anni q
    opForBit _ _ _     = error "Invalid bit encountered"

multTensor :: Tensor a -> Tensor a -> Tensor a
multTensor (Tensor xs) (Tensor ys) = Tensor (xs ++ ys)

-- | a*(m) |n> = |n+m>
generalizedCreation :: Int -> VarId -> Int -> Summed (Scaled (Tensor PauliExpr))
generalizedCreation d startIdx m =
  let validNs = [0 .. (2^d) - 1 - m]
      buildTerm n =
        let xStr = toBinString d n
            yStr = toBinString d (n + m)
        in fmap floatScalars (distr (buildTransition startIdx xStr yStr))
  in joinSummed $ Summed (map buildTerm validNs)

-- | a(m) |n> = |n-m>
generalizedAnnihilation :: Int -> VarId -> Int -> Summed (Scaled (Tensor PauliExpr))
generalizedAnnihilation d startIdx m =
  let validNs = [m .. (2^d) - 1]
      buildTerm n =
        let xStr = toBinString d n
            yStr = toBinString d (n - m)
        in fmap floatScalars (distr (buildTransition startIdx xStr yStr))
  in joinSummed $ Summed (map buildTerm validNs)

-- | Multiplies two disjoint operator trees (like A ⊗ B)
multOp :: Summed (Scaled (Tensor PauliExpr)) -> Summed (Scaled (Tensor PauliExpr)) -> Summed (Scaled (Tensor PauliExpr))
multOp (Summed xs) (Summed ys) = Summed
  [ Scale (k1 * k2) (multTensor t1 t2)
  | Scale k1 t1 <- xs
  , Scale k2 t2 <- ys
  ]

-- | Transfer 'm' units between Register A and Register B
-- | Transfer 'm' units between Register A and Register B
generalizedTransferOp :: Int -> VarId -> VarId -> Int -> Summed (Scaled (Tensor PauliExpr))
generalizedTransferOp d startA startB m =
  let a_dag_B = generalizedCreation d startB m
      a_A     = generalizedAnnihilation d startA m
      
      a_B     = generalizedAnnihilation d startB m
      a_dag_A = generalizedCreation d startA m
      
      -- a*_B a_A  (Move m from A to B)
      term1 = multOp a_dag_B a_A
      
      -- a_B a*_A  (Move m from B to A)
      term2 = multOp a_B a_dag_A
      
      -- Extract the raw lists and combine them
      Summed t1List = term1
      Summed t2List = term2
      
  in Summed (t1List ++ t2List)

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
  -- where
  --   pos v = scaleSummed (0.5) (sub (pauliI v) (pauliZ v)) -- |1> projector
  --   neg v = scaleSummed (0.5) (add (pauliI v) (pauliZ v)) -- |0> projector

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

-- Initial Hamiltonian using Swaps
swapHamiltonian :: forall t a b c. (Ord (t (Var a)), Part (t (Var a)), Traversable t, Foldable t) =>
  Program t a b c ->
  Summed (Scaled (Tensor PauliExpr))
swapHamiltonian prog =
   let
      varStruct = runFresh (genChoices (struct prog))
      pairs = distinctNTuples 2 varStruct
      d = neededBitSize (length (choices prog))
      r = (-1.0) :+ 0.0

      buildSwap tuple =
        let vars = toList tuple
        in if length vars == 2
           then let qA = var (vars !! 0) * d
                    qB = var (vars !! 1) * d
                in fmap (scale r) (generalizedTransferOp d qA qB 1)
           else error "swapHamiltonian requires view = 2 internally"
   in joinSummed $ Summed (map buildSwap pairs)

-- Summation of X operators for all physical qubits in the system
uniformHamiltonian :: Int -> [VarId] -> Summed (Scaled (Tensor PauliExpr))
uniformHamiltonian totalChoiceCount vars =
  let 
      d = neededBitSize totalChoiceCount
      qubitIds = [ v * d + j | v <- vars, j <- [0..d-1] ]
  in 
      Summed [ Scale ((-1) :+ 0) (Tensor [X q]) | q <- qubitIds ]

-- Applies X to half the qubits, leaves the rest as Identity
brokenHamiltonian :: Int -> [VarId] -> Summed (Scaled (Tensor PauliExpr))
brokenHamiltonian totalChoiceCount vars =
  let 
      d = neededBitSize totalChoiceCount
      allQubits = [ v * d + j | v <- vars, j <- [0..d-1] ]
      
      -- Calculate the halfway point and take only the first half of the qubits
      halfCount = length allQubits `div` 2
      activeQubits = take halfCount allQubits
  in 
      -- Apply the X operator ONLY to the active half
      Summed [ Scale ((-1) :+ 0) (Tensor [X q]) | q <- activeQubits ]

-- -- | Combines the classical penalties (H_P) and the swap mixing (H_B)
-- fullQuantum :: forall t a b c. (Ord (t (Var a)), Part (t (Var a)), Eq a, Eq b, Real c, Traversable t, Foldable t) =>
--   Program t a b c ->
--   Summed (Scaled (Tensor PauliExpr))
-- fullQuantum prog =
--   let Summed hp = solveQuantum prog
--       Summed hb = swapHamiltonian prog
--   in Summed (hp ++ hb)

-- | Combines H_P with your choice of Mixing Hamiltonian
fullQuantum :: forall t a b c. (Ord (t (Var a)), Part (t (Var a)), Eq a, Eq b, Real c, Traversable t, Foldable t) =>
  Program t a b c ->
  Summed (Scaled (Tensor PauliExpr))
fullQuantum prog =
  let 
      Summed hp = solveQuantum prog
      varStruct = runFresh (genChoices (struct prog))
      vars = map var (toList varStruct)

      -- Option A: Run with the Uniform Hamiltonian 
      -- Summed hb = uniformHamiltonian (length (choices prog)) vars

      -- Option B: Run with the Swap Hamiltonian 
      Summed hb = swapHamiltonian prog

      -- Option C: Broken Hamiltonian
      -- Summed hb = brokenHamiltonian (length (choices prog)) vars

  in Summed (hp ++ hb)