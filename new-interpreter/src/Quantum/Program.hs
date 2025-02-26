{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Woverlapping-patterns #-}

module Quantum.Program
  (Program (..)
  ,Var
  ,getVarPayload
  ,solveProgram
  )
  where

import Control.Monad.State
import Control.Monad

import Data.Functor

import Data.Foldable
import Data.List (nub, partition)

import Numeric.LinearAlgebra hiding ((<>), toList)
import qualified Numeric.LinearAlgebra as Matrix
import Data.Complex
import Data.Bifunctor (first, second)

type VarId = Int

data PauliExpr = I VarId | Z VarId | Add PauliExpr PauliExpr | Scale (Complex Double) PauliExpr | Tensor PauliExpr PauliExpr

pattern Sub x y = Add x (Scale (-1) y)

-- interpPauliExpr :: PauliExpr -> Pauli
-- interpPauliExpr I = ident 2
-- interpPauliExpr Z = pauliZ
-- interpPauliExpr (Add x y) = interpPauliExpr x + interpPauliExpr y
-- interpPauliExpr (Sub x y) = interpPauliExpr x - interpPauliExpr y
-- interpPauliExpr (Scale k x) = scalar k * (interpPauliExpr x)
-- interpPauliExpr (Tensor x y) = interpPauliExpr x Matrix.<> interpPauliExpr y

isPauliCombinable :: PauliExpr -> PauliExpr -> Bool
isPauliCombinable = undefined

needsParens :: PauliExpr -> Bool
needsParens I{} = False
needsParens Z{} = False
needsParens (Sub {}) = True
needsParens (Add {}) = True
needsParens (Tensor {}) = True
needsParens (Scale {}) = False

parens :: String -> String
parens x = "(" ++ x ++ ")"

showParens :: PauliExpr -> String
showParens e =
  if needsParens e
  then parens (show e)
  else show e

instance Show PauliExpr where
  show (I i) = "I(" ++ [("xyz" ++ ['a'..'w']) !! i] ++ ")"
  show (Z i) = "Z(" ++ [("xyz" ++ ['a'..'w']) !! i] ++ ")"
  show (Sub x y) = showParens x ++ " - " ++ showParens y
  show (Add x y) = showParens x ++ " + " ++ showParens y
  show (Tensor x y) = showParens x ++ " ⊗ " ++ showParens y
  show (Scale k x) = prettyShow k ++ " " ++ showParens x
    where
      prettyShow (a :+ 0) = show a
      prettyShow (0 :+ b) = show b ++ "i"
      prettyShow (a :+ b) = parens (show a ++ " + " ++ show b ++ "i")

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

solveProgram :: forall t a b. (Eq a, Eq b, Traversable t) =>
  Program t a b ->
  [(a, [PauliExpr])]
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

      encodedChoices :: [(b, VarId -> PauliExpr)]
      encodedChoices = encodeChoices (choices prog)

      results :: [(a, [(Var a, b)])]
      results = map (\x -> (constraints prog (map (first getVarPayload) x), x))
                    actualPairs

      decode :: (Var a, b) -> PauliExpr
      decode (var, choice) = decodeChoice encodedChoices choice (getVarId var)

      results2 :: [(a, [PauliExpr])]
      results2 = map (\(x, varChoices) -> (x, map decode varChoices)) results
  in
  results2

simplify :: Real a => [(a, [PauliExpr])] -> [(a, [PauliExpr])]
simplify =
  map (second (concatMap expand)) -- Add (I x) (Z y) ==> [I x, Z y]
  . combineLikePauli              -- [(0, xs), (1, ys), (0, zs)] ==> [(0, xs ++ zs), (1, ys)]

-- | combineLike (\(x, _) (y, _) -> x == y)
--               (<>)
--               [(0, xs), (1, ys), (0, zs)]
--     ==>
--   [(0, xs <> zs), (1, ys)]
combineLike ::
  (a -> a -> Bool) ->
  (a -> [a] -> b) ->
  [a] ->
  [b]
combineLike isLike combine = go 
  where
    go [] = []
    go (x:xs) =
      let (likes, notLikes) = partition (isLike x) xs
          newX = combine x likes
      in
      newX : go notLikes

-- | groupResults [(0, xs), (1, ys), (0, zs)] ==> [(0, xs ++ zs), (1, ys)]
groupFirsts :: Eq a => [(a, [PauliExpr])] -> [(a, [PauliExpr])]
groupFirsts = combineLike (\(x, _) (y, _) -> x == y) (\(x, pauli) likes -> (x, pauli ++ concatMap snd likes))

combineLikePauli :: (Eq a, Real a) => [(a, [PauliExpr])] -> [(a, [PauliExpr])]
combineLikePauli = undefined

combinePauli :: [PauliExpr] -> [PauliExpr]
combinePauli = undefined

expand :: PauliExpr -> [PauliExpr]
expand (Add x y) = expand x ++ expand y
expand x = [x]

distributeScalarMult :: PauliExpr -> PauliExpr
distributeScalarMult (Scale k x) = distributeWithScalar k x
distributeScalarMult (Add x y) = distributeScalarMult x
distributeScalarMult (Tensor x y) = Tensor (distributeScalarMult x) (distributeScalarMult y)
distributeScalarMult (I x) = I x
distributeScalarMult (Z x) = Z x

distributeWithScalar :: Complex Double -> PauliExpr -> PauliExpr
distributeWithScalar k (Add x y) = Add (distributeWithScalar k x) (distributeWithScalar k y)
distributeWithScalar k1 (Scale k2 x) = distributeWithScalar (k1 * k2) x
distributeWithScalar k x = Scale k x

encodeChoices :: [a] -> [(a, VarId -> PauliExpr)]
encodeChoices choices = zipWith (\choice i -> (choice, toPauli choiceCount i)) choices [0..]
  where
    choiceCount = length choices

decodeChoice :: Eq a => [(a, s -> t)] -> a -> s -> t
decodeChoice encodedChoices choice var =
  case lookup choice encodedChoices of
    Just pauliFn -> pauliFn var

-- type Pauli = Matrix (Complex Double)

-- varsToPauli :: forall a. Eq a => [(a, [Var a])] -> [(a, [PauliExpr])]
-- varsToPauli xs =
--   let 
--       freeVars :: [Var a]
--       freeVars = nub $ concatMap snd xs -- Collect Vars and remove duplicates

--       totalVarCount = length freeVars
--   in
--   map (second (map (toPauli totalVarCount))) xs

toPauli :: Int -> Int -> (VarId -> PauliExpr)
toPauli totalChoiceCount i
  | i > totalChoiceCount = error "toPauli: i > totalChoiceCount"
  | i >= length allBitStrings = error "toPauli: i >= length allBitStrings"
  | otherwise = tensorBitString (allBitStrings !! i)
  where
    tensorBitString = foldr1 (liftA2 Tensor)

    pos x = Scale (1/2) (Sub (Z x) (I x))
    neg x = Scale (1/2) (Add (Z x) (I x))

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

