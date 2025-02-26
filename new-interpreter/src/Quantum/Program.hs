{-# LANGUAGE ScopedTypeVariables #-}

module Quantum.Program
  (Program (..)
  ,Var
  ,getVarPayload
  ,solveProgram
  )
  where

import Control.Monad.State
import Control.Monad

import Data.Foldable
import Data.List (nub)

import Numeric.LinearAlgebra hiding ((<>), toList)
import qualified Numeric.LinearAlgebra as Matrix
import Data.Complex
import Data.Bifunctor (second)

type VarId = Int

data Var a = Var a VarId
  deriving (Show, Eq, Ord)

getVarPayload :: Var a -> a
getVarPayload (Var x _) = x

data Program t a =
  Program
    { choices :: [a]
    , struct :: t a
    , view :: Int
    , constraints :: [(Var a, a)] -> a
    }
generateVars :: Traversable t =>
  t a -> Fresh (t (Var a))
generateVars = traverse (\x -> Var x <$> fresh)

solveProgram :: forall t a. (Eq a, Traversable t) =>
  Program t a ->
  [(a, [Pauli])]
solveProgram prog =
  let
      varStruct :: t (Var a)
      varStruct = runFresh (generateVars (struct prog))

      pairs :: [[Var a]]
      pairs = distinctNTuples (view prog)
                              (toList varStruct)

      actualPairs :: [[(Var a, a)]]
      actualPairs = makeActualChoice (choices prog)
                                     pairs

      results :: [(a, [Var a])]
      results = map (\x -> (constraints prog x, map fst x))
                    actualPairs
  in
  varsToPauli results

type Pauli = Matrix (Complex Double)

varsToPauli :: forall a. Eq a => [(a, [Var a])] -> [(a, [Pauli])]
varsToPauli xs =
  let 
      freeVars :: [Var a]
      freeVars = nub $ concatMap snd xs -- Collect Vars and remove duplicates

      totalVarCount = length freeVars
  in
  map (second (map (toPauli totalVarCount))) xs

toPauli :: Int -> Var a -> Pauli
toPauli totalVarCount (Var _ x)
  | x > totalVarCount = error "compileVar: x > totalVarCount"
  | x >= length allBitStrings = error "compileVar: x >= length allBitStrings"
  | otherwise = tensorBitString (allBitStrings !! x)
  where
    tensorBitString = foldr1 (Matrix.<>)

    pos = (pauliZ - ident 2) / 2
    neg = (pauliZ + ident 2) / 2

    allBitStrings = replicateM bitSize [pos, neg]

    bitSize = neededBitSize totalVarCount

pauliZ :: Matrix (Complex Double)
pauliZ =
  (2><2)
  [ 1, 0
  , 0, -1
  ]

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

