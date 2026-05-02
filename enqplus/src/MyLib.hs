{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module MyLib (someFunc) where

import Control.Monad.State
import Control.Monad

import Data.Functor
import Data.Coerce

import Data.Foldable
import Data.List (partition, intersperse)

import Numeric.LinearAlgebra hiding ((<>), toList, scale, add)
import Data.Bifunctor (first, second)

import Data.Bits (testBit)

import qualified Data.Map.Strict as M

import DistinctDepthN

debugSolver :: Bool
debugSolver = False

type VarId = Int

data PauliExpr = I VarId | Z VarId | X VarId | Y VarId
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

data Var a = Var a VarId
  deriving (Show, Eq, Ord, Functor)

choice :: Var a -> a
choice (Var x _) = x

var :: Var a -> VarId
var (Var _ i) = i

data CType = St | Dt | Tt CType
   deriving (Show, Eq)

-- input is a pair of choice and datapoints
-- can also add bound for the Int
data XCon = Forall (Int ->  XCon) | Forallb Int (Int -> XCon)
               | Eqs [Int] -- [x + m = y]
               | XAnd XCon XCon | XSum Int

data Constraint input a b ch t = Next (input -> Constraint input a b ch t) | Swap (XCon a b) | Stay ch

-- define choice of one, X
contraint0 = Next (\(a,aChoice) -> Swap (Forall (\n -> [aChoice + n])))

-- define choice of two
contraintA = Next (\(a,aChoice) -> Next (\(b,bChoice) -> Swap (Forall (\ n -> [aChoice+n,bChoice]))))

-- define choices of three
contraintB = Next (\(a,aChoice) -> Next (\(b,bChoice) -> (Next (\(c,cChoice) 
                     -> Swap (Forall (\ n -> (Forall (\m -> [aChoice + n,bChoice+m,cChoice]))))))))

--define the energy cost for edges
contraintC = Next (\(a,aChoice) -> Next (\(b,bChoice) ->
      Swap (And (Forall (\ n -> [aChoice+n,bChoice])) (XSum (if aChoice == 3 && bChoice == 5 then 10 else 1)))))

--define the energy cost for Hamiltonian cycle
contraintD = Next (\(a,rankA) -> Next (\(b,rankB) ->
      let isAdjacentRank = abs (rankA - rankB) == 1 || abs (rankA - rankB) == numNodes -1 
      in
      Stay (
      if isAdjacentRank
      then if (cityA, cityB) `elem` edges 
                then 0 
                else 5
      else 0)))

data Program t a b c =
  Program
    { choices :: [b]
    , struct :: t a
    , constraints :: [Constraint (a,b) (c,[b]) c (t a)]
    }


