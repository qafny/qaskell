{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSL.Solve
  (Tree (..)
  ,Weighted (..)
  ,foldWeighted
  ,getWeight
  ,IntWeighted
  ,unWeighted
  ,generateChoices
  ,sqrtEnergy
  ,energy
  ,solveF
  ,ChoiceStrategy
  )
  where

import Control.Monad
import Data.Proxy
import Data.List (permutations)
import DSL.AdjMatrix
import Lib (Choice(Choice))

------------------------------------------------------------------------------
-- We are given some traversable data structure. Basic examples include
-- the list type [a] from the Haskell library and trees as defined below

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a) 
  fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)

instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Node t1 t2) = foldr f (foldr f b t2) t1

instance Traversable Tree where
  traverse f (Leaf a) = pure Leaf <*> f a
  traverse f (Node t1 t2) = pure Node <*> traverse f t1 <*> traverse f t2

data Weighted a b =
  Weighted
    { wWeight :: a
    , wValue :: b
    }
  deriving (Functor, Show)

foldWeighted :: (a -> b -> r) -> Weighted a b -> r
foldWeighted f (Weighted w v) = f w v

getWeight :: Weighted a b -> a
getWeight = foldWeighted (\x _ -> x)

unWeighted :: IntWeighted a -> a
unWeighted (Weighted _ value) = value

type IntWeighted = Weighted Int

------------------------------------------------------------------------------
-- We want to create choices at each position in a data structure
-- and from that, create a choice of data structures.

-- This is the basic abstraction of "traverse". We will traverse the
-- data structure with a MonadPlus operation representing (weighted) choices.
-- For now we use integers to represent the weights.

type ChoiceStrategy m t a b = t a -> m (t b)

generateChoices :: (Monad m, Traversable t) =>
                   ChoiceStrategy m t a b -> t a -> m (t b)
generateChoices strategy struct = strategy struct

-- generateChoices :: (MonadPlus m, Traversable t) => 
--                    Int -> Int -> t a -> m (t (IntWeighted a))
-- generateChoices d1 d2 struct = generateChoicesFromList [d1, d2] struct

-- generateChoicesFromList :: (MonadPlus m, Traversable t) => 
--                    [b] -> t a -> m (t (Weighted b a))
-- generateChoicesFromList ds struct =
--   traverse (\a -> msum (map (go a) ds)) struct
--   where
--     go a d = return (Weighted d a)

-- Specialized for graph isomorphism
-- generateChoicesForIsomorphism :: (MonadPlus m) => Int -> m [IntWeighted Int]
-- generateChoicesForIsomorphism numNodes =
--   let perms = permutations [0 .. numNodes - 1]
--   in msum (map (\perm -> return (zipWith Weighted perm [0 .. numNodes - 1])) perms)

-- Now that we generated all the choices we need to fold over the choices
-- to choose the "minimum" one or more generally the one(s) satisfying
-- the desired constraints

sqrtEnergy :: Foldable t => t (Int,Int) -> Int
sqrtEnergy ch = foldr (\(d,a) s -> s + d*a) 0 ch

energy :: Foldable t => t (Int,Int) -> Int
energy = square . sqrtEnergy

square :: Num a => a -> a
square x = x * x

solveF :: (Foldable f, Ord a) =>
  f a -> a
solveF = minimum
