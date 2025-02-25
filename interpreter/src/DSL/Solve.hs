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
  ,defaultListStrategy
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

-- element in a data-structure t a, t is a tree, and a is the node
-- classical case
a : vertex

t a : data-tructure frame

b : a single choice

m b : a list of choices

Given a set of choices, and a data-structure frame (t a), we generate m (t b), a list of data-structure choices

-- quantum case
a : vertex

t a : data-structure frame

b : a single ket choice |0> , |1>, the 0 and 1 could be symbolic

m b : a superposition holder: Sum b (guard b)

guard: a function (a -> b -> bool), Sum b (a function apply on b)

-- energy association





type ChoiceStrategy m t a b = t a -> m (t b)

generateChoices :: (Monad m, Comonad t, Traversable t) => (a -> m b) -> t a -> m (t b)
generateChoices strategy struct = strategy struct

defaultListStrategy :: (Traversable t, MonadPlus m) => [b] -> ChoiceStrategy m t a (a, b)
defaultListStrategy ds = traverse (\a -> msum (map (go a) ds))
  where
    go a d = return (a, d)

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




--We convert haskell choices [([(a,Var b)], Integer)] to [(Real, [Pauli String])]

countAux: [b] -> Integer -> [(b,Integer)]
countAux [] n = a
countAux (x:xs) n = (x,n):counts xs (n+1)

counts l = countAux l 0

data Pauli x = I x | Z x | X x | Y x

type Pau a = (Real, a)



findInteger [(b,Integer)] -> b -> Maybe Integer
findInteger ds d = do da,v <- ds
                      if da == d then Just v else None

genChoice n [] = []
genChoice n (x:xs) = if n `mod` 2 == 0
                     then [(1/2, I ((var (snd x)))), (-1/2, Z ((var (snd x))))] : genChoice (n `div` 2) (m-1)
                     else [(1/2, I ((var (snd x)))), (1/2, Z ((var (snd x))))] : genChoice (n `div` 2) (m-1)

convertAux cs ds = do a,c <- cs
                     case findInteger ds (choice c) of
                     Just v -> return genChoice v ds
                     None -> []

-- [[(Real, Pauli String)]] -> [(Real, [Pauli String])]
expand [] = []
expand (x:l) = do (a, v) <- x
                  (ar, vl) <- expand l
                  return (a * ar, v:vl)

combineAux x [] = x
combineAux x (y:l) = if snd x == snd y then (fst x + fst y, snd x):l else y:(combineAux x l)

combine [] = []
combine (x:l) = combine (combineAux x l)

clean [] = []
clean ((a,b):l) = if a == 0.0 then clean l else (a,b):(clean l)

insertValue v [] = []
insertValue v ((a,b):xs) = (a*v,b):(insertValue v xs)


-- [([(a,Var b)], Integer)] -> [b] -> [(Real, [Pauli String])]

convert l ds = let dsa = counts ds
               foldl (\b (cs,v) -> insertValue v $ clean $ combine $ expand (convertAux cs dsa)) [] l
