{-# LANGUAGE DeriveFunctor #-}

module Example
  where

import Eval2

import Data.Bifunctor

data Adjacent a = Adjacent a a
  deriving (Show, Functor)

type Graph' a = [Adjacent a]

{--

ghci> Example.graphPartition [Adjacent 1 2, Adjacent (-1) 1, Adjacent 2 (-1)]
1

--}

-- TODO: Is this correct? What about H_A?
graphPartition :: Graph' Int -> Int
graphPartition gr = solveF choices
  where
    calculateAdjacent :: Adjacent Int -> Int
    calculateAdjacent (Adjacent su sv) = (1 - (su * sv)) `div` 2

    choices :: [[(Int, Int)]]
    choices = map (map (second calculateAdjacent)) adjacentChoices

    adjacentChoices :: [[(Int, Adjacent Int)]]
    adjacentChoices = generateChoices 1 (-1) gr

