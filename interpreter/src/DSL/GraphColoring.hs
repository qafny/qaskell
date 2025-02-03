{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module DSL.GraphColoring
  where

import Data.Proxy

import Control.Monad
import Data.List

import DSL.AdjMatrix
import DSL.Solve

graphColoring :: forall color m a. (Eq color, Foldable m, MonadPlus m) => Proxy m -> [color] -> AdjMatrix a -> Int
graphColoring Proxy colors adj =
  let
    nodes :: [()]
    nodes = getNodes adj

    choices :: m [((), [color])]
    choices = generateChoices @m (defaultListStrategy (allCombinations colors)) nodes

    calcHA ((), colors) = square (1 - length colors)

    calcHB (((), colors1), ((), colors2)) =
      let commonColors = intersect colors1 colors2
      in
      length commonColors

    computeComponents :: [((), [color])] -> Int
    computeComponents nodeWeights =
      let adj' :: AdjMatrix (((), [color]), ((), [color]))
          adj' = updateNodeContents adj nodeWeights -- TODO: Does this make sense in the quantum setting?
      in
      sum (fmap calcHA nodeWeights) + sum (fmap calcHB adj')

    components = fmap computeComponents choices
  in
  solveF @m components
  where
    square x = x * x

allCombinations :: [a] -> [[a]]
allCombinations = subsequences

