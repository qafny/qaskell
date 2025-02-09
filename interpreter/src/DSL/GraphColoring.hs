{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module DSL.GraphColoring
  where

import Data.Proxy

import Control.Monad
import Data.List

import DSL.AdjMatrix
import DSL.Solve

import DSL.Syntax

graphColoring :: forall color m a. (Eq color, Foldable m, MonadPlus m) => Proxy m -> [color] -> AdjMatrix a -> Int
graphColoring Proxy colors0 adj =
  let
    nodes :: [()]
    nodes = getNodes adj

      ---- Step 1: State preparation
    choices :: m [((), [color])]
    choices = generateChoices @m (defaultListStrategy (allCombinations colors0)) nodes

    calcHA ((), colors) = square (1 - length colors)

    calcHB (((), colors1), ((), colors2)) =
      let commonColors = intersect colors1 colors2
      in
      length commonColors

      ---- Step 2: Traversal
    computeComponents :: [((), [color])] -> Int
    computeComponents nodeWeights =
      let adj' :: AdjMatrix (((), [color]), ((), [color]))
          adj' = updateNodeContents' adj nodeWeights -- TODO: Does this make sense in the quantum setting?
      in
      sum (fmap calcHA nodeWeights) + sum (fmap calcHB adj')

    components = fmap computeComponents choices
  in
      ---- Step 3: Minimization
  solveF @m components
  where
    square x = x * x

allCombinations :: [a] -> [[a]]
allCombinations = subsequences

graphColoringGeneral :: forall color m a. (Eq color, Foldable m, MonadPlus m) =>
  Proxy m -> Expr [color] -> Expr (AdjMatrix a) -> Super (Expr Int)
graphColoringGeneral Proxy colors0 adj =
  let
    nodes :: Expr [()]
    nodes = getList_ adj

      ---- Step 1: State preparation
    choices :: m [((), [color])]
    choices = generateChoices @m (defaultListStrategy (allCombinations colors0)) nodes

    calcHA ((), colors) = square (1 - length colors)

    calcHB (((), colors1), ((), colors2)) =
      let commonColors = intersect colors1 colors2
      in
      length commonColors

      ---- Step 2: Traversal
    computeComponents :: [((), [color])] -> Int
    computeComponents nodeWeights =
      let adj' :: AdjMatrix (((), [color]), ((), [color]))
          adj' = updateNodeContents adj nodeWeights -- TODO: Does this make sense in the quantum setting?
      in
      sum (fmap calcHA nodeWeights) + sum (fmap calcHB adj')

    components = fmap computeComponents choices
  in
  -- Solve undefined
  where
    square x = x * x

