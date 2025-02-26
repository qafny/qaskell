module Quantum.Examples where

import Quantum.Program
import Quantum.ExampleData

import Data.List (nub)

-- ghci> solveProgram (eqSum [1, 2])
-- ...
eqSum ::
  [Int] -> Program [] Int Int
eqSum inputList =
  Program
    { choices = [0, 1]
    , struct = inputList
    , view = 2
    , constraints = \[(a, choiceA), (b, choiceB)] ->
        (a * choiceA)
          +
        (b * choiceB)
    }

-- ghci> solveProgram (graphColoring 2 graph1)
-- ...
graphColoring ::
  Int -> [(Int, Int)] -> Program [] Int Int
graphColoring colorCount edges = 
  Program
    { choices = [0..colorCount-1]
    , struct = getNodes edges
    , view = 2
    , constraints = go
    }
  where
    go :: [((Int, Int))] -> Int
    go [(a, choiceA), (b, choiceB)] =
      if (a, b) `elem` edges && choiceA == choiceB
      then 1
      else 0

cliqueFinding ::
  Int -> AdjList Int -> Program [] Int Int
cliqueFinding cliqueSize edges =
  Program
    { choices = [0, 1]
    , struct = getNodes edges
    , view = 2
    , constraints = \[(a, choiceA), (b, choiceB)] ->
        if (a, b) `elem` edges
        then 0
        else choiceA * choiceB
    }

data Cover a = MkCover { vars :: [a], valuation :: [a] -> Bool }

exactCover :: Cover Int -> Program [] Int Int
exactCover cover =
  Program
    { choices = [0, 1],
      struct = vars cover,
      view = 3,
      constraints =
        \[(a, choiceA), (b, choiceB), (c, choiceC)] ->
          if valuation cover [a, b, c] && choiceA + choiceB + choiceC == 1
          then 0
          else 1
    }

-- | Get the nodes from an adjacency list
getNodes :: Eq a => [(a, a)] -> [a]
getNodes = nub . concatMap (\(x, y) -> [x, y])

