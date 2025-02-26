module Quantum.Examples where

import Quantum.Program
import Quantum.ExampleData

import Data.List (nub)

-- ghci> solveProgram (eqSum [1, 2])
-- ...
eqSum ::
  [Int] -> Program [] Int
eqSum inputList =
  Program
    { choices = [0, 1]
    , struct = inputList
    , view = 2
    , constraints = \[(varX, choiceX), (varY, choiceY)] ->
        (choiceX * getVarPayload varX)
          +
        (choiceY * getVarPayload varY)
    }

-- ghci> solveProgram (graphColoring 2 graph1)
-- ...
graphColoring ::
  Int -> [(Int, Int)] -> Program [] Int
graphColoring colorCount edges = 
  Program
    { choices = [0..colorCount-1]
    , struct = getNodes edges
    , view = 2
    , constraints = go
    }
  where
    go :: [((Var Int, Int))] -> Int
    go [(varA, choiceA), (varB, choiceB)] =
      let a = getVarPayload varA
          b = getVarPayload varB
      in
      if (a, b) `elem` edges && choiceA == choiceB
      then 1
      else 0

-- cliqueFinding ::
--   Int -> AdjList Int -> Program [] Int
-- cliqueFinding cliqueSize edges =
--   Program
--     { choices = [0, 1]
--     , struct = getNodes edges
--     , view = 
--     }

-- | Get the nodes from an adjacency list
getNodes :: Eq a => [(a, a)] -> [a]
getNodes = nub . concatMap (\(x, y) -> [x, y])

