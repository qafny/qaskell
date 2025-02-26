module Quantum.Examples where

import Quantum.Program
import Data.List (nub)

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

-- | Get the nodes from an adjacency list
getNodes :: Eq a => [(a, a)] -> [a]
getNodes = nub . concatMap (\(x, y) -> [x, y])

