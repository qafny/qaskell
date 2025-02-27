module Quantum.Examples where

import Quantum.Program
import Quantum.ExampleData

import Data.List (nub)

-- ghci> solveProgram (eqSum [1, 2])
-- ...
eqSum ::
  [Int] -> Program [] Int Int Int
eqSum inputList =
  Program
    { choices = [-1, 1]
    , struct = inputList
    , view = 2
    , constraints = \[(a, choiceA), (b, choiceB)] ->
        (a * choiceA)
          *
        (b * choiceB)
    }

-- ghci> solveProgram (graphColoring 2 graph1)
-- ...
graphColoring ::
  Int -> [(Int, Int)] -> Program [] Int Int Int
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
  Int -> AdjList Int -> Program [] Int Int Int
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

exactCover :: Cover Int -> Program [] Int Int Int
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

infixr :->
data Type = IntType | Type :-> Type
  deriving (Show, Eq)

data Expr a
  = Var String a
  | Num Int a
  | App (Expr a) (Expr a) a
  | Lambda String Type (Expr a) a
  deriving (Show)

-- inferType :: Expr () -> Program Expr () Type Int
-- inferType expr =
--   Program
--     { choices = map nAryIntType [0..4]
--     , struct = expr
--     , view = 3
--     , constraints = \c -> _
--     }
--   where
--     nAryIntType 0 = IntType
--     nAryIntType n = IntType :-> nAryIntType (n-1)

-- | Get the nodes from an adjacency list
getNodes :: Eq a => [(a, a)] -> [a]
getNodes = nub . concatMap (\(x, y) -> [x, y])

