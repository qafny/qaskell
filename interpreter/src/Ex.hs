{-# LANGUAGE ScopedTypeVariables #-}

import DSL.Syntax hiding (eqSumExample)

-- import QuantumLibrary

eqSumExample inputList =
  Program
    { choices = [0, 1]
    , inputList = inputList
    , view = 2
    , pattern = \[(choiceX, x), (choiceY, y)] -> (choiceX * x) + (choiceY * y)
    }

-- eqSumExample [7, 5]
eqSumExample :: [Int] -> Super Int (Expr Int)
eqSumExample inputList = do
  choice :: [(Int, Expr Int)]
    <- genChoicesQuantum [0, 1 :: Int] inputList

  let 
      theDistinctTuples :: [[(Int, Expr Int)]]
      theDistinctTuples = distinctNTuples 2 choice

      mappedList :: [Expr Int]
      mappedList = map go theDistinctTuples

      theSum :: Expr Int
      theSum = sum mappedList

  pure theSum

  where
    go [(choiceX, x), (choiceY, y)] =
      (fromIntegral choiceX * x)
        +
      (fromIntegral choiceY * y)

-- main :: IO ()
-- main = do
--   let (initialStateMatrix, expressionMatrix) = compile (eqSumExample [5, 7])

--   finalResult <- sendToQuantumComputer initialStateMatrix expressionMatrix
--   print finalResult

