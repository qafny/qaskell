{-# LANGUAGE BangPatterns #-}

module H.EnQ.ExactCover where

import Data.List (sort, maximumBy)  -- Added maximumBy
import Data.Ord (comparing)         -- Added comparing
import System.Random (randomRIO)
import Numeric.LinearAlgebra (Matrix, Vector, fromLists, toList, flatten, cmap, konst)
import Numeric.LinearAlgebra.Data (scalar)
import Data.Complex (Complex((:+)), magnitude)
import Control.Monad (replicateM)
import H.EnQ.AdiabaticEvolution (adiabaticEvolution, scaleComplex, uniformSuperposition, maxIndex)

-- Generate Random Clauses
generateRandomClauses :: Int -> Int -> IO [[Int]]
generateRandomClauses n maxAttempts = go [] 0
  where
    go clauses attempts
      | attempts >= maxAttempts = error "Failed to generate a unique satisfying assignment instance."
      | otherwise = do
          clause <- fmap (take 3 . sort) $ replicateM 3 (randomRIO (0, n - 1))
          if clause `elem` clauses
            then go clauses (attempts + 1)
            else do
              let numSatisfying = countSatisfyingAssignments (clause : clauses) n
              if numSatisfying == 1
                then return (clause : clauses)
                else if numSatisfying == 0
                  then go clauses attempts
                  else go (clause : clauses) attempts

-- Count Satisfying Assignments
countSatisfyingAssignments :: [[Int]] -> Int -> Int
countSatisfyingAssignments clauses n =
  length [bits | bits <- generateChoices n, all (isValidClause bits) clauses]
  where
    isValidClause bits clause = sum (map (bits !!) clause) == 1

-- Problem Hamiltonian
problemHamiltonian :: [[Int]] -> Int -> Matrix (Complex Double)
problemHamiltonian clauses n =
  fromLists [[if i == j then cost i :+ 0 else 0 :+ 0 | j <- [0 .. 2^n - 1]] | i <- [0 .. 2^n - 1]]
  where
    cost state =
      let bits = toBits n state
       in fromIntegral $ sum [1 | clause <- clauses, sum (map (bits !!) clause) /= 1]

-- Initial Hamiltonian
initialHamiltonian :: Int -> Matrix (Complex Double)
initialHamiltonian n =
  fromLists [[if i /= j then (-1) :+ 0 else 0 :+ 0 | j <- [0 .. 2^n - 1]] | i <- [0 .. 2^n - 1]]

-- Convert Integer to Binary Representation
toBits :: Int -> Int -> [Int]
toBits n x = reverse [if (x `div` 2^i) `mod` 2 == 1 then 1 else 0 | i <- [0 .. n - 1]]

-- Generate Choices
generateChoices :: Int -> [[Int]]
generateChoices n = replicateM n [0, 1]

-- Solve Classical
solveClassical :: [[Int]] -> ([Int] -> Bool)
solveClassical clauses =
  \bits -> all (isValidClause bits) clauses
  where
    isValidClause bits clause = sum (map (bits !!) clause) == 1

-- Minimize Results
minimize :: ([Int] -> Bool) -> [[Int]] -> [[Int]]
minimize isValidChoice = filter isValidChoice

-- Main Example Usage
solveExactCover :: IO ()
solveExactCover = do
  let n = 4
  clauses <- generateRandomClauses n 1000
  putStrLn $ "Generated clauses: " ++ show clauses

  let t = 100.0
  let steps = 100
  let h0 = initialHamiltonian n
  let hp = problemHamiltonian clauses n
  let psi0 = uniformSuperposition (2^n)
  let psiFinal = adiabaticEvolution h0 hp t steps psi0

  let index = maxIndex psiFinal
  let solution = toBits n index
  putStrLn $ "Quantum solution: " ++ show solution

  let classicalSolutions = minimize (solveClassical clauses) . generateChoices $ n
  putStrLn $ "Classical brute-force solutions: " ++ show classicalSolutions
