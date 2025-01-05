module H.EnQ.Ising.NumberPartition
    ( numberPartitionH
    , solveNumberPartition
    ) where

import Control.Monad (replicateM)
import System.Random (randomRIO)

import H.EnQ.Ising.Ising (generateSpins, solveHamiltonians, findMinimum, solveClassical, solveQuantum, suggestT)

-- Generate a random collection of numbers (1 to 9) of length 2 to 9
randomNumbers :: IO [Int]
randomNumbers = do
    len <- randomRIO (2, 9)               -- Random length between 2 and 9
    replicateM len (randomRIO (1, 9))    -- Generate random numbers in the range [1, 9]

-- Define the Hamiltonian for the number partitioning problem
numberPartitionH :: [Int] -> [Int] -> Double
numberPartitionH numbers spins =
  let weightedSum = sum $ zipWith (*) numbers spins
  in fromIntegral (weightedSum ^ 2)

solveNumberPartition :: IO ()
solveNumberPartition = do
  -- let numbers = [3, 1, 4, 2, 2]
  numbers <- randomNumbers
  putStrLn $ "Random numbers to partition: " ++ show numbers

  let numSpins = length numbers
  let hamiltonian spins = numberPartitionH numbers spins
  let shots = 1024
  let numSteps = 100

  -- Suggest optimal t based on energy gap
  let onError err = error ("Error in suggestT: " ++ err)
  let onSuccess optimalT = optimalT
  let optimalT = suggestT numSpins hamiltonian onError onSuccess
  putStrLn $ "Suggested optimal t: " ++ show optimalT

  -- Generate spin configurations
  let spins = generateSpins numSpins

  -- Run classical simulation
  putStrLn "Running classical simulation..."
  classicalResults <- solveHamiltonians (solveClassical hamiltonian) numSpins
  let classicalMin = findMinimum classicalResults
  putStrLn $ "Classical Result: Configuration: " ++ show (snd classicalMin) ++ ", Energy: " ++ show (fst classicalMin)

  -- Run quantum simulation
  putStrLn "Running quantum simulation..."
  quantumResults <- solveHamiltonians (\spins -> solveQuantum hamiltonian optimalT shots spins numSteps) numSpins
  let quantumMin = findMinimum quantumResults
  putStrLn $ "Quantum Result: Configuration: " ++ show (snd quantumMin) ++ ", Energy: " ++ show (fst quantumMin)
