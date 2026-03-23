module H.EnQ.HamiltonianCycle where

import Control.Monad (replicateM)
import Data.Complex (Complex ((:+)), magnitude)
import Data.List (nub, sortOn, permutations)
import H.EnQ.AdiabaticEvolution (adiabaticEvolution, maxIndex)
import Numeric.LinearAlgebra (Matrix, Vector, atIndex, (><), fromLists, toLists, cmap, diagl, tr, rows, scale)
import Numeric.LinearAlgebra.Data (fromList)
import H.AQC (generalizedSwapOp, operatorOnD, transitionMatrix)
import System.Random (randomRIO)


-- Classical Solver

-- Check if a specific permutation of cities forms a valid closed tour
isHamiltonianCycle :: Matrix Double -> [Int] -> Bool
isHamiltonianCycle graph path = all hasEdge pairs
  where
    n = length path
    -- Pair up consecutive cities, wrapping the last city back to the first
    pairs = [(path !! i, path !! ((i + 1) `mod` n)) | i <- [0 .. n - 1]]
    hasEdge (u, v) = graph `atIndex` (u, v) == 1.0

-- Find all valid Hamiltonian Cycles using brute-force permutations
solveClassical :: Matrix Double -> [[Int]]
solveClassical graph = filter (isHamiltonianCycle graph) allPaths
  where
    n = rows graph
    allPaths = permutations [0 .. n - 1]


-- QUANTUM SOLVER

-- Construct the Initial Hamiltonian H_B
initialHamiltonian :: Int -> Matrix (Complex Double)
initialHamiltonian n =
  scale ((-1) :+ 0) $ sum [ generalizedSwapOp n n i j | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]

-- Construct the Problem Hamiltonian H_P
problemHamiltonian :: Matrix Double -> Matrix (Complex Double)
problemHamiltonian graph = 
    if null missingEdges
    then scale (0 :+ 0) (operatorOnD n n []) 
    else sum [ penalty u v r | (u, v) <- missingEdges, r <- [0 .. n - 1] ]
  where
    n = rows graph
    missingEdges = [(u, v) | u <- [0 .. n - 1], v <- [0 .. n - 1], u /= v, graph `atIndex` (u, v) == 0]
    
    penalty u v r =
        let rNext = (r + 1) `mod` n
            detectorU = transitionMatrix n r r
            detectorV = transitionMatrix n rNext rNext
        in operatorOnD n n [(u, detectorU), (v, detectorV)]

-- Initial State: Uniform superposition of all VALID permutations
prepareInitialState :: Int -> Vector (Complex Double)
prepareInitialState n = fromList $ map normalize [0 .. dim - 1]
  where
    dim = n ^ n
    states = [if isValidPermutation (toBase n n i) then 1 :+ 0 else 0 :+ 0 | i <- [0 .. dim - 1]]
    norm = sqrt $ sum [magnitude x ^ 2 | x <- states]
    normalize i = states !! i / (norm :+ 0)

isValidPermutation :: [Int] -> Bool
isValidPermutation xs = length (nub xs) == length xs

toBase :: Int -> Int -> Int -> [Int]
toBase d len x = 
    let digits = reverse (toBase' x)
    in replicate (len - length digits) 0 ++ digits
  where
    toBase' 0 = []
    toBase' y = let (q, r) = y `divMod` d in r : toBase' q

decodePath :: [Int] -> [Int]
decodePath state = map fst $ sortOn snd (zip [0..] state)

-- Solve the Hamiltonian Cycle problem
solveHC :: IO ()
solveHC = do
  let n = 4
  
  -- Square with one diagonal
  -- let graph = (n >< n) 
  --       [ 0, 1, 1, 1
  --       , 1, 0, 1, 0
  --       , 1, 1, 0, 1
  --       , 1, 0, 1, 0 :: Double ]
  
  -- Complete graph
  -- let graph = (n >< n) 
  --       [ 0, 1, 1, 1
  --       , 1, 0, 1, 1
  --       , 1, 1, 0, 1
  --       , 1, 1, 1, 0 :: Double ]

  -- Star graph (no Hamiltonian Cycle exists)
  let graph = (n >< n) 
        [ 0, 1, 1, 1
        , 1, 0, 0, 0
        , 1, 0, 0, 0
        , 1, 0, 0, 0 :: Double ]

  putStrLn $ "n = " ++ show n
  putStrLn "Adjacency Matrix:"
  print graph

  -- Run Classical Solver
  let classicalCycles = solveClassical graph
  putStrLn $ "\nAll Valid Hamiltonian Cycles (Classical): " ++ show classicalCycles

  -- Run Quantum Solver
  let t = 50.0
      steps = 200

  putStrLn "\nRunning Quantum Adiabatic Evolution..."
  let hB = initialHamiltonian n
      hP = problemHamiltonian graph
      initialState = prepareInitialState n
      finalState = adiabaticEvolution hB hP t steps initialState
      
      maxAmplitudeIndex = maxIndex finalState
      winningState = toBase n n maxAmplitudeIndex
      quantumPath = decodePath winningState

  putStrLn $ "Quantum Selected State (City -> Rank): " ++ show winningState
  putStrLn $ "Decoded Hamiltonian Cycle Path: " ++ show quantumPath

  -- Verification
  if null classicalCycles
    then putStrLn "\nGraph has no valid Hamiltonian Cycles. Quantum state will just be the state with fewest penalties."
    else if quantumPath `elem` classicalCycles
      then putStrLn "\nMatch found! The quantum computer successfully found a valid Hamiltonian Cycle."
      else putStrLn "\nNo match. The adiabatic evolution might need a longer time 't' or more steps to find the true ground state."