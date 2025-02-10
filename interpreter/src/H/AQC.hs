{-# LANGUAGE FlexibleContexts #-}

module H.AQC where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data ( (><) )
import Data.Complex
import Control.Monad ( foldM )
import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Operator Mapping and Spin Compiling
--------------------------------------------------------------------------------

-- | A spin (qubit) is labelled by an integer.
--   (For circuit printing we use Spin i to represent qubit i-1.)
data Spin = Spin Int deriving (Eq)

instance Show Spin where
  show (Spin i) = "q" ++ show (i - 1)

-- | In our scheme we “compile” a qubit’s occupation into Pauli operators.
--   Using the mapping:
--
--       Spin 1  →  ½ (I + Z)
--       Spin 0  →  ½ (I – Z)
--
--   This function returns a (textual) description of the operator.
compileSpin :: Spin -> Int -> String
compileSpin spin occ =
  case occ of
    1 -> "½ (I + Z) on " ++ show spin
    0 -> "½ (I - Z) on " ++ show spin
    _ -> error "Occupation must be 0 or 1"

--------------------------------------------------------------------------------
-- Problem Definition: Number Partitioning for an Arbitrary List of Weights
--------------------------------------------------------------------------------

-- | The list of weights.
problemWeights :: [Double]
problemWeights = [1, 2, 2, 3]
-- (You can change this list to any set of numbers.)

-- | In number partitioning one seeks to split the numbers into two groups.
--   We represent a candidate solution as a binary string [n1, n2, n3, n4]
--   with n_i ∈ {0,1}. To recover a spin value in {+1, -1} we use:
--
--       2*n_i - 1
--
binToSpin :: Int -> Int
binToSpin n = if n == 1 then 1 else -1

-- | The cost function is:
--
--       C(n) = (∑ᵢ wᵢ (2 nᵢ - 1))².
--
--   (Up to an additive constant, this is equivalent to (∑ᵢ wᵢ Z_i)².)
evaluateCost :: [Int] -> Double
evaluateCost ns =
  let spinVals = map binToSpin ns
      weighted = zipWith (\w s -> w * fromIntegral s) problemWeights spinVals
  in (sum weighted) ^ 2

-- | Generate all binary configurations for 4 qubits.
genConfigurations :: [[Int]]
genConfigurations = sequence (replicate 4 [0, 1])

-- | Find the configuration that minimizes the cost.
solvePartition :: IO ()
solvePartition = do
  let configs   = genConfigurations
      evaluated = [ (config, evaluateCost config) | config <- configs ]
      (bestConfig, bestCost) = minimumBy (comparing snd) evaluated
  putStrLn "\nOptimal Partition (binary assignment):"
  putStrLn $ "Assignment: " ++ show bestConfig ++ " with cost " ++ show bestCost
  putStrLn "\nCompiled Operator Representation (using our AQC mapping):"
  let spins = zip (map Spin [1 .. length bestConfig]) bestConfig
  mapM_ (\(s, occ) ->
           putStrLn $ show s ++ " → " ++ compileSpin s occ
        ) spins

--------------------------------------------------------------------------------
-- Hamiltonian Definitions for AQC Simulation
--------------------------------------------------------------------------------

-- Number of qubits.
nQubits :: Int
nQubits = 4

-- Hilbert space dimension.
dim :: Int
dim = 2 ^ nQubits  -- 16

-- Single-qubit 2×2 matrices (with Complex Double entries)
id2 :: Matrix (Complex Double)
id2 = (2 >< 2)
      [ 1 :+ 0, 0 :+ 0
      , 0 :+ 0, 1 :+ 0 ]

pauliX :: Matrix (Complex Double)
pauliX = (2 >< 2)
         [ 0 :+ 0, 1 :+ 0
         , 1 :+ 0, 0 :+ 0 ]

pauliZ :: Matrix (Complex Double)
pauliZ = (2 >< 2)
         [ 1 :+ 0, 0 :+ 0
         , 0 :+ 0, (-1) :+ 0 ]

-- | Given a list of (qubit index, operator) pairs, build the operator on n qubits.
--   For qubits not mentioned in the list, the identity is applied.
operatorOn :: Int -> [(Int, Matrix (Complex Double))] -> Matrix (Complex Double)
operatorOn n ops = foldl1 kronecker [ opFor i | i <- [0 .. n - 1] ]
  where
    opFor i = case lookup i ops of
                Just op -> op
                Nothing -> id2

-- | H_initial = – ∑ X_i.
--   The minus sign ensures that the ground state of H_initial is |+>^{⊗n},
--   which is the uniform superposition.
hInitial :: Matrix (Complex Double)
hInitial = scale ((-1) :+ 0) $
           sum [ operatorOn nQubits [(i, pauliX)] | i <- [0 .. nQubits - 1] ]

-- | Given a list of weights, generate the terms for the problem Hamiltonian H_P.
--   Each term is given as (coefficient, [qubit indices where Z acts]),
--   where the coefficient is computed as 2 * w_i * w_j.
hpTermsFor :: [Double] -> [(Double, [Int])]
hpTermsFor ws =
  [ (2 * (ws !! i) * (ws !! j), [i, j])
  | i <- [0 .. length ws - 1]
  , j <- [i + 1 .. length ws - 1]
  ]

-- | For each term, build the corresponding operator: coefficient * (⊗_{i=0}^{n-1} O_i),
--   where O_i = pauliZ if i is in the given list, else identity.
termOperator :: Int -> (Double, [Int]) -> Matrix (Complex Double)
termOperator n (c, idxs) =
  scale (c :+ 0) (operatorOn n [ (i, pauliZ) | i <- [0 .. n - 1], i `elem` idxs ])

-- | The problem Hamiltonian H_P is the sum of the terms generated from the weight list.
hP :: Matrix (Complex Double)
hP = sum [ termOperator nQubits term | term <- hpTermsFor problemWeights ]

-- | Interpolated (adiabatic) Hamiltonian for a given s ∈ [0,1]:
--      H(s) = (1-s) * H_initial + s * H_P.
hamiltonian :: Double -> Matrix (Complex Double)
hamiltonian s = scale ((1 - s) :+ 0) hInitial + scale (s :+ 0) hP

--------------------------------------------------------------------------------
-- Circuit/Circuit-Printing Functions (for completeness)
--------------------------------------------------------------------------------

printInitialHamiltonian :: IO ()
printInitialHamiltonian = do
  putStrLn "Initial Hamiltonian H_initial = - ∑ X_i:"
  let spins = map Spin [1 .. length problemWeights]
  mapM_ (\s -> putStrLn $ "-X on " ++ show s) spins

printCostHamiltonian :: [Double] -> IO ()
printCostHamiltonian ws = do
  putStrLn "\nProblem Hamiltonian H_P (up to constant):"
  let n     = length ws
      pairs = [ (i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]
      showTerm (i, j) =
        let coeff = 2 * (ws !! i) * (ws !! j)
            s1    = Spin (i + 1)
            s2    = Spin (j + 1)
        in show coeff ++ " · Z(" ++ show s1 ++ ") Z(" ++ show s2 ++ ")"
  mapM_ (putStrLn . showTerm) pairs

printAdiabaticSchedule :: Int -> IO ()
printAdiabaticSchedule steps = do
  putStrLn "\nAdiabatic Evolution Schedule:"
  let dtVal = 1.0 / fromIntegral steps
  mapM_ (\step -> do
            let sVal = dtVal * fromIntegral step
            putStrLn $ "Step " ++ show step ++ ": s(t) = " ++ show sVal
            putStrLn $ "   → Evolve under H = " ++ show (1 - sVal) ++ "·H_initial + " ++ show sVal ++ "·H_P"
        ) [0 .. steps]

--------------------------------------------------------------------------------
-- Simulation of Adiabatic Evolution
--------------------------------------------------------------------------------

-- Simulation parameters: use a finer schedule.
numSteps :: Int
numSteps = 100

-- Use a smaller time step for simulation.
dtSim :: Double
dtSim = 0.1

-- | Time evolution operator: U = exp(-i * H * dtSim).
timeEvolution :: Matrix (Complex Double) -> Matrix (Complex Double)
timeEvolution h = expm (scale (0 :+ (-dtSim)) h)

-- | Our initial state is the uniform superposition |+>^{⊗n} = (1/√(dim)) (1,1,…,1)^T.
initialState :: Vector (Complex Double)
initialState = konst (1 / sqrt (fromIntegral dim) :+ 0) dim

-- | Simulate the adiabatic evolution over a list of schedule values (s ∈ [0,1]).
simulateAdiabatic :: [Double] -> IO (Vector (Complex Double))
simulateAdiabatic sList = foldM evolve initialState sList
  where
    evolve psi s = do
      let h   = hamiltonian s
          u   = timeEvolution h
          psi' = u #> psi
      putStrLn $ "Evolving at s = " ++ show s ++ "; state norm = " ++ show (norm_2 psi')
      return psi'

--------------------------------------------------------------------------------
-- Utility: Printing Basis States and Their Probabilities
--------------------------------------------------------------------------------

-- | Convert an integer to its binary representation, padded with zeros
--   to have length equal to nQubits (e.g., 0 -> "0000").
intToBinary :: Int -> String
intToBinary x =
  let bs  = showIntAtBase 2 intToDigit x ""
      pad = replicate (nQubits - length bs) '0'
  in pad ++ bs

-- | Print each computational basis state along with its probability,
--   rounding the probability to 4 decimal places and sorting from highest
--   probability to lowest.
printProbabilities :: Vector (Complex Double) -> IO ()
printProbabilities psi = do
  let probs          = toList (cmap (\z -> magnitude z ** 2) psi)
      probList       = zip [0 ..] probs
      sortedProbList = sortBy (\(_, p1) (_, p2) -> compare p2 p1) probList
  putStrLn "\nFinal State Probabilities (computational basis):"
  mapM_ (\(i, p) ->
            putStrLn $ "Basis state " ++ intToBinary i ++ ": " ++ printf "%.4f" p
        ) sortedProbList

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "AQC for Number Partitioning Problem on arbitrary weights"
  putStrLn "--------------------------------------------"
  
  -- Print the circuit details.
  printInitialHamiltonian
  printCostHamiltonian problemWeights
  printAdiabaticSchedule 5
  --solvePartition
  
  -- Simulate the adiabatic evolution.
  let schedule = [ fromIntegral i / fromIntegral numSteps | i <- [0 .. numSteps] ]
  putStrLn "\nSimulating Adiabatic Evolution..."
  finalState <- simulateAdiabatic schedule
  printProbabilities finalState
