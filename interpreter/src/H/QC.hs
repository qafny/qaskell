module H.QC where

-- Representing spin variables s_i in terms of Pauli Z operators.
-- We will use qubit |0> to represent s_i = +1 and |1> for s_i = -1.
data Spin = Spin Int  -- Spin i represents s_i

instance Show Spin where
  show (Spin i) = "s_" ++ show i

-- A Pauli term is a coefficient multiplied by a tensor product of Pauli-Z operators.
data PauliTerm = PauliTerm Double [(Spin, String)]  -- (Coefficient, [(Spin, "Z")])

-- A Hamiltonian is a list of Pauli terms.
type Hamiltonian = [PauliTerm]

-- Convert s_i to Pauli-Z representation: s_i -> [PauliTerm]
spinToPauli :: Spin -> [PauliTerm]
spinToPauli (Spin i) = [PauliTerm 1.0 [(Spin i, "Z")]]

-- Helper: Return the qubit index (starting at 0)
qubitIndex :: Spin -> Int
qubitIndex (Spin i) = i - 1

-- Apply a Hadamard gate to each qubit.
applyHadamard :: [Spin] -> [String]
applyHadamard spins = map (\s -> "H on qubit " ++ show (qubitIndex s)) spins

-- Apply the cost unitary (with negative phases) for each pair of spins.
-- For each cost term we generate:
--   CX from control to target,
--   Rz with angle = -2 * gamma * coefficient on target,
--   CX from control to target.
-- applyCostUnitary :: Double -> [(Spin, Spin, Double)] -> [String]
-- applyCostUnitary gamma terms = concatMap applyTerm terms
--   where
--     applyTerm (s1, s2, coeff) =
--       [ "CX from qubit " ++ show (qubitIndex s1) ++ " to qubit " ++ show (qubitIndex s2)
--       , "Rz with angle " ++ show (-2 * gamma * coeff) ++ " on qubit " ++ show (qubitIndex s2)
--       , "CX from qubit " ++ show (qubitIndex s1) ++ " to qubit " ++ show (qubitIndex s2)
--       ]

-- Apply the cost unitary (with negative phases) for each pair of spins.
-- For each cost term, generate:
--   Rzz with angle = -2 * gamma * coefficient between the two qubits.
applyCostUnitary :: Double -> [(Spin, Spin, Double)] -> [String]
applyCostUnitary gamma terms = map applyRzz terms
  where
    applyRzz (s1, s2, coeff) =
      "Rzz with angle " ++ show (-2 * gamma * coeff) ++ " between qubit " ++ show (qubitIndex s1) ++ " and qubit " ++ show (qubitIndex s2)

-- Apply the cost unitary using trotterization.
-- Each term is an Rzz with an angle depending on time step `t`.
applyTrotterizedCost :: Double -> Double -> Int -> [(Spin, Spin, Double)] -> [String]
applyTrotterizedCost gamma totalTime trotterSteps terms = concatMap applySteps [1..trotterSteps]
  where
    deltaT = totalTime / fromIntegral trotterSteps
    s t = t / totalTime
    applySteps step =
      let t = fromIntegral step * deltaT
          s_t = s t
      in map (applyRzz s_t deltaT) terms
    applyRzz s_t deltaT (s1, s2, coeff) =
      "Rzz with angle " ++ show (-2 * s_t * gamma * coeff * deltaT) ++ " between qubit " ++ show (qubitIndex s1) ++ " and qubit " ++ show (qubitIndex s2)

-- Apply the mixing unitary: Rx(2 * beta) on each qubit.
applyMixing :: Double -> [Spin] -> [String]
applyMixing beta spins = map (\s -> "Rx with angle " ++ show (2 * beta) ++ " on qubit " ++ show (qubitIndex s)) spins

-- Generate the full quantum circuit with trotterization and interpolation.
generateQuantumCircuit :: Double -> Double -> Double -> Int -> [(Double, [Spin])] -> [String]
generateQuantumCircuit gamma beta totalTime trotterSteps hamiltonian =
  let spins         = [Spin 1, Spin 2, Spin 3, Spin 4]
      hadamardGates = applyHadamard spins
      costTerms     = hamiltonianTerms hamiltonian
      costGates     = applyTrotterizedCost gamma totalTime trotterSteps costTerms
      mixingGates   = applyMixing beta spins
      measurementGates = applyMeasurement spins
  in hadamardGates ++ costGates ++ mixingGates ++ measurementGates

-- Apply measurement to each qubit.
applyMeasurement :: [Spin] -> [String]
applyMeasurement spins = map (\s -> "Measure qubit " ++ show (qubitIndex s)) spins

-- Convert our initial Hamiltonian representation (a list of (coefficient, [Spin])) 
-- to a list of cost terms of the form (Spin, Spin, coefficient). We assume that 
-- each term acts on exactly two spins.
hamiltonianTerms :: [(Double, [Spin])] -> [(Spin, Spin, Double)]
hamiltonianTerms hs = [ (s1, s2, coeff) | (coeff, [s1, s2]) <- hs ]

-- Generate the full quantum circuit as a list of string instructions.
-- The circuit applies:
--   (i) Hadamard gates on all spins,
--   (ii) The cost unitary U_C(gamma) (using the reversed-phase version),
--   (iii) The mixing unitary U_M(beta),
--   (iv) Measurement of all qubits.
-- generateQuantumCircuit :: Double -> Double -> [(Double, [Spin])] -> [String]
-- generateQuantumCircuit gamma beta hamiltonian =
--   let spins         = [Spin 1, Spin 2, Spin 3, Spin 4]
--       hadamardGates = applyHadamard spins
--       costTerms     = hamiltonianTerms hamiltonian
--       costGates     = applyCostUnitary gamma costTerms
--       mixingGates   = applyMixing beta spins
--       measurementGates = applyMeasurement spins
--   in hadamardGates ++ costGates ++ mixingGates ++ measurementGates

-- Evaluate the Hamiltonian value for a given spin configuration.
evaluateHamiltonian :: [Int] -> Double
evaluateHamiltonian spins =
  (sum $ zipWith (*) (map fromIntegral spins) [1.0, 2.0, 2.0, 3.0]) ^ 2

-- Main function to generate the circuit and display the solution.
main :: IO ()
main = do
  -- Step 1: Generate the quantum circuit with trotterization.
  -- Parameters: gamma = 0.5, beta = 0.3, total time T = 10.0, trotter steps = 100
  let gamma = 0.5
  let beta = 0.3
  let totalTime = 1.0
  let trotterSteps = 1
  let circuit = generateQuantumCircuit gamma beta totalTime trotterSteps initialHamiltonian

  -- Step 2: Display the generated quantum circuit.
  putStrLn "\nGenerated Quantum Circuit:"
  mapM_ putStrLn circuit

-- Initial Hamiltonian as a list of (coefficient, spins).
-- These coefficients come from the cost function (s1 + 2s2 + 2s3 + 3s4)^2.
initialHamiltonian :: [(Double, [Spin])]
initialHamiltonian =
  [ (4, [Spin 1, Spin 2])
  , (4, [Spin 1, Spin 3])
  , (6, [Spin 1, Spin 4])
  , (8, [Spin 2, Spin 3])
  , (12, [Spin 2, Spin 4])
  , (12, [Spin 3, Spin 4])
  ]
