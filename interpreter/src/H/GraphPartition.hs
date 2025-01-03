module H.GraphPartition
    ( graphPartitionH
    , testGraphPartitionH
    ) where

import H.Simulator

-- Define the Hamiltonian for the graph partitioning problem
graphPartitionH :: [(Int, Int)] -> [Int] -> Double
graphPartitionH edges spins =
  let -- First term: size constraint
      term1 = fromIntegral $ sum spins ^ 2
      -- Second term: edge penalty
      term2 = sum [ (1 - fromIntegral (spins !! u * spins !! v)) / 2 | (u, v) <- edges ]
      -- Weights for terms
      a = 1.0
      b = 1.0
  in a * term1 + b * term2

testGraphPartitionH :: IO ()
testGraphPartitionH = do
  let edges = [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2)] -- Example graph (a square with a diagonal)
  let numVertices = 4
  let hamiltonian = graphPartitionH edges

  -- Define continuations
  let onError err = error ("Error in suggestT: " ++ err)
  let onSuccess optimalT = optimalT

  -- Suggest optimal t
  let optimalT = suggestT numVertices hamiltonian onError onSuccess
  putStrLn $ "Suggested optimal t: " ++ show optimalT
  
  -- Define the simulator
  let simulator = Simulator numVertices hamiltonian
  
  -- Run the classical simulation
  let (bestConfig, bestEnergy) = classical simulator
  putStrLn $ "Classical Result: " ++ show bestConfig ++ ", " ++ show bestEnergy

  -- Quantum simulation
  let shots = 1024
  (bestConfigQ, bestEnergyQ) <- quantum simulator optimalT shots
  putStrLn $ "Quantum Result: " ++ show bestConfigQ ++ ", " ++ show bestEnergyQ
