module H.NumberPartition
    ( numberPartitionH
    , testNumberPartitionH
    ) where

import H.Simulator

-- Define the Hamiltonian for the number partitioning problem
numberPartitionH :: [Int] -> [Int] -> Double
numberPartitionH numbers spins =
  let weightedSum = sum $ zipWith (*) numbers spins
  in fromIntegral (weightedSum ^ 2)

testNumberPartitionH :: IO ()
testNumberPartitionH = do
  let numbers = [3, 1, 4, 2, 2]
  let numSpins = length numbers
  let hamiltonian = numberPartitionH numbers

  -- Define continuations
  let onError err = error ("Error in suggestT: " ++ err)
  let onSuccess optimalT = optimalT

  -- Suggest optimal t
  let optimalT = suggestT numSpins hamiltonian onError onSuccess
  putStrLn $ "Suggested optimal t: " ++ show optimalT
  
  -- Define the simulator
  let simulator = Simulator numSpins hamiltonian
  
  -- Run the classical simulation
  let (bestConfig, bestEnergy) = classical simulator

  -- Print the results
  putStrLn $ show bestConfig ++ ", " ++ show bestEnergy

    -- Quantum simulation
  let shots = 1024
  (bestConfig, bestEnergy) <- quantum (Simulator numSpins hamiltonian) optimalT shots
  
  -- Print the results
  putStrLn $ show bestConfig ++ ", " ++ show bestEnergy
