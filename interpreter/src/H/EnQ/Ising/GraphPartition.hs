module H.EnQ.Ising.GraphPartition
    ( graphPartitionH
    , solveGraphPartition
    , randomGraph
    ) where

import System.Random (randomRIO)
import Control.Monad (replicateM)

import H.EnQ.Ising.Ising (generateSpins, solveHamiltonians, findMinimum, solveClassical, solveQuantum, suggestT)

-- Generate a random graph with 2 to 6 vertices and at least one edge
randomGraph :: IO [(Int, Int)]
randomGraph = do
    numVertices <- randomRIO (2, 6)                -- Number of vertices
    v1 <- randomRIO (0, numVertices - 1)          -- First vertex for the guaranteed edge
    v2 <- randomRIO (0, numVertices - 1)          -- Second vertex for the guaranteed edge
    let initialEdge = if v1 /= v2 then [(v1, v2)] else [] -- Avoid self-loop for the first edge

    numEdges <- randomRIO (1, numVertices * 2 - 1) -- Additional edges (1 to twice the vertices minus the initial edge)
    extraEdges <- replicateM numEdges $ do
        v1 <- randomRIO (0, numVertices - 1)
        v2 <- randomRIO (0, numVertices - 1)
        return (v1, v2)

    -- Combine the initial edge with additional random edges
    let allEdges = initialEdge ++ extraEdges

    -- Filter out self-loops and remove duplicates
    return $ removeDuplicates $ filter (\(u, v) -> u /= v) allEdges
  where
    removeDuplicates = foldr (\edge acc -> if edge `elem` acc || swap edge `elem` acc then acc else edge : acc) []
    swap (a, b) = (b, a)

-- Define the Hamiltonian for the graph partitioning problem
graphPartitionH :: [(Int, Int)] -> [Int] -> Double
graphPartitionH edges spins =
  let -- First term: size constraint
      term1 = fromIntegral $ sum spins ^ 2
      -- Second term: edge penalty
      term2 = sum [ (1 - fromIntegral (spins !! u * spins !! v)) / 2 | (u, v) <- edges, u < length spins, v < length spins ]
      -- Weights for terms
      a = 1.0
      b = 1.0
  in a * term1 + b * term2

-- Solve the graph partitioning problem
solveGraphPartition :: IO ()
solveGraphPartition = do
    -- let edges = [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2)] -- Example graph (a square with a diagonal)
    -- Generate a random graph
    edges <- randomGraph
    putStrLn $ "Random graph edges: " ++ show edges

    let numVertices = maximum (concatMap (\(u, v) -> [u, v]) edges) + 1 -- Determine the number of vertices
    let hamiltonian spins = graphPartitionH edges spins
    let shots = 1024
    let numSteps = 100

    -- Suggest optimal t based on energy gap
    let onError err = error ("Error in suggestT: " ++ err)
    let onSuccess optimalT = optimalT
    let optimalT = suggestT numVertices hamiltonian onError onSuccess
    putStrLn $ "Suggested optimal t: " ++ show optimalT

    -- Generate spin configurations
    let spins = generateSpins numVertices

    -- Run classical simulation
    putStrLn "Running classical simulation..."
    classicalResults <- solveHamiltonians (solveClassical hamiltonian) numVertices
    let classicalMin = findMinimum classicalResults
    putStrLn $ "Classical Result: Configuration: " ++ show (snd classicalMin) ++ ", Energy: " ++ show (fst classicalMin)

    -- Run quantum simulation
    putStrLn "Running quantum simulation..."
    quantumResults <- solveHamiltonians (\spins -> solveQuantum hamiltonian optimalT shots spins numSteps) numVertices
    let quantumMin = findMinimum quantumResults
    putStrLn $ "Quantum Result: Configuration: " ++ show (snd quantumMin) ++ ", Energy: " ++ show (fst quantumMin)
