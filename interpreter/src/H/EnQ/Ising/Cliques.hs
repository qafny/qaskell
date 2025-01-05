module H.EnQ.Ising.Cliques
    ( cliquesH
    , solveCliques
    ) where

import System.Random (randomRIO)
import Control.Monad (replicateM)

import H.EnQ.Ising.Ising (generateSpins, solveHamiltonians, findMinimum, solveClassical, solveQuantum, suggestT)

-- Generate a random graph with at least k edges and at least k vertices, maximum 6 edges
randomGraph :: Int -> IO [(Int, Int)]
randomGraph k = do
    if k < 1
        then error "The minimum number of edges (k) must be at least 1."
        else do
            -- Ensure the number of vertices is at least equal to the number of edges
            numVertices <- randomRIO (k, max 6 k) -- Minimum k vertices, maximum 6 (adjustable)

            -- Generate the first k guaranteed edges
            guaranteedEdges <- generateDistinctEdges numVertices k []

            -- Generate additional random edges
            numExtraEdges <- randomRIO (0, numVertices * 2 - k) -- Extra edges beyond the k guaranteed
            extraEdges <- replicateM numExtraEdges $ randomEdge numVertices

            -- Combine guaranteed and extra edges, remove duplicates
            let allEdges = guaranteedEdges ++ extraEdges
            return $ removeDuplicates allEdges
  where
    -- Generate a random edge
    randomEdge :: Int -> IO (Int, Int)
    randomEdge numVertices = do
        v1 <- randomRIO (0, numVertices - 1)
        v2 <- randomRIO (0, numVertices - 1)
        if v1 /= v2 then return (v1, v2) else randomEdge numVertices  -- Avoid self-loops

    -- Generate k distinct edges
    generateDistinctEdges :: Int -> Int -> [(Int, Int)] -> IO [(Int, Int)]
    generateDistinctEdges numVertices 0 edges = return edges
    generateDistinctEdges numVertices remaining edges = do
        edge <- randomEdge numVertices
        if edge `elem` edges || swap edge `elem` edges
            then generateDistinctEdges numVertices remaining edges
            else generateDistinctEdges numVertices (remaining - 1) (edge : edges)

    -- Remove duplicate and reverse duplicate edges
    removeDuplicates :: [(Int, Int)] -> [(Int, Int)]
    removeDuplicates = foldr (\edge acc -> if edge `elem` acc || swap edge `elem` acc then acc else edge : acc) []

    -- Swap the order of an edge
    swap :: (Int, Int) -> (Int, Int)
    swap (a, b) = (b, a)

-- Define the Hamiltonian for the clique problem
cliquesH :: [(Int, Int)] -> Int -> [Int] -> Double
cliquesH edges k spins =
  let -- First term: Enforce clique size
      term1 = fromIntegral $ (k - sum (map (\s -> if s == 1 then 1 else 0) spins)) ^ 2

      -- Second term: Enforce clique completeness
      term2 = fromIntegral $ (k * (k - 1)) `div` 2 - sum [if spins !! u == 1 && spins !! v == 1 then 1 else 0 | (u, v) <- edges]

      -- Weights for terms
      a, b :: Double
      a = 1.0
      b = 1.0
  in a * term1 + b * max 0 term2 -- Use `max 0` to prevent negative penalties

-- Test the clique Hamiltonian with FunctionalSimulator
solveCliques :: IO ()
solveCliques = do
  -- let edges = [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2)] -- Example graph (square with a diagonal)
  -- let edges = [(3,1),(0,3),(3,2),(2,0)]
  let k = 3 -- Desired clique size
  edges <- randomGraph k
  putStrLn $ "Random graph edges: " ++ show edges

  let numVertices = maximum (concatMap (\(u, v) -> [u, v]) edges) + 1 -- Determine the number of vertices
  let hamiltonian = cliquesH edges k
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
