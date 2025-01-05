module H.EnQ.Clique where

import Numeric.LinearAlgebra (Matrix, Vector, atIndex, (#>), (><), fromLists, toLists, ident, cmap, diagl, tr, rows)
import Numeric.LinearAlgebra.Data (fromList, scalar, toList)
import Data.Complex (Complex((:+)), magnitude)
import Control.Monad (replicateM)
import System.Random (randomRIO)
import qualified Data.Set as Set
import H.EnQ.AdiabaticEvolution (adiabaticEvolution, maxIndex)

-- Generate a random graph with n vertices
generateRandomGraph :: Int -> IO (Matrix Double)
generateRandomGraph n = do
    adjacencyMatrix <- replicateM (n * n) (randomRIO (0 :: Int, 1 :: Int))
    let mat = (n >< n) (map fromIntegral adjacencyMatrix)
        zeroDiagonal = mat - diagl (replicate n 1)  -- Remove self-loops
        upperTriangular = fromLists [[if j > i then mat `atIndex` (i, j) else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
        symmetrized = upperTriangular + tr upperTriangular
    return $ cmap (\x -> if x > 0 then 1 else 0) symmetrized

-- Generate combinations of a specific size
generateChoices :: Int -> [a] -> [[a]]
generateChoices 0 _ = [[]]
generateChoices _ [] = []
generateChoices k (x:xs) = map (x :) (generateChoices (k - 1) xs) ++ generateChoices k xs

-- Solve classical cliques
solveClassical :: Matrix Double -> [[Int]] -> [[Int]]
solveClassical graph choices = filter (isClique graph) choices

-- Minimize function to find the largest cliques
minimize :: [[Int]] -> [[Int]]
minimize cliques = let maxSize = maximum (map length cliques)
                   in filter ((== maxSize) . length) cliques

-- Construct the initial Hamiltonian H_B
initialHamiltonian :: Int -> Int -> Matrix (Complex Double)
initialHamiltonian n k = fromLists $ map row [0 .. dim - 1]
  where
    dim = 2 ^ n
    row i = [if valid i j then (-1) :+ 0 else 0 :+ 0 | j <- [0 .. dim - 1]]
    valid i j = hammingWeight (toBinary n i) == k && hammingWeight (toBinary n j) == k && i /= j
    hammingWeight = length . filter (== 1)

-- Construct the problem Hamiltonian H_P
problemHamiltonian :: Matrix Double -> Int -> Matrix (Complex Double)
problemHamiltonian graph k = fromLists $ map row [0 .. dim - 1]
  where
    n = rows graph
    dim = 2 ^ n
    row i = [if i == j then penalty i else 0 :+ 0 | j <- [0 .. dim - 1]]
    penalty i = let binary = toBinary n i
                in if hammingWeight binary == k
                   then fromIntegral (sum [1 | (u, v) <- pairs n, binary !! u == 1, binary !! v == 1, graph `atIndex` (u, v) == 0]) :+ 0
                   else 1 :+ 0
    hammingWeight = length . filter (== 1)
    pairs n = [(u, v) | u <- [0 .. n - 1], v <- [u + 1 .. n - 1]]

-- Prepare the initial state
prepareInitialState :: Int -> Int -> Vector (Complex Double)
prepareInitialState n k = fromList $ map normalize [0 .. dim - 1]
  where
    dim = 2 ^ n
    states = [if hammingWeight (toBinary n i) == k then 1 :+ 0 else 0 :+ 0 | i <- [0 .. dim - 1]]
    norm = sqrt $ sum [magnitude x ^ 2 | x <- states]
    normalize i = states !! i / (norm :+ 0)
    hammingWeight = length . filter (== 1)

-- Convert an integer to binary representation
toBinary :: Int -> Int -> [Int]
toBinary n x = reverse $ take n $ reverse (toBinary' x) ++ repeat 0
  where
    toBinary' 0 = []
    toBinary' y = let (q, r) = y `divMod` 2 in r : toBinary' q

-- Check if a set of vertices forms a clique
isClique :: Matrix Double -> [Int] -> Bool
isClique graph vertices = all (\(u, v) -> graph `atIndex` (u, v) == 1) [(u, v) | u <- vertices, v <- vertices, u /= v]

-- Solve the clique problem
solveClique :: IO ()
solveClique = do
    graph <- generateRandomGraph 7
    let n = rows graph
        k = floor (2 * logBase 2 (fromIntegral n))
        t = 10.0
        steps = 100

    putStrLn $ "n = " ++ show n
    putStrLn $ "Initial k = " ++ show k
    putStrLn "Adjacency Matrix:"
    print graph

    let choices = concatMap (\size -> generateChoices size [0 .. n - 1]) [1 .. n]
        classicalCliques = solveClassical graph choices
        largestCliques = minimize classicalCliques
    putStrLn $ "\nAll Largest Cliques (Classical): " ++ show largestCliques

    let iterateK k =
            if k > 0
            then do
                putStrLn $ "\nTrying k = " ++ show k

                let hB = initialHamiltonian n k
                    hP = problemHamiltonian graph k
                    initialState = prepareInitialState n k
                    finalState = adiabaticEvolution hB hP t steps initialState
                    maxAmplitudeIndex = maxIndex finalState
                    binaryRepresentation = toBinary n maxAmplitudeIndex
                    cliqueVertices = [i | (i, bit) <- zip [0..] binaryRepresentation, bit == 1]

                putStrLn $ "Quantum Largest Clique: " ++ show cliqueVertices

                if any ((== Set.fromList cliqueVertices) . Set.fromList) largestCliques
                then putStrLn ("\nMatch found! k = " ++ show k)
                else do
                    putStrLn "No match. Reducing k."
                    iterateK (k - 1)
            else putStrLn "No match found."

    iterateK k
