module H.EnQ.Clique where

import Control.Monad (replicateM)
import Data.Complex (Complex ((:+)), magnitude)
import qualified Data.Set as Set
import H.EnQ.AdiabaticEvolution (adiabaticEvolution, maxIndex)
import Numeric.LinearAlgebra (Matrix, Vector, atIndex, (#>), (><), fromLists, toLists, ident, cmap, diagl, tr, rows, scale)
import Numeric.LinearAlgebra.Data (fromList, scalar, toList)
import H.AQC (pauliX, pauliY, pauliZ, operatorOn, xySwapOp, generalizedSwapOp)
import System.Random (randomRIO)

-- Generate a random graph with n vertices
generateRandomGraph :: Int -> IO (Matrix Double)
generateRandomGraph n = do
  adjacencyMatrix <- replicateM (n * n) (randomRIO (0 :: Int, 1 :: Int))
  let mat = (n >< n) (map fromIntegral adjacencyMatrix)
      zeroDiagonal = mat - diagl (replicate n 1) -- Remove self-loops
      upperTriangular = fromLists [[if j > i then mat `atIndex` (i, j) else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
      symmetrized = upperTriangular + tr upperTriangular
  return $ cmap (\x -> if x > 0 then 1 else 0) symmetrized

-- Generate combinations of a specific size
generateChoices :: Int -> [a] -> [[a]]
generateChoices 0 _ = [[]]
generateChoices _ [] = []
generateChoices k (x : xs) = map (x :) (generateChoices (k - 1) xs) ++ generateChoices k xs

-- Solve classical cliques
solveClassical :: Matrix Double -> [[Int]] -> [[Int]]
solveClassical graph choices = filter (isClique graph) choices

-- Minimize function to find the largest cliques
minimize :: [[Int]] -> [[Int]]
minimize cliques =
  let maxSize = maximum (map length cliques)
   in filter ((== maxSize) . length) cliques

-- Construct the initial Hamiltonian H_B

-- Old 2 dimension version
-- initialHamiltonian :: Int -> Matrix (Complex Double)
-- initialHamiltonian n =
--   scale ((-1) :+ 0) $ sum [ xySwapOp n i j | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]

-- Generalized 'd' dimension version
initialHamiltonian :: Int -> Matrix (Complex Double)
initialHamiltonian n =
  scale ((-1) :+ 0) $ sum [ generalizedSwapOp 2 n i j | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]


-- Construct the Problem Hamiltonian H_P
problemHamiltonian :: Matrix Double -> Matrix (Complex Double)
problemHamiltonian graph = 
    if null missingEdges
    then scale (0 :+ 0) (operatorOn n []) 
    else sum [ penalty u v | (u, v) <- missingEdges ]
  where
    n = rows graph
    missingEdges = [(u, v) | u <- [0 .. n - 1], v <- [u + 1 .. n - 1], graph `atIndex` (u, v) == 0]
    
    -- Penalise only when both u and v are selected: 1/2 * (I - Z_u) * 1/2 * (I - Z_v) -> Expands to: 1/4 * (I - Z_u - Z_v + Z_u * Z_v)
    penalty u v =
        let ii = operatorOn n []
            zu = operatorOn n [(u, pauliZ)]
            zv = operatorOn n [(v, pauliZ)]
            zuzv = operatorOn n [(u, pauliZ), (v, pauliZ)]
        in scale (0.25 :+ 0) (ii - zu - zv + zuzv)

-- Dicke state: uniform superposition with Hamming weight k
prepareInitialState :: Int -> Int -> Vector (Complex Double)
prepareInitialState n k = fromList $ map normalize [0 .. dim - 1]
  where
    dim = 2 ^ n
    states = [if hammingWeight (toBinary n i) == k then 1 :+ 0 else 0 :+ 0 | i <- [0 .. dim - 1]]
    norm = sqrt $ sum [magnitude x ^ 2 | x <- states]
    normalize i = states !! i / (norm :+ 0)
    hammingWeight = length . filter (== 1)


-- Convert an integer to binary representation
-- toBinary :: Int -> Int -> [Int]
-- toBinary n x = reverse $ take n $ reverse (toBinary' x) ++ repeat 0
--   where
--     toBinary' 0 = []
--     toBinary' y = let (q, r) = y `divMod` 2 in r : toBinary' q

toBinary :: Int -> Int -> [Int]
toBinary n x = 
    let bin = reverse (toBinary' x)
    in replicate (n - length bin) 0 ++ bin
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
      initialK = floor (2 * logBase 2 (fromIntegral n)) :: Int
      t = 50.0
      steps = 200

  putStrLn $ "n = " ++ show n
  putStrLn $ "Initial k = " ++ show initialK
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

            let hB = initialHamiltonian n
                hP = problemHamiltonian graph
                initialState = prepareInitialState n k
                finalState = adiabaticEvolution hB hP t steps initialState
                maxAmplitudeIndex = maxIndex finalState
                binaryRepresentation = toBinary n maxAmplitudeIndex
                cliqueVertices = [i | (i, bit) <- zip [0 ..] binaryRepresentation, bit == 1]

            putStrLn $ "Quantum Largest Clique: " ++ show cliqueVertices

            if isClique graph cliqueVertices && any ((== Set.fromList cliqueVertices) . Set.fromList) largestCliques
              then putStrLn ("\nMatch found! k = " ++ show k)
              else do
                putStrLn "No match. Reducing k."
                iterateK (k - 1)
          else putStrLn "No match found."

  iterateK initialK
