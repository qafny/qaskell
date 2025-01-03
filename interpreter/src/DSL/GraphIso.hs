{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- 9. Graph Isomorphisms

module DSL.GraphIso
  (isIsomorphicAdjMatrix)
  where

import Control.Monad
import Data.List (permutations, transpose)
import Data.Foldable (toList)
import Data.Proxy
import DSL.AdjMatrix ( AdjMatrix(..), adjMatrix, getNodes, getEdges, complementEdges )
import DSL.Solve (solveF, generateChoices, ChoiceStrategy)

import Debug.Trace

-- Hamiltonian H_A
hA :: Int -> [[Int]] -> Int
hA n x = term1 + term2
  where
    -- Each vertex in G2 maps to exactly one vertex in G1
    term1 = sum [ (1 - sum [x !! v !! i | i <- [0 .. n - 1]]) ^ 2 | v <- [0 .. n - 1] ]
    -- Each vertex in G1 is mapped from exactly one vertex in G2
    term2 = sum [ (1 - sum [x !! v !! i | v <- [0 .. n - 1]]) ^ 2 | i <- [0 .. n - 1] ]

hB :: Eq a => AdjMatrix a -> AdjMatrix a -> [[Int]] -> Int
hB g1 g2 x = term1 + term2
  where
    edgesG1 = getEdges g1
    edgesG2 = getEdges g2
    complementG1 = complementEdges g1
    complementG2 = complementEdges g2

    -- Penalize non-edges in G1 mapping to edges in G2
    term1 = sum [ x !! u !! i * x !! v !! j
                | (i, j) <- complementG1, (u, v) <- edgesG2 ]

    -- Penalize edges in G1 mapping to non-edges in G2
    term2 = sum [ x !! u !! i * x !! v !! j
                | (i, j) <- edgesG1, (u, v) <- complementG2 ]

-- Total Hamiltonian
totalH :: Eq a => AdjMatrix a -> AdjMatrix a -> [[Int]] -> Int
totalH g1 g2 x =
  let a = 1 -- Weight for H_A
      b = 1 -- Weight for H_B
  in a * hA (length $ getNodes g1) x + b * hB g1 g2 x

-- Isomorphism strategy: Generate all permutations of node indices
isomorphismStrategy :: MonadPlus m => Int -> Int -> ChoiceStrategy m [] a [[Int]]
isomorphismStrategy numVerticesG2 numVerticesG1 _ =
  return [ [ [fromEnum (v == i) | i <- [0 .. numVerticesG1 - 1]] | v <- perm ]
         | perm <- permutations [0 .. numVerticesG2 - 1]
         ]

-- Main function to return minimum Hamiltonian value
isIsomorphicAdjMatrix :: forall m a. (Foldable m, MonadPlus m, Eq a) =>
  Proxy m -> AdjMatrix a -> AdjMatrix a -> Int
isIsomorphicAdjMatrix Proxy g1 g2 =
  let n = length $ getNodes g1
      m = length $ getNodes g2
      mappings :: m [[[Int]]]
      mappings = generateChoices (isomorphismStrategy m n) [()]
      -- Compute all Hamiltonians for mappings
      hamiltonians = concatMap (map (totalH g1 g2)) (toList mappings)
  in solveF hamiltonians

