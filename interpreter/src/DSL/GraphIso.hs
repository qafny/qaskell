
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSL.GraphIso
  (isIsomorphicAdjMatrix)
  where

import Control.Monad
import Data.List (permutations, transpose)
import Data.Foldable (toList)
import Data.Proxy
import DSL.AdjMatrix ( AdjMatrix(..), adjMatrix, getNodes, getEdges, complementEdges )
import DSL.Solve (IntWeighted(..), Weighted(..), solveF, generateChoices, generateChoicesForIsomorphism)

hA :: Int -> Int -> [IntWeighted Int] -> Int
hA numNodesG1 numNodesG2 mapping =
  let
    -- Term 1: Each node in G1 maps to one node in G2
    term1 = sum [ (1 - sum [1 | Weighted w _ <- mapping, w == i])^2 | i <- [0 .. numNodesG2 - 1] ]
    -- Term 2: Each node in G2 maps from one node in G1
    term2 = sum [ (1 - sum [1 | Weighted _ v <- mapping, v == i])^2 | i <- [0 .. numNodesG1 - 1] ]
  in term1 + term2

hB :: Eq a => AdjMatrix a -> AdjMatrix a -> [IntWeighted Int] -> Int
hB g1 g2 mapping =
  let
    edgesG1 = getEdges g1
    edgesG2 = getEdges g2
    -- Map node indices
    nodeMap = [(i, u) | Weighted i u <- mapping]
    -- Non-edges in G1 map to edges in G2
    term1 = sum [ 1 | (i, j) <- complementEdges g1, (u, v) <- edgesG2, (i, u) `elem` nodeMap, (j, v) `elem` nodeMap ]
    -- Edges in G1 map to non-edges in G2
    term2 = sum [ 1 | (i, j) <- edgesG1, (u, v) <- complementEdges g2, (i, u) `elem` nodeMap, (j, v) `elem` nodeMap ]
  in term1 + term2

-- Total Hamiltonian
totalH :: Eq a => AdjMatrix a -> AdjMatrix a -> [IntWeighted Int] -> Int
totalH g1 g2 mapping =
  let a = 1 -- Weight for H_A
      b = 1 -- Weight for H_B
  in a * hA (length $ getNodes g1) (length $ getNodes g2) mapping
     + b * hB g1 g2 mapping
     
-- Main function to return minimum Hamiltonian value
isIsomorphicAdjMatrix :: forall m a. (Foldable m, MonadPlus m, Eq a) =>
  Proxy m ->          -- Proxy to disambiguate the MonadPlus instance
  AdjMatrix a ->      -- First graph
  AdjMatrix a ->      -- Second graph
  Int                 -- Returns the minimum Hamiltonian value
isIsomorphicAdjMatrix Proxy g1 g2 =
  let
    numNodes = length $ getNodes g1
    -- Ensure the graphs have the same number of nodes
    guard = if length (getNodes g1) /= length (getNodes g2)
              then error "Graphs must have the same number of nodes"
              else ()
    -- Generate all permutations using MonadPlus
    mappings :: m [IntWeighted Int]
    mappings = generateChoicesForIsomorphism numNodes
    -- Compute the Hamiltonian for each mapping
    hamiltonians = map (totalH g1 g2 . toList) (toList mappings)
  in solveF hamiltonians
