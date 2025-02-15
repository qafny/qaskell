{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module DSL.AdjMatrix
  (AdjMatrix(..)
  ,adjMatrix
  ,upperDiagonal
  ,completeGraph
  ,getNodes
  ,getEdges
  ,complementEdges
  ,updateNodeContents
  )
  where

import Data.Maybe (isJust)
import Control.Monad.Identity
import Control.Monad
import Data.List

-- import DSL.Syntax

newtype AdjMatrix a = AdjMatrix [[Maybe a]]
  deriving (Functor, Foldable, Show)

-- instance Zippable Identity AdjMatrix where
--   fill xs =
--     fmap (fmap fst) . fillPair xs

--   transposeFillPair xs (Identity (AdjMatrix adj)) =
--     let Identity (AdjMatrix newAdj) = fillPair xs (Identity (AdjMatrix (transpose adj)))
--     in
--     Identity (AdjMatrix (transpose newAdj))

--   fillPair (Identity xs) (Identity (AdjMatrix adj)) =
--     Identity $
--     AdjMatrix
--       (map (\row -> zipWith (\curr newNode -> fmap (newNode,) curr)
--                             row
--                             xs)
--            adj)

instance Traversable AdjMatrix where
  traverse f (AdjMatrix rows) = AdjMatrix <$> traverse (traverse (traverse f)) rows

-- | Remove everything except the upper diagonal. We don't lose information
-- because an adjacency matrix of an undirected graph must be symmetric
--
-- We use this so that we don't encounter an edge twice.
upperDiagonal :: AdjMatrix a -> AdjMatrix a
upperDiagonal (AdjMatrix rows) =
  AdjMatrix $ zipWith nothingDrop [0..length rows] rows
  where
    nothingDrop 0 xs = xs
    nothingDrop n (_:xs) = Nothing : nothingDrop (n-1) xs

adjMatrix :: [[Maybe a]] -> AdjMatrix a
adjMatrix = AdjMatrix

completeGraph :: [a] -> AdjMatrix (a, a)
completeGraph nodes =
  AdjMatrix $
  map (\x -> map (\y -> Just (x, y)) nodes)
          nodes

getNodes :: AdjMatrix a -> [()]
getNodes (AdjMatrix adj) = map (const ()) adj

getEdges :: Eq a => AdjMatrix a -> [(Int, Int)]
getEdges (AdjMatrix rows) =
  [(i, j) | (i, row) <- zip [0 ..] rows, (j, edge) <- zip [0 ..] row, isJust edge]

complementEdges :: Eq a => AdjMatrix a -> [(Int, Int)]
complementEdges g@(AdjMatrix rows) =
  [(i, j) | i <- [0 .. length rows - 1], j <- [0 .. length rows - 1], i /= j, (i, j) `notElem` getEdges g]

updateNodeContents :: AdjMatrix a -> [b] -> AdjMatrix (b, b)
updateNodeContents (AdjMatrix adj) nodes =
  AdjMatrix $
  zipWith (\x row ->
              zipWith (\y entry ->
                          fmap (const (x, y)) entry)
                      nodes
                      row)
          nodes
          adj

-- updateNodeContents' :: AdjMatrix a -> [b] -> AdjMatrix (b, b)
-- updateNodeContents' adj xs =
--   zipWith

-- updateNodeContents :: Zippable f g =>
--   f (g a) -> f [b] -> f (g (b, b))
-- updateNodeContents adj nodes =
--   transposeFillPair nodes (fill nodes adj)
--   -- zipWithList (\x row ->
--   --             undefined $ zipWithList (\y entry ->
--   --                           fmap (const (x, y)) entry)
--   --                     _
--   --                     row)
--   --         nodes
--   --         undefined

-- chooseNodeContents :: Functor f =>
--   AdjMatrix (a, a) ->
--   f [Weighted w b] ->
--   f (AdjMatrix (Weighted w b, Weighted w b))
-- chooseNodeContents adj = fmap (updateNodeContents adj)

