{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module DSL.AdjMatrix
  (AdjMatrix(..)
  ,adjMatrix
  ,completeGraph
  ,getNodes
  ,getEdges
  ,complementEdges
  ,updateNodeContents
  )
  where

newtype AdjMatrix a = AdjMatrix [[Maybe a]]
  deriving (Functor, Foldable)

instance Traversable AdjMatrix where
  traverse f (AdjMatrix rows) = AdjMatrix <$> traverse (traverse (traverse f)) rows

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
  [(i, j) | (i, row) <- zip [0 ..] rows, (j, edge) <- zip [0 ..] row, edge /= Nothing]

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

-- chooseNodeContents :: Functor f =>
--   AdjMatrix (a, a) ->
--   f [Weighted w b] ->
--   f (AdjMatrix (Weighted w b, Weighted w b))
-- chooseNodeContents adj = fmap (updateNodeContents adj)

