{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module AdjMatrix
  (AdjMatrix
  ,adjMatrix
  ,completeGraph
  ,getNodes
  ,updateNodeContents
  )
  where

newtype AdjMatrix a = AdjMatrix [[Maybe a]]
  deriving (Functor, Foldable, Show)

instance Traversable AdjMatrix where
  traverse f (AdjMatrix rows) = fmap AdjMatrix $ traverse (traverse (traverse f)) rows
  
adjMatrix :: [[Maybe a]] -> AdjMatrix a
adjMatrix = AdjMatrix

completeGraph :: [a] -> AdjMatrix (a, a)
completeGraph nodes =
  AdjMatrix $
  map (\x -> map (\y -> Just (x, y)) nodes)
          nodes

getNodes :: AdjMatrix a -> [()]
getNodes (AdjMatrix adj) = map (const ()) adj

-- | Example:
--   let a = [ [Just (), Just (), Just ()]
--           , [Just (), Just (), Just ()]
--           , [Nothing, Just (), Just ()]
--           ]
--
--   ghci> updateNodeContents [1,2,3] a
--   [ [ Just (1,1), Just (1,2), Just (1,3) ]
--   , [ Just (2,1), Just (2,2), Just (2,3) ]
--   , [ Nothing,    Just (3,2), Just (3,3) ]
--   ]
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

