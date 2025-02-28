module Quantum.ExampleData
  where

type AdjList a = [(a, a)]

symmetricClosure :: Eq a => AdjList a -> AdjList a
symmetricClosure origList = go origList
  where
    go [] = origList
    go ((x,y) : rest) =
      if not ((y, x) `elem` origList)
      then (x,y) : (y,x) : go rest
      else (x,y) : go rest

--  A --- B    D
--  |    /     |
--  |   /      |
--  |  /       |
--  | /        |
--  C          E
graph1 :: AdjList Int
graph1 = symmetricClosure [(1, 2), (1, 3), (2, 3), (4, 5)]

-- A --- B
-- |     |
-- |     |
-- C     D
graph2 :: AdjList Int
graph2 = symmetricClosure [(1, 2), (1, 3), (2, 4)]

-- Example graphs
  -- A --- B
  --  \   /
  --   \ /
  --    C       D --- E

graph3 :: AdjList Int
graph3 = symmetricClosure [(1, 2), (2, 3), (4, 5)]
  -- A --- B
  --  \   /
  --   \ /
  --    C
  --     \
  --      D --- E

graph4 :: AdjList Int
graph4 = symmetricClosure [(1, 2), (2, 3), (3, 4), (4, 5)]

  -- A --- B --- C
  --                \
  --                 E
  --     D ---------/

graph6 :: AdjList Int
graph6 = symmetricClosure [(1, 2), (2, 3), (3, 4), (4, 5)]

-- 0 --- 1
graph7 :: AdjList Int
graph7 = symmetricClosure [(0, 1)]

  --   0
  --  / \
  -- 1---2
  --  \ /
  --   3
graph8 :: AdjList Int
graph8 = symmetricClosure [(0, 1), (0, 2), (1, 2), (1, 3), (2, 3)]

