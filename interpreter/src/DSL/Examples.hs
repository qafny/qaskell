module DSL.Examples
  (graph1
  ,graph2
  ,graph3
  ,graph4
  ,graph5
  ,graph6
  ,graph7
  ,graph8
  ,graph9
  )
  where

import DSL.AdjMatrix

--  A --- B    D
--  |    /     |
--  |   /      |
--  |  /       |
--  | /        |
--  C          E
graph1 :: AdjMatrix ()
graph1 =
  adjMatrix
    [ -- A --
        -- A      B        C        D        E
      [Nothing, Just (), Just (), Nothing, Nothing]

      -- B --
        -- A      B        C        D        E
    , [Just (), Nothing, Just (), Nothing, Nothing]

      -- C --
        -- A      B        C        D        E
    , [Just (), Just (), Nothing, Nothing, Nothing]

      -- D --
        -- A      B        C        D        E
    , [Nothing, Nothing, Nothing, Nothing, Just ()]

      -- E --
        -- A      B        C        D        E
    , [Nothing, Nothing, Nothing, Just (), Nothing]
    ]

-- A --- B
-- |     |
-- |     |
-- C     D
graph2 :: AdjMatrix ()
graph2 =
  adjMatrix
    [ -- A
        -- A      B       C        D
      [Nothing, Just (), Just (), Nothing]

      -- B
        -- A      B       C        D
    , [Just (), Nothing, Nothing, Just ()]

      -- C
        -- A      B       C        D
    , [Just (), Nothing, Nothing, Nothing]

      -- D
        -- A      B       C        D
    , [Nothing, Just (), Nothing, Nothing]
    ]

-- Example graphs
  -- A --- B
  --  \   /
  --   \ /
  --    C       D --- E

graph3 :: AdjMatrix ()
graph3 =
  adjMatrix
    [ -- A --
        -- A      B        C        D        E
      [Nothing, Just (), Just (), Nothing, Nothing]
      -- B --
        -- A      B        C        D        E
    , [Just (), Nothing, Just (), Nothing, Nothing]
      -- C --
        -- A      B        C        D        E
    , [Just (), Just (), Nothing, Nothing, Nothing]
      -- D --
        -- A      B        C        D        E
    , [Nothing, Nothing, Nothing, Nothing, Just ()]
      -- E --
        -- A      B        C        D        E
    , [Nothing, Nothing, Nothing, Just (), Nothing]
    ]

  -- 1 --- 2
  --  \   /
  --   \ /
  --    3       4 --- 5
graph4 :: AdjMatrix ()
graph4 =
  adjMatrix
    [ -- 1 --
        -- 1      2        3        4        5
      [Nothing, Just (), Just (), Nothing, Nothing]
      -- 2 --
        -- 1      2        3        4        5
    , [Just (), Nothing, Just (), Nothing, Nothing]
      -- 3 --
        -- 1      2        3        4        5
    , [Just (), Just (), Nothing, Nothing, Nothing]
      -- 4 --
        -- 1      2        3        4        5
    , [Nothing, Nothing, Nothing, Nothing, Just ()]
      -- 5 --
        -- 1      2        3        4        5
    , [Nothing, Nothing, Nothing, Just (), Nothing]
    ]


  -- A --- B
  --  \   /
  --   \ /
  --    C
  --     \
  --      D --- E

graph5 :: AdjMatrix ()
graph5 =
  adjMatrix
    [ -- A --
        -- A      B        C        D        E
      [Nothing, Just (), Just (), Nothing, Nothing]
      -- B --
        -- A      B        C        D        E
    , [Just (), Nothing, Just (), Nothing, Nothing]
      -- C --
        -- A      B        C        D        E
    , [Just (), Just (), Nothing, Just (), Nothing]
      -- D --
        -- A      B        C        D        E
    , [Nothing, Nothing, Just (), Nothing, Just ()]
      -- E --
        -- A      B        C        D        E
    , [Nothing, Nothing, Nothing, Just (), Nothing]
    ]

  -- D --- E

  -- A --- B
  --  \   /
  --   \ /
  --    C
graph6 :: AdjMatrix ()
graph6 =
  adjMatrix
    [ -- D --
        -- D      E        A        B        C
      [Nothing, Just (), Nothing, Nothing, Nothing]
      -- E --
        -- D      E        A        B        C
    , [Just (), Nothing, Nothing, Nothing, Nothing]
      -- A --
        -- D      E        A        B        C
    , [Nothing, Nothing, Nothing, Just (), Just ()]
      -- B --
        -- D      E        A        B        C
    , [Nothing, Nothing, Just (), Nothing, Just ()]
      -- C --
        -- D      E        A        B        C
    , [Nothing, Nothing, Just (), Just (), Nothing]
    ]

  -- A --- B --- C
  --                \
  --                 E
  --     D ---------/

graph7 :: AdjMatrix ()
graph7 =
  adjMatrix
    [ -- A --
        -- A      B        C        D        E
      [Nothing, Just (), Nothing, Nothing, Nothing]
      -- B --
        -- A      B        C        D        E
    , [Just (), Nothing, Just (), Nothing, Nothing]
      -- C --
        -- A      B        C        D        E
    , [Nothing, Just (), Nothing, Nothing, Just ()]
      -- D --
        -- A      B        C        D        E
    , [Nothing, Nothing, Nothing, Nothing, Just ()]
      -- E --
        -- A      B        C        D        E
    , [Nothing, Nothing, Just (), Just (), Nothing]
    ]

-- 0 --- 1
graph8 :: AdjMatrix ()
graph8 = AdjMatrix
  [ [Just (), Just ()]
  , [Just (), Nothing]
  ]

  --   0
  --  / \
  -- 1---2
  --  \ /
  --   3
graph9 :: AdjMatrix ()
graph9 = adjMatrix
  [ [Nothing, Just (), Just (), Nothing]
  , [Just (), Nothing, Just (), Just ()]
  , [Just (), Just (), Nothing, Just ()]
  , [Nothing, Just (), Just (), Nothing]
  ]
  