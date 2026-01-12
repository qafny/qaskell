{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}


module NewDesign.Program
  where

import Data.Int

data Constraint value choice where
   Move :: forall n (n::Nat). NTuple n (value,choice) -> Constraint value choice
   Stay :: forall n (n::Nat). NTuple n (value,choice) -> Constraint value choice

-- need order? or order is part of the struct?

data Program t value choice energy =
  Program
    { choices :: [choice]
    , struct :: t value
    , constraints :: [Constraint value choice -> energy]
    }
    
--NTupleFunction keeps the same
--Move and Stay have only compilation difference. Stay compiles to aa* + a*a (ZZ), while Move compiles to a + a* (X +- iY)

--example structure for fermion and anyon
--other than the graph datastructure, users need to support the path calculation from one place to the other
--users also need to support the approximate partition of pi/2, used in the phase exp(i theta), 
--i.e., what is the smallest value of theta, as appx = pi/2/theta
data OrderedGraph t index value =
  Graph { 
          graph :: t index value
        ,  path :: index -> index -> [index]
        ,  appx :: Nat -- number of partition in pi/2. fermion is 1, and anyon can be any
        }


-- num partitioning: 1 constraint, Stay with view 2.

-- graph partitioning: 1 constraint, Stay with view 2.

-- clique finding: 2 constraints, Stay with view 1, and Stay with view 2

-- Binary Linear programming, 2 constraints, Stay with view 1, and Staty with view 2

-- extract cover: 2 constraints, Stay with view 1, and Stay with view 2

-- Set Packing: 2 constraints, Stay with view 1, and Stay with view 2

-- vertex cover: 2 constraints, Stay with view 1, and Stay with view 2

-- Sat problem: 1 constraint, Stay with view 3

-- Maximal Matching : 3 constraints, 2 for Stay with view 2, and Stay with view 1

-- knapsack problem: 2 constraints, Stay with view 1, and Stay with view 2

-- job sequencing: 2 constraints, Stay with view 1, and Stay with view 2

-- Hamiltonian circle: 2 constraints, Stay with view 2
-- another direction, use swap operation to swap vertex ranks in the circle, having one Move with view 2

-- Travel salesman: 3 constraints, Stay with view 2
-- another direction, use swap operation to swap vertex ranks in the circle, having one Move with view 2

-- Tree Problems: (minimum spinning tree) 1 constraints, Stay with view 2
-- Tree problem can utilize the fact that a connected tree much have n-1 edges for n vertices.
-- We can prepare a superposition with hamming weight of n-1, and use the swap initial hamiltonian in graph coloring.
