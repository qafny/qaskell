{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Eval2 where

import Control.Monad

------------------------------------------------------------------------------
-- We are given some traversable data structure. Basic examples include
-- the list type [a] from the Haskell library and trees as defined below

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a) 
  fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)

instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Node t1 t2) = foldr f (foldr f b t2) t1

instance Traversable Tree where
  traverse f (Leaf a) = pure Leaf <*> f a
  traverse f (Node t1 t2) = pure Node <*> traverse f t1 <*> traverse f t2

-- Some small example trees for experimentation

tr1, tr2, tr3 :: Tree Int
tr1 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
tr2 = Node (Leaf 10) (Node (Leaf 20) (Leaf 30))
tr3 = Node tr1 (Node tr1 tr2)

------------------------------------------------------------------------------
-- We want to create choices at each position in a data structure
-- and from that, create a choice of data structures.

-- This is the basic abstraction of "traverse". We will traverse the
-- data structure with a MonadPlus operation representing (weighted) choices.
-- For now we use integers to represent the weights.

generateChoices :: (MonadPlus m, Traversable t) => 
                   Int -> Int -> t a -> m (t (Int,a))
generateChoices d1 d2 = traverse $ \a -> return (d1 , a) `mplus` return (d2 , a)

-- Now that we generated all the choices we need to fold over the choices
-- to choose the "minimum" one or more generally the one(s) satisfying
-- the desired constraints

energy :: Foldable t => t (Int,Int) -> Int
energy ch = sq $ foldr (\(d,a) s -> s + d*a) 0 ch
  where sq a = a * a

solveF :: (Functor f, Foldable f, Foldable t) => f (t (Int,Int)) -> Int
solveF chs = foldr1 min $ fmap energy chs

-- The equal sum example

eqSumL :: [Int] -> Int
eqSumL ns = solveF choices
  where choices :: [[(Int,Int)]]
        choices = generateChoices 1 (-1) ns

{--

ghci> eqSumL [10,20,30]
0
ghci> eqSumL [1,5,2,8,10]
0
ghci> eqSumL [1,5,7,8,10]
1

--}

-- A version of equal sum for trees

eqSumT :: Tree Int -> Int
eqSumT tr = solveF choices
  where choices :: [ Tree (Int,Int) ]
        choices = generateChoices 1 (-1) tr

{--

ghci> eqSumT tr2
0

--}

------------------------------------------------------------------------------
-- Graph data structure with Int vertices and edges
data Graph e = Graph { vertices :: [Int], edges :: [(Int, e, Int)] }
  deriving Show

-- instance Functor Graph where
--   fmap f (Graph vs es) = Graph vs (map (\(v1, e, v2) -> (v1, f e, v2)) es)
instance Functor Graph where
  fmap f (Graph vs es) = Graph vs (pure (\(v1, e, v2) -> (v1, f e, v2)) <*> es)

instance Foldable Graph where
  foldr f acc (Graph _ es) = foldr (\(_, e, _) acc -> f e acc) acc es

instance Traversable Graph where
  -- traverse :: Applicative f => (a -> f b) -> Graph a -> f (Graph b)
  traverse f (Graph vs es) = Graph vs <$> traverse (\(v1, e, v2) -> (\e' -> (v1, e', v2)) <$> f e) es

-- Example graph for testing Traversable on edges
graphEdgesExample :: Graph Int
graphEdgesExample = Graph { vertices = [10, 20, 30], edges = [(10, 1, 20), (20, 2, 30), (30, 3, 10)] }

-- Traversing edges with a function (for example, incrementing each edge weight by 1)
incrementEdges :: Graph Int -> Graph Int
incrementEdges = fmap (+1)

-- Function to update edge weights based on the product of the vertices
-- updateEdgeWeights :: Graph Int -> Graph Int
-- updateEdgeWeights (Graph vs es) = Graph vs (map updateEdge es)
--   where
--     -- updateEdge sets the weight to be the product of the two vertices
--     updateEdge (v1, _, v2) = (v1, v1 * v2, v2)
updateEdgeWeights (Graph vs es) = Graph vs (pure updateEdge <*> es)
  where
    updateEdge (v1, _, v2) = (v1, v1 * v2, v2)

-- Traversing vertices by creating a list of each vertex (demonstrates Foldable behavior)
verticesList :: Graph e -> [Int]
verticesList = vertices

-- Test incrementEdges to increase each edge weight in graphEdgesExample
testIncrementEdges :: Graph Int
testIncrementEdges = incrementEdges graphEdgesExample

-- Collect the vertices in a list
testVerticesList :: [Int]
testVerticesList = verticesList graphEdgesExample

-- Test updateEdgeWeights to update each edge weight to the product of the vertices in graphEdgesExample
testUpdateEdgeWeights :: Graph Int
testUpdateEdgeWeights = updateEdgeWeights graphEdgesExample

-- edgeTraverse function that traverses edges, applies a user-defined operation to edge weights,
-- and collapses with a user-defined function
-- edgeTraverse :: (Num e) => (e -> e -> e) -> Graph e -> e
-- edgeTraverse collapse (Graph _ es) = foldr collapse 1 (map extractEdgeWeight es)
--   where
--     extractEdgeWeight (_, weight, _) = weight
edgeTraverse collapse (Graph _ es) = foldr collapse 1 (pure extractEdgeWeight <*> es)
  where
    extractEdgeWeight (_, weight, _) = weight

-- Example collapse operations
sumCollapse :: Int -> Int -> Int
sumCollapse = (+)

prodCollapse :: Int -> Int -> Int
prodCollapse = (*)

minCollapse :: Int -> Int -> Int
minCollapse = min

-- Example usage
-- Calculate the sum of all edge weights
testSumCollapse :: Int
testSumCollapse = edgeTraverse sumCollapse graphEdgesExample

testProdCollapse :: Int
testProdCollapse = edgeTraverse prodCollapse graphEdgesExample

-- Calculate the minimum of all edge weights
testMinCollapse :: Int
testMinCollapse = edgeTraverse minCollapse graphEdgesExample

{--

ghci> eqSumG graphEdgesExample
0

--}

eqSumG :: Graph Int -> Int
eqSumG g = solveF choices
  where choices :: [ Graph (Int,Int) ]
        choices = generateChoices 1 (-1) g

{--

ghci> graphPartition exampleGraph
1

--}
exampleGraph :: Graph Int
exampleGraph = Graph { vertices = [1, -1, 2], edges = [(1, 1, 2), (-1, 1, 1), (2, 1, -1)] }

graphPartition :: Graph Int -> Int
graphPartition gr = solveF choices
  where
    -- Calculates the "adjacency" based on vertex values for each edge
    calculateAdjacent :: (Int, Int, Int) -> Int
    calculateAdjacent (su, _, sv) = (1 - (su * sv)) `div` 2

    -- Generate choices by traversing edges and calculating adjacency values
    choices :: [[(Int, Int)]]
    choices = traverse (\(v1, weight, v2) -> [(weight, calculateAdjacent (v1, weight, v2))]) (edges gr)


