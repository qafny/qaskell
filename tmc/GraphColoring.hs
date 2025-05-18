module GraphColoring where

import Control.Comonad (Comonad(..))
import Control.Monad (MonadPlus(..))
import Data.Foldable (for_)
import Data.Map.Strict (Map, (!), mapWithKey, fromList, toList)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Text.Printf (printf)
import Control.Applicative ((<|>))

import GenericSolver
import Graph

-- === Graph colors ===

data Color = R | G | B
  deriving (Eq, Show, Enum)

graphProblem :: Graph () -> Prob Graph Color
graphProblem g = Prob g (pure R <|> pure G <|> pure B)

-- === Energy Computations ===

energySpace :: Eq a => EnergySpace Graph FocusedGraph a
energySpace = EnergySpace
  { embed     = \g -> FocusedGraph g 1  
  , localE    = \ (FocusedGraph g v) ->
                       let nodeColor = nodeValues g ! v
                           nbrs = neighbors g ! v
                           neighborColors = map (nodeValues g !) nbrs
                           countSameColor = length . filter (== nodeColor)
                       in fromIntegral $ countSameColor neighborColors
  , combineE  = (+)
  , finalizeE = id
  }

-- === Run ===

run :: Graph () -> IO ()
run g = do
  let (bestCfg, bestEnergy) = head $ optimize (graphProblem g) energySpace exhaustiveSearch
  putStrLn $ "Minimum energy: " ++ show bestEnergy
  for_ (toList $ nodeValues bestCfg) $ \(nodeId, color) ->
      printf "Node %d → %s\n" nodeId (show color)

--  let (bestCfg, bestEnergy) = head $ optimize (graphProblem g) energySpace firstSolution
--  let (bestCfg, bestEnergy) = head $ optimize (graphProblem g) energySpace (thresholdSearch 22.0)
  
g1 :: Graph ()
g1 = Graph 
  { nodeValues = fromList $ map (\n -> (n, ())) [1, 2, 3, 4]
  , neighbors  = fromList [ (1, [2, 3]), (2, [1, 3]), (3, [1, 2]), (4, [2, 3]) ]
  }

g2 :: Graph ()
g2 = Graph
  { nodeValues = fromList $ map (\n -> (n, ())) [1..8]
  , neighbors  = fromList 
      [ (1, [2, 3, 4])
      , (2, [1, 4, 5])
      , (3, [1, 4, 6])
      , (4, [1, 2, 3, 7])
      , (5, [2, 7, 8])
      , (6, [3, 7, 8])

      , (7, [4, 5, 6, 8])
      , (8, [5, 6, 7])
      ]
  }

g3 :: Graph ()
g3 = Graph
  { nodeValues = fromList $ map (\n -> (n, ())) [1..12]
  , neighbors  = fromList 
      [ (1,  [2, 4, 5])
      , (2,  [1, 3, 5, 6])
      , (3,  [2, 6, 7])
      , (4,  [1, 5, 8])
      , (5,  [1, 2, 4, 6, 8, 9])
      , (6,  [2, 3, 5, 7, 9, 10])
      , (7,  [3, 6, 10])
      , (8,  [4, 5, 9, 11])
      , (9,  [5, 6, 8, 10, 11, 12])
      , (10, [6, 7, 9, 12])
      , (11, [8, 9, 12])
      , (12, [9, 10, 11])
      ]
  }

