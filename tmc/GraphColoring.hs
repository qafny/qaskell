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

graphProblem :: Graph () -> Prob Graph () Color
graphProblem g = Prob g (const (pure R <|> pure G <|> pure B))

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
  
