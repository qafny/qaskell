module CliqueFinding where

import Data.Map.Strict (Map, (!), mapWithKey, fromList, toList)
import Data.Foldable (for_)
import qualified Data.Foldable as F
import Control.Applicative ((<|>))
import Text.Printf

import GenericSolver
import Graph

graphProblem :: Graph () -> Prob Graph () Bool
graphProblem g = Prob g (const (pure False <|> pure True))

energySpace :: Int -> EnergySpace Graph FocusedGraph Bool
energySpace k = EnergySpace
  { embed = \g -> FocusedGraph g 1
  , combineE = (+)
  , finalizeE = id

  , localE = \ (FocusedGraph g v) ->
                let nbrs = neighbors g ! v
                    neighborPresent = map (nodeValues g !) nbrs
                    trueCount = fromIntegral (length (filter (== True) (F.toList g)))
                in
                abs (trueCount - fromIntegral k)
                 +
                sum (map (\b -> if b then 1 else 0)
                         neighborPresent)
  }

-- === Run ===

run :: Graph () -> Int -> IO ()
run g k = do
  let (bestCfg, bestEnergy) = head $ optimize (graphProblem g) (energySpace k) exhaustiveSearch
  putStrLn $ "Minimum energy: " ++ show bestEnergy
  for_ (toList $ nodeValues bestCfg) $ \(nodeId, color) ->
      printf "Node %d → %s\n" nodeId (show color)

