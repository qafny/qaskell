module CliqueFinding where

import Data.Map.Strict (Map, (!), mapWithKey, fromList, toList)

import GenericSolver
import Graph

energySpace :: EnergySpace Graph FocusedGraph Bool
energySpace = EnergySpace
  { embed = \g -> FocusedGraph g 1
  , combineE = (+)
  , finalizeE = id

  , localE = \ (FocusedGraph g v) ->
                let nbrs = neighbors g ! v
                    neighborPresent = map (nodeValues g !) nbrs
                in
                if and neighborPresent
                then 1
                else 0
  }

