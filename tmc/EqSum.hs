module EqSum
  where

import Control.Comonad
import Control.Comonad.Env
import Data.Functor.Identity
import Control.Applicative

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Foldable

import GenericSolver

listProblem :: [Int] -> Prob [] Int Int
listProblem xs =
  Prob xs (\x -> pure x <|> pure (-x))

eqSum ::
  EnergySpace [] (Env [Int]) Int
eqSum = EnergySpace
  { embed = \xs -> env xs 0
  , combineE = (+)
  , finalizeE = (^ 2)
  , localE = \xs ->
      fromIntegral $ sum (ask xs)
  }

run :: [Int] -> IO ()
run xs = do
  let
    bestEnergy :: Double
    bestCfg :: [Int]
    (bestCfg, bestEnergy) = head $ optimize (listProblem xs) eqSum exhaustiveSearch

  putStrLn $ "Minimum energy: " ++ show bestEnergy
  putStrLn $ "Best config: " ++ show bestCfg

