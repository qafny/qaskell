module EqSum
  where

import Control.Comonad
-- import Data.Functor.Identity
import Control.Applicative

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import GenericSolver

listProblem :: [Int] -> Prob [] Int Int
listProblem xs =
  Prob xs (\x -> pure x <|> pure (-x))

eqSum ::
  EnergySpace [] NonEmpty Int
eqSum = EnergySpace
  { embed = \(x:xs) -> x :| xs
  , combineE = (+)
  , finalizeE = id
  , localE = \xs -> abs $ fromIntegral $ sum xs
  }

run :: [Int] -> IO ()
run xs = do
  let
    bestEnergy :: Double
    bestCfg :: [Int]
    (bestCfg, bestEnergy) = head $ optimize (listProblem xs) eqSum exhaustiveSearch

  putStrLn $ "Minimum energy: " ++ show bestEnergy
  putStrLn $ "Best config: " ++ show bestCfg

