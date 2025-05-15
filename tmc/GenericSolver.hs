module GenericSolver where

import Control.Comonad (Comonad(..))
import Control.Monad (MonadPlus(..))
import Data.List (minimumBy)
import Data.Ord (comparing)

------------------------------------------------------------------------------

data Prob s choice = Prob
  { shape      :: s ()                                
  , choose     :: forall m. MonadPlus m => m choice   
  }

data EnergySpace s w choice = EnergySpace
  { embed       :: s choice -> w choice
  , localE      :: w choice -> Double
  , combineE    :: Double -> Double -> Double
  , finalizeE   :: Double -> Double
  }

data SearchStrategy m s w choice = SearchStrategy
  { runSearch :: m (s choice) -> (s choice -> (s choice, Double)) -> m (s choice, Double)
  }

generateProblem :: (Traversable s, MonadPlus m) => Prob s choice -> m (s choice)
generateProblem (Prob sh ch) = traverse (const ch) sh

scoreSolution :: (Comonad w, Foldable w) => EnergySpace s w choice -> s choice -> (s choice, Double)
scoreSolution (EnergySpace embed local combine finalize) cfg =
  let w = embed cfg
      total = finalize (foldr combine 0.0 (extend local w))
  in (cfg, total)

optimize :: (Traversable s, Comonad w, Foldable w, MonadPlus m) => 
  Prob s choice -> EnergySpace s w choice -> SearchStrategy m s w choice ->
  m (s choice, Double)
optimize problem energySpace strategy =
  let candidates = generateProblem problem
      score      = scoreSolution energySpace
  in runSearch strategy candidates score


------------------------------------------------------------------------------
-- A few search strategies

exhaustiveSearch :: SearchStrategy [] s w choice
exhaustiveSearch = SearchStrategy $ \candidates score ->
  pure $ minimumBy (comparing snd) (map score candidates)

firstSolution :: Monad m => SearchStrategy m s w choice
firstSolution = SearchStrategy $ \candidates score -> do
  c <- candidates
  pure (score c)

thresholdSearch :: MonadPlus m => Double -> SearchStrategy m s w choice
thresholdSearch threshold = SearchStrategy $ \candidates score -> do
  c <- candidates
  let sc = score c
  if snd sc < threshold then pure sc else mzero

------------------------------------------------------------------------------
------------------------------------------------------------------------------
