module MC where

import Control.Comonad
import Control.Monad (MonadPlus(..))
import Data.Foldable (for_)
import Data.Map.Strict (Map, fromList, (!), mapWithKey, toList)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Text.Printf (printf)
import Control.Applicative ((<|>))

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Generic parts

data OptimizationProblem s choice = OptimizationProblem
  { shape      :: s ()                                
  , choose     :: forall m. MonadPlus m => m choice   
  }

generateProblem :: (Traversable s, MonadPlus m) => 
  OptimizationProblem s choice -> m (s choice)
generateProblem (OptimizationProblem sh ch) = traverse (const ch) sh

data EnergySpace s w choice = EnergySpace
  { embed       :: s choice -> w choice
  , localE      :: w choice -> Double
  , combineE    :: Double -> Double -> Double
  , finalizeE   :: Double -> Double
  }

generateSolutions :: (MonadPlus m, Comonad w, Foldable w)
  => m (s choice) -> EnergySpace s w choice -> m (s choice, Double)
generateSolutions choices (EnergySpace embed local combine finalize) = do
  cfg <- choices
  let w = embed cfg
      scores = extend local w
      total = finalize (foldr combine 0.0 scores)
  return (cfg, total)

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Graph coloring specific parts

data Graph a = Graph 
  { nodeValues :: Map Int a
  , neighbors  :: Map Int [Int]
  } deriving Show

instance Functor Graph where
    fmap f g = g { nodeValues = fmap f (nodeValues g) }

instance Foldable Graph where
    foldMap f = foldMap f . nodeValues

instance Traversable Graph where
    traverse f g = (\vals' -> g { nodeValues = vals' }) <$> traverse f (nodeValues g)

getNeighborsOf :: Int -> Graph a -> [(Int, a)]
getNeighborsOf nodeId g = 
    let vals = nodeValues g
        nbrs = neighbors g ! nodeId
    in zip nbrs (map (vals !) nbrs)

-- === Example Graphs ===

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

-- === Colors for Graph Coloring ===

data Color = R | G | B
  deriving (Eq, Show, Enum)

colorChoice :: MonadPlus m => m Color
colorChoice = pure R <|> pure G <|> pure B

graphProblem :: Graph () -> OptimizationProblem Graph Color
graphProblem g = OptimizationProblem g colorChoice

-- === Comonadic Focused Graph ===

data FocusedGraph a = FocusedGraph 
  { graph :: Graph a
  , focus :: Int
  } deriving (Show, Functor, Foldable)

getNodeValue :: FocusedGraph a -> a
getNodeValue fg = nodeValues (graph fg) ! focus fg

getNeighbors :: FocusedGraph a -> [a]
getNeighbors fg = map snd $ getNeighborsOf (focus fg) (graph fg)

instance Comonad FocusedGraph where
    extract = getNodeValue
    extend f fg = 
        let g' = graph fg
            recompute = mapWithKey (\nodeId _ -> f (FocusedGraph g' nodeId))
            newVals = recompute (nodeValues g')
        in fg { graph = g' { nodeValues = newVals } }

-- === Energy Computations ===

countMatches :: Eq a => a -> [a] -> Double
countMatches x = fromIntegral . length . filter (== x)

--

focusedEmbed :: Graph a -> FocusedGraph a
focusedEmbed g = FocusedGraph g 1  -- arbitrary starting focus

localEnergy :: Eq a => FocusedGraph a -> Double
localEnergy fg = countMatches (extract fg) (getNeighbors fg)

energySpace :: Eq a => EnergySpace Graph FocusedGraph a
energySpace = EnergySpace
  { embed     = focusedEmbed
  , localE    = localEnergy
  , combineE  = (+)
  , finalizeE = id
  }

-- === Run ===

printColoring :: Graph Color -> IO ()
printColoring g0 = 
    for_ (toList $ nodeValues g0) $ \(nodeId, color) ->
        printf "Node %d → %s\n" nodeId (show color)

runGraphColoring :: Graph () -> IO ()
runGraphColoring g = do
  let candidates = generateProblem (graphProblem g) :: [Graph Color]
      solutions  = generateSolutions candidates energySpace
  let best = minimumBy (comparing snd) solutions
  putStrLn $ "Minimum energy: " ++ show (snd best)
  printColoring (fst best)

-- === Main ===

main :: Graph () -> IO ()
main = runGraphColoring 

