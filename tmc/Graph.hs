module Graph where

import Control.Comonad
import Data.Map.Strict (Map, (!), mapWithKey, fromList, toList)


-- === Graphs ===

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

-- === Comonadic Focused Graph ===

data FocusedGraph a = FocusedGraph 
  { graph :: Graph a
  , focus :: Int
  } deriving (Show, Functor, Foldable)

instance Comonad FocusedGraph where
    extract = \fg -> nodeValues (graph fg) ! focus fg
    extend f fg = 
        let g' = graph fg
            recompute = mapWithKey (\nodeId _ -> f (FocusedGraph g' nodeId))
            newVals = recompute (nodeValues g')
        in fg { graph = g' { nodeValues = newVals } }

