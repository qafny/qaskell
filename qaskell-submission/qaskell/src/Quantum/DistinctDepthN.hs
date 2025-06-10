module Quantum.DistinctDepthN
  where

import Data.List (subsequences)
import Data.Maybe

import qualified Data.Set as Set

class Part a where
  immediateChildren :: a -> [a]
  truncateHere :: Int -> a -> Maybe a

instance Part [a] where
  immediateChildren = drop 1 . init . subsequences

  truncateHere n xs
    | n > length xs = Nothing
    | otherwise     = Just $ take n xs

data Action = Descend | TruncateHere
  deriving (Show)

distinctNTuples :: (Ord a, Part a) => Int -> a -> [a]
distinctNTuples n t = nub' $ do
  action <- [Descend, TruncateHere]
  case action of
    Descend -> do
      child <- immediateChildren t
      distinctNTuples n child

    TruncateHere -> maybeToList $ truncateHere n t

nub' :: Ord a => [a] -> [a]
nub' = Set.toList . Set.fromList

