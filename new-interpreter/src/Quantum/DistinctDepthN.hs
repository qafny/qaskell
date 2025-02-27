module Quantum.DistinctDepthN
  where

import Data.List
import Data.Maybe

class Part a where
  immediateChildren :: a -> [a]
  truncateHere :: Int -> a -> Maybe a

-- instance Part (Maybe (Tree Maybe a)) where
--   immediateChildren Nothing = []
--   immediateChildren (Just Leaf) = []
--   immediateChildren (Just (Node _ Nothing)) = []
--   immediateChildren (Just (Node _ (Just (left, right)))) = [Just left, Just right]

--   truncateHere 0 t = Just $ Nothing
--   truncateHere n Nothing = Just $ Nothing
--   truncateHere n (Just Leaf) = Just $ Just Leaf
--   truncateHere n (Just (Node x Nothing)) = Just $ Just $ Node x Nothing
--   truncateHere n (Just (Node x (Just (left, right)))) = Just $ Just $ Node x (liftA2 (,) (join (truncateHere (n-1) (Just left))) (join (truncateHere (n-1) (Just right))))

instance Part [a] where
  immediateChildren = drop 1 . init . subsequences

  truncateHere n xs
    | n > length xs = Nothing
    | otherwise     = Just $ take n xs

data Action = Descend | TruncateHere
  deriving (Show)

distinctNTuples :: (Eq a, Part a) => Int -> a -> [a]
distinctNTuples n t = nub $ do
  action <- [Descend, TruncateHere]
  case action of
    Descend -> do
      child <- immediateChildren t
      distinctNTuples n child

    TruncateHere -> maybeToList $ truncateHere n t

