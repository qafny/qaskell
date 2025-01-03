{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSL.EqSum
  (eqSum)
  where

import Control.Monad
import Data.Proxy
import DSL.Solve (solveF, generateChoices, ChoiceStrategy)

listStrategy :: (MonadPlus m, Traversable t) => [b] -> ChoiceStrategy m t a b
listStrategy weights struct = traverse (\_ -> msum (map return weights)) struct

-- Example usage:
--   ghci> minimum (eqSum (Proxy @[]) [1,2,3,4,5])
--   1
--   ghci> send (eqSum (Proxy @Super) [1,2,3,4,5])
--   1

eqSum :: forall m. (MonadPlus m) =>
  Proxy m -> -- To disambiguate the MonadPlus instance
  [Int] ->   -- Input list of integers
  m Int      -- Result of computation
eqSum Proxy ns = energies
  where
    -- Generate all choices using the universal generateChoices
    choices :: m [Int]
    choices = generateChoices (listStrategy [1, -1]) ns

    -- Compute energies for each choice
    energies :: m Int
    energies = fmap energy choices

    -- Energy calculation for each choice
    energy :: [Int] -> Int
    energy ch = sq $ foldr (\(d, a) s -> s + d * a) 0 (zip ch ns)
      where sq a = a * a
