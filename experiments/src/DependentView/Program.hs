{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module DependentView.Program
  where

import DependentView.Nat
import DependentView.Tuple
import DependentView.DepPair

newtype TupleFn a b n = TupleFn (Tuple n a -> b)

data Program t a b c =
  Program
    { choices :: [b]
    , struct :: t a

        -- This is like:
        --   constraints :: exists (n :: Nat). (Vect n (a, b) -> c)
    , constraints :: DepPair NatS (TupleFn (a, b) c)
    }

-- This is like
--   exampleTupleFn :: exists (n :: Nat). (Vect n (Bool, Bool) -> Int)
exampleTupleFn :: DepPair NatS (TupleFn (Bool, Bool) Int)
exampleTupleFn =
  DepPair
    NS2 $
    TupleFn (\((x1, y1) :> (x2, y2) :> Nil) ->
      if (x1 && y1) || (x2 && y2)
      then 1
      else 0)

