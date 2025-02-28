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
    , constraints :: DepPair NatS (TupleFn (a, b) c)
    }

