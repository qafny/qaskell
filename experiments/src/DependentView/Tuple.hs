{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module DependentView.Tuple
  where

import DependentView.Nat

data Tuple (n :: Nat) a where
  Nil :: Tuple 'Z a
  Cons :: a -> Tuple n a -> Tuple ('S n) a

infixr :>
pattern (:>) :: a -> Tuple n a -> Tuple ('S n) a
pattern x :> xs = Cons x xs

