--
-- Sigma type
--

{-# LANGUAGE PolyKinds #-}

module DependentView.DepPair where

data DepPair f g where
  DepPair :: forall k (i :: k) f p.
    f i -> p i -> DepPair f p

