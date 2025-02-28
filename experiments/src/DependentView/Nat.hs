{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module DependentView.Nat
  where

data Nat = Z | S Nat
  deriving (Show, Eq, Ord)

data NatS (n :: Nat) where
  SingZ :: NatS 'Z
  SingS :: NatS n -> NatS ('S n)

type N0 = 'Z
type N1 = 'S N0
type N2 = 'S N1
type N3 = 'S N2
type N4 = 'S N3

pattern NS0 :: NatS N0
pattern NS0 = SingZ

pattern NS1 :: NatS N1
pattern NS1 = SingS NS0

pattern NS2 :: NatS N2
pattern NS2 = SingS NS1

pattern NS3 :: NatS N3
pattern NS3 = SingS NS2

pattern NS4 :: NatS N4
pattern NS4 = SingS NS3

