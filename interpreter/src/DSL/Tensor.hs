{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}

module DSL.Tensor
  where

import Data.List

import Prettyprinter
import Data.Coerce

import DSL.Utils
import DSL.Super.Simulated

newtype Tensor a = Tensor [a]
  deriving (Show, Foldable, Functor, Applicative)

tensorPrecedence :: Int
tensorPrecedence = 2

tensorPlaces :: Tensor a -> Tensor (Int, a)
tensorPlaces (Tensor xs) = Tensor (zip [0..] xs)

tensorZipWith :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
tensorZipWith f (Tensor xs) (Tensor ys) = Tensor (zipWith f xs ys)

instance Nesting a => Pretty (Tensor a) where
  pretty (Tensor []) = pretty "[]"
  pretty (Tensor xs) =
    hsep $ intersperse (pretty "⊗") $ map (prettyNested tensorPrecedence) xs

instance Nesting a => Nesting (Tensor a) where
  prettyNested p = parensWhen (p < tensorPrecedence) . pretty

-- a is the amplitude, which is a complex number, b is the index variable, where we will allow to permit arbitrary adjacency.
-- Anni is annihilation, Dag is a dagger of a MuQ term, Tens is a tensor, sum is a linear sum, and circ is the sequencing operation
-- data SndQ a b = I | Anni a b | Dag (SndQ a b) | Tens (SndQ a b) (SndQ a b) | Sum (SndQ a b) (SndQ a b) | Circ (SndQ a b) (SndQ a b)

data Spin x = KeepPos x | KeepNeg x | Swap [x].

-- Selection step:  Sel = Gen x . Sel: for each data-structure, we create an x to represent a site.
-- We could do Gen x. Gen y . , where we create x represent a site, and create y representing another site that is not x.
-- form to n adj sites. manipulation on data-structure. If not, then ID = KeepPos || KeepNeg (but we might want this, since we have penalties adding step).
-- How to properly write two variables, 
-- Gen x. KeepPos x -> ok, 
-- Gen x. Gen y. KeepPos x . KeepPos y --> possibly ok, I mean KeepPos y . KeepPos x are the same. 
-- Gen x. Gen y. Swap [x] . KeepPos y --> ?
-- Gen x. Gen y. Swap [x,y] --> ? meaning that x and y can only be 01 or 10, they cannot be 11, or 00

--Clique finding  having two
Gen x. Gen y. KeepPos x . KeepPos y
Gen x. Gen y. Swap [x,y]

--Equal Sum
-- in gen choice step, ID means Identity matrix, which means KeepPos x || KeepNeg x
Gen x. Swap [x]
ID

-- Exact Cover
Gen x. Swap [x]
ID

Sel x = Gen x . Sel x | Spin x | Sel x || Sel x

-- Second problem is with the parallel operation, it means the sum Sel x + Sel x, should I allow it here, or should I put it in the library
-- and say that every allowed selection is ||_n (Sel x), like n parallel operations over Sel x.

-- genChoice then could become taking the Spin expression V x and map the expression onto all the sites in a system.
-- the problem is with the type
type of Gen x . v -> m a -> (a -> b) -> m b.

type of Gen x. Gen y. v -> m a -> (a -> a -> b) -> m b

-- Energy step is to add penalties to amplitudes
-- This step is applied to the Spin level. For each Spin operation, we apply a operation dealing with the existing amplitude
-- The assumption is that there is an existing energy. And the energy manipulation is to apply to an energy
-- for example, to implement the Z gate, we apply a negative operation to the existing amplitude
-- for the unspecified cases, the amplitude is identity
AppAmp (KeepPos x) = (-)

-- In clique finding step, we need to access the edges between x and y
-- In (*) operation forall case, ID means (*1)
AppAmp (KeepPos x . KeepPos y) = if g(x,y) then (* 0) else ID -- load the edge weight between x and y


Forall (*) AppAmp 1. -- the equal sum example has a sum of all AppAmp

--equal sum example, just access the x-th element.
-- In (+) operation forall case, ID means (+0)
AppAmp (KeepPos x) = g(x)
AppAmp (KeepNeg x) = - g(x)

Forall (+) AppAmp 0. -- the equal sum example has a sum of all AppAmp


--exact cover example, accesses three sites, x, y, z.
--the exact cover is just saying, there exists a h
AppAmp (KeepPos x. KeepNeg y. KeepNeg z) = if h(x,y,z) then (* 0) else ID
AppAmp (KeepNeg x. KeepPos y. KeepNeg z) = if h(x,y,z) then (* 0) else ID
AppAmp (KeepNeg x. KeepNeg y. KeepPos z) = if h(x,y,z) then (* 0) else ID

Forall (*) AppAmp 1. -- the equal sum example has a sum of all AppAmp



-- the final minimization step is to do the adiabatic computation minimize
miniStep = minimize
-- we need to translate the above expression to matrix in adiabatic evolution
-- for each Spin step, let's split the site to x vs non_x, as (x, x_1, x_2, ....), 
-- for each spin step f(x), we will generate the matrix for x as f(x) tensor I(x_1) tensor I(x_2) tensor ...
-- for all sites, we apply each one of the with a spin-step and generate a tensor, then we linear-sum all of them.




