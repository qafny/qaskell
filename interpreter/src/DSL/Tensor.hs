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

-- generate choices, m is a list (superpositions), of t b, t is a data-structure. 
-- For quantum, a data-structure is a basis-ket.
-- consider the Univ data-structure, t need to be Univ s t a, where a = () OR Univ s t () OR Univ s t (Univ s t ())
type ChoiceStrategy m t a b = t a -> m (t b)

generateChoices :: (Monad m, Traversable t) =>
                   ChoiceStrategy m t a b -> t a -> m (t b)
generateChoices = undefined

--possible example, gen_superpositions
                   
-- comonad data-structure 
data Univ s t (a:: t -> Type) = Univ {
  sextract :: s -> a,
  sval :: t s --how to encode this.
}

instance Functor (Univ s) where
   fmap f (Univ xtract v) = Univ xtract (fmap f v)

transfer f [] acc = (\x -> Univ f [])
transfer f s = (\x -> if s == x then Univ f (tl s) else transfer f (tl s))

instance Extend (Univ s) where
  duplicate (Univ f s) = Univ (transfer f s) s

instance Comonad (Univ s) where
   extract :: Univ s a -> a
   extract (Univ xtract v) = xtract v

                   
data Spin = KeepPos | KeepNeg | ToPos | ToNeg

--traverse on m level, (a -> m b) -> t a -> m (t b) , where b = [Spin], each spin sequantially applies to a data-point
firstSwap q = traverse (\x -> [ToPos, ToNeg]) q

firstSwapM m = foldr (\q -> traverse (\x -> [ToPos, ToNeg]) q) m [] -- t b

applyTwo :: t (t a) -> b -> b -> t (t b)
applyTwo (Univ xtr v) x y = Univ (fmap (\q -> let Univ xtr' v' = (xtr q) in Univ xtr' (fmap (\p -> y) v')) v) (fmap (\q -> x) v)

applyThree :: t (t (t a)) -> b -> b -> b -> t (t (t b))
applyThree (Univ xtr v) x y z = Univ (fmap (\q -> let Univ xtr' v' = (xtr q) 
           in Univ xtr' (fmap (\p -> let Univ xtr'' v'' = (xtr' p) in Univ xtr'' (fmap (\r -> z) v'')) v'')) (fmap (\q -> y) v')) (fmap (\q -> x) v)


secondSwap = traverse (\x -> [applyTwo (duplicate x) ToPos ToNeg] ++ [applyTwo (duplicate x) ToNeg ToPos]) q

secondSwapM = foldr (\q -> traverse (\x -> [applyTwo (duplicate x) ToPos ToNeg] ++ [applyTwo (duplicate x) ToNeg ToPos]) q) m []

       
--define examples, Univ s (), returns Univ ([Spin s]) () , 
-- Clique Finding, 
-- Gen x. Gen y. KeepPos x . KeepPos y
-- Gen x. Gen y. Swap [x,y]
bindChoice1 m = let dd = duplicate m in applyTwo dd KeepPos KeepPos
bindChoice2 m = secondSwapM m

----Equal Sum, 
---- Gen x. Swap [x]
---- ID
--bindChoice1 m = firstSwapM m

--bindChoice2 m = m <&> (\q -> applyTwo (duplicate q) KeepPos KeepNeg)

------Exact Cover
----bindChoice1 m = firstSwapM m
------let sd = sval d in sd <&> (\x -> duplicate [Swap [x]])
----bindChoice2 m = m <&> 
----        (applyThree (duplicate duplicate d) KeepPos KeepNeg KeepNeg)
----        ++ (applyThree (duplicate duplicate d) KeepNeg KeepPos KeepNeg)
----                ++ (applyThree (duplicate duplicate d) KeepNeg KeepNeg KeepPos)

----flatternOne :: t b -> [b]
----flatternOne (Univ f v) = v
----flatternOneM m = m <&>  flatternOne

----flatternTwo :: t t b -> [b]
----flatternTwo (Univ f v) = foldr (\q -> foldr (\p m -> [q,p]++m) (f q) []) v []
----flatternTwoM m = m <&>  flatternTwo

----flatternThree :: t t t b -> [b]
----flatternThree (Univ f v) = foldr (\q -> foldr (\Univ f' v' -> \ m -> (foldr (\r m -> [q,p,r]++m) (f q) [])) (f q) []) v []
----flatternThreeM m = m <&>  flatternThree


------ Amp adding steps

------ Clique Finding, 
------AppAmp (KeepPos x . KeepPos y) = if g(x,y) then (* 0) else ID -- load the edge weight between x and y
------Forall (*) AppAmp 1. -- the equal sum example has a sum of all AppAmp
----appAmp m = foldr (\q -> (foldr (\p -> if p == [KeepPos, KeepPos] && g(x,y) then (*0) else (*1)) (flatternTwoM q) 1, q)) m (1,[])

------equal sum example, just access the x-th element.
------ In (+) operation forall case, ID means (+0)
------AppAmp (KeepPos x) = g(x)
------AppAmp (KeepNeg x) = - g(x)
------Forall (+) AppAmp 0. -- the equal sum example has a sum of all AppAmp

----appAmp m = foldr (\q -> foldr (\p -> if p == [KeepPos] then (+ (g(x))) else (+ (- g(x)))) (flatternOneM q) 0, q) m (1,[])

------ flatternOneM q <&> if q == [KeepPos x] then (+ (g(x))) else (+0)) m []
------ foldM (dd <&> (\x -> ((sextract dd) x) <&> (\y -> (aux y, y)))) dd 0
------  where aux [KeepPos x] = (+ (g(x))) 
------        aux [KeepNeg x] = (+ (- g(x)))
  
  
------ Exact Cover, 
------AppAmp (KeepPos x. KeepNeg y. KeepNeg z) = if h(x,y,z) then (* 0) else ID
------AppAmp (KeepNeg x. KeepPos y. KeepNeg z) = if h(x,y,z) then (* 0) else ID
------AppAmp (KeepNeg x. KeepNeg y. KeepPos z) = if h(x,y,z) then (* 0) else ID

------Forall (*) AppAmp 1. -- the equal sum example has a sum of all AppAmp
----appAmp m = foldr (\q -> (foldr (\p -> if p == [KeepPos, KeepNeg,KeepNeg] && h(1,0,0) then (*0)
----                                      else if p == [KeepNeg,KeepPos,KeepNeg] && h(0,1,0) then (*0)
----                                      else if p == [KeepNeg,KeepNeg,KeepPos] && h(0,0,1) then (*0) else (*1)) (flatternTwoM q) 1, q)) m (1,[])

------appAmp dd = foldM (dd <&> (\x -> ((sextract dd) x) <&> (\y -> (aux y, y)))) dd 1
------  where aux [KeepPos x, KeepNeg y, KeepNeg z] = if h(g(x),g(y),g(z)) then (* 0) else (*1)
------        aux [KeepNeg x, KeepPos y, KeepNeg z] = if h(g(x),g(y),g(z)) then (* 0) else (*1)
------        aux [KeepNeg x, KeepNeg y, KeepPos z] = if h(g(x),g(y),g(z)) then (* 0) else (*1)

----solveF :: (Foldable f, Ord a) =>
----  f a -> a
----solveF = minimum


------ Selection step:  Sel = Gen x . Sel: for each data-structure, we create an x to represent a site.
------ We could do Gen x. Gen y . , where we create x represent a site, and create y representing another site that is not x.
------ form to n adj sites. manipulation on data-structure. If not, then ID = KeepPos || KeepNeg (but we might want this, since we have penalties adding step).
------ How to properly write two variables, 
------ Gen x. KeepPos x -> ok, 
------ Gen x. Gen y. KeepPos x . KeepPos y --> possibly ok, I mean KeepPos y . KeepPos x are the same. 
------ Gen x. Gen y. Swap [x] . KeepPos y --> ?
------ Gen x. Gen y. Swap [x,y] --> ? meaning that x and y can only be 01 or 10, they cannot be 11, or 00

------Clique finding  having two
------ Gen x. Gen y. KeepPos x . KeepPos y
------ Gen x. Gen y. Swap [x,y]

------Equal Sum
------ in gen choice step, ID means Identity matrix, which means KeepPos x || KeepNeg x
------ Gen x. Swap [x]
------ ID

------ Exact Cover
------ Gen x. Swap [x]
------ ID

------ Sel x = Gen x . Sel x | Spin x | Sel x || Sel x

------ Second problem is with the parallel operation, it means the sum Sel x + Sel x, should I allow it here, or should I put it in the library
------ and say that every allowed selection is ||_n (Sel x), like n parallel operations over Sel x.
------ the simplest case of a datatype for the Spin could be a list or a Stream
------ or we could use cellular automaton


------ genChoice then could become taking the Spin expression V x and map the expression onto all the sites in a system.
------ the problem is with the type
------ type of Gen x . v -> m a -> (a -> b) -> m b.
------ type of Gen x. Gen y. v -> m a -> (a -> a -> b) -> m b

------ Energy step is to add penalties to amplitudes
------ This step is applied to the Spin level. For each Spin operation, we apply a operation dealing with the existing amplitude
------ The assumption is that there is an existing energy. And the energy manipulation is to apply to an energy
------ for example, to implement the Z gate, we apply a negative operation to the existing amplitude
------ for the unspecified cases, the amplitude is identity
------ AppAmp (KeepPos x) = (-)

------ In clique finding step, we need to access the edges between x and y
------ In (*) operation forall case, ID means (*1)
------ AppAmp (KeepPos x . KeepPos y) = if g(x,y) then (* 0) else ID -- load the edge weight between x and y


------ Forall (*) AppAmp 1. -- the equal sum example has a sum of all AppAmp




------exact cover example, accesses three sites, x, y, z.
------the exact cover is just saying, there exists a h
------AppAmp (KeepPos x. KeepNeg y. KeepNeg z) = if h(x,y,z) then (* 0) else ID
------AppAmp (KeepNeg x. KeepPos y. KeepNeg z) = if h(x,y,z) then (* 0) else ID
------AppAmp (KeepNeg x. KeepNeg y. KeepPos z) = if h(x,y,z) then (* 0) else ID
------Forall (*) AppAmp 1. -- the equal sum example has a sum of all AppAmp



------ the final minimization step is to do the adiabatic computation minimize
----miniStep = minimize
------ we need to translate the above expression to matrix in adiabatic evolution
------ for each Spin step, let's split the site to x vs non_x, as (x, x_1, x_2, ....), 
------ for each spin step f(x), we will generate the matrix for x as f(x) tensor I(x_1) tensor I(x_2) tensor ...
------ for all sites, we apply each one of the with a spin-step and generate a tensor, then we linear-sum all of them.




