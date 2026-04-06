{-# LANGUAGE ScopedTypeVariables #-}

module Quantum.Prog where

import Data.Bifunctor (first, second)
import Data.Complex (Complex(..))
import Data.Foldable (toList)

import Quantum.Program
import Quantum.DistinctDepthN (Part, distinctNTuples)


-- | Sparse variant of Program.
-- Changing constraints from a function over all n-tuple (n fixed by view number)
-- assignments to a user-provide  association list of (assignment, cost) pairs,
-- where the assignments can be any x-tuple (x varies).
data Prog t a b c =
  Prog
    { progChoices     :: [b]
    , progStruct      :: t a
    , progConstraints :: [ ([(a, b)] , c) ]
    
    }

{-
Prog vs Program:
  - constraints :: [([(a,b)], c)] — sparse association list instead of a function t (a,b)
  -> c
  - No view field (no longer needed since enumeration is skipped)

  solveQuantum' pipeline:
  - Skips distinctNTuples and assignChoices entirely — no exhaustive enumeration
  - constraintResults is built directly from the sparse list, with annotate recovering
  VarIds by matching a values against varStruct
  - buildTensor is simplified: Tensor applied directly to the list (no toList needed since
  assignments are already [(Var a, b)])
  - Type constraints reduced from (Ord (t (Var a)), Part (t (Var a)), ...) to just (Eq a,
  Eq b, Real c, Traversable t) — the Part/Ord constraints were only needed for
  distinctNTuples

  One note: lookupVarId assumes variable values in the sparse constraint assignments are
  unique within struct (i.e. each a appears at most once).
-}


solveQuantum' :: forall t a b c. (Eq a, Eq b, Real c, Traversable t) =>
  Prog t a b c ->
  Summed (Scaled (Tensor PauliExpr))
solveQuantum' prog = compiled
  where
    Prog { progChoices = progChoices, progStruct = progStruct, progConstraints = progConstraints } = prog

    -- Assign a fresh VarId to every variable in struct, preserving order.
    varStruct :: t (Var a)
    varStruct = runFresh (genChoices progStruct)

    -- Flat lookup table: variable value -> VarId
    varMap :: [(a, VarId)]
    varMap = map (\(Var a i) -> (a, i)) (toList varStruct)

    lookupVarId :: a -> VarId
    lookupVarId a = case lookup a varMap of
      Just i  -> i
      Nothing -> error "solveQuantum': variable not found in struct"

    -- Re-attach VarIds to a flat assignment list from the sparse constraint list.
    annotate :: [(a, b)] -> [(Var a, b)]
    annotate = map (\(a, b) -> (Var a (lookupVarId a), b))

    encodedChoices = encodeChoices progChoices

    decode :: (Var a, b) -> Tensor (Summed ScaledPauli)
    decode (x, c) = decodeChoice encodedChoices c (var x)

    optimize :: (ShowParens x, Ord x) => Summed (Scaled x) -> Summed (Scaled x)
    optimize = clean . combine

    toComplex' :: c -> Complex Double
    toComplex' = fromRational . toRational

    -- Directly from the sparse list; no distinctNTuples / assignChoices needed.
    constraintResults :: [(c, [(Var a, b)])]
    constraintResults = map (\(ab, c) -> (c, annotate ab)) progConstraints

    decodeAndDistribute ::
      [(c, [(Var a, b)])] ->
      [(c, [Summed (Scaled (Tensor PauliExpr))])]
    decodeAndDistribute =
      fmap (\(c, varChoices) ->
              (c, map (fmap floatScalars . distr . decode) varChoices))

    -- Wrap the decoded list directly into a Tensor.
    buildTensor ::
      [(c, [Summed (Scaled (Tensor PauliExpr))])] ->
      [(c, Tensor (Summed (Scaled (Tensor PauliExpr))))]
    buildTensor = map (second Tensor)

    distributeSummedTensor ::
      [(c, Tensor (Summed (Scaled (Tensor PauliExpr))))] ->
      [(c, Summed (Tensor (Scaled (Tensor PauliExpr))))]
    distributeSummedTensor = map (second distr)

    commuteTensorScaling ::
      [(c, Summed (Tensor (Scaled (Tensor PauliExpr))))] ->
      [(c, Summed (Scaled (Tensor PauliExpr)))]
    commuteTensorScaling = map (second (fmap commuteScaledTensor))

    coeffsToComplex ::
      [(c, Summed (Scaled (Tensor PauliExpr)))] ->
      [(Complex Double, Summed (Scaled (Tensor PauliExpr)))]
    coeffsToComplex = map (first toComplex')

    applyScaling ::
      [(Complex Double, Summed (Scaled (Tensor PauliExpr)))] ->
      [Summed (Scaled (Tensor PauliExpr))]
    applyScaling = map (\(k, x) -> fmap (scale k) x)

    combineSums ::
      [Summed (Scaled (Tensor PauliExpr))] ->
      Summed (Scaled (Tensor PauliExpr))
    combineSums = joinSummed . Summed

    compiled =
        optimize             $
        combineSums          $
        applyScaling         $
        coeffsToComplex      $
        commuteTensorScaling $
        distributeSummedTensor $
        buildTensor          $
        decodeAndDistribute  $
        constraintResults

-- | Convert a dense Program (constraint function) into the sparse list
-- representation used by Prog, by enumerating all view-tuples and their
-- assignments and pairing each with its cost.
expandConstraints :: (Ord (t (Var a)), Part (t (Var a)), Traversable t) =>
  Program t a b c ->
  [([(a, b)], c)]
expandConstraints prog =
  let
    Program { struct = s, choices = cs, view = v, constraints = f } = prog
    varStruct    = runFresh (genChoices s)
    pairs        = distinctNTuples v varStruct
    actualTuples = assignChoices cs pairs
  in
    map (\t ->
          let assignment = toList (fmap (first choice) t)
          in  (assignment, f (fmap (first choice) t)))
        actualTuples
{-
  expandConstraints works by running the same enumeration as solveQuantum (distinctNTuples 
  + assignChoices) and applying the constraint function to each assignment, producing the  
  [([(a,b)], c)] list that Prog expects.
-}

toProg :: (Ord (t (Var a)), Part (t (Var a)), Traversable t) =>                          
  Program t a b c -> Prog t a b c                                                        
toProg p = Prog
  { progChoices     = choices p
  , progStruct      = struct p
  , progConstraints = expandConstraints p
  }       
