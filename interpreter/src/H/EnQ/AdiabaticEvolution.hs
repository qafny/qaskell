module H.EnQ.AdiabaticEvolution (
    adiabaticEvolution,
    maxIndex,
    scaleComplex,
    uniformSuperposition
) where

import Data.Complex (Complex((:+)), magnitude)
import Numeric.LinearAlgebra (Vector, Matrix, expm, cmap, konst, (#>), scalar, toList)
import Numeric.LinearAlgebra.Data
import Data.List (maximumBy)
import Data.Ord (comparing)

-- General Adiabatic Evolution: Solves for a single time step
adiabaticStep :: Matrix (Complex Double)      -- Initial Hamiltonian (h0)
             -> Matrix (Complex Double)      -- Final Hamiltonian (hp)
             -> Double                       -- Total evolution time
             -> Int                          -- Total steps
             -> Int                          -- Current step
             -> Vector (Complex Double)      -- Current state
             -> Vector (Complex Double)      -- Next state
adiabaticStep h0 hp t steps step psi =
  let dt = t / fromIntegral steps
      s = fromIntegral step * dt / t
      hT = scaleComplex (1 - s) h0 + scaleComplex s hp  -- Interpolate Hamiltonian
      uT = expm (hT * scalar (0 :+ (-dt)))             -- Compute time evolution operator
  in uT #> psi                                         -- Apply the unitary operator

-- Minimizes using the evolution logic and initial state
minimize :: (Int -> Vector (Complex Double) -> Vector (Complex Double)) -- Evolution function
         -> Int                                                        -- Total steps
         -> Vector (Complex Double)                                    -- Initial state
         -> Vector (Complex Double)                                    -- Final state
minimize stepEvolution steps psi0 =
  foldl (flip stepEvolution) psi0 [0 .. steps - 1]

-- Composed Adiabatic Evolution
adiabaticEvolution :: Matrix (Complex Double)      -- Initial Hamiltonian (h0)
                   -> Matrix (Complex Double)      -- Final Hamiltonian (hp)
                   -> Double                       -- Total evolution time
                   -> Int                          -- Number of steps
                   -> Vector (Complex Double)      -- Initial state
                   -> Vector (Complex Double)      -- Final state
adiabaticEvolution h0 hp t steps = minimize (adiabaticStep h0 hp t steps) steps

-- Scale a complex matrix
scaleComplex :: Double -> Matrix (Complex Double) -> Matrix (Complex Double)
scaleComplex x = cmap (* (x :+ 0))

-- Initialize a uniform superposition state
uniformSuperposition :: Int -> Vector (Complex Double)
uniformSuperposition n = konst (1 / sqrt (fromIntegral n) :+ 0) n

-- Find the index of the maximum magnitude element in a vector
maxIndex :: Vector (Complex Double) -> Int
maxIndex vec = fst $ maximumBy (comparing snd) (zip [0 ..] magnitudes)
  where
    magnitudes = map magnitude (toList vec)
    
-- Define the translation from second quantization to matrix
data SndQ = Crea | Anni | Suma SndQ SndQ | Circ SndQ SndQ

--data Spin x = KeepPos x | KeepNeg x | Swap [x].
-- trans (KeepPos x) = Circ Crea Anni
-- trans (KeepNeg x) = Circ Anni Crea
-- trans (Swap 1) = Suma Crea Anni
-- trans (Swap 2) x y = Suma (Circ Crea(x) Anni(y)) (Circ Anni(y) Crea(x)) -- need to define matrix for two qubits.

-- instance Functor Matrix where
--     fmap f = Matrix . (fmap (fmap f)) . getMatrix 

-- instance Applicative Matrix where
--     pure x = Matrix [[x]]
--     (Matrix as) <*> (Matrix bs) = Matrix $ zipWith (zipWith id) as bs

-- instance (Show a) => Show (Matrix a) where 
--     show = (intercalate "\n") . fmap (unwords . fmap show) . getMatrix

--scalarProduct :: (Num a) => a -> Matrix a -> Matrix a
--scalarProduct scalar matrix = (* scalar) <$> matrix

--hadamardProduct :: (Num a) => Matrix a -> Matrix a -> Matrix a
--hadamardProduct matrix1 matrix2 = (*) <$> matrix1 <*> matrix2

-- The kronecker product M1 ⊗ M2, results in a block matrix.
-- It is a matrix that has elements that are also matrices, 
-- this makes it somewhat multidimensional tensor. 
-- The resulting block matrix has the same dimensions as M1. 
-- Every resulting element M1M2ij is equal to M1ij × M2. 
-- Assuming M1 contains scalars, this would mean every element 
-- in the resulting matrix requires a further scalar multiplication. 
-- But it could also be some other multiplication method.
-- This requires a RankNTypes
kroneckerProduct :: (a -> Matrix b -> Matrix b) -> Matrix a -> Matrix b -> Matrix (Matrix b)
kroneckerProduct f m1 m2 = (`f` m2) <$> m1


genOp Crea = fromList 2 2 [0 0 1 0] 
genOp Anni = fromList 2 2 [0 1 0 0]
genOp (Suma a b) = genOp a + genOp b
genOp (Circ a b) = multStd (genOp a) (genOp b)

genSingleAux f n m 0 = []
genSingleAux f n m m = kroneckerProduct (genOp f) (genSingleAux n m (m-1))
genSingleAux f n m j = kroneckerProduct (ident 2) (genSingleAux n m (j-1))

genMatrix f n 0 = zero n n
genMatrix f n m =genMatrix f n (m-1) + genSingle f n m

