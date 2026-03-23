module H.EnQ.AdiabaticEvolution (
    adiabaticEvolution,
    maxIndex,
    scaleComplex,
    uniformSuperposition,
    genMatrix,
    genSingle,
    SndQ(..)
) where

import Prelude hiding ((<>))

import Data.Complex (Complex((:+)), magnitude)
import Numeric.LinearAlgebra (Vector, Matrix, expm, cmap, konst, (#>), scalar, toList, ident, (><), fromList, kronecker, (<>))
import Numeric.LinearAlgebra.Data (flatten)
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

-- Generate the 2x2 Basic Operators (Pauli / Ladder)
genOp :: SndQ -> Matrix (Complex Double)
genOp Crea = (2><2) [0, 0, 1, 0] 
genOp Anni = (2><2) [0, 1, 0, 0]
genOp (Suma a b) = genOp a + genOp b
genOp (Circ a b) = genOp a <> genOp b -- Standard Matrix Multiplication

-- Generate Operator for a specific qubit 'k' in an 'n' qubit system
-- Formula: I_pre (x) Op (x) I_post
genSingle :: SndQ -> Int -> Int -> Matrix (Complex Double)
genSingle op n k = 
    let opMat = genOp op
        pre   = ident (2^k)             -- Identity for qubits before k
        post  = ident (2^(n - 1 - k))   -- Identity for qubits after k
    in (pre `kronecker` opMat) `kronecker` post

-- Generate Sum of Operators across all qubits (from 0 to m-1)
genMatrix :: SndQ -> Int -> Int -> Matrix (Complex Double)
genMatrix f n 0 = (sz >< sz) (repeat (0 :+ 0 :: Complex Double)) 
  where sz = 2^n :: Int

genMatrix f n m = genMatrix f n (m-1) + genSingle f n (m-1)