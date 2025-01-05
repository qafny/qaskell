module H.EnQ.AdiabaticEvolution (
    adiabaticEvolution,
    maxIndex,
    scaleComplex,
    uniformSuperposition
) where

import Data.Complex (Complex((:+)), magnitude)
import Numeric.LinearAlgebra (Vector, Matrix, expm, cmap, konst, (#>), scalar, toList)
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