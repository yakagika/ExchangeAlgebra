{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

{- |
    Module     : ExchangeAlgebra.Optimize.Annealing
    Copyright  : (c) Kaya Akagi. 2026
    License    : OtherLicense

    Simulated annealing over an arbitrary candidate type.

    The candidate type is free (@a@ in @'Annealing' a@): investment
    schedules, parameter records, vectors, ... — anything the
    'acNeighbor' move can perturb. The objective may be stateful
    (e.g. run an @ST s@ simulation); see "ExchangeAlgebra.Optimize"
    for the interface contract.

    == Example

    Minimize @(x - 3)^2@ over 'Double':

    > import ExchangeAlgebra.Optimize
    > import ExchangeAlgebra.Optimize.Annealing
    > import System.Random (uniformR)
    > import Data.Functor.Identity (runIdentity)
    >
    > cfg :: AnnealingConfig Double
    > cfg = AnnealingConfig
    >     { acDirection = Minimize
    >     , acSteps     = 2000
    >     , acSchedule  = geometricCooling 1.0 0.995
    >     , acNeighbor  = \g x -> let (d, g') = uniformR (-0.5, 0.5) g
    >                             in (x + d, g')
    >     , acAccept    = metropolis
    >     , acSeed      = 42
    >     }
    >
    > best :: (Double, Double)
    > best = runIdentity (optimize Annealing cfg (\x -> pure ((x - 3)^2)) 0)
-}
module ExchangeAlgebra.Optimize.Annealing
    ( -- * Strategy
      Annealing (..)
    , AnnealingConfig (..)
      -- * Standard building blocks
    , geometricCooling
    , metropolis
    ) where

import           ExchangeAlgebra.Optimize
import           System.Random            (StdGen, mkStdGen, uniformR)

-- | Simulated-annealing strategy proxy. The phantom @a@ is the candidate
--   type; it is inferred from the t'AnnealingConfig' at the call site, so
--   plain @'optimize' Annealing cfg obj x0@ needs no annotation.
data Annealing a = Annealing

-- | Configuration of one annealing run.
--
--   Energies passed to 'acAccept' are /minimization-oriented/ ('orient'
--   applied), regardless of 'acDirection'.
data AnnealingConfig a = AnnealingConfig
    { acDirection :: Direction
      -- ^ Whether the objective is maximized or minimized.
    , acSteps     :: Int
      -- ^ Total number of proposal steps (@>= 0@; @0@ evaluates the
      --   initial candidate once and returns it).
    , acSchedule  :: Int -> Double
      -- ^ Cooling schedule: 1-based step number to temperature.
      --   Each temperature must be finite and @>= 0@ (@0@ acts as greedy
      --   acceptance); anything else is rejected with 'error'.
    , acNeighbor  :: StdGen -> a -> (a, StdGen)
      -- ^ Proposal move: perturb the current candidate, threading the
      --   solver's random generator.
    , acAccept    :: Double -> Double -> Double -> Double
      -- ^ Acceptance rule: @temperature -> current energy -> proposed
      --   energy -> probability@. The probability must be finite and in
      --   @[0, 1]@ (validated at every step). See 'metropolis'.
    , acSeed      :: Int
      -- ^ Seed of the solver's internal random generator.
    }

-- | Geometric cooling: @geometricCooling t0 alpha step = t0 * alpha^(step-1)@.
--
--   Typical use: @'geometricCooling' 1.0 0.995@.
geometricCooling :: Double  -- ^ initial temperature @t0@
                 -> Double  -- ^ decay factor @alpha@ (usually just below 1)
                 -> Int     -- ^ 1-based step
                 -> Double
geometricCooling t0 alpha step = t0 * alpha ^^ (step - 1)

-- | The Metropolis acceptance rule: always accept an improvement,
--   otherwise accept with probability @exp ((e - e') / t)@.
--   At @t = 0@ this degenerates to greedy acceptance.
metropolis :: Double  -- ^ temperature
           -> Double  -- ^ current energy
           -> Double  -- ^ proposed energy
           -> Double
metropolis t e e'
    | e' <= e   = 1
    | t <= 0    = 0
    | otherwise = exp ((e - e') / t)

instance Solver (Annealing a) where
    type Candidate (Annealing a) = a
    type Config    (Annealing a) = AnnealingConfig a

    optimize _ cfg obj x0
        | acSteps cfg < 0
        = error ("ExchangeAlgebra.Optimize.Annealing: acSteps must be >= 0, got "
                 ++ show (acSteps cfg))
        | otherwise
        = do e0 <- energyOf dir "initial candidate" (obj x0)
             go 1 (mkStdGen (acSeed cfg)) x0 e0 x0 e0
      where
        dir = acDirection cfg
        go !step !g !cur !eCur !best !eBest
            | step > acSteps cfg = return (best, orient dir eBest)
              -- orient is self-inverse: energy back to the user's score.
            | otherwise = do
                let t          = checkTemperature step (acSchedule cfg step)
                    (cand, g1) = acNeighbor cfg g cur
                eCand <- energyOf dir ("step " ++ show step) (obj cand)
                let p        = checkProbability step (acAccept cfg t eCur eCand)
                    (u, g2)  = uniformR (0 :: Double, 1) g1
                    accepted = p >= 1 || u < p
                    (cur', eCur')   | accepted        = (cand, eCand)
                                    | otherwise       = (cur, eCur)
                    (best', eBest') | eCand < eBest   = (cand, eCand)
                                    | otherwise       = (best, eBest)
                go (step + 1) g2 cur' eCur' best' eBest'

-- | Evaluate the objective and convert to minimization energy, rejecting
--   non-finite scores (see the contract in "ExchangeAlgebra.Optimize").
energyOf :: Monad m => Direction -> String -> m Double -> m Double
energyOf dir what mScore = do
    s <- mScore
    if isNaN s || isInfinite s
        then error ("ExchangeAlgebra.Optimize.Annealing: objective returned "
                    ++ "a non-finite score (" ++ show s ++ ") at " ++ what)
        else return (orient dir s)

checkTemperature :: Int -> Double -> Double
checkTemperature step t
    | isNaN t || isInfinite t || t < 0
    = error ("ExchangeAlgebra.Optimize.Annealing: acSchedule returned an "
             ++ "invalid temperature (" ++ show t ++ ") at step " ++ show step)
    | otherwise = t

checkProbability :: Int -> Double -> Double
checkProbability step p
    | isNaN p || isInfinite p || p < 0 || p > 1
    = error ("ExchangeAlgebra.Optimize.Annealing: acAccept returned an "
             ++ "invalid probability (" ++ show p ++ ") at step " ++ show step)
    | otherwise = p
