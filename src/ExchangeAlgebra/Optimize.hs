{-# LANGUAGE TypeFamilies #-}

{- |
    Module     : ExchangeAlgebra.Optimize
    Copyright  : (c) Kaya Akagi. 2026
    License    : OtherLicense

    A pluggable optimization solver interface.

    This module defines the strategy-agnostic 'Solver' class. Concrete
    strategies live in sibling modules:

    * "ExchangeAlgebra.Optimize.Annealing" — simulated annealing over an
      arbitrary candidate type.
    * "ExchangeAlgebra.Optimize.GA" — a real-coded genetic algorithm over
      numeric vector chromosomes.

    New strategies (differential evolution, particle swarm, CMA-ES, ...)
    are added by defining a new strategy type with a 'Solver' instance;
    the interface itself does not change.

    == Design notes

    * The objective is monadic (@'Candidate' strategy -> m Double@) so that
      it can run stateful simulations, e.g. the @ST s@ state spaces of
      "ExchangeAlgebra.Simulate". Solvers only require 'Monad', nothing
      stronger.
    * Randomness is threaded internally by each solver from a seed in its
      'Config' (a pure generator, sequentially updated), so no random-monad
      constraint leaks into the objective. Runs are reproducible given the
      same seed /provided the objective itself is deterministic/ (same
      state transitions, no external randomness, clocks or concurrency).
    * The returned 'Double' is the objective value observed when the
      returned candidate was evaluated, in the user's orientation.
      Solvers never re-evaluate a candidate they have already scored
      (re-evaluation would be a visible side effect for stateful
      objectives such as @ST s@ simulations).
    * Objective values must be finite: solvers reject @NaN@ and
      infinities with a call to 'error' as soon as they are observed,
      because non-finite energies silently break comparisons, sorting
      and best-candidate tracking. Configurations are also validated
      up front (fail-fast; invalid settings are never clamped).
    * This subsystem is a generic numeric layer: it does not touch the
      redundant-algebra core ("ExchangeAlgebra.Algebra",
      "ExchangeAlgebra.Journal") and imposes no Hat\/Not or non-negativity
      semantics on candidates. Objective values are plain 'Double's in the
      user's orientation (see 'Direction').
    * Parallel evaluation of candidates is deliberately left to the caller
      (e.g. run several 'optimize' calls with different seeds and pick the
      best); solvers themselves evaluate sequentially.
-}
module ExchangeAlgebra.Optimize
    ( -- * Solver interface
      Solver (..)
      -- * Optimization direction
    , Direction (..)
    , orient
    ) where

-- | Whether the objective is to be maximized or minimized.
--
--   Solvers convert objective values to an internal /minimization energy/
--   ('orient') and report the final score back in the user's orientation.
data Direction = Maximize | Minimize
    deriving (Show, Eq)

-- | Convert a score in the user's orientation to a minimization energy.
--
--   >>> orient Minimize 3.0
--   3.0
--
--   >>> orient Maximize 3.0
--   -3.0
orient :: Direction -> Double -> Double
orient Minimize = id
orient Maximize = negate
{-# INLINE orient #-}

-- | A pluggable optimization strategy.
--
--   @strategy@ is a dispatch value carrying no data of its own — a
--   /proxy/ such as @GA@ ("ExchangeAlgebra.Optimize.GA") or @Annealing@
--   ("ExchangeAlgebra.Optimize.Annealing"). The associated types fix what
--   a candidate solution looks like and what configuration the strategy
--   needs.
class Solver strategy where
    -- | Candidate solution type searched by this strategy.
    type Candidate strategy
    -- | Strategy-specific configuration (schedules, rates, seed,
    --   'Direction', ...).
    type Config strategy

    -- | Run the optimizer: given a configuration, a monadic objective and
    --   an initial candidate, return the best candidate found together
    --   with its objective value (in the user's orientation).
    optimize :: Monad m
             => strategy
             -> Config strategy
             -> (Candidate strategy -> m Double) -- ^ objective
             -> Candidate strategy               -- ^ initial candidate
             -> m (Candidate strategy, Double)
