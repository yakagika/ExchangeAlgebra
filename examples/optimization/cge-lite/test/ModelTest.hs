{- |
  ModelTest — the task-1b sentinel for "Model" (GE plan
  @phase1-cge-reproduction@): the four stage bodies must reproduce the Hosoe
  Ch.6 benchmark through the whole Option-A pipeline, or this suite fails
  the build.

  Five check groups, from the closed-form allocation to the full solve:

  1. __Allocation at the benchmark__ — every 'Allocation' field at the
     all-ones signal vector equals its 'C.Levels0' counterpart (prices 1,
     quantities the SAM levels, taxes\/savings the SAM cells). This checks
     each GAMS-equation transcription /individually/, so a slip in one FOC
     fails its own named check rather than a distant aggregate.
  2. __Benchmark-zero residuals__ — 'excessDemand' at 'benchmarkSignals' is
     ~0 in every retained row (the SAM /is/ the equilibrium), the dropped
     rows (imported varieties, LAB) net to ~0 in 'aggregateFlow', and the
     budget-exact entities (households\/Gov\/Inv) have ~0 net Yen cash in the
     'settle' ledger.
  3. __Household-count invariance (task 1d seam)__ — the residual vector is
     unchanged (up to float noise) when the representative household is
     split into N equal replicas.
  4. __Convergence smoke__ — 'solveEquilibrium' from a perturbed start
     converges back to the benchmark: all eight coordinates 1.0, solved
     utility = GAMS @UU@ (task 1c's headline, asserted here at smoke
     precision; 1c adds the comparative-statics shocks).
  5. __Trial determinism__ — two oracle evaluations at the same signals are
     bit-identical (the reduction order is fixed;
     state-change-and-scaling.md §3.9).
-}
module Main where

import qualified Data.Map.Strict as M

import           ExchangeAlgebra.Journal (BasePart, AccountTitles (..),
                                          CountUnit (..))
import qualified ExchangeAlgebra.Journal as EJ
import qualified ExchangeAlgebra.Algebra as EA

import qualified Calibration     as C
import           Model
import           Solver          (ConvergenceTol (..), SentinelLog (..))
import           TestHarness

-- | Absolute tolerance for "this Double chain reproduces an exact SAM
-- number": the quantities are O(100) and the chains are short, so 1e-9
-- leaves ~6 orders of headroom over accumulated ulps.
num :: String -> Double -> Double -> Check
num = approx 1e-9

params :: CGEParams
params = defaultCGEParams

cal :: C.Calibration
cal = cgeCalibration params

l :: C.Levels0
l = C.calLevels0 cal

benchAlloc :: Allocation
benchAlloc = allocation cal (benchmarkSignals params)

------------------------------------------------------------------
-- * Group 1: the closed-form allocation at the benchmark
------------------------------------------------------------------

allocationChecks :: [Check]
allocationChecks = concat
    [ perGood "pe"     aPe    (\i -> C.pWe l M.! i)
    , perGood "pm"     aPm    (\i -> C.pWm l M.! i)
    , perGood "py"     aPy    (const 1)
    , perGood "pz"     aPz    (const 1)
    , perGood "Z"      aZ     (\j -> C.z0 l M.! j)
    , perGood "Y"      aY     (\j -> C.y0 l M.! j)
    , perGood "E"      aE     (\i -> C.e0 l M.! i)
    , perGood "Ds"     aDs    (\i -> C.d0 l M.! i)
    , perGood "Dd"     aDd    (\i -> C.d0 l M.! i)
    , perGood "M"      aM     (\i -> C.m0 l M.! i)
    , perGood "Qdem"   aQdem  (\i -> C.q0 l M.! i)
    , perGood "Qphys"  aQphys (\i -> C.q0 l M.! i)
    , perGood "Xp"     aXp    (\i -> C.xp0 l M.! i)
    , perGood "Xg"     aXg    (\i -> C.xg0 l M.! i)
    , perGood "Xv"     aXv    (\i -> C.xv0 l M.! i)
    , perGood "Tz"     aTz    (\j -> C.tz0 l M.! j)
    , perGood "Tm"     aTm    (\i -> C.tm0 l M.! i)
    , [ num ("bench F" ++ show k) (C.f0 l M.! k) (aF benchAlloc M.! k)
      | k <- [(h, j) | h <- C.factors, j <- C.goods] ]
    , [ num ("bench X" ++ show k) (C.x0 l M.! k) (aX benchAlloc M.! k)
      | k <- [(i, j) | i <- C.goods, j <- C.goods] ]
    , [ num "bench Sp" (C.sp0 l) (aSp benchAlloc)
      , num "bench Sg" (C.sg0 l) (aSg benchAlloc)
      , num "bench Td" (C.td0 l) (aTd benchAlloc)
      , num "bench T (fiscal closure)"
            (C.td0 l + sum [C.tz0 l M.! j | j <- C.goods]
                     + sum [C.tm0 l M.! j | j <- C.goods])
            (aT benchAlloc)
      , num "bench UU" (C.benchmarkUtility cal) (aUU benchAlloc)
      ]
    ]
  where
    perGood name field expected =
        [ num ("bench " ++ name ++ "(" ++ show i ++ ")")
              (expected i)
              (field benchAlloc M.! i)
        | i <- C.goods ]

------------------------------------------------------------------
-- * Group 2: benchmark-zero residuals and ledger identities
------------------------------------------------------------------

residualChecks :: [Check]
residualChecks =
    -- Every retained row of the residual vector vanishes at the benchmark.
    [ num ("z " ++ show k ++ " at benchmark") 0 (M.findWithDefault 0 k zBench)
    | k <- M.keys (unknowns0 params) ]
    ++
    -- The dropped rows vanish too: imported varieties identically, LAB by
    -- Walras (asserted, not assumed — the module Haddock's derivation made
    -- a claim, this is its test).
    [ num ("dropped product row " ++ show p ++ " at benchmark")
          0 (M.findWithDefault 0 p flows)
    | p <- [BRDF, MLKF, LAB] ]
    ++
    -- Budget-exact entities: net Yen cash of every non-firm entity is 0 at
    -- the benchmark (households/Gov/Inv spend exactly their income; the
    -- firms' nets are the profit rows already checked above).
    [ num ("net Yen cash of " ++ show e ++ " at benchmark") 0
          (M.findWithDefault 0 e cashByEntity)
    | e <- Gov : Inv : Prelude.map Household (cgeHouseholds params) ]
  where
    zBench = excessDemand params (benchmarkSignals params)
    ledger = settle params (benchmarkSignals params)
    flows  = aggregateFlow ledger
    cashByEntity = EA.balanceMapBy cashKey (EJ.toAlg ledger)
      where
        cashKey :: BasePart CGEBase -> Maybe Entity
        cashKey (Cash, _, e, Yen) = Just e
        cashKey _                 = Nothing

------------------------------------------------------------------
-- * Group 3: household-count invariance (task 1d seam)
------------------------------------------------------------------

householdChecks :: [Check]
householdChecks =
    [ approx 1e-9 ("z " ++ show k ++ " invariant under household split (N=3)")
          (M.findWithDefault 0 k z1)
          (M.findWithDefault 0 k z3)
    | k <- M.keys (unknowns0 params) ]
  where
    params3 = params { cgeHouseholds = Prelude.map HouseholdId [0 .. 2] }
    z1 = excessDemand params  (benchmarkSignals params)
    z3 = excessDemand params3 (benchmarkSignals params3)

------------------------------------------------------------------
-- * Group 4: convergence smoke (the 1c headline, smoke precision)
------------------------------------------------------------------

-- | The same perturbation shape the executable demos: prices skewed both
-- ways, the exchange rate up, the two scales off in opposite directions.
perturbed :: M.Map CGEVar Double
perturbed = M.mapWithKey bump (unknowns0 params)
  where
    bump (MarketOf p) _ | p `Prelude.elem` [BRDD, BRDC] = 1.25
                        | otherwise                     = 0.85
    bump Forex        _ = 1.10
    bump (ScaleOf j)  _ = if j == C.BRD then 0.90 else 1.15

convergenceChecks :: [Check]
convergenceChecks =
    [ require "solve converged" (slConverged slog)
          ("did not converge: " ++ show slog)
    , require "solve is Newton-cheap (K = oracle calls <= 200)"
          (slIterations slog <= 200)
          ("K = " ++ show (slIterations slog))
    ]
    ++
    [ approx 1e-6 ("solved " ++ show k ++ " = 1 (benchmark)")
          1 (M.findWithDefault 0 k uEq)
    | k <- M.keys (unknowns0 params) ]
    ++
    [ approx 1e-6 "solved UU = GAMS UU"
          (C.benchmarkUtility cal)
          (aUU (allocation cal (signalsOf params uEq)))
    ]
  where
    tol = ConvergenceTol { tolNorm = 1e-9, tolMaxIter = 50 }
    (uEq, slog) = solveEquilibrium params perturbed tol

------------------------------------------------------------------
-- * Group 5: trial determinism (§3.9 — fixed reduction order)
------------------------------------------------------------------

determinismChecks :: [Check]
determinismChecks =
    [ require "excessDemand is bit-deterministic across evaluations"
          (z1 == z2)
          (show z1 ++ " /= " ++ show z2)
    ]
  where
    sig = signalsOf params perturbed
    z1 = excessDemand params sig
    z2 = excessDemand params sig

------------------------------------------------------------------

main :: IO ()
main = runChecks "cge-lite-model-test" $
       allocationChecks
    ++ residualChecks
    ++ householdChecks
    ++ convergenceChecks
    ++ determinismChecks
