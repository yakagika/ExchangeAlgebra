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
  6. __Comparative statics (task 1c)__ — under counterfactual tax policies
     (S1 tariff abolition, S2 production-tax x1.5) the solved CGE-Lite
     equilibrium matches an /independent re-solve of the full GAMS square
     system/ (@GAMS\/shock_resolve.py@, all 48 equations, stdlib Newton;
     fixtures at @GAMS\/shocks\/@) in every variable — the reproduction
     holds off the calibration point, not just at the SAM (the Model1 =
     Model2 argument of the research plan, CGE edition).
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
benchAlloc = allocation params (benchmarkSignals params)

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
          (aUU (allocation params (signalsOf params uEq)))
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
-- * Group 6: comparative statics vs independent re-solve (task 1c)
------------------------------------------------------------------

-- | One parsed fixture row: (variable, index, value) — same format (and
-- parser) as @results.csv@ in CalibrationTest.
type ResultRow = (String, [C.Account], Double)

parseResults :: String -> [ResultRow]
parseResults = Prelude.map row . drop 1 . filter (not . null) . lines
  where
    row ln =
        let (var, rest)  = break (== ',') ln
            (ix, rest')  = break (== ',') (drop 1 rest)
            val          = read (drop 1 rest')
        in  (var, parseIx ix, val)
    parseIx "" = []
    parseIx s  = case break (== '.') s of
        (a, "")       -> [read a]
        (a, _ : rest) -> read a : parseIx rest

-- | Cross-solver agreement tolerance: both this suite's solve and the
-- fixture's Newton run to residual ~1e-10 on O(1)-O(100) variables with a
-- condition proxy around 10, so 1e-6 absolute leaves ample headroom while
-- still catching any transcription drift between the two formulations.
xsv :: String -> Double -> Double -> Check
xsv = approx 1e-6

-- | Solve under a counterfactual policy and compare every fixture row.
shockChecks :: String -> Policy -> String -> [Check]
shockChecks name pol csv =
    [ require (tag "converged") (slConverged slog) (show slog)
    , require (tag "Newton-cheap (K <= 200)") (slIterations slog <= 200)
          ("K = " ++ show (slIterations slog))
    ]
    ++ Prelude.map check (parseResults csv)
  where
    tag s = name ++ ": " ++ s
    params' = params { cgePolicy = pol }
    tol' = ConvergenceTol { tolNorm = 1e-9, tolMaxIter = 50 }
    (uEq, slog) = solveEquilibrium params' (unknowns0 params') tol'
    sig = signalsOf params' uEq
    a   = allocation params' sig

    check :: ResultRow -> Check
    check (var, ix, v) = case (var, ix) of
        ("Y",  [j])    -> xsv lbl v (aY  a M.! j)
        ("F",  [h, j]) -> xsv lbl v (aF  a M.! (h, j))
        ("X",  [i, j]) -> xsv lbl v (aX  a M.! (i, j))
        ("Z",  [j])    -> xsv lbl v (aZ  a M.! j)
        ("Xp", [i])    -> xsv lbl v (aXp a M.! i)
        ("Xg", [i])    -> xsv lbl v (aXg a M.! i)
        ("Xv", [i])    -> xsv lbl v (aXv a M.! i)
        ("E",  [i])    -> xsv lbl v (aE  a M.! i)
        ("M",  [i])    -> xsv lbl v (aM  a M.! i)
        ("Q",  [i])    -> xsv lbl v (aQdem a M.! i)
        ("D",  [i])    -> xsv lbl v (aDd a M.! i)
        ("pf", [h])    -> xsv lbl v (sigPrices sig M.! factorProduct h)
        ("py", [j])    -> xsv lbl v (aPy a M.! j)
        ("pz", [j])    -> xsv lbl v (aPz a M.! j)
        ("pq", [i])    -> xsv lbl v (sigPrices sig M.! compositeOf i)
        ("pe", [i])    -> xsv lbl v (aPe a M.! i)
        ("pm", [i])    -> xsv lbl v (aPm a M.! i)
        ("pd", [i])    -> xsv lbl v (sigPrices sig M.! domesticOf i)
        ("epsilon", []) -> xsv lbl v (sigForex sig)
        ("Sp", [])     -> xsv lbl v (aSp a)
        ("Sg", [])     -> xsv lbl v (aSg a)
        ("Td", [])     -> xsv lbl v (aTd a)
        ("Tz", [j])    -> xsv lbl v (aTz a M.! j)
        ("Tm", [i])    -> xsv lbl v (aTm a M.! i)
        ("UU", [])     -> xsv lbl v (aUU a)
        _              -> bad lbl ("unrecognized fixture row: " ++ var)
      where
        lbl = tag (var ++ show ix)

-- | S1 — import-tariff abolition (the textbook's canonical experiment).
s1Policy :: Policy
s1Policy = (calibratedPolicy cal) { polTaum = M.map (const 0) (polTaum (calibratedPolicy cal)) }

-- | S2 — production-tax hike, tauz x 1.5 on both goods.
s2Policy :: Policy
s2Policy = (calibratedPolicy cal) { polTauz = M.map (* 1.5) (polTauz (calibratedPolicy cal)) }

------------------------------------------------------------------

main :: IO ()
main = do
    s1 <- readFile "optimization/CGE/GAMS/shocks/s1_tariff_abolition.csv"
    s2 <- readFile "optimization/CGE/GAMS/shocks/s2_tauz_x1.5.csv"
    runChecks "cge-lite-model-test" $
           allocationChecks
        ++ residualChecks
        ++ householdChecks
        ++ convergenceChecks
        ++ determinismChecks
        ++ shockChecks "s1 tariff abolition" s1Policy s1
        ++ shockChecks "s2 tauz x1.5"        s2Policy s2
