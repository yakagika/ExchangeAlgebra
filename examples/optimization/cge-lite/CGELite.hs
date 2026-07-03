{- |
  CGELite — the thin @cge-lite@ executable over "Model" (the Hosoe Ch.6
  CGE-Lite model; Phase 1, general-equilibrium research repo, plan
  @phase1-cge-reproduction@, task 1b).

  Runs the whole Option-A pipeline end to end and prints what task 1c's
  sentinel asserts mechanically (@cge-lite-model-test@):

    1. the residual vector at the benchmark signals (must be ~0 — the SAM
       /is/ the benchmark equilibrium);
    2. 'solveEquilibrium' from a deliberately perturbed start (must converge
       back to all-ones: prices 1.0, epsilon 1.0, Z\/Z0 1.0);
    3. the solved allocation's utility against the GAMS @UU@ and the one
       'settle' commit at the converged signals.
-}
module Main where

import qualified Data.Map.Strict as M

import           ExchangeAlgebra.Journal (norm)

import qualified Calibration     as C
import           Model
import           Solver          (ConvergenceTol (..), SentinelLog (..))

-- | A deliberately off-benchmark start for the demo solve: prices skewed
-- both ways, the exchange rate up, both firms' scales off in opposite
-- directions. (The sentinel suite runs the same shape of perturbation.)
perturbed :: CGEParams -> M.Map CGEVar Double
perturbed params = M.mapWithKey bump (unknowns0 params)
  where
    bump (MarketOf p) _ | p `elem` [BRDD, BRDC] = 1.25
                        | otherwise             = 0.85
    bump Forex        _ = 1.10
    bump (ScaleOf j)  _ = if j == C.BRD then 0.90 else 1.15

main :: IO ()
main = do
    let params = defaultCGEParams
        cal    = cgeCalibration params
    putStrLn "=== CGE-Lite (task 1b: stages + solve) ==="
    putStrLn ("households (N)     : " ++ show (length (cgeHouseholds params)))
    putStrLn ("numeraire          : " ++ show (cgeNumeraire params))
    putStrLn ("benchmark UU       : " ++ show (C.benchmarkUtility cal))

    let zBench = excessDemand params (benchmarkSignals params)
    putStrLn "--- residuals at the benchmark signals (all ~0: the SAM is the equilibrium) ---"
    mapM_ (\(k, v) -> putStrLn ("  z " ++ show k ++ " = " ++ show v))
          (M.toList zBench)

    let tol        = ConvergenceTol { tolNorm = 1e-9, tolMaxIter = 50 }
        u0         = perturbed params
        (uEq, slog) = solveEquilibrium params u0 tol
        sigEq      = signalsOf params uEq
        aEq        = allocation params sigEq
        ledger     = settle params sigEq
    putStrLn "--- solveEquilibrium from a perturbed start ---"
    putStrLn ("start              : " ++ show (M.toList u0))
    putStrLn ("solved             : " ++ show (M.toList uEq))
    putStrLn ("sentinel           : " ++ show slog)
    putStrLn ("solved UU          : " ++ show (aUU aEq)
              ++ " (GAMS: " ++ show (C.benchmarkUtility cal) ++ ")")
    putStrLn ("settled ledger norm: " ++ show (norm ledger))
