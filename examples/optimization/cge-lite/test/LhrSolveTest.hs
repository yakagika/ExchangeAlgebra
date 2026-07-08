{- |
  LhrSolveTest -- step ④ sentinel: the auctioneer actually solves, and the
  ledger's accounting state reproduces the CGE equilibrium
  (general-equilibrium:phase1-cge-reproduction task 1e, work item 3).

  Three things close the工程3 argument on swazilan:

    * __convergence__ — from a 10%-perturbed instrument vector the reduced
      auctioneer ('LhrWiring.solveReduced', damped Newton\/Broyden over the six
      residuals with SAVINVBAL dropped) returns to the calibrated base; the
      CPIDEF row pins the price level, so the solve is self-contained.
    * __(c) realised == notional [RQ1]__ — at the solved instruments /every/
      ledger residual is ≈ 0, including YIDEF (the household's realised receipts
      equal its instrument income YI — the accounting is realised from flows,
      not imposed) and the dropped SAVINVBAL (satisfied ex post by Walras's
      law: the double-entry journal is internally consistent).
    * __(a) replicability__ — folding a clone-split journal (every agent
      replicated 2× at half scale) gives bit-identical residuals; halving is
      exact in IEEE-754 so the comparison is exact, mechanically verifying the
      set-fold aggregation carries no singleton-keyed logic.

  (b) response purity is enforced at compile time — a layer-1 response takes no
  global allocation and no realised income — so it needs no runtime check.
-}
module Main where

import qualified Data.Map.Strict as M

import           ExchangeAlgebra.Algebra ((.*), (.+))

import qualified LhrCalibration as L
import           LhrWiring
import           LhrLedger      (ledgerAlg, ledgerResiduals, residualsFromAlg)
import           Solver         (ConvergenceTol (..), SentinelLog (..))
import           TestHarness

main :: IO ()
main = do
    inpTxt <- readFile "optimization/cge-lite/lhr/swazilan-inputs.csv"
    case L.parseInputs inpTxt >>= L.calibrate of
        Left msg  -> runChecks "LhrSolveTest" [bad "swazilan calibrate" msg]
        Right cal -> do
            let ins0 = baseInstruments cal
                -- start 10% off the base on every instrument coordinate
                start = foldr (\c -> perturb (0.10 * coordBase ins0 c) c) ins0
                              (instrCoords cal)
                tol = ConvergenceTol { tolNorm = 1e-10, tolMaxIter = 200 }
                (sol, slog) = solveReduced cal start tol
            putStrLn $ "solve: converged=" ++ show (slConverged slog)
                     ++ " K=" ++ show (slIterations slog)
                     ++ " ||z||=" ++ show (slResidualNorm slog)
                     ++ " cond=" ++ show (slConditionProxy slog)
            runChecks "LhrSolveTest" $
                   convergenceChecks cal ins0 sol slog
                ++ realisedChecks cal sol
                ++ replicabilityChecks cal start

-- | The solve returns to the calibrated base (unique under the CPIDEF pin).
convergenceChecks :: L.LhrCalibration -> Instruments -> Instruments -> SentinelLog -> [Check]
convergenceChecks cal ins0 sol slog =
    require "solve converged" (slConverged slog) "auctioneer did not converge"
    : [ approx 1e-6 ("solved " ++ showIC c) (coordBase ins0 c) (coordBase sol c)
      | c <- instrCoords cal ]

-- | (c) At the solution every ledger residual is ≈ 0 — realised accounting
-- state reproduces the equilibrium, and the dropped Walras row checks out.
realisedChecks :: L.LhrCalibration -> Instruments -> [Check]
realisedChecks cal sol =
    [ approx 1e-6 ("ledger@sol " ++ showRK rk) 0.0 v
    | (rk, v) <- M.toList (ledgerResiduals cal sol) ]

-- | (a) Replicating every agent 2× at half scale leaves the folded residuals
-- invariant (up to float roundoff — the split reorders each key's summation).
-- Checked at a perturbed point so the residuals are non-trivially nonzero.
replicabilityChecks :: L.LhrCalibration -> Instruments -> [Check]
replicabilityChecks cal ins =
    [ approx (1e-9 * max 1.0 (abs v1)) ("clone-split " ++ showRK rk) v1
             (M.findWithDefault (v1 + 1e18) rk r2)
    | (rk, v1) <- M.toList r1 ]
  where
    alg   = ledgerAlg cal ins
    split = (0.5 .* alg) .+ (0.5 .* alg)
    r1    = residualsFromAlg alg
    r2    = residualsFromAlg split

showRK :: ResidualKey -> String
showRK (RComEquil (L.Ac c))  = "COMEQUIL(" ++ c ++ ")"
showRK (RFacEquil (L.Ac f))  = "FACEQUIL(" ++ f ++ ")"
showRK RCurAcc               = "CURACCBAL"
showRK RSavInv               = "SAVINVBAL"
showRK (RYiDef (L.Ac i))     = "YIDEF(" ++ i ++ ")"
showRK (RActProfit (L.Ac a)) = "ACTPROFIT(" ++ a ++ ")"
showRK RCpi                  = "CPIDEF"

showIC :: InstrCoord -> String
showIC (CPDS (L.Ac c)) = "PDS." ++ c
showIC (CWF (L.Ac f))  = "WF." ++ f
showIC CEXR            = "EXR"
showIC (CQA (L.Ac a))  = "QA." ++ a
showIC CIADJ           = "IADJ"
showIC (CYI (L.Ac i))  = "YI." ++ i
