{- |
  LhrSolveTest -- step ④ sentinel: the auctioneer actually solves, and the
  ledger's accounting state reproduces the CGE equilibrium
  (general-equilibrium:phase1-cge-reproduction task 1e, 工程3 step ④ +工程4
  completion criteria iii/v).

  For each dataset the reduced auctioneer ('LhrWiring.solveReduced', damped
  Newton\/Broyden over the residuals with SAVINVBAL dropped) is started 10% off
  the calibrated base and must return to it (the CPIDEF row pins the price
  level, so the solve is self-contained):

    * __convergence__ (criterion v) — swazilan (8×8) and test.dat (32×32, with
      the promoted PQ\/PXAC closing the margin\/home fixed points) both converge.
    * __unique equilibrium__ (criterion iii) — the new sparse-uniform vector
      returns to the same calibrated base as the original 6-residual swazilan
      system; convergence to @baseInstruments@ is that check.

  For swazilan the ledger is complete, so also:

    * __(c) realised == notional [RQ1]__ — at the solution every ledger residual
      is ≈ 0 (the household's realised receipts equal its instrument income; the
      dropped SAVINVBAL checks out ex post by Walras's law).
    * __(a) replicability__ — a clone-split journal folds to bit-identical
      residuals.

  test.dat's ledger (transport-margin and home-consumption journals) lands with
  工程4 increment 2, at which point its realised\/replicability checks join here.
  (b) response purity is a compile-time property, so it needs no runtime check.
-}
module Main where

import           Data.Maybe      (catMaybes)
import qualified Data.Map.Strict as M

import           ExchangeAlgebra.Algebra ((.*), (.+))

import qualified LhrCalibration as L
import           LhrWiring
import           LhrLedger      (ledgerAlg, ledgerResiduals, residualsFromAlg)
import           Solver         (ConvergenceTol (..), SentinelLog (..))
import           TestHarness

-- | (name, inputs path, ledger complete?).
datasets :: [(String, FilePath, Bool)]
datasets =
    [ ("swazilan", "optimization/cge-lite/lhr/swazilan-inputs.csv", True)
    , ("test",     "optimization/cge-lite/lhr/test-inputs.csv",     False) ]

data Solved = Solved
    { svName        :: String
    , svCal         :: L.LhrCalibration
    , svIns0        :: Instruments
    , svSol         :: Instruments
    , svSlog        :: SentinelLog
    , svStart       :: Instruments
    , svLedgerReady :: Bool
    }

main :: IO ()
main = do
    solved <- catMaybes <$> mapM loadAndSolve datasets
    runChecks "LhrSolveTest" $
           concatMap convergenceChecks solved
        ++ concatMap ledgerChecks (filter svLedgerReady solved)

loadAndSolve :: (String, FilePath, Bool) -> IO (Maybe Solved)
loadAndSolve (name, path, ledgerReady) = do
    inpTxt <- readFile path
    case L.parseInputs inpTxt >>= L.calibrate of
        Left msg  -> do putStrLn (name ++ " calibrate FAIL: " ++ msg); pure Nothing
        Right cal -> do
            let ins0  = baseInstruments cal
                start = foldr (\c -> perturb (0.10 * coordBase ins0 c) c) ins0
                              (instrCoords cal)
                tol   = ConvergenceTol { tolNorm = 1e-10, tolMaxIter = 200 }
                (sol, slog) = solveReduced cal start tol
            putStrLn $ name ++ " solve: converged=" ++ show (slConverged slog)
                     ++ " K=" ++ show (slIterations slog)
                     ++ " ||z||=" ++ show (slResidualNorm slog)
                     ++ " cond=" ++ show (slConditionProxy slog)
            pure (Just (Solved name cal ins0 sol slog start ledgerReady))

-- | The solve returns to the calibrated base (unique under the CPIDEF pin).
convergenceChecks :: Solved -> [Check]
convergenceChecks s =
    require (svName s ++ " solve converged") (slConverged (svSlog s))
            "auctioneer did not converge"
    : [ approx 1e-6 (svName s ++ " solved " ++ showIC c)
                    (coordBase (svIns0 s) c) (coordBase (svSol s) c)
      | c <- instrCoords (svCal s) ]

-- | (c) realised == notional + (a) replicability, off the ledger.
ledgerChecks :: Solved -> [Check]
ledgerChecks s = realised ++ replic
  where
    cal = svCal s
    realised =
        [ approx 1e-6 (svName s ++ " ledger@sol " ++ showRK rk) 0.0 v
        | (rk, v) <- M.toList (ledgerResiduals cal (svSol s)) ]
    alg    = ledgerAlg cal (svStart s)
    split  = (0.5 .* alg) .+ (0.5 .* alg)
    r1     = residualsFromAlg alg
    r2     = residualsFromAlg split
    replic =
        [ approx (1e-9 * max 1.0 (abs v1)) (svName s ++ " clone-split " ++ showRK rk) v1
                 (M.findWithDefault (v1 + 1e18) rk r2)
        | (rk, v1) <- M.toList r1 ]

showRK :: ResidualKey -> String
showRK (RComEquil (L.Ac c))  = "COMEQUIL(" ++ c ++ ")"
showRK (RFacEquil (L.Ac f))  = "FACEQUIL(" ++ f ++ ")"
showRK RCurAcc               = "CURACCBAL"
showRK RSavInv               = "SAVINVBAL"
showRK (RYiDef (L.Ac i))     = "YIDEF(" ++ i ++ ")"
showRK (RActProfit (L.Ac a)) = "ACTPROFIT(" ++ a ++ ")"
showRK (RPqDef (L.Ac c))     = "PQDEF(" ++ c ++ ")"
showRK (ROutAggFoc (L.Ac a) (L.Ac c)) = "OUTAGGFOC(" ++ a ++ "," ++ c ++ ")"
showRK RCpi                  = "CPIDEF"

showIC :: InstrCoord -> String
showIC (CPDS (L.Ac c)) = "PDS." ++ c
showIC (CWF (L.Ac f))  = "WF." ++ f
showIC CEXR            = "EXR"
showIC (CQA (L.Ac a))  = "QA." ++ a
showIC CIADJ           = "IADJ"
showIC (CYI (L.Ac i))  = "YI." ++ i
showIC (CPQ (L.Ac c))  = "PQ." ++ c
showIC (CPXAC (L.Ac a) (L.Ac c)) = "PXAC." ++ a ++ "." ++ c
