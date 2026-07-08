{- |
  LhrLedgerTest -- step ③b-2 sentinel: the EA double-entry journal reproduces
  the reduced residuals (general-equilibrium:phase1-cge-reproduction task 1e,
  work item 3).

  The differential test the codex\/Fable cross-check asked for: the residuals
  read off the ledger ('LhrLedger.ledgerResiduals', a @balanceMapBy@ fold of
  the posted flows) must equal the direct algebraic residuals
  ('LhrWiring.residuals') — at the base and at each instrument perturbed by
  5%, so a dropped leg, a wrong sign, or a double-count surfaces.  Perturbing
  EXR specifically exercises the single EXR-conversion convention (the ROW
  Dollar leg); a漏れ there would show only off the base.
-}
module Main where

import qualified Data.Map.Strict as M

import qualified LhrCalibration as L
import           LhrWiring
import qualified LhrLedger      as LG
import           TestHarness

main :: IO ()
main = do
    inpTxt <- readFile "optimization/cge-lite/lhr/swazilan-inputs.csv"
    case L.parseInputs inpTxt >>= L.calibrate of
        Left msg  -> runChecks "LhrLedgerTest" [bad "swazilan calibrate" msg]
        Right cal -> do
            let ins0   = baseInstruments cal
                points = ("base", ins0)
                       : [ (showIC c, perturb (0.05 * max 1.0 (abs (coordBase ins0 c))) c ins0)
                         | c <- instrCoords cal ]
            runChecks "LhrLedgerTest"
                (concat [ diffChecks cal nm ins | (nm, ins) <- points ])

-- | Ledger residuals must equal direct residuals key-for-key at this point.
diffChecks :: L.LhrCalibration -> String -> Instruments -> [Check]
diffChecks cal nm ins =
    require (nm ++ " residual key coverage")
            (M.keys direct == M.keys ledger)
            ("direct keys " ++ show (map showRK (M.keys direct))
             ++ " vs ledger " ++ show (map showRK (M.keys ledger)))
    : [ approx (1e-9 * max 1.0 (abs d)) (nm ++ " " ++ showRK rk) d
               (M.findWithDefault (d + 1e18) rk ledger)
      | (rk, d) <- M.toList direct ]
  where
    direct = residuals cal ins
    ledger = LG.ledgerResiduals cal ins

showRK :: ResidualKey -> String
showRK (RComEquil (L.Ac c))  = "COMEQUIL(" ++ c ++ ")"
showRK (RFacEquil (L.Ac f))  = "FACEQUIL(" ++ f ++ ")"
showRK RCurAcc               = "CURACCBAL"
showRK RSavInv               = "SAVINVBAL"
showRK (RYiDef (L.Ac i))     = "YIDEF(" ++ i ++ ")"
showRK (RActProfit (L.Ac a)) = "ACTPROFIT(" ++ a ++ ")"
showRK RCpi                  = "CPIDEF"

showIC :: InstrCoord -> String
showIC (CPDS (L.Ac c)) = "dPDS." ++ c
showIC (CWF (L.Ac f))  = "dWF." ++ f
showIC CEXR            = "dEXR"
showIC (CQA (L.Ac a))  = "dQA." ++ a
showIC CIADJ           = "dIADJ"
showIC (CYI (L.Ac i))  = "dYI." ++ i
