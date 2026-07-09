{- |
  LhrLedgerTest -- step ③b-2 / 工程4-1 criterion (iv) sentinel: the EA
  double-entry journal reproduces the reduced residuals
  (general-equilibrium:phase1-cge-reproduction task 1e, work item 3).

  The differential test the codex\/Fable cross-check asked for: the residuals
  read off the ledger ('LhrLedger.ledgerResiduals', a @balanceMapBy@ fold of
  the posted flows) must equal the direct algebraic residuals
  ('LhrWiring.residuals') — at the base and at each instrument perturbed by
  5%, so a dropped leg, a wrong sign, or a double-count surfaces.  Perturbing
  EXR specifically exercises the single EXR-conversion convention (the ROW
  Dollar leg); perturbing PXAC exercises the home-consumption imputation and
  perturbing PQ the transport-margin flow — the two journals test.dat adds over
  swazilan.  A漏れ in any of those shows only off the base.

  == Shared keys only

  The ledger closes the six market\/income families it actually books
  (COMEQUIL, FACEQUIL, CURACCBAL, YIDEF, ACTPROFIT, SAVINVBAL) plus CPIDEF.  The
  promoted-instrument /price identities/ PQDEF and OUTAGGFOC are not ledger
  flows (they pin PQ\/PXAC, they are not a Not\/Hat pair), so the differential
  test compares on the shared keys and asserts the ledger produces exactly
  those.

  Generalised from the swazilan-only step ③b-2 sentinel to both datasets with
  the工程4 sparse-uniform vector (PQ\/PXAC promoted).
-}
module Main where

import           Data.List       (sort)
import qualified Data.Map.Strict as M

import qualified LhrCalibration as L
import           LhrWiring
import qualified LhrLedger      as LG
import           TestHarness

datasets :: [(String, FilePath)]
datasets =
    [ ("swazilan", "optimization/cge-lite/lhr/swazilan-inputs.csv")
    , ("test",     "optimization/cge-lite/lhr/test-inputs.csv") ]

main :: IO ()
main = do
    checkss <- mapM runDataset datasets
    runChecks "LhrLedgerTest" (concat checkss)

runDataset :: (String, FilePath) -> IO [Check]
runDataset (name, path) = do
    inpTxt <- readFile path
    case L.parseInputs inpTxt >>= L.calibrate of
        Left msg  -> pure [bad (name ++ " calibrate") msg]
        Right cal -> do
            let ins0   = baseInstruments cal
                points = ("base", ins0)
                       : [ (showIC c, perturb (0.05 * max 1.0 (abs (coordBase ins0 c))) c ins0)
                         | c <- instrCoords cal ]
            pure (concat [ diffChecks cal (name ++ " " ++ nm) ins | (nm, ins) <- points ])

-- | A price-identity residual (PQDEF\/OUTAGGFOC) is not a ledger flow; it pins
-- a promoted instrument and is excluded from the differential comparison.
isPriceIdentity :: ResidualKey -> Bool
isPriceIdentity (RPqDef _)      = True
isPriceIdentity (ROutAggFoc _ _) = True
isPriceIdentity _               = False

-- | Ledger residuals must equal direct residuals key-for-key on the shared
-- (ledger-booked) keys at this point, and the ledger must produce exactly
-- those keys.
diffChecks :: L.LhrCalibration -> String -> Instruments -> [Check]
diffChecks cal nm ins =
    require (nm ++ " ledger key set = shared direct keys")
            (sort (M.keys ledger) == sort shared)
            ("ledger keys " ++ show (map showRK (M.keys ledger))
             ++ " vs shared " ++ show (map showRK shared))
    : [ approx (1e-9 * max 1.0 (abs d)) (nm ++ " " ++ showRK rk) d
               (M.findWithDefault (d + 1e18) rk ledger)
      | (rk, d) <- M.toList direct, not (isPriceIdentity rk) ]
  where
    direct = residuals cal ins
    ledger = LG.ledgerResiduals cal ins
    shared = [ rk | rk <- M.keys direct, not (isPriceIdentity rk) ]

showRK :: ResidualKey -> String
showRK (RComEquil (L.Ac c))   = "COMEQUIL(" ++ c ++ ")"
showRK (RFacEquil (L.Ac f))   = "FACEQUIL(" ++ f ++ ")"
showRK RCurAcc                = "CURACCBAL"
showRK RSavInv                = "SAVINVBAL"
showRK (RYiDef (L.Ac i))      = "YIDEF(" ++ i ++ ")"
showRK (RActProfit (L.Ac a))  = "ACTPROFIT(" ++ a ++ ")"
showRK (RPqDef (L.Ac c))      = "PQDEF(" ++ c ++ ")"
showRK (ROutAggFoc (L.Ac a) (L.Ac c)) = "OUTAGGFOC(" ++ a ++ "," ++ c ++ ")"
showRK RCpi                   = "CPIDEF"

showIC :: InstrCoord -> String
showIC (CPDS (L.Ac c)) = "dPDS." ++ c
showIC (CWF (L.Ac f))  = "dWF." ++ f
showIC CEXR            = "dEXR"
showIC (CQA (L.Ac a))  = "dQA." ++ a
showIC CIADJ           = "dIADJ"
showIC (CYI (L.Ac i))  = "dYI." ++ i
showIC (CPQ (L.Ac c))  = "dPQ." ++ c
showIC (CPXAC (L.Ac a) (L.Ac c)) = "dPXAC." ++ a ++ "." ++ c
