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

  Generalized from the swazilan-only step ③b-2 sentinel to both datasets with
  the工程4 sparse-uniform vector (PQ\/PXAC promoted).
-}
module Main where

import           Data.List       (sort)
import qualified Data.Map.Strict as M

import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Algebra.Internal as Internal
import           ExchangeAlgebra.Algebra
    ( Alg
    , AccountTitles (Cash, Products)
    , CountUnit (Amount, Yen, Dollar)
    , Hat (Hat, Not)
    , HatBase ((:<))
    , Redundant ((.+))
    )

import qualified LhrCalibration as L
import           LhrCalibration
    ( Ac (..)
    , LhrBase (..)
    , LhrCalibration (..)
    , LhrParams (..)
    , LhrSets (..)
    )
import           LhrLedger (LhrProduct (..), LhrEntity (..), LBase)
import           LhrWiring
import qualified LhrLedger      as LG
import           TestHarness

datasets :: [(String, FilePath)]
datasets =
    [ ("swazilan", "optimization/cge-lite/lhr/swazilan-inputs.csv")
    , ("test",     "optimization/cge-lite/lhr/test-inputs.csv")
    , ("zimbabwe", "optimization/cge-lite/lhr/zimbabwe-inputs.csv") ]

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
    [ require (nm ++ " ledger key set = shared direct keys")
            (sort (M.keys ledger) == sort shared)
            ("ledger keys " ++ show (map showRK (M.keys ledger))
             ++ " vs shared " ++ show (map showRK shared)) ]
    ++ [ approx 1e-12 (nm ++ " legacy base " ++ show key)
             (M.findWithDefault 0 key oldNets) (M.findWithDefault 0 key newNets)
    | key <- M.keys (M.union oldNets newNets) ]
    ++ [ approx 1e-12 (nm ++ " legacy residual " ++ showRK key)
                (M.findWithDefault 0 key oldResiduals)
                (M.findWithDefault 0 key newResiduals)
       | key <- M.keys (M.union oldResiduals newResiduals) ]
    ++ [ approx 1e-12 (nm ++ " legacy ledger residual " ++ showRK key)
                (M.findWithDefault 0 key oldLedgerResiduals)
                (M.findWithDefault 0 key newLedgerResiduals)
       | key <- M.keys (M.union oldLedgerResiduals newLedgerResiduals) ]
    ++ [ approx (1e-9 * max 1.0 (abs d)) (nm ++ " " ++ showRK rk) d
               (M.findWithDefault (d + 1e18) rk ledger)
      | (rk, d) <- M.toList direct, not (isPriceIdentity rk) ]
  where
    direct = residuals cal ins
    ledger = LG.ledgerResiduals cal ins
    shared = [ rk | rk <- M.keys direct, not (isPriceIdentity rk) ]
    oldAlg = legacyLedgerAlg cal ins
    newAlg = LG.ledgerAlg cal ins
    oldNets = EA.balanceMapBy Just oldAlg
    newNets = EA.balanceMapBy Just newAlg
    oldResiduals = LG.residualsFromAlg oldAlg
    newResiduals = LG.residualsFromAlg newAlg
    oldLedgerResiduals = M.insert RCpi cpidef oldResiduals
    newLedgerResiduals = LG.ledgerResiduals cal ins
    fwd = forwardSolution cal ins
    g :: String -> [String] -> Double
    g name indices = M.findWithDefault 0.0 (name, indices) fwd
    cpidef = sum [ M.findWithDefault 0.0 c (paramCwts (calParams cal)) * g "PQ" [name]
                 | c@(Ac name) <- setCdm (calSets cal) ]
           - baseCpi0 (calBase cal)

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


-- | A cash flow in a currency: @payer@ pays @v@ (Hat), @payee@ receives (Not).
legacyCashIn :: CountUnit
             -> Double
             -> LhrEntity
             -> LhrEntity
             -> Alg Double LBase
legacyCashIn cur v payer payee =
       (v `Internal.Single` (Hat :< (Cash, PMoney, payer, cur)))
    .+ (v `Internal.Single` (Not :< (Cash, PMoney, payee, cur)))

-- | One payment leg: @e@ pays @v@ in @cur@.
legacyPayLeg :: Double -> CountUnit -> LhrEntity -> Alg Double LBase
legacyPayLeg v cur e = v `Internal.Single` (Hat :< (Cash, PMoney, e, cur))

-- | One receipt leg: @e@ receives @v@ in @cur@.
legacyRecvLeg :: Double -> CountUnit -> LhrEntity -> Alg Double LBase
legacyRecvLeg v cur e = v `Internal.Single` (Not :< (Cash, PMoney, e, cur))

-- | A one-sided physical creation (endowment supply) with no absorber leg.
legacySupply :: Double -> LhrProduct -> LhrEntity -> Alg Double LBase
legacySupply q p e = q `Internal.Single` (Not :< (Products, p, e, Amount))

-- | A one-sided physical absorption (final demand) with no supplier leg.
legacyAbsorb :: Double -> LhrProduct -> LhrEntity -> Alg Double LBase
legacyAbsorb q p e = q `Internal.Single` (Hat :< (Products, p, e, Amount))

-- | Reproduce the pre-migration flows using signed internal entries.
legacyLedgerAlg :: LhrCalibration -> Instruments -> Alg Double LBase
legacyLedgerAlg cal ins = mconcat $ concat
    [ factorHire, factorIncome, intermediates
    , activityOutput, homeConsumption, activityTaxes
    , commoditySales, commodityTrade, commodityTax
    , householdIncome, householdSpend
    , govFlows, saveInvFlows ]
  where
    fwd  = forwardSolution cal ins
    g nm ix = M.findWithDefault 0.0 (nm, ix) fwd
    sets = calSets cal
    pars = calParams cal
    base = calBase cal
    acn (Ac s) = s
    exr  = g "EXR" []
    cpi  = baseCpi0 base

    aSet = setA sets; cSet = setC sets; fSet = setF sets; hSet = setH sets
    insdng = setInsdng sets; insd = setInsd sets

    param2 m a b = M.findWithDefault 0.0 (a, b) m
    trnsfr a b = param2 (paramTrnsfr pars) a b

    factorHire =
        [ legacySupply (M.findWithDefault 0.0 f (baseQfs0 base)) (PFactor f) (EFactorMkt f)
        | f <- fSet ]
        ++
        [ legacyAbsorb qf (PFactor f) (EActivity a)
          .+ legacyCashIn Yen (g "WF" [acn f] * param2 (baseWfdist0 base) f a * qf)
                        (EActivity a) (EFactorMkt f)
        | f <- fSet, a <- aSet, let qf = g "QF" [acn f, acn a], qf /= 0 ]

    factorIncome =
        [ legacyCashIn Yen (g "YIF" [acn i, acn f]) (EFactorMkt f) (instEntity i)
        | i <- insd, f <- fSet, g "YIF" [acn i, acn f] /= 0 ]
        ++
        [ legacyCashIn Yen (mv (paramTf pars) f * g "YF" [acn f]) (EFactorMkt f) EGov
        | f <- fSet ]
        ++
        [ legacyPayLeg (trnsfr (Ac "ROW") f * exr) Yen (EFactorMkt f)
          .+ legacyRecvLeg (trnsfr (Ac "ROW") f) Dollar ERow
        | f <- fSet, trnsfr (Ac "ROW") f /= 0 ]

    intermediates =
        [ legacyAbsorb qint (PComposite c) (EActivity a)
          .+ legacyCashIn Yen (g "PQ" [acn c] * qint) (EActivity a) (ECommodity c)
        | c <- cSet, a <- aSet, let qint = g "QINT" [acn c, acn a], qint /= 0 ]

    activityOutput =
        [ legacyCashIn Yen (netOutputValue a) (ECommodity (outputCommodity a)) (EActivity a)
        | a <- aSet ]

    homeConsumption =
        [ legacyCashIn Yen (g "PXAC" [a, c] * v) (EInstNG (Ac h)) (EActivity (Ac a))
        | (("QHA", [a, c, h]), v) <- M.toList fwd, v /= 0 ]

    activityTaxes =
        [ legacyCashIn Yen (mv (paramTa pars) a * g "PA" [acn a] * g "QA" [acn a])
                     (EActivity a) EGov
        | a <- aSet ]
        ++
        [ legacyCashIn Yen (mv (paramTva pars) a * g "PVA" [acn a] * g "QVA" [acn a])
                     (EActivity a) EGov
        | a <- aSet ]

    commoditySales =
        [ legacySupply (g "QQ" [acn c]) (PComposite c) (ECommodity c) | c <- cSet ]
        ++ concat
        [ [ legacyAbsorb (g "QH" [acn c, acn h]) (PComposite c) (EInstNG h)
            .+ legacyCashIn Yen (g "PQ" [acn c] * g "QH" [acn c, acn h]) (EInstNG h) (ECommodity c)
          | h <- hSet, g "QH" [acn c, acn h] /= 0 ]
        | c <- cSet ]
        ++
        [ legacyAbsorb (g "QG" [acn c]) (PComposite c) EGov
          .+ legacyCashIn Yen (g "PQ" [acn c] * g "QG" [acn c]) EGov (ECommodity c)
        | c <- cSet, g "QG" [acn c] /= 0 ]
        ++
        [ legacyAbsorb (g "QINV" [acn c]) (PComposite c) ESaveInv
          .+ legacyCashIn Yen (g "PQ" [acn c] * g "QINV" [acn c]) ESaveInv (ECommodity c)
        | c <- cSet, g "QINV" [acn c] /= 0 ]
        ++
        [ legacyAbsorb (mv (paramQdst pars) c) (PComposite c) ESaveInv
          .+ legacyCashIn Yen (g "PQ" [acn c] * mv (paramQdst pars) c) ESaveInv (ECommodity c)
        | c <- cSet, mv (paramQdst pars) c /= 0 ]
        ++
        [ legacyAbsorb (g "QT" [acn c]) (PComposite c) (ECommodity c)
        | c <- setCt sets, g "QT" [acn c] /= 0 ]

    commodityTrade =
        [ legacyPayLeg (mv (basePwm0 base) c * exr * g "QM" [acn c]) Yen (ECommodity c)
          .+ legacyRecvLeg (mv (basePwm0 base) c * g "QM" [acn c]) Dollar ERow
        | c <- setCm sets, g "QM" [acn c] /= 0 ]
        ++
        [ legacyPayLeg (mv (basePwe0 base) c * g "QE" [acn c]) Dollar ERow
          .+ legacyRecvLeg (mv (basePwe0 base) c * exr * g "QE" [acn c]) Yen (ECommodity c)
        | c <- setCe sets, g "QE" [acn c] /= 0 ]

    commodityTax =
        [ legacyCashIn Yen (mv (paramTq pars) c * g "PQ" [acn c] * g "QQ" [acn c]) (ECommodity c) EGov
        | c <- setCdm sets, mv (paramTq pars) c /= 0 ]
        ++
        [ legacyCashIn Yen (mv (paramTm pars) c * mv (basePwm0 base) c * exr * g "QM" [acn c])
                     (ECommodity c) EGov
        | c <- setCm sets, mv (paramTm pars) c /= 0 ]
        ++
        [ legacyCashIn Yen (mv (paramTe pars) c * mv (basePwe0 base) c * exr * g "QE" [acn c])
                     (ECommodity c) EGov
        | c <- setCe sets, mv (paramTe pars) c /= 0 ]

    householdIncome =
        [ legacyCashIn Yen (trii i ip) (instEntity ip) (EInstNG i)
        | i <- insdng, ip <- insdng, trii i ip /= 0 ]
        ++
        [ legacyCashIn Yen (trnsfr i (Ac "GOV") * cpi) EGov (EInstNG i)
        | i <- insdng, trnsfr i (Ac "GOV") /= 0 ]
        ++
        [ legacyRecvLeg (trnsfr i (Ac "ROW") * exr) Yen (instEntity i)
          .+ legacyPayLeg (trnsfr i (Ac "ROW")) Dollar ERow
        | i <- insd, trnsfr i (Ac "ROW") /= 0 ]

    householdSpend =
        [ legacyCashIn Yen (g "TINS" [acn i] * g "YI" [acn i]) (EInstNG i) EGov | i <- insdng ]
        ++
        [ legacyCashIn Yen (g "MPS" [acn i] * (1 - g "TINS" [acn i]) * g "YI" [acn i])
                     (EInstNG i) ESaveInv
        | i <- insdng ]

    govFlows =
        [ legacyCashIn Yen (g "GSAV" []) EGov ESaveInv ]

    saveInvFlows =
        [ (baseFsav0 base * exr `Internal.Single` (Not :< (Cash, PMoney, ESaveInv, Yen)))
          .+ (baseFsav0 base `Internal.Single` (Hat :< (Cash, PMoney, ERow, Dollar))) ]

    mv m k = M.findWithDefault 0.0 k m
    netOutputValue a =
        sum [ g "PXAC" [acn a, c] * v
            | (("QXAC", [a', c]), v) <- M.toList fwd, a' == acn a ]
    trii i ip = mv (paramShii pars) (i, ip)
              * (1 - mv (paramMpsbar pars) ip) * (1 - mv (paramTinsbar pars) ip)
              * g "YI" [acn ip]
    outputCommodity a = head ([ c | c <- cSet, mv (paramTheta pars) (a, c) /= 0 ] ++ [Ac "COM"])
    instEntity i
        | i `elem` insdng = EInstNG i
        | otherwise       = EGov
