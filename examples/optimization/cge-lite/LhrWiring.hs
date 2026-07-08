{- |
  LhrWiring -- Lofgren-Harris-Robinson standard CGE, layer 2a: the
  Walrasian auctioneer (@EquilibriumWiring@) that composes the per-agent
  layer-1 responses ('LhrModel') into a whole-economy state.

  This is the Phase-1 wiring of the工程3 design note
  (@general-equilibrium:docs/lhr-instrument-residual-closure.md@ v3, §2 layer
  2a / §3 / §5).  Step ③a implemented here is the /forward pass/: from the
  reduced instrument vector (PDS, WF, EXR, QA, IADJ, YI) it derives every
  other price and quantity by composing the responses, reconstructing the full
  solution vector.  The residual assembly (COMEQUIL/FACEQUIL/... read off an
  EA journal via @balanceMapBy@) and the WALRAS drop are step ③b.

  == Why a forward pass exists at all (invariant (ii))

  The auctioneer never re-implements agent behaviour: it /calls/
  'LhrModel.activityPlan' \/ 'householdPlan' \/ 'commodityPlan' \/
  'instIncomeMap' with the derived signal and stitches the results together.
  It owns only what an agent may not (invariant (vii)): the derived prices,
  the net-output composition @theta*QA - sum QHA@, the transfer map (TRIIDEF),
  and the closure identities (government, savings-investment).

  == Scope of ③a (swazilan)

  For a dataset with no transport margins and no home production (swazilan:
  @CT@ empty, @SHRHOME = 0@) the derivation is /acyclic/: trade prices come
  straight from the instruments, quantities from those, and the composite
  prices (PQ, PX, PXAC, PA, PINTA) from the quantities.  With margins
  (@PM/PE/PDD@ depend on @PQ@) or home use (@QXAC(net)@ depends on @QHA@) the
  pass becomes a fixed point; that is deferred to工程4 with @test.dat@, where
  the loop is closed explicitly.  Here the margin/home terms are structurally
  empty, so the ordered derivation below terminates.
-}
module LhrWiring
    ( Instruments (..)
    , baseInstruments
    , forwardSolution
      -- * Candidate residuals (step ③b-1: sensitivity analysis before the ledger)
    , ResidualKey (..)
    , residuals
    , InstrCoord (..)
    , instrCoords
    , coordBase
    , perturb
      -- * Reduced-system solve (step ④: the auctioneer's root find)
    , instrToMap
    , mapToInstr
    , reducedResiduals
    , solveReduced
    ) where

import qualified Data.Map.Strict as M

import           LhrCalibration  (Ac (..), LhrBase (..), LhrCalibration (..),
                                  LhrParams (..), LhrSets (..))
import           LhrModel
import           Solver          (ConvergenceTol, SentinelLog, solveRoot)

-- | The reduced instrument vector the auctioneer searches over.
data Instruments = Instruments
    { insPDS  :: !(M.Map Ac Double)   -- ^ PDS(c), c in CD
    , insWF   :: !(M.Map Ac Double)   -- ^ WF(f)
    , insEXR  :: !Double              -- ^ EXR
    , insQA   :: !(M.Map Ac Double)   -- ^ QA(a)
    , insIADJ :: !Double              -- ^ IADJ
    , insYI   :: !(M.Map Ac Double)   -- ^ YI(i), i in INSDNG
    } deriving (Eq, Show)

-- | The calibrated base instruments (the point the fixture solves at).
baseInstruments :: LhrCalibration -> Instruments
baseInstruments cal = Instruments
    { insPDS  = M.fromList [ (c, mv (basePds0 base) c) | c <- setCd sets ]
    , insWF   = M.fromList [ (f, mv (baseWf0 base) f)  | f <- setF sets ]
    , insEXR  = baseExr0 base
    , insQA   = M.fromList [ (a, mv (baseQa0 base) a)  | a <- setA sets ]
    , insIADJ = baseIadj0 base
    , insYI   = M.fromList [ (i, mv (baseYi0 base) i)  | i <- setInsdng sets ]
    }
  where
    sets = calSets cal
    base = calBase cal

mv :: Ord k => M.Map k Double -> k -> Double
mv m k = M.findWithDefault 0.0 k m

-- | Reconstruct the full solution vector (variable, dotted index) -> value
-- from the instruments.  At the base instruments this equals the ground-truth
-- solution dump; that is what the step-③a sentinel checks.
forwardSolution :: LhrCalibration -> Instruments -> M.Map (String, [String]) Double
forwardSolution cal ins = M.fromList $ concat
    [ scalarE "EXR" exr, scalarE "IADJ" iadj, scalarE "WALRAS" 0.0
    , scalarE "DPI" dpi, scalarE "CPI" cpi
    , mapE1 "PDS" pdsM, mapE1 "WF" wfM, mapE1 "QA" qaM, mapE1 "YI" yiM
    , mapE1 "PM" pmM, mapE1 "PE" peM, mapE1 "PDD" pddM
    , mapE1 "PQ" pqM, mapE1 "PX" pxM, mapE1 "PA" paM, mapE1 "PINTA" pintaM
    , mapE1 "PVA" pvaM, mapE2 "PXAC" pxacM
    , mapE1 "QVA" qvaM, mapE1 "QINTA" qintaM, mapE2 "QF" qfM, mapE2 "QINT" qintM
    , mapE2 "QXAC" qxacNetM, mapE1 "QX" qxM, mapE1 "QD" qdM, mapE1 "QE" qeM
    , mapE1 "QM" qmM, mapE1 "QQ" qqM
    , mapE2 "QH" qhM, mapE3 "QHA" qhaM, mapE1 "QG" qgM, mapE1 "QINV" qinvM
    , mapE1 "QT" qtM
    , mapE1 "EH" ehM, mapE1 "MPS" mpsM, mapE1 "TINS" tinsM
    , mapE1 "YF" yfM, mapE2 "YIF" yifM
    , scalarE "YG" yg, scalarE "EG" eg, scalarE "GSAV" gsav
    , scalarE "TABS" tabs, scalarE "GOVSHR" govshr, scalarE "INVSHR" invshr
    ]
  where
    sets = calSets cal
    pars = calParams cal
    base = calBase cal

    aSet = setA sets; cSet = setC sets; fSet = setF sets; hSet = setH sets
    ctSet = setCt sets
    insdng = setInsdng sets; insd = setInsd sets

    -- instruments -----------------------------------------------------------
    exr  = insEXR ins
    iadj = insIADJ ins
    pdsM = insPDS ins
    wfM  = insWF ins
    qaM  = insQA ins
    yiM  = insYI ins

    -- (A) trade prices from instruments (margin sums are over CT, empty here)
    pmM = M.fromList
        [ (c, mv (basePwm0 base) c * (1 + mv (paramTm pars) c) * exr
              + sum [ mv pqM t * mv (paramIcm pars) (t, c) | t <- ctSet ])
        | c <- setCm sets ]
    peM = M.fromList
        [ (c, mv (basePwe0 base) c * (1 - mv (paramTe pars) c) * exr
              - sum [ mv pqM t * mv (paramIce pars) (t, c) | t <- ctSet ])
        | c <- setCe sets ]
    pddM = M.fromList
        [ (c, mv pdsM c + sum [ mv pqM t * mv (paramIcd pars) (t, c) | t <- ctSet ])
        | c <- setCd sets ]
    dpi = sum [ mv (paramDwts pars) c * mv pdsM c | c <- setCd sets ]

    -- (B) activity real quantities (Leontief) driven by the QA instrument
    qvaM   = M.fromList [ (a, mv (paramIva pars) a * mv qaM a)  | a <- aSet ]
    qintaM = M.fromList [ (a, mv (paramInta pars) a * mv qaM a) | a <- aSet ]
    qintM  = M.fromList
        [ ((c, a), ica_ca * mv qintaM a)
        | a <- aSet, c <- cSet, Just ica_ca <- [M.lookup (c, a) (paramIca pars)] ]
    qxacGrossM = M.fromList
        [ ((a, c), theta_ac * mv qaM a)
        | a <- aSet, c <- cSet, Just theta_ac <- [M.lookup (a, c) (paramTheta pars)] ]

    -- (C) net output: theta*QA - sum_h QHA.  With home production this is a
    -- fixed point (QHA needs prices that need net output), closed at工程4.
    -- swazilan has SHRHOME = 0 so there is no home use: net = gross, and the
    -- structural guard breaks the QHA -> PXAC -> QHA thunk cycle.
    qxacNetM = M.unionWith (-) qxacGrossM homeUseM
    homeUseM
        | M.null (calShrhome cal) = M.empty
        | otherwise = M.fromListWith (+)
            [ ((a, c), v) | ((a, c, _), v) <- M.toList qhaM ]

    -- price signal for the commodity response (trade prices only)
    psTrade = PriceSignal
        { psPA = M.empty, psPINTA = M.empty
        , psPDS = pdsM, psPDD = pddM, psPM = pmM, psPE = peM
        , psPQ = M.empty, psPXAC = M.empty, psWF = wfM, psEXR = exr }

    -- (D) commodity producer/trader: aggregate + CET + Armington
    coms = [ (c, commodityPlan pars sets c (qxacNetForCom c) psTrade) | c <- cSet ]
    qxacNetForCom c =
        M.fromList [ (a, v) | ((a, c'), v) <- M.toList qxacNetM, c' == c ]
    qxM = M.fromList [ (c, cpQX cp) | (c, cp) <- coms, c `elem` setCx sets ]
    qdM = M.fromList [ (c, cpQD cp) | (c, cp) <- coms, c `elem` setCd sets ]
    qeM = M.fromList [ (c, cpQE cp) | (c, cp) <- coms, c `elem` setCe sets ]
    qmM = M.fromList [ (c, cpQM cp) | (c, cp) <- coms, c `elem` setCm sets ]
    qqM = M.fromList [ (c, cpQQ cp) | (c, cp) <- coms, c `elem` setCdm sets ]

    -- (E) composite / output prices from the quantities
    pqM = M.fromList
        [ (c, ( (if c `elem` setCd sets then mv pddM c * mv qdM c else 0)
              + (if c `elem` setCm sets then mv pmM c * mv qmM c else 0) )
              / ((1 - mv (paramTq pars) c) * mv qqM c))
        | c <- setCdm sets ]
    pxM = M.fromList
        [ (c, ( (if c `elem` setCd sets then mv pdsM c * mv qdM c else 0)
              + (if c `elem` setCe sets then mv peM c * mv qeM c else 0) )
              / mv qxM c)
        | c <- setCx sets ]
    -- OUTAGGFOC inverted for PXAC(a,c).
    pxacM = M.fromList
        [ ((a, c), mv pxM c * mv qxM c / aggr * deltaac_ac
                   * spow (mv qxacNetM (a, c)) (negate rhoac_c - 1))
        | c <- cSet, M.member c (paramAlphaac pars)
        , let rhoac_c = mv (paramRhoac pars) c
              aggr = sum [ d * spow (mv qxacNetM (a', c)) (negate rhoac_c)
                         | a' <- aSet, Just d <- [M.lookup (a', c) (paramDeltaac pars)] ]
        , a <- aSet, Just deltaac_ac <- [M.lookup (a, c) (paramDeltaac pars)] ]
    paM = M.fromList
        [ (a, sum [ mv pxacM (a, c) * theta_ac
                  | c <- cSet, Just theta_ac <- [M.lookup (a, c) (paramTheta pars)] ])
        | a <- aSet ]
    pintaM = M.fromList
        [ (a, sum [ mv pqM c * ica_ca
                  | c <- cSet, Just ica_ca <- [M.lookup (c, a) (paramIca pars)] ])
        | a <- aSet ]
    cpi = sum [ mv (paramCwts pars) c * mv pqM c | c <- setCdm sets ]

    -- full price signal for the household and activity responses
    psFull = psTrade { psPA = paM, psPINTA = pintaM, psPQ = pqM, psPXAC = pxacM }

    -- (F) household budgets from the YI instrument (EHDEF)
    mpsM  = M.fromList [ (i, mv (paramMpsbar pars) i)  | i <- insdng ]
    tinsM = M.fromList [ (i, mv (paramTinsbar pars) i) | i <- insdng ]
    ehM = M.fromList
        [ (h, (1 - sumShii) * (1 - mv mpsM h) * (1 - mv tinsM h) * mv yiM h)
        | h <- hSet
        , let sumShii = sum [ mv (paramShii pars) (i', h) | i' <- insdng ] ]

    -- (G) household response (LES)
    hhs  = [ (h, householdPlan pars sets h (mv ehM h) psFull) | h <- hSet ]
    qhM  = M.fromList [ ((c, h), v) | (h, hp) <- hhs, (c, v) <- M.toList (hpQH hp) ]
    qhaM = M.fromList
        [ ((a, c, h), v) | (h, hp) <- hhs, ((a, c), v) <- M.toList (hpQHA hp) ]

    -- (H) activity factor response (zero-profit PVA + CES factor demand)
    acts = [ (a, activityPlan pars base sets a (mv qaM a) psFull) | a <- aSet ]
    pvaM = M.fromList [ (a, apPVA ap) | (a, ap) <- acts ]
    qfM  = M.fromList [ ((f, a), v) | (a, ap) <- acts, (f, v) <- M.toList (apQF ap) ]

    -- (I) transfers (TRIIDEF) then institution income
    triiM = M.fromList
        [ ((i, ip), shii_iip * (1 - mv mpsM ip) * (1 - mv tinsM ip) * mv yiM ip)
        | i <- insdng, ip <- insdng
        , Just shii_iip <- [M.lookup (i, ip) (paramShii pars)] ]
    transSig = TransferSignal triiM
    yfM = M.fromList
        [ (f, sum [ mv wfM f * mv (baseWfdist0 base) (f, a) * mv qfM (f, a)
                  | a <- aSet ])
        | f <- fSet ]
    incSig = IncomeSignal yfM yiM
    insts = [ (i, instIncomeMap pars base sets i incSig transSig psFull) | i <- insd ]
    yifM = M.fromList
        [ ((i, f), v) | (i, ii) <- insts, (f, v) <- M.toList (iiYIF ii) ]

    -- (J) government + macro closure
    qgM = M.fromList
        [ (c, baseGadj0 base * qbarg_c)
        | c <- cSet, let qbarg_c = mv (paramQbarg pars) c, qbarg_c /= 0 ]
    qinvM = M.fromList
        [ (c, iadj * qbarinv_c)
        | c <- cSet, let qbarinv_c = mv (paramQbarinv pars) c, qbarinv_c /= 0 ]
    qtM = M.fromList
        [ (c, sum [ mv (paramIcm pars) (c, cp) * (if cp `elem` setCm sets then mv qmM cp else 0)
                  + mv (paramIce pars) (c, cp) * (if cp `elem` setCe sets then mv qeM cp else 0)
                  + mv (paramIcd pars) (c, cp) * (if cp `elem` setCd sets then mv qdM cp else 0)
                  | cp <- cSet ])
        | c <- ctSet ]

    yg = sum [ mv tinsM i * mv yiM i | i <- insdng ]
       + sum [ mv (paramTf pars) f * mv yfM f | f <- fSet ]
       + sum [ mv (paramTva pars) a * mv pvaM a * mv qvaM a | a <- aSet ]
       + sum [ mv (paramTa pars) a * mv paM a * mv qaM a | a <- aSet ]
       + sum [ mv (paramTm pars) c * mv (basePwm0 base) c * mv qmM c | c <- setCm sets ] * exr
       + sum [ mv (paramTe pars) c * mv (basePwe0 base) c * mv qeM c | c <- setCe sets ] * exr
       + sum [ mv (paramTq pars) c * mv pqM c * mv qqM c | c <- setCdm sets ]
       + sum [ mv yifM (Ac "GOV", f) | f <- fSet ]
       + mv (paramTrnsfr pars) (Ac "GOV", Ac "ROW") * exr
    eg = sum [ mv pqM c * mv qgM c | c <- cSet ]
       + sum [ mv (paramTrnsfr pars) (i, Ac "GOV") | i <- insdng ] * cpi
    gsav = yg - eg

    tabs = sum [ mv pqM c * mv qhM (c, h) | c <- cSet, h <- hSet ]
         + sum [ mv pxacM (a, c) * v | ((a, c, _), v) <- M.toList qhaM ]
         + sum [ mv pqM c * mv qgM c | c <- cSet ]
         + sum [ mv pqM c * mv qinvM c | c <- setCinv sets ]
         + sum [ mv pqM c * mv (paramQdst pars) c | c <- setCdm sets ]
    invAbs = sum [ mv pqM c * mv qinvM c | c <- setCinv sets ]
           + sum [ mv pqM c * mv (paramQdst pars) c | c <- setCdm sets ]
    govAbs = sum [ mv pqM c * mv qgM c | c <- cSet ]
    invshr = invAbs / tabs
    govshr = govAbs / tabs

    -- assembly helpers ------------------------------------------------------
    scalarE nm v = [((nm, []), v)]
    mapE1 nm m = [ ((nm, [s]), v) | (Ac s, v) <- M.toList m ]
    mapE2 nm m = [ ((nm, [a, b]), v) | ((Ac a, Ac b), v) <- M.toList m ]
    mapE3 nm m = [ ((nm, [a, b, c]), v) | ((Ac a, Ac b, Ac c), v) <- M.toList m ]

------------------------------------------------------------------
-- * Candidate residuals (step ③b-1)
------------------------------------------------------------------

-- | The seven candidate residual families the reduced system might close on.
-- Which are true residuals (move under perturbation) versus structural
-- identities (≡ 0 by construction) is settled numerically by the sensitivity
-- matrix, not assumed — cf. the codex\/Fable cross-check on
-- @docs/lhr-instrument-residual-closure.md@.
data ResidualKey
    = RComEquil Ac   -- ^ composite-good market clearing (COMEQUIL), c in CDM
    | RFacEquil Ac   -- ^ factor market clearing (FACEQUIL), f in F
    | RCurAcc        -- ^ current-account balance (CURACCBAL)
    | RSavInv        -- ^ savings = investment (SAVINVBAL; LHR's WALRAS row)
    | RYiDef Ac      -- ^ institution income identity (YIDEF), i in INSDNG
    | RActProfit Ac  -- ^ activity zero-profit gap (PVADEF), a in A
    | RCpi           -- ^ numeraire pin (CPIDEF)
    deriving (Eq, Ord, Show)

-- | Compute every candidate residual directly (algebraic transcription of the
-- @build_system@ constraint block) from the forward state.  At the base
-- instruments all are ≈ 0; off the base the true residuals move.  These are
-- the /oracle/ residuals: step ③b-2 will read the same numbers off an EA
-- journal and assert agreement.
residuals :: LhrCalibration -> Instruments -> M.Map ResidualKey Double
residuals cal ins = M.fromList $
       [ (RComEquil c, comeq c)  | c <- setCdm sets ]
    ++ [ (RFacEquil f, faceq f)  | f <- setF sets ]
    ++ [ (RCurAcc, curacc) ]
    ++ [ (RSavInv, savinv) ]
    ++ [ (RYiDef i, yidef i)     | i <- setInsdng sets ]
    ++ [ (RActProfit a, actpr a) | a <- setA sets ]
    ++ [ (RCpi, cpidef) ]
  where
    fwd = forwardSolution cal ins
    g nm ix = M.findWithDefault 0.0 (nm, ix) fwd
    sets = calSets cal
    pars = calParams cal
    base = calBase cal
    acn (Ac s) = s

    -- COMEQUIL(c): QQ(c) = sum QINT + sum QH + QG + QINV + qdst + QT.
    comeq c = g "QQ" [acn c]
        - ( sum [ g "QINT" [acn c, acn a] | a <- setA sets ]
          + sum [ g "QH" [acn c, acn h]   | h <- setH sets ]
          + g "QG" [acn c] + g "QINV" [acn c]
          + mv (paramQdst pars) c
          + g "QT" [acn c] )
    -- FACEQUIL(f): sum_a QF(f,a) = QFS(f) (fixed).
    faceq f = sum [ g "QF" [acn f, acn a] | a <- setA sets ] - mv (baseQfs0 base) f
    -- CURACCBAL: imports + ROW transfers in = exports + transfers out + FSAV.
    curacc = sum [ mv (basePwm0 base) c * g "QM" [acn c] | c <- setCm sets ]
           + sum [ mv (paramTrnsfr pars) (Ac "ROW", f) | f <- setF sets ]
           - sum [ mv (basePwe0 base) c * g "QE" [acn c] | c <- setCe sets ]
           - sum [ mv (paramTrnsfr pars) (i, Ac "ROW") | i <- setInsd sets ]
           - baseFsav0 base
    -- SAVINVBAL: institution + gov + foreign saving = investment + stocks.
    savinv = sum [ g "MPS" [acn i] * (1 - g "TINS" [acn i]) * g "YI" [acn i]
                 | i <- setInsdng sets ]
           + g "GSAV" [] + baseFsav0 base * g "EXR" []
           - sum [ g "PQ" [acn c] * g "QINV" [acn c] | c <- setCinv sets ]
           - sum [ g "PQ" [acn c] * mv (paramQdst pars) c | c <- setCdm sets ]
    -- YIDEF(i): YI instrument = receipts (factor income + transfers).
    yidef i = g "YI" [acn i] - yirecv i
    yirecv i = sum [ g "YIF" [acn i, acn f] | f <- setF sets ]
             + sum [ trii i ip | ip <- setInsdng sets ]
             + mv (paramTrnsfr pars) (i, Ac "GOV") * baseCpi0 base
             + mv (paramTrnsfr pars) (i, Ac "ROW") * g "EXR" []
    trii i ip = mv (paramShii pars) (i, ip)
              * (1 - mv (paramMpsbar pars) ip) * (1 - mv (paramTinsbar pars) ip)
              * g "YI" [acn ip]
    -- ACTPROFIT(a): zero-profit gap PA(1-ta)QA - PVA*QVA - PINTA*QINTA (PVADEF).
    actpr a = g "PA" [acn a] * (1 - mv (paramTa pars) a) * g "QA" [acn a]
            - g "PVA" [acn a] * g "QVA" [acn a]
            - g "PINTA" [acn a] * g "QINTA" [acn a]
    -- CPIDEF: sum cwts*PQ = CPIbar.
    cpidef = sum [ mv (paramCwts pars) c * g "PQ" [acn c] | c <- setCdm sets ]
           - baseCpi0 base

------------------------------------------------------------------
-- * Instrument perturbation (numerical sensitivity matrix)
------------------------------------------------------------------

-- | One coordinate of the reduced instrument vector.
data InstrCoord
    = CPDS Ac | CWF Ac | CEXR | CQA Ac | CIADJ | CYI Ac
    deriving (Eq, Ord, Show)

-- | The instrument coordinates for a dataset, in the reduced-vector order.
instrCoords :: LhrCalibration -> [InstrCoord]
instrCoords cal =
       [ CPDS c | c <- setCd (calSets cal) ]
    ++ [ CWF f  | f <- setF (calSets cal) ]
    ++ [ CEXR ]
    ++ [ CQA a  | a <- setA (calSets cal) ]
    ++ [ CIADJ ]
    ++ [ CYI i  | i <- setInsdng (calSets cal) ]

-- | The base value of one instrument coordinate (for a relative step size).
coordBase :: Instruments -> InstrCoord -> Double
coordBase ins c = case c of
    CPDS a -> mv (insPDS ins) a
    CWF f  -> mv (insWF ins) f
    CEXR   -> insEXR ins
    CQA a  -> mv (insQA ins) a
    CIADJ  -> insIADJ ins
    CYI i  -> mv (insYI ins) i

-- | Perturb one instrument coordinate by an additive step.
perturb :: Double -> InstrCoord -> Instruments -> Instruments
perturb e c ins = case c of
    CPDS a -> ins { insPDS = M.insertWith (+) a e (insPDS ins) }
    CWF f  -> ins { insWF  = M.insertWith (+) f e (insWF ins) }
    CEXR   -> ins { insEXR = insEXR ins + e }
    CQA a  -> ins { insQA  = M.insertWith (+) a e (insQA ins) }
    CIADJ  -> ins { insIADJ = insIADJ ins + e }
    CYI i  -> ins { insYI  = M.insertWith (+) i e (insYI ins) }

------------------------------------------------------------------
-- * Reduced-system solve (step ④)
------------------------------------------------------------------

-- | The instrument vector as a coordinate-keyed map (the solver's unknowns).
instrToMap :: LhrCalibration -> Instruments -> M.Map InstrCoord Double
instrToMap cal ins = M.fromList [ (c, coordBase ins c) | c <- instrCoords cal ]

-- | Rebuild the instrument record from a coordinate-keyed map.
mapToInstr :: LhrCalibration -> M.Map InstrCoord Double -> Instruments
mapToInstr cal um = Instruments
    { insPDS  = M.fromList [ (c, u (CPDS c)) | c <- setCd sets ]
    , insWF   = M.fromList [ (f, u (CWF f))  | f <- setF sets ]
    , insEXR  = u CEXR
    , insQA   = M.fromList [ (a, u (CQA a))  | a <- setA sets ]
    , insIADJ = u CIADJ
    , insYI   = M.fromList [ (i, u (CYI i))  | i <- setInsdng sets ]
    }
  where
    sets = calSets cal
    u k = M.findWithDefault 0.0 k um

-- | The reduced oracle for the auctioneer: instruments (coordinate-keyed) to
-- the six residuals it closes on, mapped onto the same coordinate keys.  The
-- Walras-dependent SAVINVBAL is dropped; the pairing of a residual to a
-- coordinate is a cosmetic label (Newton uses the full Jacobian), so it is a
-- fixed positional zip of the two length-6 lists.
reducedResiduals :: LhrCalibration -> M.Map InstrCoord Double -> M.Map InstrCoord Double
reducedResiduals cal um =
    M.fromList (zip (instrCoords cal) [ r M.! rk | rk <- kept ])
  where
    r    = residuals cal (mapToInstr cal um)
    kept = filter (/= RSavInv) (M.keys r)

-- | Solve the reduced system from a starting instrument vector: the Walrasian
-- auctioneer's actual root find.  Returns the settled instruments and the
-- solve's sentinel log (K, conditioning, convergence).
solveReduced :: LhrCalibration -> Instruments -> ConvergenceTol
             -> (Instruments, SentinelLog)
solveReduced cal ins0 tol =
    let (sol, slog) = solveRoot (reducedResiduals cal) (instrToMap cal ins0) tol
    in (mapToInstr cal sol, slog)
