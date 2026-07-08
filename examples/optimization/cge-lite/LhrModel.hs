{- |
  LhrModel -- Lofgren-Harris-Robinson standard CGE as an AS-ABM, layer 0
  (signal/plan types) and layer 1 (per-agent behavioural responses).

  This is the /per-agent/ reconstruction mandated by the工程3 design note
  (@general-equilibrium:docs/lhr-instrument-residual-closure.md@ v3).  Each
  behavioural equation is a verbatim closed-form transcription of one
  @build_system@ block in the Python ground truth
  (@benchmarks/lhr-standard-cge/lhr_resolve.py@); equation names in the
  haddocks match that file and @mod100.gms@.

  == What layer 1 is (and is not)

  A response is a /pure function of prices + own scale/income + own
  parameters/.  It never sees a global allocation (invariant (i), enforced by
  these signatures taking no @Allocation@), never solves for its own income
  (invariant (iv) — income arrives as an argument), and never assumes a market
  clears (invariant (vii) — clearing is the auctioneer's residual, layer 2a).
  The reduced-instrument wiring (layer 2a), the ledger types
  (@Transaction@/@SettlementLeg@), and @post@ (layer 3) arrive with工程3
  step ③, where records are actually kept; this module deliberately stops at
  the quantity responses that step ② snapshot-tests against the fixture.

  == Price provenance

  At the calibrated base every price in 'PriceSignal' equals its fixture
  value, so the responses are exercised in isolation without a solve.  Under
  the layer-2a auctioneer (step ③) the same 'PriceSignal' is /derived/ from
  the reduced instruments (PDS, WF, EXR, ...) before the responses run; the
  agents are indifferent to which of the two supplied it.
-}
module LhrModel
    ( -- * Layer 0 — signal and plan types
      PriceSignal (..)
    , IncomeSignal (..)
    , TransferSignal (..)
    , ActivityPlan (..)
    , HouseholdPlan (..)
    , CommodityPlan (..)
    , InstIncome (..)
      -- * Layer 1 — per-agent behavioural responses
    , activityPlan
    , householdPlan
    , commodityPlan
    , instIncomeMap
      -- * Shared numeric guard
    , spow
    ) where

import qualified Data.Map.Strict as M

import           LhrCalibration  (Ac (..), LhrBase (..), LhrParams (..),
                                  LhrSets (..))

------------------------------------------------------------------
-- * Numeric guard
------------------------------------------------------------------

-- | GAMS-style power guard (cf. @spow@ in @lhr_resolve.py@ / VARLOW.INC):
-- clamps a wayward base away from a non-positive value so a fractional
-- exponent stays real.  At any feasible point @base > 0@ so it is the plain
-- power; it only matters for a stray Newton probe in step ④.
spow :: Double -> Double -> Double
spow base e = max base 1e-9 ** e

-- Map lookup with a 0 default (absent key = structural zero in the SAM).
mv :: Ord k => M.Map k Double -> k -> Double
mv m k = M.findWithDefault 0.0 k m

------------------------------------------------------------------
-- * Layer 0 — signal and plan types
------------------------------------------------------------------

-- | The shared price vector.  A record of maps (not a @Map Market Price@)
-- because the LHR price schema is fixed and indexing by economic role keeps
-- the transcription legible.  It carries no ledger type (invariant (iii)).
-- Only prices actually /read/ by a response appear here; derived prices an
-- agent computes for itself (PVA) or that are pure outputs (PX, DPI) do not.
data PriceSignal = PriceSignal
    { psPA    :: !(M.Map Ac Double)         -- ^ activity output price PA(a)
    , psPINTA :: !(M.Map Ac Double)         -- ^ intermediate-bundle price PINTA(a)
    , psPDS   :: !(M.Map Ac Double)         -- ^ domestic supply price PDS(c)
    , psPDD   :: !(M.Map Ac Double)         -- ^ domestic demand price PDD(c)
    , psPM    :: !(M.Map Ac Double)         -- ^ import price PM(c)
    , psPE    :: !(M.Map Ac Double)         -- ^ export price PE(c)
    , psPQ    :: !(M.Map Ac Double)         -- ^ composite price PQ(c)
    , psPXAC  :: !(M.Map (Ac, Ac) Double)   -- ^ activity output price PXAC(a,c)
    , psWF    :: !(M.Map Ac Double)         -- ^ factor price WF(f)
    , psEXR   :: !Double                    -- ^ exchange rate EXR
    } deriving (Eq, Show)

-- | Income the auctioneer supplies to the institution block (invariant (iv)):
-- factor income YF(f) it aggregates from the activities' QF, and the current
-- institution-income instrument YI(i) used for cross-institution references.
data IncomeSignal = IncomeSignal
    { isYF :: !(M.Map Ac Double)            -- ^ YF(f) factor income
    , isYI :: !(M.Map Ac Double)            -- ^ YI(i) institution income (instrument)
    } deriving (Eq, Show)

-- | Inter-institution transfers TRII(i,ip), supplied by the auctioneer
-- (invariant: a response may not global-read the whole YI vector to build its
-- own transfers; it reads them here, typed).  The TRIIDEF map that produces
-- these is the auctioneer's job in step ③.
newtype TransferSignal = TransferSignal { unTransferSignal :: M.Map (Ac, Ac) Double }
    deriving (Eq, Show)

-- | Activity (firm) response.  @apQXACgross@ is /gross/ output @theta*QA@;
-- household home-use is netted out by the wiring, not here (invariant: the
-- per-agent boundary — the firm does not read QHA).
data ActivityPlan = ActivityPlan
    { apPVA       :: !Double                -- ^ zero-profit value-added price (PVADEF)
    , apQVA       :: !Double                -- ^ value added QVA (LEOAGGVA)
    , apQINTA     :: !Double                -- ^ intermediate aggregate QINTA (LEOAGGINT)
    , apQF        :: !(M.Map Ac Double)     -- ^ factor demand QF(f) (CES, CESVAFOC)
    , apQINT      :: !(M.Map Ac Double)     -- ^ intermediate demand QINT(c) (INTDEM)
    , apQXACgross :: !(M.Map Ac Double)     -- ^ gross output theta(a,c)*QA(a) by commodity
    } deriving (Eq, Show)

-- | Household response (LES).  @hpQHA@ is home consumption per (activity,
-- commodity); empty when the dataset has no home production (swazilan).
data HouseholdPlan = HouseholdPlan
    { hpQH  :: !(M.Map Ac Double)           -- ^ market demand QH(c) (HMDEM)
    , hpQHA :: !(M.Map (Ac, Ac) Double)     -- ^ home demand QHA(a,c) (HADEM)
    } deriving (Eq, Show)

-- | Commodity producer + trade response: aggregate activity output into a
-- domestic good (OUTAGGFN), transform it between domestic and export (CET),
-- and combine domestic with imports (Armington).
data CommodityPlan = CommodityPlan
    { cpQX :: !Double                       -- ^ aggregate output QX (OUTAGGFN)
    , cpQD :: !Double                       -- ^ domestic sales QD (CET)
    , cpQE :: !Double                       -- ^ exports QE (CET/ESUPPLY)
    , cpQM :: !Double                       -- ^ imports QM (Armington/COSTMIN)
    , cpQQ :: !Double                       -- ^ composite supply QQ (Armington)
    } deriving (Eq, Show)

-- | Institution income response.  @iiYIrecv@ is the receipt total the
-- auctioneer compares against the YI instrument (YIDEF residual);
-- @iiEH@ is the household consumption budget (EHDEF), present only for @H@.
data InstIncome = InstIncome
    { iiYIF    :: !(M.Map Ac Double)        -- ^ factor income YIF(i,f) by factor (YIFDEF)
    , iiYIrecv :: !Double                   -- ^ receipt total for the YIDEF residual
    , iiEH     :: !(Maybe Double)           -- ^ household budget EH(h) (EHDEF), @H@ only
    } deriving (Eq, Show)

------------------------------------------------------------------
-- * Layer 1 — per-agent behavioural responses
------------------------------------------------------------------

-- | Activity @a@ at output scale @QA@ under a price signal.  Returns the
-- firm's cost-minimising factor mix, its Leontief aggregates, and gross
-- output; the scale itself is demand-determined (CRS), supplied as an
-- argument rather than chosen here.
activityPlan :: LhrParams -> LhrBase -> LhrSets -> Ac -> Double -> PriceSignal -> ActivityPlan
activityPlan pars base sets a qa ps = ActivityPlan
    { apPVA       = pva
    , apQVA       = qva
    , apQINTA     = qinta
    , apQF        = qf
    , apQINT      = qint
    , apQXACgross = qxacg
    }
  where
    iva_a     = mv (paramIva pars) a
    inta_a    = mv (paramInta pars) a
    tva_a     = mv (paramTva pars) a
    rhova_a   = mv (paramRhova pars) a
    alphava_a = mv (paramAlphava pars) a

    -- PVA as the CES value-added dual /unit cost/ of the factor bundle, NOT
    -- from PVADEF.  Deriving it from the revenue side (PA(1-ta) = PVA*iva +
    -- PINTA*inta) makes the firm zero-profit by construction, so the PVADEF
    -- gap degenerates to an identity and cannot pin QA (numerically confirmed:
    -- cge-lite-lhr-residual-test showed a zero ACTPROFIT row).  The cost dual
    -- lets revenue (PA) and cost (PVA) diverge off the base, so the zero-profit
    -- gap is a true residual the auctioneer closes on QA.  At the calibrated
    -- base the two coincide (PVA0 = 1), so the forward pass stays exact.
    pva = (1 / (1 - tva_a)) * (1 / alphava_a)
          * spow ( sum [ spow deltava_fa (1 / (1 + rhova_a))
                         * spow (mv (psWF ps) f * mv (baseWfdist0 base) (f, a))
                                (rhova_a / (1 + rhova_a))
                       | f <- setF sets
                       , Just deltava_fa <- [M.lookup (f, a) (paramDeltava pars)] ] )
                 ((1 + rhova_a) / rhova_a)
    qva   = iva_a * qa       -- LEOAGGVA
    qinta = inta_a * qa      -- LEOAGGINT

    -- CES value-added factor demand: invert CESVAFOC using
    -- S = sum_f deltava(f,a) QF(f,a)^(-rhova) = (QVA/alphava)^(-rhova).
    sAgg = spow (qva / alphava_a) (negate rhova_a)
    qf = M.fromList
        [ (f, spow ( pva * (1 - tva_a) * qva * deltava_fa
                     / (wf_f * wfdist_fa * sAgg) )
                   (1 / (rhova_a + 1)))
        | f <- setF sets
        , Just deltava_fa <- [M.lookup (f, a) (paramDeltava pars)]
        , let wf_f      = mv (psWF ps) f
              wfdist_fa = mv (baseWfdist0 base) (f, a) ]

    -- INTDEM: QINT(c,a) = ica(c,a) * QINTA(a).
    qint = M.fromList
        [ (c, ica_ca * qinta)
        | c <- setC sets
        , Just ica_ca <- [M.lookup (c, a) (paramIca pars)] ]

    -- Gross output theta(a,c)*QA(a); home-use netting is the wiring's job.
    qxacg = M.fromList
        [ (c, theta_ac * qa)
        | c <- setC sets
        , Just theta_ac <- [M.lookup (a, c) (paramTheta pars)] ]

-- | Household @h@ with consumption budget @EH@ under a price signal (LES
-- demand).  Income arrives as the @EH@ argument; the household never solves
-- for it (invariant (iv)).
householdPlan :: LhrParams -> LhrSets -> Ac -> Double -> PriceSignal -> HouseholdPlan
householdPlan pars sets h eh ps = HouseholdPlan { hpQH = qh, hpQHA = qha }
  where
    -- Supernumerary income Y* = EH - sum PQ*gammam - sum PXAC*gammah.
    ystar = eh
          - sum [ mv (psPQ ps) c * mv (paramGammam pars) (c, h) | c <- setCdm sets ]
          - sum [ v * mv (paramGammah pars) (a, c, h)
                | ((a, c), v) <- M.toList (psPXAC ps) ]

    -- HMDEM: PQ*QH = PQ*gammam + betam*Y*  =>  QH = gammam + betam*Y*/PQ.
    qh = M.fromList
        [ (c, gammam_ch + betam_ch * ystar / pq_c)
        | c <- setC sets
        , let betam_ch = mv (paramBetam pars) (c, h)
        , betam_ch /= 0
        , let gammam_ch = mv (paramGammam pars) (c, h)
              pq_c      = mv (psPQ ps) c ]

    -- HADEM: PXAC*QHA = PXAC*gammah + betah*Y*  =>  QHA = gammah + betah*Y*/PXAC.
    qha = M.fromList
        [ ((a, c), gammah_ach + betah_ach * ystar / pxac_ac)
        | a <- setA sets, c <- setC sets
        , let betah_ach = mv (paramBetah pars) (a, c, h)
        , betah_ach /= 0
        , let gammah_ach = mv (paramGammah pars) (a, c, h)
              pxac_ac    = mv (psPXAC ps) (a, c) ]

-- | Commodity @c@ producer/trader.  Takes the /net/ output each activity
-- delivers to @c@ (@theta*QA@ less home use, composed by the wiring) and the
-- price signal; returns the aggregate, the CET domestic/export split, and the
-- Armington import/composite.  Degenerate branches (CET2/ARMINGTON2) apply
-- when @c@ is not in both of a transformation's sets.
commodityPlan :: LhrParams -> LhrSets -> Ac -> M.Map Ac Double -> PriceSignal -> CommodityPlan
commodityPlan pars sets c qxacNet ps = CommodityPlan
    { cpQX = qx, cpQD = qd, cpQE = qe, cpQM = qm, cpQQ = qq }
  where
    isCE = c `elem` setCe sets
    isCD = c `elem` setCd sets
    isCM = c `elem` setCm sets

    -- OUTAGGFN: QX = alphaac * (sum_a deltaac(a,c) QXAC(a,c)^(-RHOAC))^(-1/RHOAC).
    qx = case M.lookup c (paramAlphaac pars) of
        Just alphaac_c ->
            let rhoac_c = mv (paramRhoac pars) c
                agg = sum [ deltaac_ac * spow (mv qxacNet a) (negate rhoac_c)
                          | a <- setA sets
                          , Just deltaac_ac <- [M.lookup (a, c) (paramDeltaac pars)] ]
            in alphaac_c * spow agg (negate (1 / rhoac_c))
        Nothing -> sum (M.elems qxacNet)     -- no aggregation nest

    -- CET (QX -> QD, QE) when c in CE and CD (ESUPPLY gives QE = QD*k), else
    -- CET2 degeneracy QX = QD (+ QE).
    (qd, qe)
        | isCE && isCD =
            let deltat_c = mv (paramDeltat pars) c
                rhot_c   = mv (paramRhot pars) c
                alphat_c = mv (paramAlphat pars) c
                pe_c     = mv (psPE ps) c
                pds_c    = mv (psPDS ps) c
                k     = spow ((pe_c / pds_c) * ((1 - deltat_c) / deltat_c))
                             (1 / (rhot_c - 1))
                denom = alphat_c
                        * spow (deltat_c * spow k rhot_c + (1 - deltat_c))
                               (1 / rhot_c)
                qd'   = qx / denom
            in (qd', qd' * k)
        | isCD      = (qx, 0)
        | otherwise = (0, qx)

    -- Armington (QD -> QM, QQ) when c in CM and CD (COSTMIN gives QM = QD*m),
    -- else ARMINGTON2 degeneracy QQ = QD (+ QM).
    (qm, qq)
        | isCM && isCD =
            let deltaq_c = mv (paramDeltaq pars) c
                rhoq_c   = mv (paramRhoq pars) c
                alphaq_c = mv (paramAlphaq pars) c
                pdd_c    = mv (psPDD ps) c
                pm_c     = mv (psPM ps) c
                m'    = spow ((pdd_c / pm_c) * (deltaq_c / (1 - deltaq_c)))
                             (1 / (1 + rhoq_c))
                qm'   = qd * m'
                qq'   = alphaq_c
                        * spow (deltaq_c * spow qm' (negate rhoq_c)
                                + (1 - deltaq_c) * spow qd (negate rhoq_c))
                               (negate (1 / rhoq_c))
            in (qm', qq')
        | isCD      = (0, qd)                 -- domestic-only: QQ = QD (ARMINGTON2)
        -- Import-only (CM and not CD): QQ = QM is demand-determined (no
        -- domestic anchor to respond off).  The level is the auctioneer's
        -- COMEQUIL residual, not a commodity response (invariant (vii)); this
        -- 0 is a placeholder the step-② snapshot deliberately does not check.
        | otherwise = (0, 0)

-- | Institution @i@ receipt map.  Reads factor income YF and (for
-- cross-references) YI from the 'IncomeSignal', and inter-institution
-- transfers from the 'TransferSignal' (never global-reads the YI vector to
-- build its own transfers).  Returns factor income by source, the receipt
-- total the YIDEF residual checks, and — for a household — the LES budget.
instIncomeMap :: LhrParams -> LhrBase -> LhrSets -> Ac
              -> IncomeSignal -> TransferSignal -> PriceSignal -> InstIncome
instIncomeMap pars base sets i inc trans ps = InstIncome
    { iiYIF    = yif
    , iiYIrecv = yiRecv
    , iiEH     = eh
    }
  where
    yfMap   = isYF inc
    yiMap   = isYI inc
    triiMap = unTransferSignal trans
    exr     = psEXR ps
    cpibar  = baseCpi0 base

    -- YIFDEF: YIF(i,f) = shif(i,f) * ((1 - tf(f))*YF(f) - trnsfr(ROW,f)*EXR).
    yif = M.fromList
        [ (f, shif_if * ((1 - tf_f) * yf_f - trnsfrROWf * exr))
        | f <- setF sets
        , Just shif_if <- [M.lookup (i, f) (paramShif pars)]
        , let tf_f       = mv (paramTf pars) f
              yf_f       = mv yfMap f
              trnsfrROWf = mv (paramTrnsfr pars) (Ac "ROW", f) ]

    -- YIDEF: YI(i) = sum_f YIF(i,f) + sum_ip TRII(i,ip)
    --                + trnsfr(i,GOV)*CPI + trnsfr(i,ROW)*EXR.
    yiRecv = sum (M.elems yif)
           + sum [ mv triiMap (i, ip) | ip <- setInsdng sets ]
           + mv (paramTrnsfr pars) (i, Ac "GOV") * cpibar
           + mv (paramTrnsfr pars) (i, Ac "ROW") * exr

    -- EHDEF: EH(h) = (1 - sum_i shii(i,h)) * (1 - MPS(h)) * (1 - TINS(h)) * YI(h),
    -- reading the YI instrument (not the receipt total) and fixed MPS/TINS.
    eh
        | i `elem` setH sets =
            let sumShii = sum [ mv (paramShii pars) (i', i) | i' <- setInsdng sets ]
                mps_h   = mv (paramMpsbar pars) i
                tins_h  = mv (paramTinsbar pars) i
                yi_h    = mv yiMap i
            in Just ((1 - sumShii) * (1 - mps_h) * (1 - tins_h) * yi_h)
        | otherwise = Nothing
