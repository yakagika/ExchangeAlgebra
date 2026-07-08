{-# LANGUAGE DeriveGeneric #-}

{- |
  LhrLedger -- Lofgren-Harris-Robinson standard CGE, layer 3: the EA
  double-entry journal.  This is where the AS-ABM's /accounting state/ lives
  (RQ1): the forward-pass state is posted flow-by-flow as balanced Not\/Hat
  pairs, and the reduced residuals are read straight off the ledger with one
  @balanceMapBy@ pass — exactly the Hosoe @Model.aggregateZ@ pattern, with LHR
  accounts.

  == The residuals fall out of the fold (no separate solve)

  Under the creation\/absorption reading (@Not@ = supply\/receipt, @Hat@ =
  demand\/payment), @balanceMapBy@ nets @Not − Hat@ per key:

    * physical @PComposite c@  →  COMEQUIL(c)  (QQ supply − total demand)
    * physical @PFactor f@     →  FACEQUIL(f)  (endowment − hire)
    * @EActivity a@ Yen cash   →  ACTPROFIT(a) (revenue − cost; PVADEF gap,
      a true residual because PVA is the CES dual cost — see 'LhrModel')
    * @ERow@ Dollar cash       →  CURACCBAL    (EXR conversion lives here alone)
    * @EHousehold h@ Yen cash  →  YIDEF(h): the household spends its /instrument/
      income YI while receiving realised YIrecv, so its cash imbalance is
      exactly YIrecv − YI (the dual-decision gap, not a spurious budget row)
    * @ESaveInv@ Yen cash      →  SAVINVBAL    (kept as the ex-post Walras check)

  Structural-zero cash (commodity aggregator, government, factor pool) is
  budget-exact by construction and mapped to 'Nothing'.  CPIDEF is the
  numeraire, computed outside the ledger.

  The @cge-lite-lhr-ledger-test@ sentinel asserts these equal the direct
  residuals ('LhrWiring.residuals') at 1e-12 at the base and at perturbed
  instruments (a bookkeeping-bug detector), and that the EXR conversion is
  self-consistent under an EXR perturbation.
-}
module LhrLedger
    ( LhrProduct (..)
    , LhrEntity (..)
    , LBase
    , ledgerResiduals
    ) where

import           Data.Hashable                 (Hashable (..))
import qualified Data.Map.Strict               as M
import           GHC.Generics                  (Generic)

import           ExchangeAlgebra.Algebra
import           ExchangeAlgebra.Algebra.Base
import           ExchangeAlgebra.Algebra.Base.Element

import           LhrCalibration (Ac (..), LhrBase (..), LhrCalibration (..),
                                 LhrParams (..), LhrSets (..))
import           LhrWiring      (Instruments, ResidualKey (..), forwardSolution)

-- | 'Ac' (runtime SAM account) hashes through its string, so the ledger axes
-- can key on it.  Orphan (Ac is defined in "LhrCalibration"); harmless in this
-- example package.
instance Hashable Ac where
    hashWithSalt s (Ac x) = hashWithSalt s x

------------------------------------------------------------------
-- * Ledger axes
------------------------------------------------------------------

-- | Goods axis.  Physical market rows key on the composite good and the
-- factor; cash legs carry 'PMoney' (the fold ignores the product on @Cash@).
data LhrProduct
    = PComposite !Ac   -- ^ composite good QQ (COMEQUIL market)
    | PFactor !Ac      -- ^ primary factor QF\/QFS (FACEQUIL market)
    | PMoney           -- ^ placeholder on cash legs
    deriving (Eq, Ord, Show, Generic)

instance Hashable LhrProduct
instance Element LhrProduct where wildcard = PMoney
instance BaseClass LhrProduct

-- | Transacting entities.
data LhrEntity
    = EActivity !Ac    -- ^ activity (firm): ACTPROFIT cash
    | ECommodity !Ac   -- ^ commodity producer\/trader: structural-zero cash
    | EHousehold !Ac   -- ^ household (INSDNG): YIDEF cash
    | EGov             -- ^ government: structural-zero cash
    | ESaveInv         -- ^ savings-investment account: SAVINVBAL cash
    | ERow             -- ^ rest of world: CURACCBAL (Dollar) cash
    | EFactorMkt !Ac   -- ^ factor pool: structural-zero cash
    | EEntityWild
    deriving (Eq, Ord, Show, Generic)

instance Hashable LhrEntity
instance Element LhrEntity where wildcard = EEntityWild
instance BaseClass LhrEntity

-- | The LHR posting base: (account title, good, entity, unit).
type LBase = HatBase (AccountTitles, LhrProduct, LhrEntity, CountUnit)

------------------------------------------------------------------
-- * Smart constructors (one flow = one balanced pair)
------------------------------------------------------------------

type L = Alg Double LBase

-- | A physical flow: @supplier@ creates @q@ of @p@ (Not), @absorber@ absorbs
-- it (Hat).  Nets to @supply − demand@ on @(Products, p, _, Amount)@.
phys :: Double -> LhrProduct -> LhrEntity -> LhrEntity -> L
phys q p supplier absorber =
       (q :@ (Not :< (Products, p, supplier, Amount)))
    .+ (q :@ (Hat :< (Products, p, absorber, Amount)))

-- | A cash flow in a currency: @payer@ pays @v@ (Hat), @payee@ receives (Not).
cashIn :: CountUnit -> Double -> LhrEntity -> LhrEntity -> L
cashIn cur v payer payee =
       (v :@ (Hat :< (Cash, PMoney, payer, cur)))
    .+ (v :@ (Not :< (Cash, PMoney, payee, cur)))

-- | One payment leg: @e@ pays @v@ in @cur@.
payLeg :: Double -> CountUnit -> LhrEntity -> L
payLeg v cur e = v :@ (Hat :< (Cash, PMoney, e, cur))

-- | One receipt leg: @e@ receives @v@ in @cur@.
recvLeg :: Double -> CountUnit -> LhrEntity -> L
recvLeg v cur e = v :@ (Not :< (Cash, PMoney, e, cur))

-- | A one-sided physical creation (endowment supply) with no absorber leg.
supply :: Double -> LhrProduct -> LhrEntity -> L
supply q p e = q :@ (Not :< (Products, p, e, Amount))

-- | A one-sided physical absorption (final demand) with no supplier leg.
absorb :: Double -> LhrProduct -> LhrEntity -> L
absorb q p e = q :@ (Hat :< (Products, p, e, Amount))

------------------------------------------------------------------
-- * Residual read-out
------------------------------------------------------------------

-- | Read the reduced residuals off the journal in one @balanceMapBy@ pass,
-- oriented to match the @build_system@ sign conventions
-- ('LhrWiring.residuals').  CPIDEF is added separately (numeraire, not a
-- ledger flow).
ledgerResiduals :: LhrCalibration -> Instruments -> M.Map ResidualKey Double
ledgerResiduals cal ins =
    M.insert RCpi cpidef
    $ M.mapWithKey orient
    $ balanceMapBy zKey (ledgerAlg cal ins)
  where
    fwd = forwardSolution cal ins
    g nm ix = M.findWithDefault 0.0 (nm, ix) fwd
    pars = calParams cal
    sets = calSets cal
    acn (Ac s) = s
    cpidef = sum [ M.findWithDefault 0.0 c (paramCwts pars) * g "PQ" [acn c]
                 | c <- setCdm sets ]
           - baseCpi0 (calBase cal)

    -- build_system signs: COMEQUIL = supply−demand, FACEQUIL = demand−supply,
    -- YIDEF = YI−receipts.  The fold gives Not−Hat; flip the two that disagree.
    orient (RFacEquil _) v = negate v
    orient (RYiDef _)    v = negate v
    orient _             v = v

-- | Project a posting base onto the residual it feeds (or 'Nothing' for a
-- structural-zero row).
zKey :: (AccountTitles, LhrProduct, LhrEntity, CountUnit) -> Maybe ResidualKey
zKey (Products, PComposite c, _, Amount) = Just (RComEquil c)
zKey (Products, PFactor f, _, Amount)    = Just (RFacEquil f)
zKey (Cash, _, EActivity a, Yen)         = Just (RActProfit a)
zKey (Cash, _, ERow, Dollar)             = Just RCurAcc
zKey (Cash, _, EHousehold h, Yen)        = Just (RYiDef h)
zKey (Cash, _, ESaveInv, Yen)            = Just RSavInv
zKey _                                   = Nothing

------------------------------------------------------------------
-- * The journal (all SAM flows, double-entry, from the forward state)
------------------------------------------------------------------

ledgerAlg :: LhrCalibration -> Instruments -> L
ledgerAlg cal ins = mconcat $ concat
    [ factorHire, factorIncome, intermediates, activityOutput, activityTaxes
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

    -- (1) factor hire: activity absorbs QF (FACEQUIL demand), pays WF*wfdist*QF;
    --     endowment QFS is supplied to the factor market (FACEQUIL supply).
    factorHire =
        [ supply (M.findWithDefault 0.0 f (baseQfs0 base)) (PFactor f) (EFactorMkt f)
        | f <- fSet ]
        ++
        [ absorb qf (PFactor f) (EActivity a)   -- hire = factor demand only
          .+ cashIn Yen (g "WF" [acn f] * param2 (baseWfdist0 base) f a * qf)
                        (EActivity a) (EFactorMkt f)
        | f <- fSet, a <- aSet, let qf = g "QF" [acn f, acn a], qf /= 0 ]

    -- (2) factor income: YF distributed as YIF to institutions, tf*YF to gov,
    --     trnsfr(ROW,f) abroad.  The factor pool nets to 0.
    factorIncome =
        [ cashIn Yen (g "YIF" [acn i, acn f]) (EFactorMkt f) (instEntity i)
        | i <- insd, f <- fSet, g "YIF" [acn i, acn f] /= 0 ]
        ++
        [ cashIn Yen (mv (paramTf pars) f * g "YF" [acn f]) (EFactorMkt f) EGov
        | f <- fSet ]
        ++
        -- foreign factor income out: pool pays trnsfr*EXR (Yen), ROW gets Dollar
        [ payLeg (trnsfr (Ac "ROW") f * exr) Yen (EFactorMkt f)
          .+ recvLeg (trnsfr (Ac "ROW") f) Dollar ERow
        | f <- fSet, trnsfr (Ac "ROW") f /= 0 ]

    -- (3) intermediates: activity absorbs composite QINT (COMEQUIL demand),
    --     pays PQ*QINT to the commodity.
    intermediates =
        [ absorb qint (PComposite c) (EActivity a)
          .+ cashIn Yen (g "PQ" [acn c] * qint) (EActivity a) (ECommodity c)
        | c <- cSet, a <- aSet, let qint = g "QINT" [acn c, acn a], qint /= 0 ]

    -- (4) activity output: activity sells gross output theta*QA at PXAC to the
    --     commodity aggregator (activity revenue = PA*QA).
    activityOutput =
        [ cashIn Yen (g "PA" [acn a] * g "QA" [acn a]) (ECommodity (outputCommodity a)) (EActivity a)
        | a <- aSet ]

    -- (5) activity taxes: production tax ta*PA*QA and VAT tva*PVA*QVA to gov.
    activityTaxes =
        [ cashIn Yen (mv (paramTa pars) a * g "PA" [acn a] * g "QA" [acn a])
                     (EActivity a) EGov
        | a <- aSet ]
        ++
        [ cashIn Yen (mv (paramTva pars) a * g "PVA" [acn a] * g "QVA" [acn a])
                     (EActivity a) EGov
        | a <- aSet ]

    -- (6) commodity sells composite QQ (COMEQUIL supply) to final demanders;
    --     each demander pays PQ*qty.
    commoditySales =
        [ supply (g "QQ" [acn c]) (PComposite c) (ECommodity c) | c <- cSet ]
        ++ concat
        [ [ absorb (g "QH" [acn c, acn h]) (PComposite c) (EHousehold h)
            .+ cashIn Yen (g "PQ" [acn c] * g "QH" [acn c, acn h]) (EHousehold h) (ECommodity c)
          | h <- hSet, g "QH" [acn c, acn h] /= 0 ]
        | c <- cSet ]
        ++
        [ absorb (g "QG" [acn c]) (PComposite c) EGov
          .+ cashIn Yen (g "PQ" [acn c] * g "QG" [acn c]) EGov (ECommodity c)
        | c <- cSet, g "QG" [acn c] /= 0 ]
        ++
        [ absorb (g "QINV" [acn c]) (PComposite c) ESaveInv
          .+ cashIn Yen (g "PQ" [acn c] * g "QINV" [acn c]) ESaveInv (ECommodity c)
        | c <- cSet, g "QINV" [acn c] /= 0 ]
        ++
        [ absorb (mv (paramQdst pars) c) (PComposite c) ESaveInv
          .+ cashIn Yen (g "PQ" [acn c] * mv (paramQdst pars) c) ESaveInv (ECommodity c)
        | c <- cSet, mv (paramQdst pars) c /= 0 ]
        ++
        [ absorb (g "QT" [acn c]) (PComposite c) (ECommodity c)  -- transport margin use
        | c <- setCt sets, g "QT" [acn c] /= 0 ]

    -- (7) commodity trade (EXR conversion lives HERE, at the ROW Dollar leg):
    --     import — commodity pays cif in Yen, ROW receives cif in Dollar;
    --     export — ROW pays fob in Dollar, commodity receives fob in Yen.
    commodityTrade =
        [ payLeg (mv (basePwm0 base) c * exr * g "QM" [acn c]) Yen (ECommodity c)
          .+ recvLeg (mv (basePwm0 base) c * g "QM" [acn c]) Dollar ERow
        | c <- setCm sets, g "QM" [acn c] /= 0 ]
        ++
        [ payLeg (mv (basePwe0 base) c * g "QE" [acn c]) Dollar ERow
          .+ recvLeg (mv (basePwe0 base) c * exr * g "QE" [acn c]) Yen (ECommodity c)
        | c <- setCe sets, g "QE" [acn c] /= 0 ]

    -- (8) commodity tax + tariff: tq*PQ*QQ, tm*pwm*EXR*QM, te*pwe*EXR*QE to gov.
    commodityTax =
        [ cashIn Yen (mv (paramTq pars) c * g "PQ" [acn c] * g "QQ" [acn c]) (ECommodity c) EGov
        | c <- setCdm sets, mv (paramTq pars) c /= 0 ]
        ++
        [ cashIn Yen (mv (paramTm pars) c * mv (basePwm0 base) c * exr * g "QM" [acn c])
                     (ECommodity c) EGov
        | c <- setCm sets, mv (paramTm pars) c /= 0 ]
        ++
        [ cashIn Yen (mv (paramTe pars) c * mv (basePwe0 base) c * exr * g "QE" [acn c])
                     (ECommodity c) EGov
        | c <- setCe sets, mv (paramTe pars) c /= 0 ]

    -- (9) household income: it spends its INSTRUMENT YI, so posting receipts
    --     (YIF via factorIncome + transfers) against dispositions leaves the
    --     YIrecv − YI gap on household cash.  Transfers in (TRII) here.
    householdIncome =
        [ cashIn Yen (trii i ip) (instEntity ip) (EHousehold i)
        | i <- insdng, ip <- insdng, trii i ip /= 0 ]
        ++
        [ cashIn Yen (trnsfr h (Ac "GOV") * cpi) EGov (EHousehold h)
        | h <- hSet, trnsfr h (Ac "GOV") /= 0 ]
        ++
        -- transfers from ROW to every institution (incl. GOV): ROW pays Dollar,
        -- the institution receives the EXR-converted Yen (single conversion).
        [ recvLeg (trnsfr i (Ac "ROW") * exr) Yen (instEntity i)
          .+ payLeg (trnsfr i (Ac "ROW")) Dollar ERow
        | i <- insd, trnsfr i (Ac "ROW") /= 0 ]

    -- (10) household spending (all on the instrument YI): consumption already
    --      paid in (6); here direct tax, saving, transfers-out.
    householdSpend =
        [ cashIn Yen (g "TINS" [acn h] * g "YI" [acn h]) (EHousehold h) EGov | h <- hSet ]
        ++
        [ cashIn Yen (g "MPS" [acn h] * (1 - g "TINS" [acn h]) * g "YI" [acn h])
                     (EHousehold h) ESaveInv
        | h <- hSet ]

    -- (11) government saving to S-I; (gov receipts/spends already posted).
    govFlows =
        [ cashIn Yen (g "GSAV" []) EGov ESaveInv ]

    -- (12) foreign saving FSAV: ROW provides FSAV (Dollar), S-I books FSAV*EXR.
    saveInvFlows =
        [ (baseFsav0 base * exr :@ (Not :< (Cash, PMoney, ESaveInv, Yen)))
          .+ (baseFsav0 base :@ (Hat :< (Cash, PMoney, ERow, Dollar))) ]

    -- helpers -----------------------------------------------------------------
    mv m k = M.findWithDefault 0.0 k m
    trii i ip = mv (paramShii pars) (i, ip)
              * (1 - mv (paramMpsbar pars) ip) * (1 - mv (paramTinsbar pars) ip)
              * g "YI" [acn ip]
    -- Which commodity an activity's PA revenue is booked against (for the
    -- ECommodity counterpart of activity output).  With one commodity per
    -- activity this is that commodity; the cash counterpart cancels in the
    -- commodity's (structural-zero) balance regardless.
    outputCommodity a = head ([ c | c <- cSet, mv (paramTheta pars) (a, c) /= 0 ] ++ [Ac "COM"])
    instEntity i
        | i `elem` hSet = EHousehold i
        | otherwise     = EGov
