{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{- |
  CGELite — a @Simulate.Lite@ skeleton for the Hosoe Ch.6 two-good CGE toy
  (Phase 1, general-equilibrium research repo, plan @phase1-cge-reproduction@).

  == Status (R1 — skeleton only)

  This is a __rebuild__ of a skeleton that was written on 2026-06-16 but lost
  when its worktree was pruned before it was committed (see the plan's
  2026-07-02 progress note). It reproduces the same landing point: an HKD
  world, the four bookkeeping-event BSP stages carried over from the classic
  "CGE" example's @EventName@ list (content-free), 'runLite' wiring that
  actually builds and runs, and the 'excessDemand'\/'settle' split mandated by
  the design doc below. __No solver is implemented__ — 'solveEquilibrium' is a
  typed stub that errors when called (R2 scope). Calibration (SAM -> full
  parameter set) __is done__ (task 1a, 2026-07-02): 'CGEParams' carries the
  full Hosoe Ch.6 calibration via 'cgeCalibration' (see "Calibration" and its
  sentinel suite @cge-lite-test@); the stages don't consume it yet (R2).

  == Design of record

  * general-equilibrium repo @docs/cge-scaffold-analysis.md@ — benchmark
    (Hosoe Ch.6, @GAMS/results.csv@) and the event mapping this module's
    stages are named after.
  * general-equilibrium repo @docs/state-change-and-scaling.md@ (esp. §2/§3
    for the K(N)\/condition-number sentinel and the trial-independence
    contract, §5/§6 for the auctioneer-as-outer-loop decision and the
    'excessDemand'\/'settle' split).
  * general-equilibrium repo @plan/in-progress/phase1-cge-reproduction.md@
    (task 1b for this skeleton, task 1d for the N-parametrization type-design
    requirement threaded through 'Entity'\/'HouseholdId'\/'CGEParams' below).

  == API note (0.5.0.0 @de124f7@ cleanup)

  Written against @haskell-exchange-algebra@ develop post
  @de124f7@ (0.5.0.0 API cleanup C1-C3), @49699e3@ (decR\/decL Haddock
  direction fix; Hat\/Not is /not/ credit\/debit — see 'Entity'\/'CGEBase'
  below, which never rely on that equation) and @5507cb0@ (norm\/projWithBase
  RULES removal — irrelevant here, this module never mixes a @HatNot@
  wildcard query with a netted norm). 'runLite', 'stageOf', @EA.balanceMapBy@
  are unchanged since 31e49fe; the umbrella "ExchangeAlgebra" module no
  longer re-exports "ExchangeAlgebra.Simulate" (C1), so this file imports
  "ExchangeAlgebra.Journal" and "ExchangeAlgebra.Simulate.Lite" directly
  (never the umbrella) — the same policy the C1 changelog documents the
  bundled examples as already following.
-}
module Main where

import           GHC.Generics                    (Generic)
import           Data.Hashable                   (Hashable (..))
import qualified Data.Map.Strict                 as M

import           ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Journal         as EJ
import qualified ExchangeAlgebra.Algebra         as EA
import           ExchangeAlgebra.Simulate.Lite
                     ( HK, InitT
                     , carry
                     , Stage, stageOf
                     , SimSpec, mkSimSpec
                     , runLite )

import qualified Calibration                     as C

------------------------------------------------------------------
-- * Products (fixed — the Hosoe Ch.6 two-good structure)
------------------------------------------------------------------

-- | The eight Hosoe Ch.6 product varieties (bread\/milk, domestic\/foreign\/
-- composite, plus the two primary factors), following 'ExchangeAlgebra''s
-- "wildcard-last" 'Element' convention (the existing classic @CGE.hs@ example
-- uses the same list and the same trick: @'allProducts' = ['fstProduct' ..
-- 'lastProduct']@ excludes the trailing wildcard for free because it is the
-- 'Enum'\/'Bounded' /last/ constructor).
--
-- Unlike 'Entity' (see below), this set is __not__ part of the N-variable
-- scope of GE plan task 1d — task 1d is about the /household population/,
-- not the sector count; the Hosoe toy stays fixed at two goods.
data Product
    = BRDD   -- ^ Bread, domestic-produced.
    | MLKD   -- ^ Milk, domestic-produced.
    | BRDF   -- ^ Bread, imported (foreign).
    | MLKF   -- ^ Milk, imported (foreign).
    | BRDC   -- ^ Bread, Armington composite (domestic + imported).
    | MLKC   -- ^ Milk, Armington composite.
    | CAP    -- ^ Capital (primary factor).
    | LAB    -- ^ Labor (primary factor); Hosoe Ch.6 numeraire, @pf(LAB) = 1@.
    | ProductWild -- ^ Wildcard; never posted, only used in projection queries.
    deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance Hashable Product

instance Element Product where
    wildcard = ProductWild

instance BaseClass Product where

fstProduct, lastProduct :: Product
fstProduct = BRDD
lastProduct = LAB

-- | All eight real products (excludes 'ProductWild').
allProducts :: [Product]
allProducts = [fstProduct .. lastProduct]

-- | Map a calibration good (SAM account @BRD@\/@MLK@) to its domestic\/
-- imported\/composite 'Product' variety. The calibration side ("Calibration")
-- indexes by SAM account exactly as the GAMS source does; the ledger side
-- (this module) needs the variety split — these three functions (plus
-- 'factorProduct') are the whole seam between the two indexings, so R2's
-- stage bodies never hand-translate accounts inline.
domesticOf, importedOf, compositeOf :: C.Account -> Product
domesticOf  C.BRD = BRDD
domesticOf  C.MLK = MLKD
domesticOf  a     = error ("domesticOf: not a good: " ++ show a)
importedOf  C.BRD = BRDF
importedOf  C.MLK = MLKF
importedOf  a     = error ("importedOf: not a good: " ++ show a)
compositeOf C.BRD = BRDC
compositeOf C.MLK = MLKC
compositeOf a     = error ("compositeOf: not a good: " ++ show a)

-- | Map a calibration factor (SAM account @CAP@\/@LAB@) to its 'Product'.
factorProduct :: C.Account -> Product
factorProduct C.CAP = CAP
factorProduct C.LAB = LAB
factorProduct a     = error ("factorProduct: not a factor: " ++ show a)

------------------------------------------------------------------
-- * Entities — N-parametrized households (GE plan task 1d)
------------------------------------------------------------------

-- | A household identifier, kept as a plain wrapped 'Int' rather than a
-- hard-coded sum-type constant, so the household /population size/ is a
-- runtime parameter of 'CGEParams' and never baked into the 'Entity' type.
--
-- GE plan task 1d ("N-可変 toy 経済生成") needs to flex N (household
-- replication, possibly with heterogeneity) to populate the K(N)\/
-- condition-number sentinel (@state-change-and-scaling.md@ §2\/§3: the
-- auctioneer's iteration count K must be measured /as a function of N/, and
-- there is currently no way to vary N at all). This R1 toy fixes
-- @'cgeHouseholds' = ['representativeHousehold']@ (N = 1, matching the Hosoe
-- Ch.6 benchmark's single @HOH@ row), but every downstream type — 'Entity',
-- the four stages, 'CGEParams' — already accepts an arbitrary household
-- list, so growing to N > 1 is wiring more 'HouseholdId's through
-- 'CGEParams', not a type-level refactor.
newtype HouseholdId = HouseholdId Int
    deriving (Eq, Ord, Show)

instance Hashable HouseholdId where
    hashWithSalt s (HouseholdId i) = hashWithSalt s i

-- | The toy's single representative household (N = 1). See 'HouseholdId'.
representativeHousehold :: HouseholdId
representativeHousehold = HouseholdId 0

-- | Transacting entities: the two Hosoe Ch.6 sector firms, the N-parametrized
-- household population ('HouseholdId', task 1d), and the three aggregate
-- final-demand accounts (government \/ investment \/ rest-of-world).
-- 'EntityWild' is the projection wildcard, never posted.
--
-- Note: unlike the classic @CGE.hs@ example's @Entity@, this type is __not__
-- 'Bounded'\/'Enum' — 'Household' carries an open-ended 'HouseholdId', so
-- there is no finite "last constructor" to enumerate against. Callers that
-- need "every entity" go through 'allEntities' (parametrized by
-- 'CGEParams') instead of @[minBound .. maxBound]@.
data Entity
    = FirmBRD               -- ^ The bread industry (Hosoe Ch.6 @BRDIND@).
    | FirmMLK                -- ^ The milk industry (Hosoe Ch.6 @MLKIND@).
    | Household !HouseholdId -- ^ One household in the N-parametrized population.
    | Gov                    -- ^ Government (Hosoe Ch.6 @GOV@).
    | Inv                    -- ^ Investment (Hosoe Ch.6 @INV@).
    | Ext                    -- ^ Rest of world / exports (Hosoe Ch.6 @EXT@).
    | EntityWild             -- ^ Wildcard; never posted.
    deriving (Eq, Show, Ord, Generic)

instance Hashable Entity

instance Element Entity where
    wildcard = EntityWild

instance BaseClass Entity where

-- | Every entity in the model for a given parametrization: the four fixed
-- aggregate accounts plus one entry per 'cgeHouseholds' member. @length
-- (allEntities params) - 4 == length (cgeHouseholds params)@, i.e. N.
allEntities :: CGEParams -> [Entity]
allEntities params =
    [FirmBRD, FirmMLK, Gov, Inv, Ext] ++ Prelude.map Household (cgeHouseholds params)

------------------------------------------------------------------
-- * Base and Note types
------------------------------------------------------------------

-- | The posting base: (account title, product, entity, unit) — the same
-- axis order the classic @CGE.hs@ example's @VEHatBase@ uses, so the two
-- remain visually comparable. Hat\/Not on this base is __not__ credit\/debit
-- (post-'49699e3': the side is the account division combined with Hat\/Not,
-- see 'whichSide' in "ExchangeAlgebra.Algebra.Base") — a stage that posts
-- goods movements should follow the library-wide convention used throughout
-- "MarketModel" (@Hat@ = dispatch\/decrease, @Not@ = receipt\/increase for a
-- @Products@-titled base), not a credit\/debit gloss.
type CGEBase = HatBase (AccountTitles, Product, Entity, CountUnit)

instance ExBaseClass CGEBase where
    getAccountTitle (_ :< (a, _, _, _))   = a
    setAccountTitle (h :< (_, p, e, u)) a = h :< (a, p, e, u)

-- | The value type. Per the 0.5.0.0 value-type guidance (umbrella Haddock,
-- @de124f7@ C3): 'Prelude.Double' is the fast, low-friction default; the
-- classic @CGE.hs@ example is intentionally frozen on the deprecated
-- @NN.Double@ (SICE), but this is a /new/ file, so it follows the current
-- guidance instead of the frozen example.
type V = Double

-- | The BSP stage tag. Four constructors survive from the classic @CGE.hs@
-- example's ten-event @EventName@ (see @cge-scaffold-analysis.md@ §2): the
-- two price-determination events (@PriceDetermination@,
-- @ElementPriceDetermination@) are dropped because price adjustment is now
-- the /outer/ auctioneer loop (Option A, not a BSP stage — see
-- 'excessDemand'\/'settle'), and @ToAmount@\/@ToPrice@\/@Order@ were either
-- classic-engine unit-conversion plumbing or ripple-effect-only bookkeeping
-- that @cge-scaffold-analysis.md@ §0 already marked for disposal. What
-- remains is exactly the four genuine within-period bookkeeping events, run
-- in this order every term.
data CGEEvent
    = EvPlank                -- ^ The 'Note' class's required blank tag.
    | EvProduction            -- ^ CES value-added + Leontief intermediate-input demand.
    | EvSavingAndInvestment   -- ^ Household\/government\/foreign savings -> investment demand.
    | EvSalesPurchase         -- ^ Market transactions cleared at this pass's price vector.
    | EvConsumption           -- ^ Household\/government final demand.
    deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance Hashable CGEEvent

instance Note CGEEvent where
    plank = EvPlank

-- | The simulation term. A plain 'Int' (as in "MarketModel"); the Hosoe Ch.6
-- benchmark is static, so 'cgeSpec' always runs the single term @(1, 1)@.
type Term = Int

-- | A ledger note = (event, term). The '(,)' 'Note' instance is already
-- provided generically by "ExchangeAlgebra.Journal" for any @(Note a, Note
-- b)@ pair, so no instance declaration is needed here.
type CGENote = (CGEEvent, Term)

------------------------------------------------------------------
-- * Calibration parameters (task 1a — full Hosoe Ch.6 set)
------------------------------------------------------------------

-- | Calibration parameters: the N-parametrized household list (task 1d),
-- the numeraire pin, and — since task 1a — the full Hosoe Ch.6 calibration
-- ('C.Calibration': benchmark levels + CES\/CET\/Armington scale and share
-- coefficients, elasticities, tax rates, savings propensities), the Lite
-- counterpart of the classic @CGE.hs@ example's @InitVar@. The calibration
-- is verified against the GAMS ground truth by the @cge-lite-test@ suite;
-- R2's stage bodies read it through 'wParams'.
data CGEParams = CGEParams
    { cgeHouseholds  :: ![HouseholdId]
      -- ^ The N-parametrized household population (task 1d). A single
      -- 'representativeHousehold' for the current N = 1 toy.
    , cgeNumeraire   :: !Product
      -- ^ The numeraire good, @pf(LAB) = 1@ in the Hosoe Ch.6 benchmark.
    , cgeCalibration :: !C.Calibration
      -- ^ Benchmark levels ('C.calLevels0') + calibrated parameters
      -- ('C.calParams'), straight from the SAM (task 1a).
    } deriving (Eq, Show)

-- | The default: N = 1, numeraire = 'LAB', the Hosoe Ch.6 'C.calibration'.
defaultCGEParams :: CGEParams
defaultCGEParams = CGEParams
    { cgeHouseholds  = [representativeHousehold]
    , cgeNumeraire   = LAB
    , cgeCalibration = C.calibration
    }

------------------------------------------------------------------
-- * Prices and excess demand
------------------------------------------------------------------

-- | A price, in numeraire units.
type Price = Double

-- | A trial or settled price vector — __per-market__ (one entry per
-- 'Product'), /not/ per-agent (state-change-and-scaling.md §2: keeping the
-- price vector's dimension at @M@ = the product count, @M << N@, is a
-- necessary condition for the auctioneer's iteration count K to stay
-- N-independent; per-agent pricing would make the fixed point O(N)-dimensional
-- and is explicitly ruled out).
type Prices = M.Map Product Price

-- | Per-product net excess demand @z(p)@. Positive = excess demand (price
-- should rise under tâtonnement); negative = excess supply.
type ExcessDemand = M.Map Product V

------------------------------------------------------------------
-- * World (HKD, product-only)
------------------------------------------------------------------

-- | The product-only HKD world for one 'runLite' pass. Both auxiliary
-- fields ('wParams', 'wPrices') are declared 'carry': they are the /inputs/
-- to a single BSP pass (baked in at construction by 'initWorld') and never
-- change during that pass — the classic tâtonnement-inside-a-stage design is
-- exactly what Option A avoids (state-change-and-scaling.md §0\/§5): price
-- adjustment happens /between/ separate 'runLite' calls, driven by the (not
-- yet implemented) outer 'solveEquilibrium' loop, never by a stage writing
-- 'wPrices' mid-pass.
data World f = World
    { wLedger :: HK f (Journal CGENote V CGEBase)
      -- ^ The single ledger field (per 'SimSpec's Haddock: exactly one
      -- @Journal@ field per world is the supported shape).
    , wParams :: HK f CGEParams
      -- ^ Calibration parameters for this pass (carried, read-only).
    , wPrices :: HK f Prices
      -- ^ This pass's trial\/settled price vector (carried, read-only — see
      -- the trial-independence contract on 'excessDemand').
    } deriving Generic

-- | Build the initial ('InitT') world for one pass at the given prices.
initWorld :: CGEParams -> Prices -> World InitT
initWorld params prices = World
    { wLedger = carry mempty
    , wParams = carry params
    , wPrices = carry prices
    }

------------------------------------------------------------------
-- * Stages (content-free — R1 skeleton scope)
------------------------------------------------------------------

-- | CES value-added + Leontief intermediate-input demand
-- (cge-scaffold-analysis.md §3.1). Content-free in R1: the state space and
-- the 'CGEParams'\/'Prices' plumbing it will eventually read are already
-- wired through 'World'\/'stRun''s snapshot argument; only the economic body
-- is missing.
productionStage :: Stage World Term CGENote V CGEBase
productionStage = stageOf EvProduction [()] $ \_w _t _g () -> mempty

-- | Household\/government\/foreign savings routed to investment demand.
savingAndInvestmentStage :: Stage World Term CGENote V CGEBase
savingAndInvestmentStage = stageOf EvSavingAndInvestment [()] $ \_w _t _g () -> mempty

-- | Market transactions cleared at this pass's 'wPrices'. This is the stage
-- that, once implemented, both 'excessDemand' (trial prices) and 'settle'
-- (converged prices) drive — the /only/ difference between the two callers
-- is which price vector was baked into the 'World' by 'initWorld', not the
-- stage list.
salesPurchaseStage :: Stage World Term CGENote V CGEBase
salesPurchaseStage = stageOf EvSalesPurchase [()] $ \_w _t _g () -> mempty

-- | Household\/government final demand (Cobb-Douglas utility maximization in
-- the Hosoe Ch.6 benchmark).
consumptionStage :: Stage World Term CGENote V CGEBase
consumptionStage = stageOf EvConsumption [()] $ \_w _t _g () -> mempty

-- | The four stages, in declared (= execution) order. Matches the relative
-- order of the surviving events in the classic @CGE.hs@ example's
-- @EventName@ list.
cgeStages :: [Stage World Term CGENote V CGEBase]
cgeStages = [productionStage, savingAndInvestmentStage, salesPurchaseStage, consumptionStage]

-- | The 'SimSpec' for one pass: a single static term @(1, 1)@ (the Hosoe
-- Ch.6 benchmark has no time dimension), the four 'cgeStages', sequential
-- (no agent-level parallelism needed at N = 1).
cgeSpec :: CGEParams -> SimSpec World Term CGENote V CGEBase
cgeSpec _params = mkSimSpec (1, 1) cgeSeed wLedger cgeStages
  where
    -- No stage draws randomness (the model is deterministic given prices),
    -- so the seed value is inert; kept as a named constant rather than a
    -- magic literal at the call site.
    cgeSeed :: Int
    cgeSeed = 0

------------------------------------------------------------------
-- * excessDemand / settle (trial-independence contract)
------------------------------------------------------------------
--
-- state-change-and-scaling.md §3.4/§3.8/§6: the auctioneer (Option A, a
-- 'runLite'-external loop) must evaluate candidate prices through a function
-- that (a) never durably mutates state a later trial or the final commit
-- could observe, and (b) is followed by exactly one real commit at the
-- converged price. That contract is split into two functions here so it is
-- enforced by which one a caller can even reach for "the real ledger":
-- 'excessDemand' can only ever hand back an 'ExcessDemand' 'M.Map', never a
-- 'Journal'.

-- | Evaluate the excess demand @z(p)@ at a candidate price vector.
--
-- === Trial-independence contract (state-change-and-scaling.md §3.4/§3.8)
--
-- This is a /pure/, /trial/ evaluation, meant to be called many times by the
-- (not yet implemented) auctioneer loop ('solveEquilibrium') while it
-- searches for a market-clearing price. Concretely:
--
--   * 'initWorld' builds a brand-new 'World' from @(params, prices)@ on
--     every call — no mutable cell is shared between calls, so evaluating
--     @z@ at two different price vectors cannot interfere with each other
--     (this is what makes finite-difference Jacobian columns, or independent
--     line-search trial points, safe to evaluate without extra bookkeeping).
--   * 'runLite' materializes this pass's ledger internally (that is how any
--     BSP pass commits its stages' messages — Option A intentionally leaves
--     that mechanism untouched); what §3.4 asks this /caller/ to avoid is
--     retaining that ledger. 'aggregateFlow' folds it to 'ExcessDemand' in
--     one @EA.balanceMapBy@ pass and nothing else escapes the 'runLite'
--     continuation — so distinct trials never keep more than one ledger
--     alive at a time (the property finite-difference evaluation actually
--     needs).
--   * a future /dynamic/ (multi-period, DSGE-style) extension must pass the
--     current-period state @s_t@ this function reads as an explicit,
--     immutable argument — @z(p; s_t)@ as a pure oracle — rather than
--     reading it live off a field that later terms mutate. This R1 static
--     (@T = 1@) toy has no @s_t@ yet, so the seam is documented here, not
--     typed; a dynamic 'excessDemand' would gain an extra parameter, not
--     lose its purity.
--
-- 'settle' is the one exception to "never keep the ledger" — see there.
excessDemand :: CGEParams -> Prices -> ExcessDemand
excessDemand params prices =
    runLite (cgeSpec params) (initWorld params prices) (aggregateFlow . wLedger)

-- | Commit the ledger for one term at a /settled/ (already converged, or at
-- least final) price vector.
--
-- Unlike 'excessDemand', this call's ledger __is__ meant to be kept — it is
-- the single "1 回 commit" the trial-independence contract (see
-- 'excessDemand') reserves for the converged price. The (not yet
-- implemented) auctioneer ('solveEquilibrium') must call this /at most once/
-- per term, only after its price search has converged (or been given up
-- on) — never per trial.
settle :: CGEParams -> Prices -> Journal CGENote V CGEBase
settle params prices =
    runLite (cgeSpec params) (initWorld params prices) wLedger

-- | Net physical-quantity flow per product, aggregated over a whole pass's
-- ledger in one @EA.balanceMapBy@ fold (state-change-and-scaling.md §3.3:
-- the @dec_κ@ 1-pass aggregator, /not/ one wildcard projection per product —
-- that would reintroduce the O(N^2) cost the design doc explicitly rules
-- out). Follows the "Hat = dispatch, Not = receipt" convention used
-- throughout "MarketModel" for @Products@-titled bases (Hat\/Not is not
-- credit\/debit — see 'CGEBase').
--
-- Always 'M.empty' in R1: the four stages are still content-free, so no
-- pass ever posts a @Products@-titled 'CGEBase'. Once R2's stage bodies post
-- real demand\/supply, this is the function 'excessDemand' folds through —
-- its /sign/ convention (this function currently reports net /receipts/,
-- not signed excess /demand/) becomes an R2 decision once real postings
-- exist to pin it down.
aggregateFlow :: Journal CGENote V CGEBase -> M.Map Product V
aggregateFlow ledger = EA.balanceMapBy productKey (EJ.toAlg ledger)
  where
    productKey :: BasePart CGEBase -> Maybe Product
    productKey (Products, p, _, Amount) | p /= ProductWild = Just p
    productKey _                                           = Nothing

------------------------------------------------------------------
-- * Sentinel instrumentation (R3 stub)
------------------------------------------------------------------

-- | Sentinel instrumentation for the auctioneer's outer loop: the running
-- iteration count K and a condition-number proxy of the Jacobian\/Broyden
-- update, so a future 'solveEquilibrium' can populate the K(N)\/
-- condition-number sentinel (state-change-and-scaling.md §2/§3;
-- phase1-cge-reproduction.md task 1d cross-cutting requirement (a): "K is
-- N-independent" is a claim to measure, not assume). Deliberately /not/ a
-- 'World' field — K spans many 'excessDemand' trials across one
-- 'solveEquilibrium' call, whereas a 'World' is rebuilt fresh (and discarded)
-- every single trial (see 'excessDemand'), so the counter belongs to the
-- outer loop's own state, threaded through 'solveEquilibrium' directly.
--
-- Both fields are stub placeholders: real measurement is R3 scope, gated on
-- R2's solver existing to drive them.
data SentinelLog = SentinelLog
    { slIterations     :: !Int
      -- ^ K: number of 'excessDemand' evaluations performed by the current
      -- 'solveEquilibrium' call. Always @0@ until R2 wires the iteration.
    , slConditionProxy :: !(Maybe Double)
      -- ^ A condition-number proxy for the last Jacobian\/Broyden update.
      -- 'Nothing' until R3 measures one.
    } deriving (Eq, Show)

-- | The zero sentinel: no iterations run, no condition number measured.
emptySentinelLog :: SentinelLog
emptySentinelLog = SentinelLog { slIterations = 0, slConditionProxy = Nothing }

------------------------------------------------------------------
-- * Solver (R2 — NOT IMPLEMENTED)
------------------------------------------------------------------

-- | Convergence criteria for 'solveEquilibrium'. Type-only for now (R2
-- decides how @tolMaxIter@ interacts with damping\/line-search backtracking).
data ConvergenceTol = ConvergenceTol
    { tolNorm    :: !Double  -- ^ Stop when @norm (excessDemand ...) < tolNorm@.
    , tolMaxIter :: !Int     -- ^ Give up after this many outer iterations.
    } deriving (Eq, Show)

-- | The auctioneer's outer loop (Option A — a 'runLite'-external loop, fixed
-- in state-change-and-scaling.md §5): search for a price vector at which
-- 'excessDemand' is (approximately) zero, then 'settle' exactly once at that
-- price.
--
-- __R2, not implemented.__ The default solver is damped Newton\/Broyden with
-- line search (§6) — /not/ naive tâtonnement, because this CGE's demand
-- system is not known to satisfy gross substitutes, so naive tâtonnement's
-- global convergence cannot be assumed (§3 codex caveat). A naive-tâtonnement
-- variant is kept for comparison\/stress-testing per the design doc, but it
-- is equally unimplemented — this stub does not give it a separate name
-- because R1 scope is "the type the solver will have", not "every solver
-- variant's type". R2 implements the iteration body (each step: one
-- 'excessDemand' trial, a damped Newton\/Broyden price update, a line
-- search); R3 wires 'SentinelLog' (K and the condition-number proxy) from
-- inside that loop; R1 stops here.
solveEquilibrium :: CGEParams -> Prices -> ConvergenceTol -> (Prices, SentinelLog)
solveEquilibrium _params _p0 _tol =
    error "R2: damped Newton/Broyden + line search — not yet implemented (see general-equilibrium docs/state-change-and-scaling.md §6)"

------------------------------------------------------------------
-- * Main (build/run verification)
------------------------------------------------------------------

-- | Runs the R1 skeleton end to end at a fixed (unconverged) guess price
-- vector — all prices at 1.0, matching the Hosoe Ch.6 numeraire convention
-- (@pf(LAB) = 1@) and its benchmark solution (all prices converge to 1.0).
-- Exercises 'excessDemand' and 'settle' (both trivial while the stages are
-- content-free) so that "stack build && stack run" is a real end-to-end
-- check of the 'runLite' wiring, not just a type-checks-in-isolation claim.
-- 'solveEquilibrium' is deliberately /not/ called (it 'error's — R2).
main :: IO ()
main = do
    let params  = defaultCGEParams
        prices0 :: Prices
        prices0 = M.fromList [ (p, 1.0) | p <- allProducts ]
        z       = excessDemand params prices0
        ledger  = settle params prices0
    let cal = cgeCalibration params
        cp  = C.calParams cal
    putStrLn "=== CGE-Lite skeleton (R1) ==="
    putStrLn ("households (N)     : " ++ show (length (cgeHouseholds params)))
    putStrLn ("numeraire          : " ++ show (cgeNumeraire params))
    putStrLn "--- calibration (task 1a; full check = cge-lite-test) ---"
    putStrLn ("alpha              : " ++ show (M.toList (C.alpha cp)))
    putStrLn ("b                  : " ++ show (M.toList (C.b cp)))
    putStrLn ("ssp / ssg / taud   : " ++ show (C.ssp cp, C.ssg cp, C.taud cp))
    putStrLn ("benchmark UU       : " ++ show (C.benchmarkUtility cal))
    putStrLn ("trial prices       : " ++ show (M.toList prices0))
    putStrLn ("excess demand z(p) : " ++ show (M.toList z))
    putStrLn ("settled ledger norm: " ++ show (norm ledger))
    putStrLn "solveEquilibrium (R2 damped Newton/Broyden) not yet wired; see general-equilibrium docs/state-change-and-scaling.md section 6."
