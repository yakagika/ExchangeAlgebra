{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{- |
  Model — the CGE-Lite model proper for the Hosoe Ch.6 two-good CGE toy
  (Phase 1, general-equilibrium research repo, plan @phase1-cge-reproduction@,
  task 1b). The thin @cge-lite@ executable ("Main" in @CGELite.hs@) and the
  @cge-lite-model-test@ sentinel suite both import this module.

  == Status (1b — stage bodies implemented)

  The four BSP stages post the full Hosoe Ch.6 behavioral system (every agent
  behavior is a /verbatim closed-form transcription/ of one @stdcge.gms@
  equation — the math-correspondence convention "Calibration" already
  follows), and 'solveEquilibrium' drives the whole economy to the benchmark
  equilibrium through the "Solver" core. What remains for task 1c is the
  benchmark-shock comparative statics; task 1d (N-variable household
  population) already has its seam here ('HouseholdId', 'CGEParams',
  'allocation' being household-count-independent).

  == The root problem (how the GAMS square system becomes @z(u) = 0@)

  A pure price-tâtonnement cannot determine the /scale/ of a constant-returns
  firm (zero profit pins prices, demand pins quantities), so the solver's
  unknown vector 'CGEVar' carries __8 coordinates__, one per market-clearing
  /instrument/:

  +----------------------+--------------------------+---------------------------------------+
  | unknown (key)        | value                    | paired residual (same key)            |
  +======================+==========================+=======================================+
  | 'MarketOf' @BRDC@\/@MLKC@ | composite price @pq(i)@  | composite-good imbalance @Qdem - Qphys@ |
  +----------------------+--------------------------+---------------------------------------+
  | 'MarketOf' @BRDD@\/@MLKD@ | domestic price @pd(i)@   | domestic-good imbalance @Dd - Ds@     |
  +----------------------+--------------------------+---------------------------------------+
  | 'MarketOf' @CAP@     | factor price @pf(CAP)@   | factor imbalance @sum_j F - FF@       |
  +----------------------+--------------------------+---------------------------------------+
  | 'Forex'              | exchange rate @epsilon@  | balance of payments (Ext's US$ net)   |
  +----------------------+--------------------------+---------------------------------------+
  | 'ScaleOf' @BRD@\/@MLK@ | output scale @Z(j)\/Z0(j)@ | firm j's profit (ledger cash net)     |
  +----------------------+--------------------------+---------------------------------------+

  @pf(LAB)@ is the numeraire (pinned at 1, GAMS @pf.fx("LAB") = 1@) and the
  LAB market row is dropped by Walras's law. The drop is sound because every
  non-firm agent is budget-exact /by construction/ at any signal vector
  (households, government, investment and Ext each spend exactly their
  notional income), so at any root of the retained 8 rows the value sum of
  all imbalances collapses to @pf(LAB) * z(LAB) = 0@ — see the derivation
  note on 'aggregateZ'. Conversely the benchmark (all prices 1, @Z = Z0@)
  zeroes all 8 rows because the SAM /is/ the benchmark equilibrium (the
  @cge-lite-model-test@ "benchmark-zero" group asserts this row by row), and
  a root of the 8 rows satisfies the full GAMS system: the CES\/CET dual
  identities recover @eqpqs@\/@eqpzd@ from the FOC transcriptions once the
  market and profit rows clear.

  The pairing is also the ABM story: each price rises on its market's excess
  demand, the exchange rate on the trade deficit, and each firm expands on
  positive profit (Marshallian quantity adjustment) — 'Solver.naiveTatonnement'
  over this key set /is/ that decentralized adjustment rule, while the
  default 'Solver.solveRoot' (damped Newton\/Broyden) is the fast auctioneer.

  == Ledger conventions (how @z@ is read off the journal)

  * physical __creation__ posts @Not@ (production, endowment provision,
    import arrival), physical __absorption__ posts @Hat@ (input use, final
    consumption); 'EA.balanceMapBy' nets Not − Hat, so a market row reads
    /supply − demand/ and 'aggregateZ' negates it into excess demand.
  * __cash transfers__ post the standard matched pair (Hat at the payer, Not
    at the payee) valued from the shared 'allocation' — both sides of every
    transfer agree exactly because every agent evaluates the /same/
    allocation. A firm's net Yen cash is then exactly its profit, which is
    why the 'ScaleOf' residual needs no extra bookkeeping.
  * the domestic good is self-traded (the sector firm runs its own Armington
    aggregation), so its cash pair would fall on one entity and cancel; the
    pair is skipped and only the physical legs remain (the imbalance
    @Dd - Ds@ is physical, its value re-enters the profit row only at roots,
    where it is 0).
  * __foreign currency is Ext's @Dollar@-unit cash__: imports credit Ext with
    @pWm*M@ US$ (Not), exports and the @Sf@ capital transfer debit it (Hat),
    so Ext's Dollar net /is/ the balance-of-payments residual ('Forex' row).
    Imported varieties (@BRDF@\/@MLKF@) are supplied perfectly elastically at
    @pm = epsilon*pWm@ — they are posted (creation at Ext, absorption at the
    firm) but form no market row (the two legs net to 0 identically).

  == Trial independence (state-change-and-scaling.md §3.4\/§3.8)

  Unchanged from the skeleton: 'excessDemand' builds a fresh 'World' per
  call, folds the pass's ledger straight into the residual map (one
  'EA.balanceMapBy' pass, the @dec_κ@ 1-pass aggregator), and retains
  nothing; 'settle' is the single commit at converged signals. A dynamic
  extension must pass the period state @s_t@ explicitly — @z(u; s_t)@ stays
  a pure oracle.

  == Fiscal closure (the one simultaneous knot)

  Government demand needs total tax revenue @T@, but the tariff part of @T@
  needs import volumes, which need composite demand, which needs government
  demand. Given prices and scales the loop is /linear in the scalar @T@/, so
  'allocation' resolves it in closed form (@T = A \/ (1 - B)@, see
  @eqTrev@ in the code) — no in-pass iteration, no linear system.

  == API note (0.5.0.0 @de124f7@ cleanup)

  As the skeleton: written against develop post @de124f7@ (C1-C3), @49699e3@
  (Hat\/Not is not credit\/debit) and @5507cb0@; imports
  "ExchangeAlgebra.Journal" and "ExchangeAlgebra.Simulate.Lite" directly,
  never the umbrella.
-}
module Model
    ( -- * Products and entities
      Product (..)
    , allProducts
    , marketProducts
    , domesticOf, importedOf, compositeOf, factorProduct
    , HouseholdId (..)
    , representativeHousehold
    , Entity (..)
    , allEntities
    , firmOf
      -- * Base, value, note
    , CGEBase
    , V
    , CGEEvent (..)
    , Term
    , CGENote
      -- * Parameters and signals
    , Policy (..)
    , calibratedPolicy
    , CGEParams (..)
    , defaultCGEParams
    , Price
    , Prices
    , Scales
    , Signals (..)
    , CGEVar (..)
    , unknowns0
    , signalsOf
    , benchmarkSignals
      -- * The closed-form allocation
    , Allocation (..)
    , allocation
      -- * World and stages
    , World (..)
    , initWorld
    , cgeStages
    , cgeSpec
      -- * Oracle / commit / solve
    , ExcessDemand
    , excessDemand
    , settle
    , aggregateZ
    , aggregateFlow
    , solveEquilibrium
    ) where

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
import           Solver                          (ConvergenceTol (..),
                                                  SentinelLog (..), solveRoot)

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

-- | The six products that /have a market/ (a 'MarketOf' price and a
-- clearing residual): domestic goods, composites, factors. The imported
-- varieties are excluded — Ext supplies them perfectly elastically at the
-- derived price @pm(i) = epsilon * pWm(i)@ (GAMS @eqpm@), so they carry no
-- independent price and no market row.
marketProducts :: [Product]
marketProducts = [BRDD, MLKD, BRDC, MLKC, CAP, LAB]

-- | Map a calibration good (SAM account @BRD@\/@MLK@) to its domestic\/
-- imported\/composite 'Product' variety. The calibration side ("Calibration")
-- indexes by SAM account exactly as the GAMS source does; the ledger side
-- (this module) needs the variety split — these three functions (plus
-- 'factorProduct') are the whole seam between the two indexings, so the
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
-- condition-number sentinel (@state-change-and-scaling.md@ §2\/§3). This toy
-- fixes @'cgeHouseholds' = ['representativeHousehold']@ (N = 1, matching the
-- Hosoe Ch.6 benchmark's single @HOH@ row), but the stages already fan out
-- over an arbitrary household list, splitting the endowment\/consumption\/
-- saving\/tax aggregates equally (an N-replication whose ledger nets are
-- N-invariant — asserted by the model test suite); heterogeneity is a matter
-- of replacing the equal split.
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
-- Each sector firm runs /both/ of its sector's constant-returns activities:
-- the CET production block (GAMS @eqpy@..@eqE@) and the Armington
-- aggregation of its own good (@eqpqs@..@eqD@) — the GAMS model names no
-- separate importer\/aggregator institution, and folding the two activities
-- into one entity keeps the entity set at the SAM's institutions. The
-- firm's profit residual is therefore the /sum/ of both activities' margins;
-- at a root each vanishes separately (see 'aggregateZ').
--
-- Note: unlike the classic @CGE.hs@ example's @Entity@, this type is __not__
-- 'Bounded'\/'Enum' — 'Household' carries an open-ended 'HouseholdId', so
-- there is no finite "last constructor" to enumerate against. Callers that
-- need "every entity" go through 'allEntities' (parametrized by
-- 'CGEParams') instead of @[minBound .. maxBound]@.
data Entity
    = FirmBRD                -- ^ The bread industry (Hosoe Ch.6 @BRDIND@).
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
-- (allEntities params) - 5 == length (cgeHouseholds params)@, i.e. N.
allEntities :: CGEParams -> [Entity]
allEntities params =
    [FirmBRD, FirmMLK, Gov, Inv, Ext]
        ++ Prelude.map Household (cgeHouseholds params)

-- | The sector firm of a calibration good.
firmOf :: C.Account -> Entity
firmOf C.BRD = FirmBRD
firmOf C.MLK = FirmMLK
firmOf a     = error ("firmOf: not a good: " ++ show a)

------------------------------------------------------------------
-- * Base and Note types
------------------------------------------------------------------

-- | The posting base: (account title, product, entity, unit) — the same
-- axis order the classic @CGE.hs@ example's @VEHatBase@ uses, so the two
-- remain visually comparable. Hat\/Not on this base is __not__ credit\/debit
-- (post-'49699e3': the side is the account division combined with Hat\/Not,
-- see 'whichSide' in "ExchangeAlgebra.Algebra.Base") — the stages follow the
-- library-wide convention used throughout "MarketModel" (@Hat@ =
-- dispatch\/decrease, @Not@ = receipt\/increase), specialized here to the
-- creation\/absorption reading documented in the module header.
type CGEBase = HatBase (AccountTitles, Product, Entity, CountUnit)

instance ExBaseClass CGEBase where
    getAccountTitle (_ :< (a, _, _, _))   = a
    setAccountTitle (h :< (_, p, e, u)) a = h :< (a, p, e, u)

-- | The value type. Per the 0.5.0.0 value-type guidance (umbrella Haddock,
-- @de124f7@ C3): 'Prelude.Double' is the fast, low-friction default; the
-- classic @CGE.hs@ example is intentionally frozen on the deprecated
-- @NN.Double@ (SICE), but this is a /new/ file, so it follows the current
-- guidance instead of the frozen example. Signed values matter here: trial
-- signals can make notional quantities (e.g. the fiscal closure's @T@)
-- negative, and the residual fold 'EA.balanceMapBy' needs a signed type.
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
    = EvPlank                 -- ^ The 'Note' class's required blank tag.
    | EvProduction            -- ^ CES value-added + Leontief intermediate-input demand.
    | EvSavingAndInvestment   -- ^ Household\/government\/foreign savings -> investment demand.
    | EvSalesPurchase         -- ^ Trade: imports, tariffs, Armington assembly.
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

-- | The policy (tax) rates the behavioral equations run at — separated from
-- the calibration because comparative statics (task 1c) means exactly
-- "change these rates, keep every calibrated share\/scale parameter at its
-- benchmark value, re-solve". 'calibratedPolicy' is the benchmark point;
-- a shock scenario overrides fields (e.g. tariff abolition:
-- @pol { polTaum = 0 <$ polTaum pol }@).
data Policy = Policy
    { polTauz :: !(M.Map C.Account Double)
      -- ^ Production tax rates @tauz(j)@ (eqTz, eqE, eqDs).
    , polTaum :: !(M.Map C.Account Double)
      -- ^ Import tariff rates @taum(i)@ (eqTm, eqM).
    } deriving (Eq, Show)

-- | The benchmark policy: the rates the SAM itself implies ('C.tauz',
-- 'C.taum').
calibratedPolicy :: C.Calibration -> Policy
calibratedPolicy cal = Policy
    { polTauz = C.tauz (C.calLevels0 cal)
    , polTaum = C.taum (C.calLevels0 cal)
    }

-- | Calibration parameters: the N-parametrized household list (task 1d),
-- the numeraire pin, and the full Hosoe Ch.6 calibration ('C.Calibration':
-- benchmark levels + CES\/CET\/Armington scale and share coefficients,
-- elasticities, tax rates, savings propensities), the Lite counterpart of
-- the classic @CGE.hs@ example's @InitVar@. The calibration is verified
-- against the GAMS ground truth by the @cge-lite-test@ suite; the stage
-- bodies read it through 'wParams'.
data CGEParams = CGEParams
    { cgeHouseholds  :: ![HouseholdId]
      -- ^ The N-parametrized household population (task 1d). A single
      -- 'representativeHousehold' for the current N = 1 toy.
    , cgeNumeraire   :: !Product
      -- ^ The numeraire good, @pf(LAB) = 1@ in the Hosoe Ch.6 benchmark.
    , cgeCalibration :: !C.Calibration
      -- ^ Benchmark levels ('C.calLevels0') + calibrated parameters
      -- ('C.calParams'), straight from the SAM (task 1a).
    , cgePolicy      :: !Policy
      -- ^ The tax rates in force (task 1c): 'calibratedPolicy' at the
      -- benchmark, overridden for comparative-statics scenarios.
    } deriving (Eq, Show)

-- | The default: N = 1, numeraire = 'LAB', the Hosoe Ch.6 'C.calibration'
-- at its own calibrated policy.
defaultCGEParams :: CGEParams
defaultCGEParams = CGEParams
    { cgeHouseholds  = [representativeHousehold]
    , cgeNumeraire   = LAB
    , cgeCalibration = C.calibration
    , cgePolicy      = calibratedPolicy C.calibration
    }

------------------------------------------------------------------
-- * Signals: prices, exchange rate, firm scales
------------------------------------------------------------------

-- | A price, in numeraire units.
type Price = Double

-- | A trial or settled market-price vector — one entry per 'marketProducts'
-- member (including the numeraire), __per-market__, /not/ per-agent
-- (state-change-and-scaling.md §2: keeping the price vector's dimension at
-- @M@ = the market count, @M << N@, is a necessary condition for the
-- auctioneer's iteration count K to stay N-independent; per-agent pricing
-- would make the fixed point O(N)-dimensional and is explicitly ruled out).
-- Imported varieties are absent: their price is derived (@pm = epsilon *
-- pWm@), never searched.
type Prices = M.Map Product Price

-- | Per-sector relative output scale @Z(j)\/Z0(j)@ (benchmark = 1). Keyed by
-- the calibration good, like every sector-indexed object in "Calibration".
-- Kept /relative/ so all solver coordinates sit near 1 at the root (mixing
-- O(1) prices with O(100) raw quantities would skew the finite-difference
-- Jacobian steps for nothing).
type Scales = M.Map C.Account Double

-- | One full signal vector for a BSP pass: what 'initWorld' bakes into the
-- 'World' and every stage reads. This is the model-facing shape of the
-- solver's flat @'M.Map' 'CGEVar' Double@ (see 'signalsOf').
data Signals = Signals
    { sigPrices :: !Prices  -- ^ Market prices (all six 'marketProducts').
    , sigForex  :: !Double  -- ^ The exchange rate @epsilon@ (Yen per US$).
    , sigScales :: !Scales  -- ^ Relative sector output scales @Z\/Z0@.
    } deriving (Eq, Show)

-- | The solver's coordinate key: one constructor per market-clearing
-- instrument, doubling as the residual key (module header table). 'Ord'
-- fixes the flat vector's coordinate order deterministically.
data CGEVar
    = MarketOf !Product   -- ^ Value: that market's price. Residual: its physical imbalance (excess demand).
    | Forex               -- ^ Value: the exchange rate. Residual: the balance of payments (Ext's US$ net).
    | ScaleOf !C.Account  -- ^ Value: the sector's relative output scale. Residual: the sector firm's profit.
    deriving (Eq, Ord, Show)

-- | The initial (and benchmark) unknown vector: all eight free coordinates
-- at 1 — the numeraire ('cgeNumeraire') is /not/ a key (pinned by
-- 'signalsOf'), and the benchmark root is exactly this point (all prices 1,
-- @epsilon = 1@, @Z = Z0@).
unknowns0 :: CGEParams -> M.Map CGEVar Double
unknowns0 params = M.fromList $
       [ (MarketOf p, 1) | p <- marketProducts, p /= cgeNumeraire params ]
    ++ [ (Forex, 1) ]
    ++ [ (ScaleOf j, 1) | j <- C.goods ]

-- | Unflatten a solver vector into 'Signals', pinning the numeraire at 1
-- (GAMS @pf.fx("LAB") = 1@). Missing keys default to 1 (benchmark value),
-- so partial vectors — e.g. a price-only experiment — stay runnable.
signalsOf :: CGEParams -> M.Map CGEVar Double -> Signals
signalsOf params u = Signals
    { sigPrices = M.fromList
        [ (p, if p == cgeNumeraire params
                  then 1
                  else M.findWithDefault 1 (MarketOf p) u)
        | p <- marketProducts ]
    , sigForex  = M.findWithDefault 1 Forex u
    , sigScales = M.fromList [ (j, M.findWithDefault 1 (ScaleOf j) u) | j <- C.goods ]
    }

-- | The benchmark signal vector (every coordinate 1) — the point the whole
-- Phase-1 exercise is about: 'excessDemand' must vanish here.
benchmarkSignals :: CGEParams -> Signals
benchmarkSignals params = signalsOf params (unknowns0 params)

------------------------------------------------------------------
-- * The closed-form allocation (GAMS equations, verbatim)
------------------------------------------------------------------

-- | Every model quantity at a given signal vector — one field per GAMS
-- variable (same names, @a@-prefixed), evaluated in closed form. All four
-- stages post their bookkeeping slices from one shared 'allocation' (each
-- agent evaluates the same pure function of the same snapshot, so both legs
-- of every cross-entity transfer agree exactly); the sentinel suite reads it
-- directly against 'C.Levels0'.
data Allocation = Allocation
    { aPe    :: !(M.Map C.Account Double)  -- ^ @pe(i) = epsilon*pWe(i)@ (eqpe).
    , aPm    :: !(M.Map C.Account Double)  -- ^ @pm(i) = epsilon*pWm(i)@ (eqpm).
    , aPy    :: !(M.Map C.Account Double)  -- ^ @py(j)@ — Cobb-Douglas unit cost, the closed dual of eqpy+eqF.
    , aPz    :: !(M.Map C.Account Double)  -- ^ @pz(j) = ay(j)*py(j) + sum_i ax(i,j)*pq(i)@ (eqpzs).
    , aZ     :: !(M.Map C.Account Double)  -- ^ @Z(j) = scale_j * Z0(j)@ — the 'ScaleOf' signal, made absolute.
    , aY     :: !(M.Map C.Account Double)  -- ^ @Y(j) = ay(j)*Z(j)@ (eqY).
    , aF     :: !(M.Map (C.Account, C.Account) Double)  -- ^ @F(h,j) = beta(h,j)*py(j)*Y(j)/pf(h)@ (eqF).
    , aX     :: !(M.Map (C.Account, C.Account) Double)  -- ^ @X(i,j) = ax(i,j)*Z(j)@ (eqX).
    , aE     :: !(M.Map C.Account Double)  -- ^ @E(i)@ export supply (eqE).
    , aDs    :: !(M.Map C.Account Double)  -- ^ @D(i)@ domestic supply (eqDs).
    , aDd    :: !(M.Map C.Account Double)  -- ^ @D(i)@ domestic demand (eqD).
    , aM     :: !(M.Map C.Account Double)  -- ^ @M(i)@ import demand (eqM).
    , aQdem  :: !(M.Map C.Account Double)  -- ^ @Q(i) = Xp+Xg+Xv+sum_j X(i,j)@ — composite demand (eqpqd read as a demand definition).
    , aQphys :: !(M.Map C.Account Double)  -- ^ @gamma(i)*(deltam*M^eta + deltad*Dd^eta)^(1\/eta)@ — the Armington output physically produced from the FOC inputs (eqpqs read as a technology).
    , aXp    :: !(M.Map C.Account Double)  -- ^ @Xp(i)@ household demand (eqXp).
    , aXg    :: !(M.Map C.Account Double)  -- ^ @Xg(i)@ government demand (eqXg).
    , aXv    :: !(M.Map C.Account Double)  -- ^ @Xv(i)@ investment demand (eqXv).
    , aTz    :: !(M.Map C.Account Double)  -- ^ @Tz(j) = tauz(j)*pz(j)*Z(j)@ (eqTz).
    , aTm    :: !(M.Map C.Account Double)  -- ^ @Tm(i) = taum(i)*pm(i)*M(i)@ (eqTm).
    , aSp    :: !Double                    -- ^ @Sp = ssp*sum_h pf(h)*FF(h)@ (eqSp).
    , aSg    :: !Double                    -- ^ @Sg = ssg*T@ (eqSg).
    , aTd    :: !Double                    -- ^ @Td = taud*sum_h pf(h)*FF(h)@ (eqTd).
    , aT     :: !Double                    -- ^ @T = Td + sum Tz + sum Tm@ — total tax revenue (the fiscal closure scalar).
    , aUU    :: !Double                    -- ^ @UU = prod_i Xp(i)**alpha(i)@ (obj) — reporting only.
    } deriving (Eq, Show)

-- | Evaluate the whole behavioral system at a signal vector — each binding
-- transcribes one @stdcge.gms@ statement (same order as the GAMS equation
-- block where dependencies allow; the fiscal closure is the one reordering,
-- see the module header). Household-count-independent: the stages split the
-- household aggregates by equal shares when posting. Tax rates come from
-- 'cgePolicy' (not the calibration) so comparative-statics scenarios
-- re-solve with every calibrated parameter fixed (task 1c).
allocation :: CGEParams -> Signals -> Allocation
allocation params sig = Allocation
    { aPe = pe', aPm = pm', aPy = py', aPz = pz'
    , aZ = z', aY = y', aF = f', aX = x'
    , aE = e', aDs = ds', aDd = dd', aM = m'
    , aQdem = qdem', aQphys = qphys'
    , aXp = xp', aXg = xg', aXv = xv'
    , aTz = tz', aTm = tm'
    , aSp = sp', aSg = sg', aTd = td', aT = t'
    , aUU = uu'
    }
  where
    cal = cgeCalibration params
    l   = C.calLevels0 cal
    p   = C.calParams  cal
    -- Policy rates (task 1c): tauz/taum in force, not necessarily the SAM's.
    tauz i = polTauz (cgePolicy params) M.! i
    taum i = polTaum (cgePolicy params) M.! i

    overGoods, overFactors :: (C.Account -> Double) -> M.Map C.Account Double
    overGoods   g = M.fromList [(i, g i) | i <- C.goods]
    overFactors g = M.fromList [(h, g h) | h <- C.factors]

    eps  = sigForex sig
    pq i = sigPrices sig M.! compositeOf i
    pd i = sigPrices sig M.! domesticOf i
    pf h = sigPrices sig M.! factorProduct h

    -- eqpe: pe(i) = epsilon*pWe(i);  eqpm: pm(i) = epsilon*pWm(i)
    pe' = overGoods $ \i -> eps * C.pWe l M.! i
    pm' = overGoods $ \i -> eps * C.pWm l M.! i
    -- The Cobb-Douglas unit-cost dual of eqpy + eqF:
    --   py(j) = (1/b(j)) * prod_h (pf(h)/beta(h,j))**beta(h,j)
    -- (substituting eqF into eqpy and solving for py; at py' the factor
    -- choices below satisfy eqpy exactly, so zero profit holds in the
    -- value-added nest identically).
    py' = overGoods $ \j ->
        (1 / C.b p M.! j)
        * product [ (pf h / C.beta p M.! (h, j)) ** (C.beta p M.! (h, j))
                  | h <- C.factors ]
    -- eqpzs: pz(j) = ay(j)*py(j) + sum(i, ax(i,j)*pq(i))
    pz' = overGoods $ \j ->
        C.ay p M.! j * py' M.! j + sum [C.ax p M.! (i, j) * pq i | i <- C.goods]

    -- Z(j) = scale_j * Z0(j) — the Marshallian scale instrument ('ScaleOf').
    z'  = overGoods $ \j -> sigScales sig M.! j * C.z0 l M.! j
    -- eqY: Y(j) = ay(j)*Z(j)
    y'  = overGoods $ \j -> C.ay p M.! j * z' M.! j
    -- eqF: F(h,j) = beta(h,j)*py(j)*Y(j)/pf(h)
    f'  = M.fromList
        [ ((h, j), C.beta p M.! (h, j) * py' M.! j * y' M.! j / pf h)
        | h <- C.factors, j <- C.goods ]
    -- eqX: X(i,j) = ax(i,j)*Z(j)
    x'  = M.fromList
        [ ((i, j), C.ax p M.! (i, j) * z' M.! j) | i <- C.goods, j <- C.goods ]
    -- eqE: E(i) = (theta^phi * xie * (1+tauz)*pz/pe)**(1/(1-phi)) * Z(i)
    e'  = overGoods $ \i ->
        (   C.theta p M.! i ** C.phi p M.! i
          * C.xie p M.! i
          * (1 + tauz i) * pz' M.! i / pe' M.! i
        ) ** (1 / (1 - C.phi p M.! i)) * z' M.! i
    -- eqDs: D(i) = (theta^phi * xid * (1+tauz)*pz/pd)**(1/(1-phi)) * Z(i)
    ds' = overGoods $ \i ->
        (   C.theta p M.! i ** C.phi p M.! i
          * C.xid p M.! i
          * (1 + tauz i) * pz' M.! i / pd i
        ) ** (1 / (1 - C.phi p M.! i)) * z' M.! i
    -- eqTz: Tz(j) = tauz(j)*pz(j)*Z(j)
    tz' = overGoods $ \j -> tauz j * pz' M.! j * z' M.! j

    -- Household block (endowment income is notional — GAMS uses FF, not
    -- factor sales, in eqSp/eqTd/eqXp; that is what makes the household
    -- budget-exact at any signal vector, which the Walras drop relies on).
    incomeF = sum [pf h * C.ff l M.! h | h <- C.factors]
    -- eqSp: Sp = ssp*sum(h, pf(h)*FF(h))
    sp' = C.ssp p * incomeF
    -- eqTd: Td = taud*sum(h, pf(h)*FF(h))
    td' = C.taud p * incomeF
    -- eqXp: Xp(i) = alpha(i)*(sum(h, pf(h)*FF(h)) - Sp - Td)/pq(i)
    xp' = overGoods $ \i -> C.alpha p M.! i * (incomeF - sp' - td') / pq i

    -- Armington FOC coefficients (imports/domestic per unit of composite):
    --   eqM: M(i) = (gamma^eta * deltam * pq/((1+taum)*pm))**(1/(1-eta)) * Q(i)
    --   eqD: D(i) = (gamma^eta * deltad * pq/pd)**(1/(1-eta)) * Q(i)
    kM  = overGoods $ \i ->
        (   C.gamma p M.! i ** C.eta p M.! i
          * C.deltam p M.! i
          * pq i / ((1 + taum i) * pm' M.! i)
        ) ** (1 / (1 - C.eta p M.! i))
    kD  = overGoods $ \i ->
        (   C.gamma p M.! i ** C.eta p M.! i
          * C.deltad p M.! i
          * pq i / pd i
        ) ** (1 / (1 - C.eta p M.! i))

    -- Fiscal closure (module header): T enters Xg (via eqXg with eqSg
    -- substituted) and Xv (via eqXv), which enter Q, which enters Tm, which
    -- enters T — linear in the scalar T, resolved as T = A/(1-B).
    --   Qbase(i) = the T-independent part of Q(i); Qcoef(i) = dQ(i)/dT.
    qbase = overGoods $ \i ->
        xp' M.! i
        + sum [x' M.! (i, j) | j <- C.goods]
        + C.lambda p M.! i * (sp' + eps * C.sf l) / pq i
    qcoef = overGoods $ \i ->
        (C.mu p M.! i * (1 - C.ssg p) + C.lambda p M.! i * C.ssg p) / pq i
    -- eqTrev (the closure): T = Td + sum Tz + sum_i taum*pm*kM*Q(i)
    t'  = ( td' + sum [tz' M.! j | j <- C.goods]
          + sum [taum i * pm' M.! i * kM M.! i * qbase M.! i | i <- C.goods]
          )
        / ( 1 - sum [taum i * pm' M.! i * kM M.! i * qcoef M.! i | i <- C.goods] )
    -- eqSg: Sg = ssg*T
    sg' = C.ssg p * t'
    -- eqXg: Xg(i) = mu(i)*(T - Sg)/pq(i)
    xg' = overGoods $ \i -> C.mu p M.! i * (t' - sg') / pq i
    -- eqXv: Xv(i) = lambda(i)*(Sp + Sg + epsilon*Sf)/pq(i)
    xv' = overGoods $ \i -> C.lambda p M.! i * (sp' + sg' + eps * C.sf l) / pq i
    -- eqpqd read as the demand definition: Q(i) = Xp + Xg + Xv + sum_j X(i,j)
    qdem' = overGoods $ \i ->
        xp' M.! i + xg' M.! i + xv' M.! i + sum [x' M.! (i, j) | j <- C.goods]
    -- eqM / eqD at that composite demand:
    m'  = overGoods $ \i -> kM M.! i * qdem' M.! i
    dd' = overGoods $ \i -> kD M.! i * qdem' M.! i
    -- eqTm: Tm(i) = taum(i)*pm(i)*M(i)
    tm' = overGoods $ \i -> taum i * pm' M.! i * m' M.! i
    -- eqpqs read as the technology: what the FOC input bundle physically
    -- yields. Off a root this differs from Q(i) demanded — that gap is the
    -- composite market row.
    qphys' = overGoods $ \i ->
        C.gamma p M.! i
        * (   C.deltam p M.! i * m' M.! i ** C.eta p M.! i
            + C.deltad p M.! i * dd' M.! i ** C.eta p M.! i
          ) ** (1 / C.eta p M.! i)

    -- obj: UU = prod(i, Xp(i)**alpha(i))
    uu' = product [xp' M.! i ** (C.alpha p M.! i) | i <- C.goods]

------------------------------------------------------------------
-- * World (HKD, product-only)
------------------------------------------------------------------

-- | The product-only HKD world for one 'runLite' pass. Both auxiliary
-- fields ('wParams', 'wSignals') are declared 'carry': they are the /inputs/
-- to a single BSP pass (baked in at construction by 'initWorld') and never
-- change during that pass — the classic tâtonnement-inside-a-stage design is
-- exactly what Option A avoids (state-change-and-scaling.md §0\/§5): signal
-- adjustment happens /between/ separate 'runLite' calls, driven by the
-- outer 'solveEquilibrium' loop, never by a stage writing 'wSignals'
-- mid-pass.
data World f = World
    { wLedger  :: HK f (Journal CGENote V CGEBase)
      -- ^ The single ledger field (per 'SimSpec's Haddock: exactly one
      -- @Journal@ field per world is the supported shape).
    , wParams  :: HK f CGEParams
      -- ^ Calibration parameters for this pass (carried, read-only).
    , wSignals :: HK f Signals
      -- ^ This pass's trial\/settled signals (carried, read-only — see the
      -- trial-independence contract on 'excessDemand').
    } deriving Generic

-- | Build the initial ('InitT') world for one pass at the given signals.
initWorld :: CGEParams -> Signals -> World InitT
initWorld params sig = World
    { wLedger  = carry mempty
    , wParams  = carry params
    , wSignals = carry sig
    }

------------------------------------------------------------------
-- * Stages (task 1b — the four bookkeeping events)
------------------------------------------------------------------
--
-- Each stage is a pure function of the same snapshot; every agent
-- re-evaluates 'allocation' (cheap closed-form arithmetic at two goods —
-- if a large-N variant ever makes the recomputation measurable, memoizing
-- the allocation into a carried world field is the seam). Posting sign
-- conventions are documented in the module header; every posting names the
-- GAMS object it carries.

-- | Production-side agents: one per sector (the firm's production block)
-- plus one per household (the endowment provision — a household-side event,
-- but bookkeeping-wise part of the factor-hire transaction cluster).
data ProductionAgent
    = ProdSector !C.Account       -- ^ The sector firm's production block.
    | ProdEndowment !HouseholdId  -- ^ One household's factor endowment provision.

-- | CES (Cobb-Douglas) value added + Leontief intermediate inputs + CET
-- split + production tax, per sector; factor endowment provision, per
-- household.
productionStage :: CGEParams -> Stage World Term CGENote V CGEBase
productionStage params =
    stageOf EvProduction agents $ \w _t _g agent ->
        let ps  = wParams w
            sig = wSignals w
            a   = allocation ps sig
            n   = fromIntegral (length (cgeHouseholds ps))
            pqOf i = sigPrices sig M.! compositeOf i
            pfOf h = sigPrices sig M.! factorProduct h
        in case agent of
            ProdEndowment hh -> mconcat
                -- Endowment provision (the supply side of eqpf): household
                -- hh's equal share of FF(h), created into the factor market.
                [ (C.ff (C.calLevels0 (cgeCalibration ps)) M.! h / n)
                      .@ Not :< (Products, factorProduct h, Household hh, Amount)
                | h <- C.factors ]
            ProdSector j -> mconcat $
                let fj = firmOf j in
                -- Factor hire (eqF): absorb F(h,j), pay pf(h)*F(h,j) to the
                -- households (equal shares).
                [ mconcat $
                    [ (aF a M.! (h, j)) .@ Hat :< (Products, factorProduct h, fj, Amount)
                    , (pfOf h * aF a M.! (h, j)) .@ Hat :< (Cash, factorProduct h, fj, Yen)
                    ]
                    ++ [ (pfOf h * aF a M.! (h, j) / n)
                             .@ Not :< (Cash, factorProduct h, Household hh, Yen)
                       | hh <- cgeHouseholds ps ]
                | h <- C.factors ]
                ++
                -- Intermediate inputs (eqX): absorb X(i,j) of composite i,
                -- pay pq(i)*X(i,j) to sector i's firm (the composite seller).
                [ mconcat
                    [ (aX a M.! (i, j)) .@ Hat :< (Products, compositeOf i, fj, Amount)
                    , (pqOf i * aX a M.! (i, j)) .@ Hat :< (Cash, compositeOf i, fj, Yen)
                    , (pqOf i * aX a M.! (i, j)) .@ Not :< (Cash, compositeOf i, firmOf i, Yen)
                    ]
                | i <- C.goods ]
                ++
                -- CET output (eqE/eqDs): create the domestic supply D(j) and
                -- sell the exports E(j) — the exported units leave the
                -- economy at creation (no domestic product leg), while Ext
                -- pays pWe(j)*E(j) in US$ (Hat on its Dollar cash) and the
                -- firm books pe(j)*E(j) in Yen.
                [ (aDs a M.! j) .@ Not :< (Products, domesticOf j, fj, Amount)
                , (aPe a M.! j * aE a M.! j) .@ Not :< (Cash, domesticOf j, fj, Yen)
                , (C.pWe (C.calLevels0 (cgeCalibration ps)) M.! j * aE a M.! j)
                      .@ Hat :< (Cash, domesticOf j, Ext, Dollar)
                -- Production tax (eqTz): Tz(j) to the government.
                , (aTz a M.! j) .@ Hat :< (Cash, domesticOf j, fj, Yen)
                , (aTz a M.! j) .@ Not :< (Cash, domesticOf j, Gov, Yen)
                ]
  where
    agents = Prelude.map ProdSector C.goods
                 ++ Prelude.map ProdEndowment (cgeHouseholds params)

-- | Savings and direct tax routed to the investment account, and the
-- investment demand it finances: Sp\/Sg\/Td\/@epsilon*Sf@\/Xv. A single
-- aggregate agent — every posting is an institution-level transfer.
savingAndInvestmentStage :: CGEParams -> Stage World Term CGENote V CGEBase
savingAndInvestmentStage _params =
    stageOf EvSavingAndInvestment [()] $ \w _t _g () ->
        let ps  = wParams w
            sig = wSignals w
            a   = allocation ps sig
            n   = fromIntegral (length (cgeHouseholds ps))
            pqOf i = sigPrices sig M.! compositeOf i
        in mconcat $
            -- Direct tax (eqTd) and private saving (eqSp), equal household
            -- shares.
            [ mconcat
                [ (aTd a / n) .@ Hat :< (Cash, ProductWild, Household hh, Yen)
                , (aSp a / n) .@ Hat :< (Cash, ProductWild, Household hh, Yen)
                ]
            | hh <- cgeHouseholds ps ]
            ++
            [ (aTd a) .@ Not :< (Cash, ProductWild, Gov, Yen)
            , (aSp a) .@ Not :< (Cash, ProductWild, Inv, Yen)
            -- Government saving (eqSg).
            , (aSg a) .@ Hat :< (Cash, ProductWild, Gov, Yen)
            , (aSg a) .@ Not :< (Cash, ProductWild, Inv, Yen)
            -- Foreign saving (the Sf capital inflow): Ext provides Sf US$,
            -- the investment account books epsilon*Sf Yen.
            , (C.sf (C.calLevels0 (cgeCalibration ps)))
                  .@ Hat :< (Cash, ProductWild, Ext, Dollar)
            , (sigForex sig * C.sf (C.calLevels0 (cgeCalibration ps)))
                  .@ Not :< (Cash, ProductWild, Inv, Yen)
            ]
            ++
            -- Investment demand (eqXv): absorb Xv(i), pay the sector firm.
            [ mconcat
                [ (aXv a M.! i) .@ Hat :< (Products, compositeOf i, Inv, Amount)
                , (pqOf i * aXv a M.! i) .@ Hat :< (Cash, compositeOf i, Inv, Yen)
                , (pqOf i * aXv a M.! i) .@ Not :< (Cash, compositeOf i, firmOf i, Yen)
                ]
            | i <- C.goods ]

-- | Trade and Armington assembly, per sector: import the FOC bundle M(i)
-- (paying Ext and the tariff), absorb the domestic FOC bundle D(i), create
-- the composite the bundle physically yields.
salesPurchaseStage :: CGEParams -> Stage World Term CGENote V CGEBase
salesPurchaseStage _params =
    stageOf EvSalesPurchase C.goods $ \w _t _g j ->
        let ps  = wParams w
            sig = wSignals w
            a   = allocation ps sig
            fj  = firmOf j
        in mconcat
            -- Imports (eqM): Ext creates M(j) of the imported variety
            -- (perfectly elastic supply), the firm absorbs it, pays
            -- pm(j)*M(j) — which Ext converts to pWm(j)*M(j) US$ — plus the
            -- tariff Tm(j) (eqTm) to the government.
            [ (aM a M.! j) .@ Not :< (Products, importedOf j, Ext, Amount)
            , (aM a M.! j) .@ Hat :< (Products, importedOf j, fj, Amount)
            , (aPm a M.! j * aM a M.! j) .@ Hat :< (Cash, importedOf j, fj, Yen)
            , (C.pWm (C.calLevels0 (cgeCalibration ps)) M.! j * aM a M.! j)
                  .@ Not :< (Cash, importedOf j, Ext, Dollar)
            , (aTm a M.! j) .@ Hat :< (Cash, importedOf j, fj, Yen)
            , (aTm a M.! j) .@ Not :< (Cash, importedOf j, Gov, Yen)
            -- Domestic input (eqD): absorb Dd(j). Self-traded (the firm buys
            -- from its own CET output), so the cash pair would cancel on one
            -- entity and is skipped — module header, ledger conventions.
            , (aDd a M.! j) .@ Hat :< (Products, domesticOf j, fj, Amount)
            -- Armington assembly (eqpqs as technology): create what the
            -- input bundle physically yields.
            , (aQphys a M.! j) .@ Not :< (Products, compositeOf j, fj, Amount)
            ]

-- | Final-demand agents: one per household plus the government.
data ConsumptionAgent
    = ConsHousehold !HouseholdId  -- ^ One household's consumption (its 'aXp' share).
    | ConsGovernment              -- ^ Government consumption ('aXg').

-- | Household (eqXp) and government (eqXg) final demand: absorb the
-- composite, pay the sector firm.
consumptionStage :: CGEParams -> Stage World Term CGENote V CGEBase
consumptionStage params =
    stageOf EvConsumption agents $ \w _t _g agent ->
        let ps  = wParams w
            sig = wSignals w
            a   = allocation ps sig
            n   = fromIntegral (length (cgeHouseholds ps))
            pqOf i = sigPrices sig M.! compositeOf i
            buyBlock buyer qty = mconcat
                [ mconcat
                    [ (qty i) .@ Hat :< (Products, compositeOf i, buyer, Amount)
                    , (pqOf i * qty i) .@ Hat :< (Cash, compositeOf i, buyer, Yen)
                    , (pqOf i * qty i) .@ Not :< (Cash, compositeOf i, firmOf i, Yen)
                    ]
                | i <- C.goods ]
        in case agent of
            ConsHousehold hh -> buyBlock (Household hh) (\i -> aXp a M.! i / n)
            ConsGovernment   -> buyBlock Gov            (\i -> aXg a M.! i)
  where
    agents = ConsGovernment : Prelude.map ConsHousehold (cgeHouseholds params)

-- | The four stages, in declared (= execution) order. Matches the relative
-- order of the surviving events in the classic @CGE.hs@ example's
-- @EventName@ list. There is no read dependency between them (each posts
-- from the same shared 'allocation'), so the order is bookkeeping taxonomy,
-- not a computational pipeline.
cgeStages :: CGEParams -> [Stage World Term CGENote V CGEBase]
cgeStages params =
    [ productionStage params
    , savingAndInvestmentStage params
    , salesPurchaseStage params
    , consumptionStage params
    ]

-- | The 'SimSpec' for one pass: a single static term @(1, 1)@ (the Hosoe
-- Ch.6 benchmark has no time dimension), the four 'cgeStages', sequential
-- (no agent-level parallelism needed at N = 1).
cgeSpec :: CGEParams -> SimSpec World Term CGENote V CGEBase
cgeSpec params = mkSimSpec (1, 1) cgeSeed wLedger (cgeStages params)
  where
    -- No stage draws randomness (the model is deterministic given signals),
    -- so the seed value is inert; kept as a named constant rather than a
    -- magic literal at the call site.
    cgeSeed :: Int
    cgeSeed = 0

------------------------------------------------------------------
-- * excessDemand / settle (trial-independence contract)
------------------------------------------------------------------
--
-- state-change-and-scaling.md §3.4/§3.8/§6: the auctioneer (Option A, a
-- 'runLite'-external loop) must evaluate candidate signals through a function
-- that (a) never durably mutates state a later trial or the final commit
-- could observe, and (b) is followed by exactly one real commit at the
-- converged signals. That contract is split into two functions here so it is
-- enforced by which one a caller can even reach for "the real ledger":
-- 'excessDemand' can only ever hand back a residual 'M.Map', never a
-- 'Journal'.

-- | The residual vector, keyed like the unknown vector ('CGEVar', module
-- header table).
type ExcessDemand = M.Map CGEVar V

-- | Evaluate the residuals @z(u)@ at a candidate signal vector.
--
-- === Trial-independence contract (state-change-and-scaling.md §3.4/§3.8)
--
-- This is a /pure/, /trial/ evaluation, meant to be called many times by
-- the auctioneer loop ('solveEquilibrium') while it searches for a
-- market-clearing signal vector. Concretely:
--
--   * 'initWorld' builds a brand-new 'World' from @(params, signals)@ on
--     every call — no mutable cell is shared between calls, so evaluating
--     @z@ at two different signal vectors cannot interfere (this is what
--     makes finite-difference Jacobian columns, or independent line-search
--     trial points, safe to evaluate without extra bookkeeping).
--   * 'runLite' materializes this pass's ledger internally (that is how any
--     BSP pass commits its stages' messages — Option A intentionally leaves
--     that mechanism untouched); what §3.4 asks this /caller/ to avoid is
--     retaining that ledger. 'aggregateZ' folds it to the residual map in
--     one @EA.balanceMapBy@ pass and nothing else escapes the 'runLite'
--     continuation — so distinct trials never keep more than one ledger
--     alive at a time (the property finite-difference evaluation actually
--     needs).
--   * a future /dynamic/ (multi-period, DSGE-style) extension must pass the
--     current-period state @s_t@ this function reads as an explicit,
--     immutable argument — @z(u; s_t)@ as a pure oracle — rather than
--     reading it live off a field that later terms mutate. This static
--     (@T = 1@) toy has no @s_t@ yet, so the seam is documented here, not
--     typed; a dynamic 'excessDemand' would gain an extra parameter, not
--     lose its purity.
--
-- 'settle' is the one exception to "never keep the ledger" — see there.
excessDemand :: CGEParams -> Signals -> ExcessDemand
excessDemand params sig =
    runLite (cgeSpec params) (initWorld params sig) (aggregateZ . wLedger)

-- | Commit the ledger for one term at a /settled/ (already converged, or at
-- least final) signal vector.
--
-- Unlike 'excessDemand', this call's ledger __is__ meant to be kept — it is
-- the single "1 回 commit" the trial-independence contract (see
-- 'excessDemand') reserves for the converged signals. The auctioneer
-- ('solveEquilibrium') must call this /at most once/
-- per term, only after its search has converged (or been given up
-- on) — never per trial.
settle :: CGEParams -> Signals -> Journal CGENote V CGEBase
settle params sig =
    runLite (cgeSpec params) (initWorld params sig) wLedger

-- | Fold a pass's ledger into the residual vector in one @EA.balanceMapBy@
-- pass (state-change-and-scaling.md §3.3: the @dec_κ@ 1-pass aggregator,
-- /not/ one wildcard projection per key). Three bucket families
-- (module header table):
--
--   * @'MarketOf' p@ — net physical flow of a market product. The fold's
--     Not − Hat orientation reads /supply − demand/ under the
--     creation\/absorption convention, so the row is negated into excess
--     demand (price should rise when positive).
--   * 'Forex' — Ext's net US$ cash: @pWm*M − pWe*E − Sf@, already
--     demand-minus-supply for foreign currency (the rate should rise when
--     positive).
--   * @'ScaleOf' j@ — the sector firm's net Yen cash = its profit (the
--     firm should expand when positive).
--
-- === Why dropping the LAB row is sound (the Walras argument)
--
-- Households, government, investment and Ext are /budget-exact by
-- construction/ at any signal vector ('allocation' spends exactly the
-- notional incomes eqSp\/eqTd\/eqXp\/eqXg\/eqXv\/eqepsilon define), so
-- summing every entity's cash identity: at any root of the retained rows,
-- the two firms' cash (the 'ScaleOf' rows) is 0, Ext's US$ net (the 'Forex'
-- row) is 0, every retained market row is 0 — and the composite row's
-- vanishing forces @pq(i)@ to equal the Armington unit cost (CES duality),
-- which zeroes the aggregation margin inside the firm rows, leaving the
-- production margin zero too. The remaining term of the economy-wide value
-- identity is @pf(LAB) * z(LAB)@ with @pf(LAB) = 1@, hence @z(LAB) = 0@:
-- the LAB market clears at any root of the other eight rows, and carrying
-- its row would only make the square system rank-deficient.
--
-- (The imported varieties' rows net to 0 /identically/ — creation at Ext
-- always equals absorption at the firm — so they are dropped from the key
-- map outright; 'aggregateFlow' still reports them for the test suite.)
aggregateZ :: Journal CGENote V CGEBase -> ExcessDemand
aggregateZ ledger =
    M.mapWithKey orient (EA.balanceMapBy zKey (EJ.toAlg ledger))
  where
    orient (MarketOf _) v = negate v  -- supply − demand -> excess demand
    orient _            v = v

    zKey :: BasePart CGEBase -> Maybe CGEVar
    zKey (Products, p, _, Amount)
        | p `Prelude.elem` marketProducts = Just (MarketOf p)
    zKey (Cash, _, Ext,     Dollar)       = Just Forex
    zKey (Cash, _, FirmBRD, Yen)          = Just (ScaleOf C.BRD)
    zKey (Cash, _, FirmMLK, Yen)          = Just (ScaleOf C.MLK)
    zKey _                                = Nothing

-- | Net physical-quantity flow per product (every product, including the
-- imported varieties and the numeraire), aggregated in one
-- @EA.balanceMapBy@ pass. Not part of the solve — 'aggregateZ' is the
-- oracle's fold — but the natural ledger read-out for reporting and for the
-- sentinel suite (imported-variety rows ≡ 0, LAB row 0 at roots).
aggregateFlow :: Journal CGENote V CGEBase -> M.Map Product V
aggregateFlow ledger = EA.balanceMapBy productKey (EJ.toAlg ledger)
  where
    productKey :: BasePart CGEBase -> Maybe Product
    productKey (Products, p, _, Amount) | p /= ProductWild = Just p
    productKey _                                           = Nothing

------------------------------------------------------------------
-- * Solver (Option A auctioneer over the 8-coordinate instrument vector)
------------------------------------------------------------------

-- | The auctioneer's outer loop (Option A — a 'runLite'-external loop, fixed
-- in state-change-and-scaling.md §5): search for the signal vector at which
-- 'excessDemand' vanishes. The numerical core lives in "Solver" (damped
-- Newton\/Broyden + line search per §6, with 'Solver.naiveTatonnement' kept
-- alongside for comparison), verified standalone against artificial oracles
-- with known roots by the @cge-lite-solver-test@ suite; this wrapper owns
-- only the CGE-specific bits:
--
--   * __numeraire pin__: 'signalsOf' holds @pf(LAB) = 1@; the LAB market
--     never enters the unknown or residual keys (Walras — see 'aggregateZ').
--   * __lower bounds__: trial coordinates are clamped at @1e-6@ inside the
--     oracle closure, mirroring the GAMS @*.lo = 0.00001@ guards — the CES\/
--     CET forms are only defined on positive prices\/scales, and the clamp
--     keeps a wayward line-search probe from feeding them a NaN. (The clamp
--     flattens the oracle only /outside/ the economically meaningful
--     region; the residual-norm backtracking never accepts a step into a
--     flat that does not reduce @||z||@.)
--   * __oracle = 'excessDemand'__: each solver trial is one full BSP pass
--     at the trial signals, so 'slIterations' /is/ the K of the K(N)
--     sentinel, measured in BSP passes (state-change-and-scaling.md §2\/§3;
--     task 1d cross-cutting requirement (a): "K is N-independent" is a
--     claim to measure, not assume).
--
-- 'settle' at the returned signals ('signalsOf' of the returned vector)
-- remains the caller's one commit.
solveEquilibrium :: CGEParams -> M.Map CGEVar Double -> ConvergenceTol
                 -> (M.Map CGEVar Double, SentinelLog)
solveEquilibrium params u0 tol = solveRoot oracle u0 tol
  where
    oracle u = excessDemand params (signalsOf params (M.map (max 1e-6) u))
