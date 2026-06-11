{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

{- |
  MarketModel — the value-type-polymorphic core of the Phase 5 market-scale
  experiment (proposal §2F, plan E1-E7). It is shared by two thin @Main@s that
  differ only in their value type:

    * @marketEx1.hs@  picks @v = 'MoneyDouble'@  (fast IEEE-754); and
    * @marketEx1d.hs@ picks @v = 'MoneyDecimal'@ (exact base-10).

  This mirrors the SICE @simulateEx2@\/@simulateEx2Fast@ twin (one value-type
  line of difference). The value type is a /type/, so it cannot be switched at
  run time — hence two executables, one model.

  == What the model does (heavy stages, E3; carryover bookkeeping, Phase 5 fix)

  Every term runs four BSP stages (each a pure map from the read-only snapshot
  to messages), in declared order. The model is __self-contained per term__ in
  the classic carry-forward sense (cf. @simulateEx2@'s @finalStockTransfer@): a
  firm's stock at the /start/ of term @t@ is held on a single
  @(Carryover, t)@ note, and the /closing/ stage of term @t@ nets that period
  and rolls the surplus into @(Carryover, t+1)@. This keeps every per-term
  inventory read at @O(\\#firms)@ (indexed by term axis) regardless of how long
  the history is — no full-ledger @balanceMapBy@ sweep — and makes the inventory
  semantics __window-transparent__: @(Carryover, t)@ carries the term value
  @t@, so a @RetainRecent w@ run (w ≥ 1) never evicts the opening stock a term
  needs.

    1. __trade__ — orders flow along the trade network's in-edges. Each firm
       @j@ folds its in-edges @suppliersOf g j@ into a /6-entry/ bilateral
       posting per supplier (the @simulateEx2@\/Gate @purchasePosting@ shape).
       The order amount is @a_{ij} · demand_j@ where @demand_j@ is
       __inventory-connected__: @max(0, target − opening_j)@ and @opening_j@ is
       firm @j@'s stock read from this term's @(Carryover, t)@ note __by an
       indexed per-note projection__ (@O(\\log + \\#firms)@), not a ledger-wide
       sweep. This stage is run per firm via 'stageFor', so 'ParChunk' actually
       fans the work out across firms.

    2. __production__ — each firm consumes @demand_j@ units of its own product
       (a usage-cost line). This is the demand-satisfaction step, kept as
       genuine accounting work, also fanned out per firm via 'stageFor'.

    3. __report__ — a per-firm one-pass aggregation ('EA.postFromNetBy', the
       §2A flagship): the term's net product position is classified by owning
       firm and any net /shortage/ (Hat product) is turned, in one O(m) pass,
       into an explicit replenishment marker.

    4. __closing__ (carryover) — a single aggregate stage that nets this term's
       own-product position per firm (@balanceMapBy@ over this term's entries
       only, indexed by the term axis) and, for each firm with positive net
       stock @v@, posts the matched pair
       @v .\@ Hat :< (Products, j, j, Amount) .| (Closing, t)@ and
       @v .\@ Not :< (Products, j, j, Amount) .| (Carryover, t+1)@. The
       cumulative net is invariant (the pair telescopes), exactly as
       @finalStockTransfer@ does.

  == simple \/ tuned trade stage (E4)

  'tradeStageSimple' writes the trade Σ the natural way (a 6-entry @.\@@ posting
  per in-edge, summed per firm). 'tradeStageTuned' computes the /same/ per-firm
  journal by manually fusing the per-base accumulation into a
  'M.insertWith'-accumulated map rebuilt with 'EA.sigmaFromMap'. With an exact
  value type ('MoneyDecimal') the two are __bit-for-bit identical__ after @bar@
  (asserted in the test suite); the difference is only in evaluation strategy,
  measured by the experiment harness.

  == Determinism

  All randomness is the deterministic @(G, A)@ construction (one 'StdGen',
  'System.Random.split') plus 'ExchangeAlgebra.Simulate.Lite''s per-agent
  'deriveGen'. A 'Sequential' run and a 'ParChunk' run of the same 'MarketParams'
  produce the same ledger (the DET check the harness records).

  The SICE "frozen" examples (simulateEx1\/Ex2\/Ex2Fast) are not touched.
-}

module MarketModel
    ( -- * Model types
      Firm
    , MTag(..)
    , MNote
    , MBase
    , World(..)
      -- * Parameters (from the environment)
    , MarketParams(..)
    , NetKind(..)
    , ParSpec(..)
    , RetainSpec(..)
    , readParams
      -- * (G, A) construction
    , buildNetwork
    , buildCoef
      -- * Trade stages (E4 simple/tuned pair)
    , tradeStageSimple
    , tradeStageTuned
      -- * The other heavy stages
    , productionStage
    , reportStage
    , carryoverStage
      -- * Inventory read-out (indexed, per-firm)
    , openingOf
    , demandOf
    , openingMap
      -- * Spec assembly and run
    , marketSpec
    , RunResult(..)
    , runMarket
    ) where

import           GHC.Generics                  (Generic)
import           Data.Hashable                 (Hashable)
import           Data.Binary                   (Binary)
import           Data.List                     (foldl')
import           Data.Maybe                    (fromMaybe)
import qualified Data.Map.Strict               as M
import           System.Environment            (lookupEnv)
import           System.Random                 (StdGen, mkStdGen, split)
import           Text.Read                     (readMaybe)

import           ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Journal       as EJ
import qualified ExchangeAlgebra.Algebra       as EA
import           ExchangeAlgebra.Simulate.Lite
                     ( HK, InitT
                     , carry
                     , Stage, stage, stageFor
                     , Par(..)
                     , SimSpec, mkSimSpec, specParallel
                     , runLite, runLiteWithPolicy )
import           ExchangeAlgebra.Simulate        (StateTime(..))
import qualified ExchangeAlgebra.Simulate.Policy as Policy
import           ExchangeAlgebra.Simulate.Network
                     ( TradeNetwork, InputCoefficients
                     , completeNetwork, kRegular, erdosRenyi, scaleFree
                     , defaultCoefOptions, randomCoefficients
                     , edgeCount, coefficient, suppliersOf )

------------------------------------------------------------------
-- * Model types
------------------------------------------------------------------

-- | A firm is an integer id.
type Firm  = Int

-- | The /event/ axis of a model note. An ADT (not a @String@) so that a
-- mistyped tag is a __compile error__ rather than a projection that silently
-- matches nothing: every write (@.| (Trade, t)@) and every read
-- (@projWithNote [(Trade, t)]@) names the same constructor, checked by the
-- type-checker. 'PlankTag' is the explicit blank tag ('plank' of the pointed
-- set), kept as its own constructor in the existing library style (cf. the
-- @SimEvent@ note in the test suite).
data MTag = PlankTag | Trade | Production | Report | Closing | Carryover
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable MTag

-- | Needed so a @Journal MNote v b@ is 'Binary' (the spill\/'runLiteWithPolicy'
-- path serialises the ledger); derived structurally from 'Generic'.
instance Binary MTag

instance Note MTag where
    plank = PlankTag

-- | Note = (event tag, term). The term is the last component, so the
-- 'Policy.HasTermAxis' instance for pairs applies (used by 'runLiteWithPolicy').
-- The tuple 'Note' instance gives the 2-axis (event, term) index automatically.
type MNote = (MTag, Int)

-- | A four-axis base @(AccountTitles, owner, counterparty, CountUnit)@ — the
-- same shape the SICE @simulateEx2@ example uses. The /owner/ axis gives every
-- firm its own product- and cash-bases, so per-firm netting is just @bar@ over
-- that firm's bases.
type MBase = HatBase (AccountTitles, Firm, Firm, CountUnit)

instance Element Firm where
    wildcard = -1
instance BaseClass Firm where

instance ExBaseClass MBase where
    getAccountTitle (h :< (a, _, _, _))   = a
    setAccountTitle (h :< (_, o, c, u)) a = h :< (a, o, c, u)

-- | The simulation term is a plain 'Int'. The library ships no 'StateTime'
-- instance for 'Int' (it is example-/test-specific), so the model supplies one
-- here. @initTerm@\/@lastTerm@ are only used by the classic engine and the spill
-- bridge; 'runLiteWithPolicy' takes its term range from the 'SimSpec', so these
-- constants do not constrain a run. (This is the same orphan the SICE test
-- harness declares; MarketModel is never imported alongside it.)
instance StateTime Int where
    initTerm = 1
    lastTerm = maxBound

-- | Product-only HKD world: the accumulating ledger plus the read-only @(G, A)@
-- trade structure carried as plain 'carry' fields (they never change during a
-- run, so no 'Field' rule beyond 'carry' and no per-field instance is needed).
data World v f = World
  { wLedger :: HK f (Journal MNote v MBase)
  , wNet    :: HK f (TradeNetwork Firm)
  , wCoef   :: HK f (InputCoefficients Firm v)
  } deriving Generic

------------------------------------------------------------------
-- * Parameters
------------------------------------------------------------------

-- | Which network generator builds @G@.
data NetKind = NetComplete | NetKRegular | NetErdosRenyi | NetScaleFree
  deriving (Eq, Show)

-- | Agent-level parallelism for the stages.
data ParSpec = ParSeq | ParChunkN !Int
  deriving (Eq, Show)

-- | In-memory retention policy (drives 'runLiteWithPolicy' when not 'RetainAllT'
-- or when a spill path is given).
data RetainSpec = RetainAllT | RetainRecentT !Int
  deriving (Eq, Show)

-- | The full set of runtime parameters, read from the environment by
-- 'readParams'. Defaults reproduce the original skeleton (N=20, T=5).
data MarketParams = MarketParams
  { mpN       :: !Int          -- ^ @EA_N@: number of firms (default 20).
  , mpT       :: !Int          -- ^ @EA_T@: number of terms (default 5).
  , mpK       :: !Int          -- ^ @EA_K@: mean in-degree for k-regular\/scale-free, or the ER p·(N-1) target (default 6).
  , mpNet     :: !NetKind      -- ^ @EA_NET@: complete|kreg|sf|er (default er).
  , mpPar     :: !ParSpec      -- ^ @EA_PAR@: seq|par:<chunk> (default seq).
  , mpRetain  :: !RetainSpec   -- ^ @EA_RETAIN@: all|recent:<w> (default all).
  , mpSpill   :: !(Maybe FilePath) -- ^ @EA_SPILL@: optional binary spill path.
  , mpSeed    :: !Int          -- ^ @EA_SEED@: master seed for (G, A) and stages (default 2025).
  , mpTarget  :: !Double       -- ^ @EA_TARGET@: per-firm inventory target (default 10).
  } deriving (Eq, Show)

-- | Default parameters: the original skeleton's small regime.
defaultParams :: MarketParams
defaultParams = MarketParams
  { mpN = 20, mpT = 5, mpK = 6
  , mpNet = NetErdosRenyi, mpPar = ParSeq
  , mpRetain = RetainAllT, mpSpill = Nothing
  , mpSeed = 2025, mpTarget = 10 }

-- | Read 'MarketParams' from the environment, falling back to 'defaultParams'
-- per-field. A present-but-unparseable variable is a hard 'error' (never
-- silently defaulted), as required by the plan.
readParams :: IO MarketParams
readParams = do
    n   <- envInt    "EA_N"      (mpN  defaultParams)
    t   <- envInt    "EA_T"      (mpT  defaultParams)
    k   <- envInt    "EA_K"      (mpK  defaultParams)
    net <- envNet    "EA_NET"    (mpNet defaultParams)
    par <- envPar    "EA_PAR"    (mpPar defaultParams)
    ret <- envRetain "EA_RETAIN" (mpRetain defaultParams)
    sp  <- lookupEnv "EA_SPILL"
    sd  <- envInt    "EA_SEED"   (mpSeed defaultParams)
    tg  <- envDouble "EA_TARGET" (mpTarget defaultParams)
    pure MarketParams
      { mpN = n, mpT = t, mpK = k, mpNet = net, mpPar = par
      , mpRetain = ret, mpSpill = sp, mpSeed = sd, mpTarget = tg }

-- helpers --------------------------------------------------------

envInt :: String -> Int -> IO Int
envInt key dflt = envWith key dflt $ \s ->
    case readMaybe s of Just i -> i; Nothing -> badEnv key s "an integer"

envDouble :: String -> Double -> IO Double
envDouble key dflt = envWith key dflt $ \s ->
    case readMaybe s of Just d -> d; Nothing -> badEnv key s "a number"

envNet :: String -> NetKind -> IO NetKind
envNet key dflt = envWith key dflt $ \s -> case s of
    "complete" -> NetComplete
    "kreg"     -> NetKRegular
    "er"       -> NetErdosRenyi
    "sf"       -> NetScaleFree
    _          -> badEnv key s "one of complete|kreg|er|sf"

envPar :: String -> ParSpec -> IO ParSpec
envPar key dflt = envWith key dflt $ \s -> case s of
    "seq" -> ParSeq
    _ | Just c <- stripPrefix' "par:" s
      , Just n <- readMaybe c -> ParChunkN n
    _ -> badEnv key s "seq or par:<chunk>"

envRetain :: String -> RetainSpec -> IO RetainSpec
envRetain key dflt = envWith key dflt $ \s -> case s of
    "all" -> RetainAllT
    _ | Just w <- stripPrefix' "recent:" s
      , Just n <- readMaybe w -> RetainRecentT n
    _ -> badEnv key s "all or recent:<window>"

envWith :: String -> a -> (String -> a) -> IO a
envWith key dflt parse = do
    mv <- lookupEnv key
    pure $ case mv of
        Nothing -> dflt
        Just "" -> dflt
        Just s  -> parse s

badEnv :: String -> String -> String -> a
badEnv key s expected =
    error ("MarketModel: environment variable " ++ key ++ "=" ++ show s
            ++ " could not be parsed (expected " ++ expected ++ ")")

stripPrefix' :: String -> String -> Maybe String
stripPrefix' []     ys     = Just ys
stripPrefix' _      []     = Nothing
stripPrefix' (x:xs) (y:ys) | x == y = stripPrefix' xs ys
                           | otherwise = Nothing

------------------------------------------------------------------
-- * (G, A) construction
------------------------------------------------------------------

-- | Build the trade network @G@ deterministically from the parameters and the
-- supplied generator (the @G@-half of a 'System.Random.split').
buildNetwork :: StdGen -> MarketParams -> TradeNetwork Firm
buildNetwork g mp =
    case mpNet mp of
      NetComplete   -> completeNetwork fs
      NetKRegular   -> kRegular   g fs (mpK mp)
      NetScaleFree  -> scaleFree  g fs (max 1 (mpK mp `div` 2))
      NetErdosRenyi -> erdosRenyi g fs p
  where
    fs = [1 .. mpN mp]
    -- mean in-degree k -> p = k / (N-1) for the ER model.
    p  = if mpN mp <= 1 then 0
         else fromIntegral (mpK mp) / fromIntegral (mpN mp - 1)

-- | Build the input coefficients @A@ matching @G@ (the @A@-half of the split).
-- 'randomCoefficients' applies the shrink-only Hawkins–Simon rescale, so every
-- buyer's column sum is strictly @< 1@ (productivity), which keeps the net
-- shortage strictly positive each term.
buildCoef :: (HatVal v) => StdGen -> TradeNetwork Firm -> InputCoefficients Firm v
buildCoef g net = randomCoefficients g defaultCoefOptions net

------------------------------------------------------------------
-- * Inventory read-out (carryover-indexed, per-firm) + demand
------------------------------------------------------------------

-- | Classify a base by the owning firm IFF it is that firm's own product stock
-- @(Products, j, j, _)@. (BasePart strips the Hat/Not side.)
ownerOfProduct :: BasePart MBase -> Maybe Firm
ownerOfProduct bp = case bp of
    (Products, o, c, _) | o == c -> Just o
    _                            -> Nothing

-- | Firm @j@'s /opening/ stock at the start of term @t@: the net value held on
-- the @(Carryover, t)@ note's own-product base. This is an __indexed,
-- per-note read__ — 'EJ.projWithNote' resolves the single note in
-- @O(\\log + \\#firms)@ via the Journal's note index, then 'EA.balanceMapBy'
-- nets only /that note's/ bases (@\\#firms@ entries) — never a full-ledger
-- sweep. Window-transparent: @(Carryover, t)@ has term value @t@, so a
-- @RetainRecent w@ run (w ≥ 1) still has it resident when term @t@ runs.
--
-- Term 1 has no carryover note yet, so every firm reads @0@ (and demand falls
-- back to the full @target@), exactly the original skeleton's behaviour.
openingMap :: forall v. (HatVal v, Real v)
           => Int -> Journal MNote v MBase -> M.Map Firm v
openingMap t ledger =
    EA.balanceMapBy ownerOfProduct
        (EJ.toAlg (EJ.projWithNote [(Carryover, t)] ledger))

-- | Firm @j@'s /opening/ stock at the start of term @t@, read for a __single
-- firm__ without building the all-firm map. This is the per-agent hot path:
-- 'EJ.projWithNote' resolves the one @(Carryover, t)@ note in @O(\\log)@ via
-- the Journal's note index, then 'EA.balanceBy' nets only firm @j@'s own-product
-- base @(Products, j, j, Amount)@ by two /concrete/ (wildcard-free) projections
-- — each a @Map.lookup@ in @O(\\log \\#firms)@ — so the whole read is
-- @O(\\log + \\log \\#firms)@. Calling this once per firm in a stage is
-- @O(N \\log N)@ for the term, /not/ the @O(N^2)@ of rebuilding the all-firm
-- 'openingMap' inside every per-agent lambda.
--
-- Term 1 has no carryover note yet, so every firm reads @0@ (and demand falls
-- back to the full @target@), exactly the original skeleton's behaviour.
openingOf :: forall v. (HatVal v, Real v)
          => Int -> Firm -> Journal MNote v MBase -> v
openingOf t j ledger =
    EA.balanceBy [Not :< (Products, j, j, Amount)]
                 [Hat :< (Products, j, j, Amount)]
                 (EJ.toAlg (EJ.projWithNote [(Carryover, t)] ledger))

-- | Firm @j@'s order-driving demand this term:
-- @demand_j = max(0, target − opening_j)@, read for a __single firm__ via
-- 'openingOf' (indexed per-note + per-base, not a ledger sweep nor an all-firm
-- map). This is what the per-agent trade and production stages call.
demandOf :: forall v. (HatVal v, Real v)
         => Double -> Int -> Firm -> Journal MNote v MBase -> Double
demandOf target t j ledger =
    max 0 (target - realToFrac (openingOf t j ledger :: v))

------------------------------------------------------------------
-- * Trade stages (E4): simple and tuned, exact-equal under MoneyDecimal
------------------------------------------------------------------

-- | The 6-entry bilateral purchase posting for one edge @(i, j)@ at amount
-- @amt@ (supplier @i@, buyer @j@) — the @simulateEx2@\/Gate @purchasePosting@
-- shape, owner-tagged for this model's 4-axis base. Built with the smart
-- constructor '.\@' (non-negativity enforced once).
purchasePosting :: (HatVal v) => v -> Firm -> Firm -> EA.Alg v MBase
purchasePosting amt i j =
       amt .@ Not :< (Products,  j, j, Amount)   -- goods into buyer j's own stock
  .+   amt .@ Hat :< (Cash,      j, j, Yen)       -- buyer j pays cash
  .+   amt .@ Not :< (Purchases, j, j, Yen)       -- buyer j records the purchase
  .+   amt .@ Not :< (Cash,      i, i, Yen)       -- supplier i receives cash
  .+   amt .@ Not :< (Sales,     i, i, Yen)       -- supplier i records the sale
  .+   amt .@ Hat :< (Products,  i, i, Amount)    -- goods leave supplier i's stock

-- | The per-edge order amount @a_{ij} · demand_j@ as a 'Double' (the unit the
-- demand and coefficients live in); the caller converts to @v@. @demand_j@ is
-- the buyer @j@'s single demand value (every in-edge of @j@ shares it), read
-- once per firm by the caller via 'demandOf'.
orderAmount :: (HatVal v, Real v)
            => InputCoefficients Firm v -> Double -> Firm -> Firm -> Double
orderAmount coef d i j =
    let a = realToFrac (fromMaybe 0 (coefficient coef i j)) :: Double
    in a * d

-- | __simple__ trade stage, fanned out __per firm__ ('stageFor' over the firm
-- list, so 'ParChunk' sparks each firm's in-edge fold independently). Buyer @j@
-- folds its in-edges @suppliersOf g j@ into a Σ of 6-entry postings (the
-- Gate\/@simulateEx2@ @purchasePosting@ shape), one posting per supplier @i@.
-- Every edge @(i,j)@ is owned by exactly one buyer @j@, so the per-firm
-- partition posts each edge exactly once (no double counting).
tradeStageSimple :: forall v. (HatVal v, Real v)
                 => [Firm] -> Double -> Stage (World v) Int MNote v MBase
tradeStageSimple fs target = stageFor "trade" fs $ \w t _g j ->
    let net  = wNet w
        coef = wCoef w
        d    = demandOf target t j (wLedger w)    -- buyer j's single demand (indexed read)
        sup  = suppliersOf net j
        one i = let amt = realToFrac (orderAmount coef d i j) :: v
                in if amt <= 0 then mempty else purchasePosting amt i j
        alg = EA.sigma sup one
    in if EA.isZero alg then mempty else alg .| (Trade, t)

-- | __tuned__ trade stage: the /same/ journal (after @bar@), also per-firm
-- token, with firm @j@'s in-edges fused into one base->value map
-- ('M.insertWith') rebuilt with 'EA.sigmaFromMap'. With an exact value type the
-- per-firm journals merge to a result @bar@-identical to 'tradeStageSimple';
-- with FP it is deterministic but may reassociate.
tradeStageTuned :: forall v. (HatVal v, Real v)
                => [Firm] -> Double -> Stage (World v) Int MNote v MBase
tradeStageTuned fs target = stageFor "trade" fs $ \w t _g j ->
    let net  = wNet w
        coef = wCoef w
        d    = demandOf target t j (wLedger w)    -- buyer j's single demand (indexed read)
        sup  = suppliersOf net j
        accum = foldl' step M.empty sup
        step acc i =
            let amt = realToFrac (orderAmount coef d i j) :: v
            in if amt <= 0 then acc
               else foldl' (\m (b, vv) -> M.insertWith (+) b vv m) acc
                      [ (Not :< (Products,  j, j, Amount), amt)
                      , (Hat :< (Cash,      j, j, Yen),    amt)
                      , (Not :< (Purchases, j, j, Yen),    amt)
                      , (Not :< (Cash,      i, i, Yen),    amt)
                      , (Not :< (Sales,     i, i, Yen),    amt)
                      , (Hat :< (Products,  i, i, Amount), amt) ]
        alg = EA.sigmaFromMap accum (\b vv -> vv .@ b)
    in if EA.isZero alg then mempty else alg .| (Trade, t)

------------------------------------------------------------------
-- * Production stage (E3 b): demand-driven consumption of own product
------------------------------------------------------------------

-- | The production / final-demand step. Every firm @j@ consumes @demand_j@ units
-- of its /own/ product to meet this term's demand: @Hat:<(Products, j, j, _)@.
-- Cash flows the other way (a usage cost line), so the posting is a genuine
-- two-entry accounting event per firm, not a busy-loop.
--
-- Pairing this consumption with the trade stage's receipts is what makes the
-- report's shortage __provably positive__: in 'tradeStageSimple'\/'tradeStageTuned'
-- firm @j@ receives only @Σ_i a_{ij} · demand_j@ into its own stock, and the
-- Hawkins–Simon column sum @Σ_i a_{ij} < 1@ guarantees that receipt is strictly
-- below the @demand_j@ consumed here. The net own-product position is therefore a
-- strict /shortage/ @demand_j · (1 − Σ_i a_{ij}) > 0@, which the report turns into
-- a replenishment marker.
productionStage :: forall v. (HatVal v, Real v)
                => [Firm] -> Double -> Stage (World v) Int MNote v MBase
productionStage fs target = stageFor "production" fs $ \w t _g j ->
    let amt  = realToFrac (demandOf target t j (wLedger w)) :: v   -- firm j's own demand (indexed read)
    in if amt <= 0 then mempty
       else ((amt .@ Hat :< (Products,    j, j, Amount))   -- consume own product
          .+ (amt .@ Not :< (SalesCost,   j, j, Yen)))      -- as a usage-cost line
            .| (Production, t)

------------------------------------------------------------------
-- * Report stage (E3 c, §2A flagship): per-firm one-pass aggregation
------------------------------------------------------------------

-- | Per-firm one-pass shortage report. The term's net product position is
-- classified by owning firm with 'EA.postFromNetBy' (bar — explicit in the
-- name): for every firm whose own product nets to a /shortage/
-- (@Hat:<(Products, j, j, _)@) it emits, in one O(m) pass keyed by the owning
-- firm, an explicit replenishment marker for exactly the shortfall.
--
-- This reads only this term's __trade + production flow__ (the
-- @(Trade, t)@\/@(Production, t)@ notes, indexed) — deliberately /excluding/
-- the @(Carryover, t)@ opening note — so the shortage is exactly
-- @consumed − received = demand_j · (1 − Σ_i a_{ij}) > 0@ (Hawkins–Simon), the
-- same quantity the report measured before carryover bookkeeping was added.
reportStage :: forall v. (HatVal v)
            => Stage (World v) Int MNote v MBase
reportStage = stage "report" $ \w t ->
    let ledger   = wLedger w
        -- this term's trade+production flow only (indexed per-note read);
        -- excludes the (Carryover, t) opening stock and the (Closing, t)
        -- roll-out (which is posted in a later stage anyway).
        flow     = EJ.toAlg (EJ.projWithNote [(Trade, t), (Production, t)] ledger)
        shortageK b = case b of
            Hat :< (Products, o, c, _) | o == c -> Just o
            _                                   -> Nothing
        shortage = EA.postFromNetBy shortageK
                     (\j v -> v .@ Not :< (Products, j, j, Amount))
                     flow
    in if EA.isZero shortage then mempty else shortage .| (Report, t)

------------------------------------------------------------------
-- * Closing / carryover stage (Phase 5 fix): per-term self-contained books
------------------------------------------------------------------

-- | The /closing/ stage — the last stage of every term. It nets firm @j@'s own
-- product position over __this term's entries only__ (the @(Carryover, t)@
-- opening note plus the @(Trade, t)@ receipts and @(Production, t)@
-- consumption), read by an __indexed term-axis query__ ('EJ.filterByAxis' on
-- axis 1) so the cost is @O(\\#this-term-entries)@, never history-proportional.
--
-- For every firm whose net closing stock @v@ is strictly positive it posts the
-- matched carry-forward pair (the @finalStockTransfer@ shape):
--
--   * @v .\@ Hat :< (Products, j, j, Amount) .| (Closing, t)@   — close the books
--   * @v .\@ Not :< (Products, j, j, Amount) .| (Carryover, t+1)@ — open next term
--
-- The pair telescopes, so the ledger-wide cumulative own-product net is
-- /unchanged/ by closing; it only re-labels the surplus onto a single
-- next-term note so the next term's @opening_j@ is a single indexed read. Firms
-- with non-positive net stock carry nothing forward (their next opening is 0),
-- which is the steady shortage regime (Hawkins–Simon).
carryoverStage :: forall v. (HatVal v, Real v)
               => Stage (World v) Int MNote v MBase
carryoverStage = stage "closing" $ \w t ->
    let -- this term's entries only (indexed by the term axis = axis 1).
        termAlg = EJ.toAlg (EJ.filterByAxis 1 (NoteAxisKey (t :: Int)) (wLedger w))
        netMap  = EA.balanceMapBy ownerOfProduct termAlg :: M.Map Firm v
        -- carry only strictly-positive closing stock forward.
        perFirm (j, v) =
            if v <= 0 then mempty
            else ((v .@ Hat :< (Products, j, j, Amount)) .| (Closing,   t))
              <> ((v .@ Not :< (Products, j, j, Amount)) .| (Carryover, t + 1))
        out = mconcat [ perFirm kv | kv <- M.toList netMap ]
    in out

------------------------------------------------------------------
-- * Spec assembly and run
------------------------------------------------------------------

-- | The 'SimSpec' for a run. @tradeStageSimple@ vs @tradeStageTuned@ is chosen by
-- the @useTuned@ flag; the other two heavy stages are shared.
marketSpec :: forall v. (HatVal v, Real v)
           => Bool                 -- ^ use the tuned trade stage?
           -> MarketParams
           -> SimSpec (World v) Int MNote v MBase
marketSpec useTuned mp =
    (mkSimSpec (1, mpT mp) (mpSeed mp) wLedger
        [ tradeStage fs (mpTarget mp)
        , productionStage fs (mpTarget mp)
        , reportStage
        , carryoverStage ])
      { specParallel = toPar (mpPar mp) }
  where
    fs         = [1 .. mpN mp]
    tradeStage = if useTuned then tradeStageTuned else tradeStageSimple
    toPar ParSeq        = Sequential
    toPar (ParChunkN c) = ParChunk c

-- | Build the initial world from the parameters (constructs @(G, A)@ from a
-- 'System.Random.split' of the seed, exactly as the original skeleton did).
initWorld :: forall v. (HatVal v) => MarketParams -> World v InitT
initWorld mp =
    let (gG, gA) = split (mkStdGen (mpSeed mp))
        net      = buildNetwork gG mp
        coef     = buildCoef gA net :: InputCoefficients Firm v
    in World { wLedger = carry mempty
             , wNet    = carry net
             , wCoef   = carry coef }

-- | A summary of one run, for the @Main@s to print.
data RunResult = RunResult
  { rrFirms     :: !Int
  , rrEdges     :: !Int
  , rrTerms     :: !Int
  , rrFinalNorm :: !Double      -- ^ norm of the final ledger.
  , rrShortage  :: !Double      -- ^ norm of the final term's report (net shortage).
  } deriving (Eq, Show)

-- | Run the model under the parameters and return a 'RunResult'. When the
-- retention is 'RetainAllT' and there is no spill path, the pure 'runLite' is
-- used; otherwise 'runLiteWithPolicy' (IO) applies the window\/spill policy.
runMarket :: forall v.
             ( HatVal v, Real v, Binary v )
          => Bool -> MarketParams -> IO RunResult
runMarket useTuned mp = do
    let spec  = marketSpec useTuned mp
        w0    = initWorld mp :: World v InitT
        -- the network is a deterministic function of the params, so re-derive
        -- the (cheap) edge count for the summary instead of cracking the Field.
        net   = buildNetwork (fst (split (mkStdGen (mpSeed mp)))) mp
        nF    = mpN mp
        eC    = edgeCount net
        tT    = mpT mp
        project final =
            ( realToFrac (norm (wLedger final))
            , reportNorm (wLedger final) tT )
    (fNorm, sNorm) <- case mpRetain mp of
        RetainAllT | mpSpill mp == Nothing ->
            pure (runLite spec w0 project)
        _ -> runLiteWithPolicy (policyOf mp) spec w0 project
    pure RunResult { rrFirms = nF, rrEdges = eC, rrTerms = tT
                   , rrFinalNorm = fNorm, rrShortage = sNorm }
  where
    -- the net shortage norm recorded on the final term's report note.
    reportNorm :: Journal MNote v MBase -> Int -> Double
    reportNorm ledger tT =
        realToFrac (norm (EJ.projWithNote [(Report, tT)] ledger))

-- | The 'Policy.LedgerPolicy' derived from the retention\/spill parameters.
policyOf :: MarketParams -> Policy.LedgerPolicy
policyOf mp = Policy.defaultLedgerPolicy
    { Policy.retain  = case mpRetain mp of
                         RetainAllT      -> Policy.RetainAll
                         RetainRecentT w -> Policy.RetainRecent w
    , Policy.spillTo = mpSpill mp }
