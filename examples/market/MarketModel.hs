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

  == What the model does (heavy stages, E3)

  Every term runs three BSP stages (each a pure map from the read-only snapshot
  to messages), in declared order:

    1. __trade__ — orders flow along the trade network's in-edges. For each edge
       @(i, j)@ (supplier @i@, buyer @j@) the buyer orders
       @a_{ij} · demand_j@ of supplier @i@'s product, booked as a /6-entry/
       bilateral posting (the @simulateEx2@\/Gate @purchasePosting@ shape).
       @demand_j@ is __inventory-connected__ (E3): it is
       @max(0, target − prevNetInventory_j)@, where @prevNetInventory_j@ is read
       from the /previous/ term's ledger snapshot in __one pass__ via
       'EA.balanceMapBy' (no per-firm projection loop).

    2. __production__ — each firm consumes the inputs it ordered (a
       coefficient-weighted draw on its input stock) and books the matching
       output of its own product. This is the @input(coef × demand) → output@
       Leontief step, kept as genuine accounting work (no busy-loop).

    3. __report__ — a per-firm one-pass aggregation ('EA.postFromNetBy', the
       §2A flagship): the term's net product position is classified by owning
       firm and any net /shortage/ (Hat product) is turned, in one O(m) pass,
       into an explicit replenishment marker.

  == simple \/ tuned trade stage (E4)

  'tradeStageSimple' writes the trade Σ the natural way ('sigmaEdges' of a
  6-entry @.\@@ posting per edge). 'tradeStageTuned' computes the /same/ journal
  by manually fusing the per-base accumulation with the lower-level
  'EA.foldEntriesToMap' \/ 'EJ.sigmaOnFromMap' API. With an exact value type
  ('MoneyDecimal') the two are __bit-for-bit identical__ (asserted in the test
  suite); the difference is only in evaluation strategy, measured by the
  experiment harness.

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
      -- * Spec assembly and run
    , marketSpec
    , RunResult(..)
    , runMarket
    ) where

import           GHC.Generics                  (Generic)
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
                     , Stage, stage
                     , Par(..)
                     , SimSpec, mkSimSpec, specParallel
                     , runLite, runLiteWithPolicy )
import           ExchangeAlgebra.Simulate        (StateTime(..))
import qualified ExchangeAlgebra.Simulate.Policy as Policy
import           ExchangeAlgebra.Simulate.Network
                     ( TradeNetwork, InputCoefficients
                     , completeNetwork, kRegular, erdosRenyi, scaleFree
                     , defaultCoefOptions, randomCoefficients
                     , nodes, edges, edgeCount, coefficient )

------------------------------------------------------------------
-- * Model types
------------------------------------------------------------------

-- | A firm is an integer id.
type Firm  = Int

-- | Note = (event tag, term). The term is the last component, so the
-- 'Policy.HasTermAxis' instance for pairs applies (used by 'runLiteWithPolicy').
type MNote = (String, Int)

-- | A four-axis base @(AccountTitles, owner, counterparty, CountUnit)@ — the
-- same shape the SICE @simulateEx2@ example uses. The /owner/ axis gives every
-- firm its own product- and cash-bases, so per-firm netting is just @bar@ over
-- that firm's bases.
type MBase = HatBase (AccountTitles, Firm, Firm, CountUnit)

instance Element Firm where
    wiledcard = -1
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
-- * Inventory-connected demand (E3, one pass)
------------------------------------------------------------------

-- | Each firm's order-driving demand this term, read from the /previous/ term's
-- ledger snapshot in __one pass__: @demand_j = max(0, target − netInventory_j)@.
--
-- @netInventory_j@ is the signed net of firm @j@'s own @Products@ base (Not adds,
-- Hat subtracts). 'EA.balanceMapBy' computes all firms' nets in a single fold
-- over the snapshot ledger's algebra — no per-firm projection loop (which would
-- be the O(N·proj) shape this whole proposal removes).
--
-- Returns a total map @Firm -> demand@ defaulting to @target@ for firms with no
-- recorded inventory yet (term 1).
demandMap :: forall v. (HatVal v, Real v)
          => Double -> [Firm] -> Journal MNote v MBase -> M.Map Firm Double
demandMap target fs ledger =
    let !alg     = EJ.toAlg ledger
        -- one pass: net product inventory per owning firm.
        invMap   = EA.balanceMapBy ownerOfProduct alg :: M.Map Firm v
    in M.fromList
         [ (j, max 0 (target - realToFrac (M.findWithDefault 0 j invMap)))
         | j <- fs ]
  where
    -- classify a base by the owning firm IFF it is that firm's own product
    -- stock @(Products, j, j, _)@. (BasePart strips the Hat/Not side.)
    ownerOfProduct :: BasePart MBase -> Maybe Firm
    ownerOfProduct bp = case bp of
        (Products, o, c, _) | o == c -> Just o
        _                            -> Nothing

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
-- demand and coefficients live in); the caller converts to @v@.
orderAmount :: (HatVal v, Real v)
            => InputCoefficients Firm v -> M.Map Firm Double -> Firm -> Firm -> Double
orderAmount coef dm i j =
    let a = realToFrac (fromMaybe 0 (coefficient coef i j)) :: Double
        d = M.findWithDefault 0 j dm
    in a * d

-- | __simple__ trade stage: the natural Σ over edges. For each edge a 6-entry
-- posting is built with '.\@'\/'.+' and the edge sum is taken with 'sigmaEdges'.
tradeStageSimple :: forall v. (HatVal v, Real v)
                 => Double -> Stage (World v) Int MNote v MBase
tradeStageSimple target = stage "trade" $ \w t ->
    let net  = wNet w
        coef = wCoef w
        fs   = nodes net
        dm   = demandMap target fs (wLedger w)
        per (i, j) = let amt = realToFrac (orderAmount coef dm i j) :: v
                     in if amt <= 0 then mempty
                        else purchasePosting amt i j
        -- sum the per-edge 6-entry postings into one Alg, then attach one note.
        alg = EA.sigma (edges net) per
    in if EA.isZero alg then mempty else alg .| ("trade", t)

-- | __tuned__ trade stage: the /same/ journal, computed by manually fusing the
-- per-base accumulation. Each edge contributes six @(base, value)@ pairs into a
-- single 'EA.foldEntriesToMap'-style accumulator, then 'EJ.sigmaOnFromMap'
-- rebuilds one journal in one shot. With an exact value type the result is
-- bit-for-bit identical to 'tradeStageSimple' (the per-base values are summed in
-- the same key order); with FP it is deterministic but may reassociate.
tradeStageTuned :: forall v. (HatVal v, Real v)
                => Double -> Stage (World v) Int MNote v MBase
tradeStageTuned target = stage "trade" $ \w t ->
    let net  = wNet w
        coef = wCoef w
        fs   = nodes net
        dm   = demandMap target fs (wLedger w)
        -- fold every edge's six contributions into one base->value map.
        accum = foldl' step M.empty (edges net)
        step acc (i, j) =
            let amt = realToFrac (orderAmount coef dm i j) :: v
            in if amt <= 0 then acc
               else foldl' (\m (b, v) -> M.insertWith (+) b v m) acc
                      [ (Not :< (Products,  j, j, Amount), amt)
                      , (Hat :< (Cash,      j, j, Yen),    amt)
                      , (Not :< (Purchases, j, j, Yen),    amt)
                      , (Not :< (Cash,      i, i, Yen),    amt)
                      , (Not :< (Sales,     i, i, Yen),    amt)
                      , (Hat :< (Products,  i, i, Amount), amt) ]
    -- rebuild a single journal on the ("trade", t) note from the merged map.
    in EJ.sigmaOnFromMap ("trade", t) accum (\b v -> v .@ b)

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
                => Double -> Stage (World v) Int MNote v MBase
productionStage target = stage "production" $ \w t ->
    let net  = wNet w
        fs   = nodes net
        dm   = demandMap target fs (wLedger w)
        perFirm j =
            let amt = realToFrac (M.findWithDefault 0 j dm) :: v
            in if amt <= 0 then mempty
               else (amt .@ Hat :< (Products,    j, j, Amount))   -- consume own product
                 .+ (amt .@ Not :< (SalesCost,   j, j, Yen))      -- as a usage-cost line
        alg = EA.sigma fs perFirm
    in if EA.isZero alg then mempty else alg .| ("production", t)

------------------------------------------------------------------
-- * Report stage (E3 c, §2A flagship): per-firm one-pass aggregation
------------------------------------------------------------------

-- | Per-firm one-pass shortage report. The term's net product position is
-- classified by owning firm with 'EA.postFromNetBy' (bar — explicit in the
-- name): for every firm whose own product nets to a /shortage/
-- (@Hat:<(Products, j, j, _)@) it emits, in one O(m) pass keyed by the owning
-- firm, an explicit replenishment marker for exactly the shortfall.
--
-- This reads the /current/ term's flow off the snapshot ledger restricted to
-- this term, so it summarises what trade+production just did.
reportStage :: forall v. (HatVal v)
            => Stage (World v) Int MNote v MBase
reportStage = stage "report" $ \w t ->
    let ledger   = wLedger w
        -- this term's flow only (Note's term axis is the last component).
        termJ    = EJ.filterWithNote (\(_, tt) _ -> tt == t) ledger
        flow     = EJ.toAlg termJ
        shortageK b = case b of
            Hat :< (Products, o, c, _) | o == c -> Just o
            _                                   -> Nothing
        shortage = EA.postFromNetBy shortageK
                     (\j v -> v .@ Not :< (Products, j, j, Amount))
                     flow
    in if EA.isZero shortage then mempty else shortage .| ("report", t)

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
        [ tradeStage (mpTarget mp)
        , productionStage (mpTarget mp)
        , reportStage ])
      { specParallel = toPar (mpPar mp) }
  where
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
        realToFrac (norm (EJ.projWithNote [("report", tT)] ledger))

-- | The 'Policy.LedgerPolicy' derived from the retention\/spill parameters.
policyOf :: MarketParams -> Policy.LedgerPolicy
policyOf mp = Policy.defaultLedgerPolicy
    { Policy.retain  = case mpRetain mp of
                         RetainAllT      -> Policy.RetainAll
                         RetainRecentT w -> Policy.RetainRecent w
    , Policy.spillTo = mpSpill mp }
