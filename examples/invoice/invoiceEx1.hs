{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeApplications     #-}

{- |
  invoiceEx1 — a deliberately small, self-contained /worked example/ for the
  SICE performance evaluation: a __circulant invoice-trade network__.

  == The model in one paragraph

  @n@ firms sit on a ring. The trade structure is 'circulant' @[1..n] k@: buyer
  @j@ buys from the @k@ firms that follow it cyclically (its @min k (n-1)@
  suppliers), so the network is built in @O(kN)@ and stays usable at large @N@
  (unlike @kRegular@\/@erdosRenyi@, which scan the @O(N^2)@ ordered pairs). Every
  term, every edge @(supplier i -> buyer j)@ posts __one 6-entry invoice
  journal__ at a fixed amount @a@ (=100) with consumption tax rate @r@ (=0.10):

    * buyer  @j@: 仕入 @a@ (借) + 仮払消費税 @a·r@ (借) + 現金 @a+a·r@ (貸)
    * seller @i@: 売上 @a@ (貸) + 仮受消費税 @a·r@ (貸) + 現金 @a+a·r@ (借)

  Settlement is the cash line of the /same/ journal — there is no second stage,
  and no stage reads the ledger. This is intentionally the simplest thing that is
  still __real double-entry accounting__: a single 'runLite' stage that fans out
  over the edge list, tags each edge's bare 'Alg' with the note @(Trade, t)@, and
  commits the term's Σ in one shot.

  == The two observables (proposal §performance)

    (i)  __wall-time__ — 'runLite' is pure, so the result is forced with
         'Control.DeepSeq.force'\/'evaluate' /before/ the second clock read; a
         lazy read would under-measure. Timed with 'Data.Time.Clock' (wall).

    (ii) __consumption-tax invariant__ — the whole final ledger is netted per
         account title with 'EA.balanceMapBy'. Because every transaction posts
         仮受消費税 = 仮払消費税 = @a·r@, the closed forms are known exactly:

         > total transactions        = edgeCount · T
         > Σ 仮受消費税 (all firms)   = edgeCount · T · a · r      (= Σ 仮払消費税)
         > Σ (仮受 − 仮払)            = 0

         The run asserts all three and prints PASS/FAIL. This is what makes the
         benchmark __correct accounting__ rather than a bare HashMap stress test:
         a dropped or mismatched posting breaks the equality.

  Usage:

  > invoiceEx1                 -- runs n ∈ {200, 500, 1000}, T=50, k=10
  > invoiceEx1 1000            -- runs just n = 1000
  > invoiceEx1 200 500 1000    -- runs the listed n values
  > EA_T=50 EA_K=10 invoiceEx1 -- T and k are overridable from the environment
-}

module Main (main) where

import           GHC.Generics                (Generic)
import           Data.Hashable               (Hashable)
import           Control.Monad               (forM_)
import           Control.DeepSeq             (force)
import           Control.Exception           (evaluate)
import qualified Data.Map.Strict             as M
import           System.Environment          (getArgs, lookupEnv)
import           Data.Time.Clock             (getCurrentTime, diffUTCTime)
import           Text.Read                   (readMaybe)
import           Text.Printf                 (printf)

import           ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Journal     as EJ
import qualified ExchangeAlgebra.Algebra     as EA
import           ExchangeAlgebra.Simulate.Lite
                     ( HK, InitT, carry
                     , Stage, stageOf
                     , Par(..)
                     , SimSpec, mkSimSpec, specParallel, runLite )
import           ExchangeAlgebra.Simulate.Network
                     ( circulant, edges, edgeCount )
import           ExchangeAlgebra.Value       (MoneyDouble)

------------------------------------------------------------------
-- * Model types
------------------------------------------------------------------

-- | A firm is an integer id.
type Firm = Int

-- | The event axis of a note: a one-tag ADT (only 'Trade' is ever written), so
-- a mistyped read is a compile error rather than a silently empty projection.
-- 'PlankTag' is the pointed-set blank ('plank'), kept in the library style.
data ITag = PlankTag | Trade
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable ITag

instance Note ITag where
    plank = PlankTag

-- | Note = (event tag, term). The tuple 'Note' instance gives the 2-axis index.
type INote = (ITag, Int)

-- | A four-axis base @(AccountTitles, owner, counterparty, CountUnit)@ — the
-- @simulateEx2@\/@MarketModel@ shape. The owner axis gives each firm its own
-- cash- and tax-bases, so per-firm and per-title netting is one 'EA.balanceMapBy'.
type MBase = HatBase (AccountTitles, Firm, Firm, CountUnit)

instance Element Firm where
    wildcard = -1
instance BaseClass Firm

instance ExBaseClass MBase where
    getAccountTitle (_ :< (a, _, _, _))   = a
    setAccountTitle (h :< (_, o, c, u)) a = h :< (a, o, c, u)

-- | Product-only HKD world: just the accumulating ledger. The read-only trade
-- structure is not carried — the edge list is a deterministic function of the
-- parameters, so it is computed once and closed over by the stage (cf. @fs@ in
-- @MarketModel@), keeping the world a single-field record.
data World v f = World
  { wLedger :: HK f (Journal INote v MBase) }
  deriving Generic

------------------------------------------------------------------
-- * The single posting and the single stage
------------------------------------------------------------------

-- | The 6-entry invoice journal for one edge @(supplier i -> buyer j)@ at goods
-- amount @a@ and consumption tax @tax@ (= @a·r@). Built with the smart
-- constructor '.@' (non-negativity enforced once). The Hat\/Not side follows the
-- library's increase\/decrease convention (an account that /increases/ carries
-- 'Not', one that /decreases/ carries 'Hat'; cf. @Bookkeeping.up@\/@down@):
--
--   * buyer  @j@: 仕入↑ Not, 仮払消費税↑ Not, 現金↓ Hat (pays @a+tax@)
--   * seller @i@: 売上↑ Not, 仮受消費税↑ Not, 現金↑ Not (receives @a+tax@)
--
-- Each party is debit-credit balanced (@a+tax@ each side); settlement is the
-- cash line of this same journal, so no ledger read is needed.
invoicePosting :: (HatVal v) => v -> v -> Firm -> Firm -> EA.Alg v MBase
invoicePosting a tax i j =
       a         .@ Not :< (Purchases,              j, j, Yen)   -- 買手 j 仕入 (借)
  .+   tax       .@ Not :< (ConsumptionTaxPaid,     j, j, Yen)   -- 買手 j 仮払消費税 (借)
  .+   (a + tax) .@ Hat :< (Cash,                   j, j, Yen)   -- 買手 j 現金 (貸)
  .+   a         .@ Not :< (Sales,                  i, i, Yen)   -- 売手 i 売上 (貸)
  .+   tax       .@ Not :< (ConsumptionTaxReceived, i, i, Yen)   -- 売手 i 仮受消費税 (貸)
  .+   (a + tax) .@ Not :< (Cash,                   i, i, Yen)   -- 売手 i 現金 (借)

-- | The one and only stage: fan out over the edge list, each edge emitting its
-- bare 6-entry 'Alg'; the runner attaches the single note @(Trade, t)@ once.
tradeStage :: (HatVal v)
           => [(Firm, Firm)] -> v -> v -> Stage (World v) Int INote v MBase
tradeStage es a tax = stageOf Trade es $ \_w _t _g (i, j) ->
    invoicePosting a tax i j

-- | The 'SimSpec': terms @1..nTerms@, one 'tradeStage', strictly 'Sequential'.
invoiceSpec :: (HatVal v)
            => Int -> Int -> [(Firm, Firm)] -> v -> v
            -> SimSpec (World v) Int INote v MBase
invoiceSpec seed nTerms es a tax =
    (mkSimSpec (1, nTerms) seed wLedger [ tradeStage es a tax ])
      { specParallel = Sequential }

-- | The initial world: an empty ledger carried across term boundaries.
initWorld :: (HatVal v) => World v InitT
initWorld = World { wLedger = carry mempty }

------------------------------------------------------------------
-- * Consumption-tax invariant read-out
------------------------------------------------------------------

-- | Classify a base by account title IFF it is a consumption-tax title.
-- 'EA.balanceMapBy' nets Not (add) against Hat (subtract) per key; all tax lines
-- are posted 'Not', so each key's net is the running total.
classifyTax :: (AccountTitles, Firm, Firm, CountUnit) -> Maybe AccountTitles
classifyTax (acct, _, _, _)
  | acct == ConsumptionTaxReceived = Just ConsumptionTaxReceived
  | acct == ConsumptionTaxPaid     = Just ConsumptionTaxPaid
  | otherwise                      = Nothing

------------------------------------------------------------------
-- * Runner
------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    nTerms <- envInt "EA_T" 50
    k      <- envInt "EA_K" 10
    let ns = case args of
               [] -> [200, 500, 1000]
               _  -> [ maybe (badArg s) id (readMaybe s) | s <- args ]
    printf "invoiceEx1 (MoneyDouble): circulant invoice-trade network, T=%d k=%d\n"
        nTerms k
    printf "  a=100 (goods)  r=0.10 (consumption tax)  posting = 6-entry per edge, per term\n\n"
    forM_ ns $ \n -> runOne n nTerms k
  where
    badArg s = error ("invoiceEx1: argument " ++ show s ++ " is not an integer")

-- | Run the model for a single @n@ and print one result line.
runOne :: Int -> Int -> Int -> IO ()
runOne n nTerms k = do
    let firms  = [1 .. n]
        net    = circulant firms k
        es     = edges net
        eC     = edgeCount net
        a      = 100  :: MoneyDouble
        r      = 0.10 :: MoneyDouble
        tax    = a * r
        spec   = invoiceSpec 2025 nTerms es a tax
        w0     = initWorld :: World MoneyDouble InitT
        -- pure projection of the final ledger: (norm, Σ仮受, Σ仮払).
        project final =
            let ledg   = wLedger final
                alg    = EJ.toAlg ledg
                taxMap = EA.balanceMapBy classifyTax alg :: M.Map AccountTitles MoneyDouble
                recv   = M.findWithDefault 0 ConsumptionTaxReceived taxMap
                paid   = M.findWithDefault 0 ConsumptionTaxPaid     taxMap
            in ( realToFrac (norm ledg) :: Double
               , realToFrac recv        :: Double
               , realToFrac paid        :: Double )
    -- (i) wall-time: force the pure result to normal form BEFORE the 2nd clock.
    t0 <- getCurrentTime
    (normD, recvD, paidD) <- evaluate (force (runLite spec w0 project))
    t1 <- getCurrentTime
    let wall     = realToFrac (diffUTCTime t1 t0) :: Double
        -- (ii) closed-form tax theory: each of edgeCount·T transactions posts a·r.
        txCount  = eC * nTerms
        expected = fromIntegral txCount * 100 * 0.10 :: Double   -- a·r
        netTax   = recvD - paidD
        ok       = nearly recvD expected
                && nearly paidD expected
                && nearly netTax 0
        verdict  = if ok then "PASS" else "FAIL"
    printf "  n=%-5d edges=%-7d tx=%-9d wall=%8.4fs  norm=%.1f\n"
        n eC txCount wall normD
    printf "         Σ仮受消費税=%.1f  Σ仮払消費税=%.1f  expected=%.1f  net(仮受-仮払)=%.1f  [%s]\n\n"
        recvD paidD expected netTax verdict

-- | Absolute tolerance comparison (the amounts here are exact in 'Double', so a
-- generous 1e-3 leaves no doubt while tolerating any last-ULP reassociation).
nearly :: Double -> Double -> Bool
nearly x y = abs (x - y) < 1e-3

-- | Read an integer environment variable, defaulting when unset\/blank and
-- aborting on a present-but-unparseable value.
envInt :: String -> Int -> IO Int
envInt key dflt = do
    mv <- lookupEnv key
    pure $ case mv of
        Nothing -> dflt
        Just "" -> dflt
        Just s  -> maybe (error ("invoiceEx1: " ++ key ++ "=" ++ show s
                                  ++ " is not an integer")) id (readMaybe s)
