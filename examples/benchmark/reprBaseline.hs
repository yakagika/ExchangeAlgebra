{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}

-- | Measure construction, 'EA.norm', Cash queries, 'EA.bar', and account relabeling
-- through the public 'EA.Alg' API. See the workload and measurement sections for
-- the inputs and evaluation boundaries.
--
-- Run @stack exec repr-baseline -- 100000 +RTS -N1 -T@ from the repository root.
-- @-N1@ fixes the number of capabilities at one, and @-T@ enables RTS statistics.
-- Each sample runs in a fresh process so heap from earlier samples cannot affect
-- build residency. The column and edge prototypes remain on @codex/repr-probe@.
module Main
    ( -- * Command-line entry point
      main
    ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (replicateM, unless)
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import ExchangeAlgebra.Accounting.Account (AccountTitles(..))
import ExchangeAlgebra.Algebra
    ( BaseClass
    , CountUnit(..)
    , Element(..)
    , Hat(..)
    , HatBase(..)
    , (.@)
    , (.+)
    )
import qualified ExchangeAlgebra.Algebra as EA
import ExchangeAlgebra.Algebra.Value (MoneyDouble(..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import GHC.Exts (keepAlive#)
import GHC.IO (IO(..))
import GHC.Stats
    ( GCDetails(gcdetails_live_bytes)
    , RTSStats(allocated_bytes, gc, max_live_bytes)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.Environment (getArgs, getExecutablePath)
import System.Exit (die)
import System.Mem (performMajorGC)
import System.Process (readProcess)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- * Workload

-- | This newtype avoids orphan instances for 'Int' and uses the same runtime
-- representation and value hash as the probe's 'Int' for comparison.
newtype Company = Company Int
    deriving newtype (Eq, Ord, Show, Hashable)

instance Element Company where
    wildcard = Company (-1)

instance BaseClass Company

-- | The account, product, owner, and unit of a term.
type Base = (AccountTitles, Company, Company, CountUnit)

-- | A benchmark base with its 'Hat' or 'Not' orientation.
type HatBase2 = HatBase Base

-- | The ledger under measurement.
type Alg = EA.Alg MoneyDouble HatBase2

-- | Construct the six terms for a purchase from supplier to buyer.
purchasePosting :: MoneyDouble -> Company -> Company -> Alg
purchasePosting amount buyer supplier =
       amount .@ Not :< (Products,  supplier, buyer,    Amount)
    .+ amount .@ Hat :< (Cash,      wildcard, buyer,    Yen)
    .+ amount .@ Not :< (Purchases, wildcard, buyer,    Yen)
    .+ amount .@ Not :< (Cash,      wildcard, supplier, Yen)
    .+ amount .@ Not :< (Sales,     wildcard, supplier, Yen)
    .+ amount .@ Hat :< (Products,  supplier, supplier, Amount)

-- | Input size for the synthetic network.
data Scale = Scale
    { companies :: !Int
    , edges     :: !Int
    }

-- | Number of suppliers per buyer.
degree :: Int
degree = 10

-- | Number of terms generated for each purchase relationship.
termsPerEdge :: Int
termsPerEdge = 6

-- | Number of owners whose Cash entries are queried.
queryCount :: Int
queryCount = 1000

-- | Find the buyer and supplier for a zero-based purchase relationship.
edgeAt :: Scale -> Int -> (Company, Company)
edgeAt scale edge = (Company buyer, Company ((buyer + offset + 1) `rem` companies scale))
  where
    (buyer, offset) = edge `quotRem` degree

-- | Compute the positive amount for a buyer-supplier pair.
edgeAmount :: Company -> Company -> Double
edgeAmount (Company buyer) (Company supplier) =
    fromIntegral (1 + (31 * buyer + 17 * supplier) `rem` 2048)

-- | Construct the six terms for a zero-based purchase relationship.
edgePosting :: Scale -> Int -> Alg
edgePosting scale edge = purchasePosting (MoneyDouble (edgeAmount buyer supplier)) buyer supplier
  where
    (buyer, supplier) = edgeAt scale edge

-- | Select evenly spaced owners from the network.
queryCompanies :: Scale -> [Company]
queryCompanies scale = coerce [0, stride .. (queryCount - 1) * stride]
  where
    stride = companies scale `quot` queryCount

-- | Construct the ledger with the probe's fold order.
buildAlg :: Scale -> Alg
buildAlg scale = EA.sigma [0 .. edges scale - 1] (edgePosting scale)

-- | Force the primary map and all term values without demanding the lazy query index.
forceAlg :: Alg -> ()
forceAlg = EA.foldEntries touch ()
  where
    touch () (MoneyDouble value) (side :< (account, good, owner, unit)) =
        value `seq` side `seq` account `seq` good `seq` owner `seq` unit `seq` ()

-- | Extract the numeric value from a ledger amount.
unwrap :: MoneyDouble -> Double
unwrap (MoneyDouble value) = value

-- | Read gross Cash for one owner and one orientation.
cashAlg :: Alg -> Hat -> Company -> Double
cashAlg alg side company =
    unwrap (EA.norm (EA.proj [side :< (Cash, wildcard, company, Yen)] alg))

-- | Return the norm and term count after explicit netting with @bar@.
barAlg :: Alg -> (Double, Int)
barAlg alg = (unwrap (EA.norm reduced), EA.foldEntries count 0 reduced)
  where
    reduced = EA.bar alg
    count !total _ _ = total + 1

-- | Relabel Sales as Purchases.
replaceSales :: AccountTitles -> AccountTitles
replaceSales Sales = Purchases
replaceSales account = account

-- | Change the account name while preserving each term's other elements.
relabelAlg :: Alg -> Alg
relabelAlg = EA.mapBasePart replace
  where
    replace (account, good, owner, unit) = (replaceSales account, good, owner, unit)

-- * Validation

-- | Total term count, Sales term count, and gross Purchases amount.
type RelabelCheck = (Int, Int, Double)

-- | Gross norm, owner Cash pairs, netting result, and account relabeling result.
type Observation = (Double, [(Double, Double)], (Double, Int), RelabelCheck)

-- | Include one term in the relabeling check.
summarizeRelabel :: AccountTitles -> Double -> RelabelCheck -> RelabelCheck
summarizeRelabel account value (!count, !sales, !purchases) = case account of
    Sales     -> (count + 1, sales + 1, purchases)
    Purchases -> (count + 1, sales, purchases + value)
    _         -> (count + 1, sales, purchases)

-- | Summarize the relabeled ledger for comparison with the expected result.
checkAlgRelabel :: Alg -> RelabelCheck
checkAlgRelabel = EA.foldEntries step (0, 0, 0)
  where
    step summary (MoneyDouble amount) (_ :< (account, _, _, _)) =
        summarizeRelabel account amount summary

-- | Accumulate the absolute net Cash amount and surviving Cash term count.
netTotals :: (Double, Int) -> (Double, Double) -> (Double, Int)
netTotals (!total, !count) (hats, nots)
    | hats == nots = (total, count)
    | otherwise    = (total + abs (hats - nots), count + 1)

-- | Calculate the expected results from incoming and outgoing purchase amounts.
expected :: Scale -> Observation
expected scale =
    ( 6 * total
    , map cash (queryCompanies scale)
    , (4 * total + cashTotal, edges scale + 3 * size + cashCount)
    , (termsPerEdge * edges scale, 0, 2 * total)
    )
  where
    size = companies scale
    cash company@(Company owner) =
        ( sum [ edgeAmount company (Company ((owner + offset) `rem` size))
              | offset <- [1 .. degree]
              ]
        , sum [ edgeAmount (Company ((owner - offset) `mod` size)) company
              | offset <- [1 .. degree]
              ]
        )
    (!total, !cashTotal, !cashCount) = List.foldl' addOwner (0, 0, 0) [0 .. size - 1]
    addOwner (!gross, !net, !count) company =
        let (!outgoing, !incoming) = cash (Company company)
            (!nextNet, !nextCount) = netTotals (net, count) (outgoing, incoming)
        in (gross + outgoing, nextNet, nextCount)

-- | Exit with an error if any measured operation disagrees with its expected result.
validate :: Scale -> Observation -> IO ()
validate scale (gross, queries, (net, bases), (count, sales, purchases)) = do
    let (wantGross, wantQueries, (wantNet, wantBases),
         (wantCount, wantSales, wantPurchases)) = expected scale
        close x y = abs (x - y) <= 1e-9 * max (abs x) (abs y)
        check label ok = unless ok (die ("Result mismatch in " ++ label))
    check "norm" (close gross wantGross)
    check "all Cash queries" $
        length queries == length wantQueries &&
        and (zipWith (\(h, n) (wh, wn) -> close h wh && close n wn) queries wantQueries)
    check "bar" (bases == wantBases && close net wantNet)
    check "relabel" (count == wantCount && sales == wantSales && close purchases wantPurchases)

-- * Measurement

-- | Elapsed nanoseconds and allocated bytes for one operation.
data Operation = Operation !Integer !Integer
    deriving (Read, Show)

-- | Operation timings, live bytes after construction, and peak live bytes through construction.
data Sample = Sample ![Operation] !Integer !Integer
    deriving (Read, Show)

-- | An operation result, its timing, and RTS statistics after the final collection.
type Timed a = IO (a, Operation, RTSStats)

-- | Retain a value until the action completes.
keepingAlive :: a -> IO b -> IO b
keepingAlive value (IO action) = IO (\state -> keepAlive# value state action)

-- | Time an action and use major collections to delimit its allocation measurement.
measure :: IO a -> Timed a
measure action = do
    performMajorGC
    before <- getRTSStats
    start <- getMonotonicTimeNSec
    result <- action
    end <- getMonotonicTimeNSec
    after <- keepingAlive result (performMajorGC >> getRTSStats)
    let allocated = allocated_bytes after - allocated_bytes before
    pure (result, Operation (toInteger (end - start)) (toInteger allocated), after)

-- | Time the first query and read its allocation from the current thread.
measureIndex :: IO a -> IO (a, Operation)
measureIndex action = do
    start <- getMonotonicTimeNSec
    before <- getAllocationCounter
    result <- action
    after <- getAllocationCounter
    end <- getMonotonicTimeNSec
    pure (result, Operation (toInteger (end - start)) (toInteger (before - after)))

-- | Evaluate a result to normal form before returning it.
evaluateFully :: NFData a => a -> IO a
evaluateFully = evaluate . force

-- | Evaluate the ledger with the supplied forcing function before returning it.
evaluateLedger :: (a -> ()) -> a -> IO a
evaluateLedger forceLedger ledger = evaluate (forceLedger ledger) >> pure ledger

-- | Read both Cash orientations per owner and time the first Hat query separately.
queryAlg :: Scale -> Alg -> IO ([(Double, Double)], Operation)
queryAlg scale ledger = case queryCompanies scale of
    [] -> die "No query companies"
    first : rest -> do
        (firstHat, indexTiming) <- measureIndex (evaluate (cashAlg ledger Hat first))
        results <- evaluateFully $
            (firstHat, cashAlg ledger Not first)
            : [(cashAlg ledger Hat owner, cashAlg ledger Not owner) | owner <- rest]
        pure (results, indexTiming)

-- | Measure each operation in order and validate its result after timing.
sample :: Scale
    -> IO a
    -> (a -> Double)
    -> (a -> IO ([(Double, Double)], Operation))
    -> (a -> (Double, Int))
    -> (a -> IO a)
    -> (a -> RelabelCheck)
    -> IO Sample
sample scale build norm query bar relabel observeRelabel = do
    (ledger, buildTiming, built) <- measure build
    (gross, normTiming, _) <- measure (evaluate (norm ledger))
    ((queries, indexTiming), queryTiming, _) <- measure (query ledger)
    (net, barTiming, _) <- measure (evaluateFully (bar ledger))
    (updated, relabelTiming, _) <- keepingAlive ledger (measure (relabel ledger))
    validate scale (gross, queries, net, observeRelabel updated)
    pure (Sample
        [buildTiming, normTiming, queryTiming, indexTiming, barTiming, relabelTiming]
        (toInteger (gcdetails_live_bytes (gc built)))
        (toInteger (max_live_bytes built)))

-- | Measure the current ledger representation in a child with RTS statistics enabled.
sampleAlg :: Scale -> IO Sample
sampleAlg scale = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "Enable GHC statistics with +RTS -T")
    sample scale
        (evaluateLedger forceAlg (buildAlg scale))
        (unwrap . EA.norm)
        (queryAlg scale)
        barAlg
        (evaluateLedger forceAlg . relabelAlg)
        checkAlgRelabel

-- | Select the middle observation after sorting, using the upper middle for an even count.
median :: [Integer] -> Maybe Integer
median values = listToMaybe (drop (length values `quot` 2) (List.sort values))

-- | Show the supported invocation when command-line input is invalid.
usage :: String
usage = "Usage: repr-baseline (100000|1000000) +RTS -N1 -T"

-- | Parse the number of purchase relationships into a supported network size.
readScale :: String -> IO Scale
readScale size = case readMaybe size of
    Just count | count == 100000 || count == 1000000 -> pure (Scale (count `quot` degree) count)
    _ -> die usage

-- | Start three fresh child processes and report medians and per-term build metrics.
runSamples :: Scale -> IO ()
runSamples scale = do
    executable <- getExecutablePath
    samples <- replicateM 3 $ do
        output <- readProcess executable ["sample", size, "+RTS", "-N1", "-T"] ""
        case readMaybe output of
            Just result -> pure result
            Nothing     -> die "Could not read the validated sample"
    let operations = [timings | Sample timings _ _ <- samples]
        operationNames = ["norm", "query", "query-index", "bar", "relabel"]
        requireMedian = maybe (die "Missing observations in validated samples") pure . median
        report offset label = do
            let cells = [timing | timings <- operations, timing <- take 1 (drop offset timings)]
            nanoseconds <- requireMedian [ns | Operation ns _ <- cells]
            bytes <- requireMedian [allocated | Operation _ allocated <- cells]
            putStrLn (unwords [label, show nanoseconds, show bytes])
            pure nanoseconds
    putStrLn ("validated current " ++ size)
    putStrLn "operation median_ns median_allocated_bytes"
    buildNanoseconds <- report 0 "build"
    sequence_ (zipWith report [1 ..] operationNames)
    liveBytes <- requireMedian [live | Sample _ live _ <- samples]
    maxLiveBytes <- requireMedian [peak | Sample _ _ peak <- samples]
    putStrLn ("build_live_bytes " ++ show liveBytes)
    putStrLn ("build_max_live_bytes " ++ show maxLiveBytes)
    printf "build_ns_per_posting %.2f\n" (perTerm buildNanoseconds)
    printf "live_bytes_per_posting %.2f\n" (perTerm liveBytes)
  where
    size = show (edges scale)
    perTerm total = fromIntegral total / fromIntegral (termsPerEdge * edges scale) :: Double

-- | Run the sample collector or emit one validated sample in a child process.
main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        ["sample", size] -> readScale size >>= sampleAlg >>= print
        [size]           -> readScale size >>= runSamples
        _                -> die usage
