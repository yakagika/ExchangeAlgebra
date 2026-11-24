{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

-- | Measure the public 'EJ.Journal' API on a sparse production and trade model
-- for comparison with other ledger representations. The network supplies the
-- model's fixed inputs, and the journal holds its accounting state. Start at
-- @main@ for the run or at the model section for the term operations.
--
-- Each term builds the opening, production, and sales and purchase entries,
-- reads their norm, transfers sales and purchases to gross profit, queries
-- every company's inventory and gross profit, and closes the ledger. The
-- explicitly netted closing balance supplies the next opening.
--
-- From the repository root, run
-- @stack exec sparse-trade-model -- 10000 +RTS -N1 -T@.
-- @-N1@ fixes one capability for comparable timings, and @-T@ enables the
-- RTS statistics used to measure live bytes. The difference between major
-- collections before and after build excludes the fixed inputs and opening
-- balance retained across both samples.
--
-- The reference-oracle checks every company's inventory and gross profit, and
-- the journal norm, in every term. It uses the network inputs, production
-- quantities, and opening constants in 'Double'. After the first term it uses
-- the EA opening norm only for the norm comparison. Each comparison allows
-- relative error of 1e-9 when the expected magnitude is at least 1 and
-- absolute error of 1e-9 below that magnitude.
module Main
    ( -- * Command-line entry point
      main
    ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Word (Word64)
import ExchangeAlgebra.Accounting.Account (AccountTitles(..))
import ExchangeAlgebra.Accounting.Closing (closingEntries)
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import ExchangeAlgebra.Accounting.Transfer
    ( TransferShare(Relabel)
    , TransferTable
    , TransferTableError
    , mkTransferTable
    , transferEntriesBy
    , (.->)
    , (|%)
    )
import ExchangeAlgebra.Algebra
    ( BaseClass
    , CountUnit(..)
    , Element(..)
    , Hat(..)
    , HatBase(..)
    , (.+)
    , (.@)
    )
import qualified ExchangeAlgebra.Algebra as EA
import ExchangeAlgebra.Algebra.Value (MoneyDouble(..))
import qualified ExchangeAlgebra.Journal as EJ
import ExchangeAlgebra.Journal (Note(..), (.|))
import qualified ExchangeAlgebra.Simulation.Network as Network
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import GHC.Exts (keepAlive#)
import GHC.Generics (Generic)
import GHC.IO (IO(..))
import GHC.Stats
    ( GCDetails(gcdetails_live_bytes)
    , RTSStats(gc)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performMajorGC)
import System.Random (mkStdGen)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- * Model

-- | A zero-based company identifier. The wildcard denotes a missing product owner.
newtype Company = Company Int
    deriving newtype (Eq, Ord, Show, Hashable, NFData)

instance Element Company where
    wildcard = Company (-1)

instance BaseClass Company

-- | The term number, starting at one.
type Term = Int

-- | The event component of a journal note @(event, term)@.
data EventName
    = Opening
    | Production
    | SalesPurchase
    | Plank
    deriving stock (Eq, Ord, Show, Generic)

instance Hashable EventName

instance Note EventName where
    plank = Plank

-- | The account, product owner, recording company, and unit, with orientation.
type HatBase2 = HatBase (AccountTitles, Company, Company, CountUnit)

instance ExBaseClass HatBase2 where
    getAccountTitle (_    :< (account, _, _, _)) = account
    setAccountTitle (side :< (_, owner, company, unit)) account =
        side :< (account, owner, company, unit)

-- | Terms in quantities or yen before attaching a note.
type Alg = EA.Alg MoneyDouble HatBase2

-- | The accounting state for one term, indexed by event and term number.
type Ledger = EJ.Journal (EventName, Term) MoneyDouble HatBase2

-- | Fixed non-accounting inputs. An edge lists the supplier before the buyer.
data Inputs = Inputs
    { network      :: !(Network.TradeNetwork Company)
    , coefficients :: !(Network.InputCoefficients Company Double)
    , companies    :: ![Company]
    , tradeEdges   :: ![(Company, Company)]
    }

-- | Generate the network and coefficients and prepare their ordered traversals.
prepareInputs :: Int -> IO Inputs
prepareInputs count = do
    let identifiers = map Company [0 .. count - 1]
    graph <- evaluate $ force $ Network.kRegular (mkStdGen 0) identifiers 10
    rates <- evaluate $ force $ Network.randomCoefficients (mkStdGen 1)
        Network.defaultCoefOptions graph
    firms <- evaluate $ force $ Network.nodes graph
    links <- evaluate $ force $ Network.edges graph
    pure Inputs
        { network      = graph
        , coefficients = rates
        , companies    = firms
        , tradeEdges   = links
        }

-- | Output quantity for company @j@ in term @t@.
quantity :: Term -> Company -> Double
quantity term (Company company) =
    fromIntegral (1 + (7 * company + term) `rem` 5)

-- | Read the coefficient for a supplier and buyer from the generated network.
-- Invariant: 'Network.randomCoefficients' supplies a coefficient for every edge.
edgeCoefficient :: Inputs -> Company -> Company -> Double
edgeCoefficient inputs supplier buyer =
    case Network.coefficient (coefficients inputs) supplier buyer of
        Just rate -> rate
        Nothing   -> error "Generated coefficient coverage invariant"

-- | First-term opening inventory (quantity), cash, and capital stock (yen).
initialOpening :: [Company] -> Alg
initialOpening firms = EA.sigma firms openingPosting
  where
    openingPosting :: Company -> Alg
    openingPosting company =
           100  .@ Not :< (Products,     company,  company, Amount)
        .+ 1000 .@ Not :< (Cash,         wildcard, company, Yen)
        .+ 1000 .@ Not :< (CapitalStock, wildcard, company, Yen)

-- | Six terms for a purchase by the buyer from the supplier at one yen per unit.
purchasePosting :: MoneyDouble -> Company -> Company -> Alg
purchasePosting amount buyer supplier =
       amount .@ Not :< (Products,  supplier, buyer,    Amount)
    .+ amount .@ Hat :< (Cash,      wildcard, buyer,    Yen)
    .+ amount .@ Not :< (Purchases, wildcard, buyer,    Yen)
    .+ amount .@ Not :< (Cash,      wildcard, supplier, Yen)
    .+ amount .@ Not :< (Sales,     wildcard, supplier, Yen)
    .+ amount .@ Hat :< (Products,  supplier, supplier, Amount)

-- | Build one journal with separate notes for the opening, production, and trade.
buildLedger :: Inputs -> Term -> Alg -> Ledger
buildLedger inputs term opening = openingNote .+ production .+ salesPurchase
  where
    openingNote :: Ledger
    openingNote = opening .| (Opening, term)

    production :: Ledger
    production = (outputs .+ consumed) .| (Production, term)

    outputs :: Alg
    outputs = EA.sigma (companies inputs) output

    consumed :: Alg
    consumed = EA.sigma (tradeEdges inputs) useInput

    salesPurchase :: Ledger
    salesPurchase = EJ.sigmaOn (SalesPurchase, term) (tradeEdges inputs) purchase

    output :: Company -> Alg
    output company = MoneyDouble (quantity term company) .@ Not :<
        (Products, company, company, Amount)

    useInput :: (Company, Company) -> Alg
    useInput (supplier, buyer) =
        inputAmount supplier buyer .@ Hat :< (Products, supplier, buyer, Amount)

    purchase :: (Company, Company) -> Alg
    purchase (supplier, buyer) = purchasePosting (inputAmount supplier buyer) buyer supplier

    inputAmount :: Company -> Company -> MoneyDouble
    inputAmount supplier buyer = MoneyDouble (edgeCoefficient inputs supplier buyer
        * quantity term buyer)

-- | Relabel sales and purchases into gross profit while preserving their debit or credit side.
grossProfitRules :: Either (TransferTableError HatBase2) (TransferTable HatBase2)
grossProfitRules = mkTransferTable
    [ (Sales,     wildcard, wildcard, wildcard) .-> target |% Relabel
    , (Purchases, wildcard, wildcard, wildcard) .-> target |% Relabel
    ]
  where
    target :: (AccountTitles, Company, Company, CountUnit)
    target = (GrossProfit, wildcard, wildcard, wildcard)

-- | Per-company inventory quantity and gross profit in yen for the term.
queryBalances :: [Company] -> Ledger -> Ledger -> [(Double, Double)]
queryBalances firms ledger transfer = map balances firms
  where
    inventory :: Alg
    inventory = EJ.toAlg ledger

    profit :: Alg
    profit = EJ.toAlg (ledger .+ transfer)

    balances :: Company -> (Double, Double)
    balances company =
        ( readBalance (Products, company, company, Amount) inventory
        , readBalance (GrossProfit, wildcard, company, Yen) profit
        )

    readBalance :: (AccountTitles, Company, Company, CountUnit) -> Alg -> Double
    readBalance base algebra = unwrap (EA.balanceBy [Not :< base] [Hat :< base] algebra)

-- | Extract the underlying number of units from an EA readout.
unwrap :: MoneyDouble -> Double
unwrap (MoneyDouble number) = number

-- * Validation

-- | Check the term norm and all company balances against the reference-oracle.
-- Only the permitted opening-norm offset can come from EA. The inventory
-- formula starts from the initial constant and accumulates changes through
-- the current term independently of the ledger and its carry.
validate :: Inputs
         -> Term
         -> Double
         -> Double
         -> [(Double, Double)]
         -> IO ()
validate inputs term openingNorm observedNorm observedBalances = do
    check "norm" observedNorm (openingNorm + totalOutput + 7 * totalInputs)
    forM_ (zip (companies inputs) observedBalances) verifyCompany
  where
    totalOutput :: Double
    totalOutput = sum [quantity term company | company <- companies inputs]

    totalInputs :: Double
    totalInputs = sum [sold term supplier buyer | (supplier, buyer) <- tradeEdges inputs]

    sold :: Term -> Company -> Company -> Double
    sold period supplier buyer = edgeCoefficient inputs supplier buyer * quantity period buyer

    outgoing :: Term -> Company -> Double
    outgoing period company = sum
        [sold period company buyer | buyer <- Network.buyersOf (network inputs) company]

    verifyCompany :: (Company, (Double, Double)) -> IO ()
    verifyCompany (company, (observedStock, observedProfit)) = do
        let expectedStock :: Double
            expectedStock = 100 + sum
                [quantity period company - outgoing period company | period <- [1 .. term]]

            expectedProfit :: Double
            expectedProfit = outgoing term company - sum
                [rate * quantity term company | (_, rate) <- Network.inputsOf
                    (coefficients inputs) company]

        check ("inventory of " ++ show company) observedStock expectedStock
        check ("gross profit of " ++ show company) observedProfit expectedProfit

    check :: String -> Double -> Double -> IO ()
    check label observed expected =
        unless (abs (observed - expected) <= 1e-9 * max 1 (abs expected)) $
            die $ "Term " ++ show term ++ " " ++ label ++ " expected "
                ++ show expected ++ ", got " ++ show observed

-- * Measurement

-- | Elapsed monotonic nanoseconds and current-thread allocated bytes.
data Operation = Operation !Word64 !Int64

-- | Operation measurements, the build live-byte difference, and the number of terms.
data Sample = Sample
    { operations   :: ![(String, Operation)]
    , liveBytes    :: !Integer
    , postingCount :: !Int
    }

-- | Demand the value and every component of each hat base through the public fold.
forceAlg :: Alg -> ()
forceAlg = EA.foldEntries touch ()
  where
    touch :: () -> MoneyDouble -> HatBase2 -> ()
    touch () (MoneyDouble value) (side :< (account, owner, company, unit)) =
        value `seq` side `seq` account `seq` owner `seq` company `seq` unit `seq` ()

-- | Traverse each note's terms without demanding the lazy query indexes.
forceLedger :: Ledger -> ()
forceLedger = HashMap.foldl' touch () . EJ.toMap
  where
    touch :: () -> Alg -> ()
    touch () = forceAlg

-- | Count all terms, including distinct terms with the same base under a note.
countLedger :: Ledger -> Int
countLedger = HashMap.foldl' (EA.foldEntries count) 0 . EJ.toMap
  where
    count :: Int -> MoneyDouble -> HatBase2 -> Int
    count !total _ _ = total + 1

-- | Keep a value reachable through the end of an IO action.
keepingAlive :: a -> IO b -> IO b
keepingAlive value (IO action) = IO (\state -> keepAlive# value state action)

-- | Take a major collection and read its live heap size in bytes.
majorLiveBytes :: IO Integer
majorLiveBytes = do
    performMajorGC
    stats <- getRTSStats
    pure (toInteger (gcdetails_live_bytes (gc stats)))

-- | Measure an action that forces its result before returning.
measure :: IO a -> IO (a, Operation)
measure action = do
    start  <- getMonotonicTimeNSec
    before <- getAllocationCounter
    result <- action
    after  <- getAllocationCounter
    end    <- getMonotonicTimeNSec
    pure (result, Operation (end - start) (before - after))

-- | Evaluate a checked step or terminate with its error.
checked :: Show e => Either e a -> IO a
checked = either (die . show) pure

-- | Measure one term and return its EA carry and scalar measurements.
runTerm :: Inputs
        -> TransferTable HatBase2
        -> Term
        -> Alg
        -> IO (Alg, Sample)
runTerm inputs rules term opening = do
    (ledger, buildTime, ledgerBytes) <- keepingAlive (inputs, opening) $ do
        before <- majorLiveBytes
        (built, timing) <- measure $ do
            let built = buildLedger inputs term opening
            evaluate (forceLedger built)
            pure built
        after <- keepingAlive built majorLiveBytes
        pure (built, timing, after - before)

    (observedNorm, normTime) <- measure $ evaluate $ unwrap $ EA.norm ledger
    (transfer, transferTime) <- measure $ do
        entries <- checked $ transferEntriesBy id rules ledger
        evaluate (forceLedger entries)
        pure entries
    (balances, queryTime) <- measure
                           $ evaluate
                           $ force
                           $ queryBalances (companies inputs) ledger transfer
    (carry, closingTime) <- measure $ do
        let algebra = EJ.toAlg ledger
        entries <- checked (closingEntries algebra)
        let carried = EA.bar (algebra .+ entries)
        evaluate (forceAlg carried)
        pure carried

    let openingNorm :: Double
        openingNorm
            | term == 1 = 2100 * fromIntegral (length (companies inputs))
            | otherwise = unwrap (EA.norm opening)

    validate inputs term openingNorm observedNorm balances
    sample <- evaluate Sample
        { operations =
            [ ("build", buildTime)
            , ("norm", normTime)
            , ("transfer", transferTime)
            , ("query", queryTime)
            , ("closing", closingTime)
            ]
        , liveBytes = ledgerBytes
        , postingCount = countLedger ledger
        }
    pure (carry, sample)

-- | Print one term's measurements in execution order.
printTerm :: Term -> Sample -> IO ()
printTerm term sample = do
    forM_ (operations sample) printOperation
    printf "term %d ledger_live_bytes %d\n" term (liveBytes sample)
    printf "term %d postings %d\n" term (postingCount sample)
  where
    printOperation :: (String, Operation) -> IO ()
    printOperation (label, Operation elapsed allocated) =
        printf "term %d %s %d %d\n" term label elapsed allocated

-- | The middle value in three samples.
medianOfThree :: Ord a => a -> a -> a -> a
medianOfThree first second third =
    max (min first second) (min (max first second) third)

-- | Print medians of the three terms after the first term.
printMedians :: Sample -> Sample -> Sample -> IO ()
printMedians first second third = do
    sequence_ $ zipWith3 printOperation (operations first) (operations second) (operations third)
    printf "median ledger_live_bytes %d\n" $
        medianOfThree (liveBytes first) (liveBytes second) (liveBytes third)
    printf "median live_bytes_per_posting %.2f\n" $
        medianOfThree (bytesPerPosting first) (bytesPerPosting second) (bytesPerPosting third)
  where
    printOperation :: (String, Operation) -> (String, Operation) -> (String, Operation) -> IO ()
    printOperation (label, Operation time1 bytes1) (_, Operation time2 bytes2)
        (_, Operation time3 bytes3) =
            printf "median %s %d %d\n" label
                (medianOfThree time1 time2 time3) (medianOfThree bytes1 bytes2 bytes3)

    bytesPerPosting :: Sample -> Double
    bytesPerPosting sample =
        fromIntegral (liveBytes sample) / fromIntegral (postingCount sample)

-- * Command-line entry point

-- | Run four validated terms for a network of at least eleven companies.
-- Print measurements only after every term passes validation.
main :: IO ()
main = do
    args <- getArgs
    count <- case args of
        [argument] | Just n <- readMaybe argument, n >= 11 -> pure n
        _ -> die "Usage: sparse-trade-model <company-count, at least 11> +RTS -N1 -T"

    statsEnabled <- getRTSStatsEnabled
    unless statsEnabled (die "Enable RTS statistics with +RTS -N1 -T")
    inputs <- prepareInputs count
    rules  <- checked grossProfitRules

    let opening = initialOpening (companies inputs)
    evaluate (forceAlg opening)

    (carry1, first)  <- runTerm inputs rules 1 opening
    (carry2, second) <- runTerm inputs rules 2 carry1
    (carry3, third)  <- runTerm inputs rules 3 carry2
    (_, fourth)     <- runTerm inputs rules 4 carry3

    printf "validated sparse-trade-model N=%d E=%d T=4\n" count
        (Network.edgeCount (network inputs))
    mapM_ (uncurry printTerm) [(1, first), (2, second), (3, third), (4, fourth)]
    printMedians second third fourth
