-- ANCHOR: setup
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty((:|)))

import ExchangeAlgebra hiding (Yen, Dollar, map)
import qualified ExchangeAlgebra.Algebra.Core as Algebra
import qualified ExchangeAlgebra.Journal as Journal
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import qualified ExchangeAlgebra.Accounting.Entries as Entries
import qualified ExchangeAlgebra.Accounting.Closing as Closing

import qualified ExchangeAlgebra.Accounting.TrialBalance as TrialBalance
import qualified ExchangeAlgebra.Accounting.Statements as Statements
import qualified ExchangeAlgebra.Accounting.Consolidation as Consolidation

-- | Currency coordinate retained in the source journal.
data Unit
    = Yen       -- ^ Japanese yen.
    | Dollar    -- ^ US dollars.
    | Unit      -- ^ Inapplicable in an entry; unrestricted in a query.
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Unit where
    wildcard = Unit

instance ExBaseClass (HatBase (AccountTitles, Unit)) where
    getAccountTitle (_ :< (account, _)) = account
    setAccountTitle (direction :< (_, unit)) account = direction :< (account, unit)

-- | Date and description; the example supplies valid calendar dates.
data EntryNote = EntryNote (Integer, Int, Int) String
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Journal.Note EntryNote where
    plank = EntryNote (0, 0, 0) ""

-- | One entry over account and currency coordinates; the alias adds no validation.
type Entry = Alg MoneyDecimal (HatBase (AccountTitles, Unit))

-- | January journal with one distinct note per transaction.
type Book = Journal.Journal EntryNote MoneyDecimal (HatBase (AccountTitles, Unit))
-- ANCHOR_END: setup

-- ANCHOR: january
-- | Attach a January date and description after constructing all postings.
record :: Int -> String -> Entry -> Book
record day description algebra = algebra Journal..| EntryNote (2026, 1, day) description

-- | January 1: capital paid into Bank A's ordinary deposit account.
opening :: Book
opening = record 1 "Opening capital"
    (10000 .@ Not :< (Deposits, Yen)
  .+ 10000 .@ Not :< (CapitalStock, Yen))

-- | Transactions through January 30, before adjusting entries.
sourceJournal :: Book
sourceJournal = opening
    .+ record 10 "Withdrawal from Bank A"
        (3000 .@ Not :< (Cash, Yen) .+ 3000 .@ Hat :< (Deposits, Yen))
    .+ record 15 "Withdrawal from Bank A"
        (2000 .@ Not :< (Cash, Yen) .+ 2000 .@ Hat :< (Deposits, Yen))
    .+ record 20 "Purchase of goods"
        (500 .@ Not :< (Purchases, Yen) .+ 500 .@ Hat :< (Cash, Yen))
    .+ record 25 "Yen-dollar exchange"
        (1500 .@ Hat :< (Cash, Yen) .+ 10 .@ Not :< (Cash, Dollar))
    .+ record 28 "Cash sale"
        (2000 .@ Not :< (Cash, Yen) .+ 2000 .@ Not :< (Sales, Yen))
    .+ record 30 "January and February rent"
        (1200 .@ Not :< (RentExpense, Yen) .+ 1200 .@ Hat :< (Cash, Yen))
-- ANCHOR_END: january

-- ANCHOR: conversion
-- | Express dollars in yen at 150 yen per dollar, preserving account and side.
toYen :: MoneyDecimal
      -> HatBase (AccountTitles, Unit)
      -> (MoneyDecimal, HatBase (AccountTitles, Unit))
toYen amount (direction :< (account, Dollar)) = (amount * 150, direction :< (account, Yen))
toYen amount basis = (amount, basis)

-- | Reporting representation at the January 31 rate, equal to the exchange rate.
yenJournal :: Book
yenJournal = Journal.map (Algebra.mapPosting toYen) sourceJournal

-- | Admit the converted journal without changing its notes or posting sequences.
acceptedJanuary :: Either String (Admission.Admitted EntryNote (HatBase (AccountTitles, Unit)))
acceptedJanuary = either (Left . show . Admission.failureErrors) Right
    (Admission.admit yenJournal)
-- ANCHOR_END: conversion

-- ANCHOR: reporting
-- | Print a labeled result, retaining the library's diagnostic constructors.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Stop the example on an unexpected checked failure.
require :: Show e => Either e a -> IO a
require = either (ioError . userError . show) pure

-- | Drop the currency coordinate only after checking every posting is in yen.
-- This reporting projection loses the currency axis; it does not net postings.
auditLedger :: Entry -> Either String (Alg MoneyDecimal (HatBase AccountTitles))
auditLedger ledger
    | Algebra.foldEntries allYen True ledger = Right (Algebra.mapPosting dropUnit ledger)
    | otherwise = Left "Audit requires yen on every posting"
  where
    allYen :: Bool -> MoneyDecimal -> HatBase (AccountTitles, Unit) -> Bool
    allYen result _ (_ :< (_, unit)) = result && unit == Yen

    dropUnit :: MoneyDecimal
             -> HatBase (AccountTitles, Unit)
             -> (MoneyDecimal, HatBase AccountTitles)
    dropUnit amount (direction :< (account, _)) = (amount, direction :< account)
-- ANCHOR_END: reporting

-- ANCHOR: adjustment
-- | January 31: defer February's half of the rent, using the rent transfer table.
rentAdjustment :: Entry -> Either String Entry
rentAdjustment ledger = do
    table <- either (Left . show) Right $ mkTransferTable
        [(RentExpense, (.#)) .-> (PrepaidExpenses, (.#)) |% Portion 1 2]
    either (Left . show) Right (transferEntries table ledger)

-- | Keep the currency axis when building the periodic inventory adjustment.
yenBase :: Entries.MkBase (HatBase (AccountTitles, Unit))
yenBase direction account = direction :< (account, Yen)

-- | No beginning inventory; the January 31 physical count is worth 200 yen at cost.
inventoryAdjustment :: Entry
inventoryAdjustment = Entries.cogsAdjustmentEntries yenBase 0 200

-- | Add adjustments under distinct notes and admit the resulting journal again.
adjustedJanuary :: Book -> Either String Book
adjustedJanuary journal = do
    rent <- rentAdjustment (Journal.toAlg journal)
    let adjusted :: Book
        adjusted = journal
                .+ record 31 "Defer February rent" rent
                .+ record 31 "Ending inventory" inventoryAdjustment
    admitted <- either (Left . show . Admission.failureErrors) Right (Admission.admit adjusted)
    pure (Admission.admittedJournal admitted)
-- ANCHOR_END: adjustment

-- ANCHOR: closing
-- | Generate additional closing entries from the adjusted ledger, then admit them.
closedJanuary :: Book -> Either String Book
closedJanuary journal = do
    additions <- either (Left . show) Right (Closing.closingEntries (Journal.toAlg journal))
    let closed :: Book
        closed = journal .+ record 31 "Close January" additions
    admitted <- either (Left . show . Admission.failureErrors) Right (Admission.admit closed)
    pure (Admission.admittedJournal admitted)
-- ANCHOR_END: closing

-- ANCHOR: statement-display
-- | Print the balance-sheet lines and totals for this example.
printBalanceSheet :: Statements.FinancialStatements MoneyDecimal -> IO ()
printBalanceSheet statements = do
    let lines' = Statements._statementLines statements
        inSections sections =
            [ line | section <- sections
                   , line <- lines'
                   , Statements._lineSection line == section
            ]
        assets = inSections [Statements.CurrentAssetsSection, Statements.NoncurrentAssetsSection]
        liabilitiesAndEquity = inSections
            [ Statements.CurrentLiabilitiesSection
            , Statements.NoncurrentLiabilitiesSection
            , Statements.EquitySection
            ]
    mapM_ printLine assets
    printNamed "Total assets" (sum (map Statements._lineAmount assets))
    mapM_ printLine liabilitiesAndEquity
    printNamed "Total liabilities and equity"
        (sum (map Statements._lineAmount liabilitiesAndEquity))

-- | Keep the displayed section, account label, side, and magnitude visible.
printLine :: Statements.StatementLine MoneyDecimal -> IO ()
printLine line = putStrLn $ unwords
    [ show (Statements._lineSection line)
    , show (Statements._lineLabel line)
    , show (Statements._lineSide line)
    , showValue (Statements._lineAmount line)
    ]
-- ANCHOR_END: statement-display

-- ANCHOR: entity-balances
-- | Parent and subsidiary trial balances immediately after the February 1 contribution.
entityBalances :: Alg MoneyDecimal (HatBase AccountTitles)
               -> NonEmpty (Consolidation.TrialBalanceSource String MoneyDecimal)
entityBalances januaryEnd =
    Consolidation.TrialBalanceSource "shop" parent :|
    [Consolidation.TrialBalanceSource "subsidiary" subsidiary]
  where
    parent = bar (januaryEnd
        .+ (3000 .@ Not :< InvestmentInAssociate)
        .+ (3000 .@ Hat :< Deposits))
    subsidiary = (3000 .@ Not :< Cash)
               .+ (3000 .@ Not :< CapitalStock)
-- ANCHOR_END: entity-balances

-- ANCHOR: elimination
-- | Debit the subsidiary's capital and credit the parent's investment.
elimination :: Consolidation.WorksheetAdjustment String String MoneyDecimal
elimination = Consolidation.WorksheetAdjustment
    "investment against subsidiary capital"
    ("shop" :| ["subsidiary"])
    ((3000 .@ Hat :< CapitalStock)
  .+ (3000 .@ Hat :< InvestmentInAssociate))
-- ANCHOR_END: elimination

-- ANCHOR: linkage
-- | Link February's opening and reporting balances at the formation date.
linkage :: TrialBalance.AccountBalance MoneyDecimal -> Consolidation.WorksheetLinkage MoneyDecimal
linkage retained = Consolidation.WorksheetLinkage
    { Consolidation._profitOrLossNetIncome = Statements.PeriodBreakEven
    , Consolidation._profitOrLossNetIncomeAttributableToOwners = Statements.PeriodBreakEven
    , Consolidation._statementOfChangesNetIncomeAttributableToOwners = Statements.PeriodBreakEven
    , Consolidation._openingRetainedEarnings = retained
    , Consolidation._retainedEarningsDividends = 0
    , Consolidation._statementOfChangesClosingRetainedEarnings = retained
    , Consolidation._balanceSheetRetainedEarnings = retained
    , Consolidation._openingNonControllingInterests = TrialBalance.NoBalance
    , Consolidation._nonControllingInterestsPeriodShare = Statements.PeriodBreakEven
    , Consolidation._nonControllingInterestsDividends = 0
    , Consolidation._statementOfChangesClosingNonControllingInterests = TrialBalance.NoBalance
    , Consolidation._balanceSheetNonControllingInterests = TrialBalance.NoBalance
    }
-- ANCHOR_END: linkage

-- ANCHOR: results
main :: IO ()
main = do
    accepted <- require acceptedJanuary
    adjusted <- require (adjustedJanuary (Admission.admittedJournal accepted))
    closed <- require (closedJanuary adjusted)
    januaryEnd <- require (auditLedger (Journal.toAlg closed))
    let retained = TrialBalance.balanceFor RetainedEarnings
            (TrialBalance.accountBalances januaryEnd)
        input = Consolidation.WorksheetInput
            (entityBalances januaryEnd) [elimination] (linkage retained)
    worksheet <- require (Consolidation.validateConsolidationWorksheet input)
    let combined = Consolidation.combinedWorksheet worksheet
        net = bar combined
        context = (Statements.jcciSecondGradeContext Statements.Combined)
            { Statements._presentationProfile = Statements.CanonicalEnglish }
        balances = TrialBalance.accountBalances net
    unless (TrialBalance.balanceFor RetainedEarnings balances == retained) $
        ioError (userError "Combined retained earnings do not match the worksheet linkage")
    printNamed "Combined net totals (debit, credit)" (norm (decL net), norm (decR net))
    printNamed "Investment balance" (TrialBalance.balanceFor InvestmentInAssociate balances)
    printNamed "Group capital stock" (TrialBalance.balanceFor CapitalStock balances)
    statement <- require (Statements.present context TrialBalance.AfterClosing combined)
    putStrLn "\nOpening consolidated balance sheet at February 1 (yen)"
    printBalanceSheet statement
-- ANCHOR_END: results
