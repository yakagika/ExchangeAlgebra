-- ANCHOR: setup
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Text.Printf (printf)
import Text.Read (readMaybe)

import ExchangeAlgebra hiding (Yen, Dollar, map)
import qualified ExchangeAlgebra.Algebra.Core as Algebra
import ExchangeAlgebra.Journal ((.|))
import qualified ExchangeAlgebra.Journal as Journal
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import qualified ExchangeAlgebra.Accounting.Entries as Entries
import qualified ExchangeAlgebra.Accounting.Closing as Closing

import qualified ExchangeAlgebra.Accounting.TrialBalance as TrialBalance
import qualified ExchangeAlgebra.Accounting.Statements as Statements
import qualified ExchangeAlgebra.IO.Output.Statements as Output

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
record day description algebra = algebra .| EntryNote (2026, 1, day) description

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

-- ANCHOR: statement-context
-- | Choose English labels for the shop's standalone statements.
context :: Statements.ReportingContext MoneyDecimal
context = (Statements.jcciSecondGradeContext Statements.Standalone)
    { Statements._presentationProfile = Statements.CanonicalEnglish }

-- | Present the adjusted Purchases balance under the cost-of-sales title.
incomeContext :: Statements.ReportingContext MoneyDecimal
incomeContext = context
    { Statements._presentationRelabels =
        [Statements.PresentationRelabel Purchases SalesCost "Periodic inventory adjustment"]
    }
-- ANCHOR_END: statement-context

-- ANCHOR: book-rows
-- | Select one date while retaining the postings from every matching note.
onJanuaryDay :: Int -> Book -> Entry
onJanuaryDay day = Journal.toAlg . Journal.filterWithNote matches
  where
    matches (EntryNote date _) _ = date == (2026, 1, day)

-- | Put the note's date on each posting in the day selected for the dated reports.
printDatedBooks :: Book -> IO ()
printDatedBooks journal = do
    dated <- traverse datedPostings [1 .. 30]
    let postings = foldr (.+) mempty dated
        postingDay (_ :< (_, day)) = day
    putStrLn "Journal by day"
    mapM_ print (Output.journalRows postings postingDay)
    putStrLn "\nGeneral ledger (Cash)"
    mapM_ print (Output.accountLedgerRows [Cash] postings postingDay)
  where
    datedPostings day = do
        let date = printf "2026-01-%02d" day
        calendarDay <- require $ maybe (Left ("Invalid date " ++ date)) Right (readMaybe date)
        ledger <- require (auditLedger (onJanuaryDay day journal))
        pure $ Algebra.mapPosting
            (\amount (direction :< account) -> (amount, direction :< (account, calendarDay)))
            ledger
-- ANCHOR_END: book-rows

-- ANCHOR: trial-and-worksheet-rows
-- | Use the January 31 adjusting entries separately from the pre-adjustment ledger.
printTrialAndWorksheet :: Book -> Book -> Book -> IO ()
printTrialAndWorksheet original adjusted closed = do
    beforeAdjusting <- require (auditLedger (Journal.toAlg original))
    adjustments <- require (auditLedger (onJanuaryDay 31 adjusted))
    afterClosing <- require (auditLedger (Journal.toAlg closed))
    putStrLn "\nPre-adjustment compound trial balance"
    mapM_ print (Output.compoundTrialBalanceRows beforeAdjusting)
    putStrLn "\nEight-column worksheet"
    mapM_ print (Output.worksheetRows beforeAdjusting adjustments)
    putStrLn "\nPost-closing trial balance"
    mapM_ print (Output.postClosingTrialBalanceRows afterClosing)
-- ANCHOR_END: trial-and-worksheet-rows

-- ANCHOR: statement-rows
-- | Select the financial statement sections appropriate to each stage.
printStatementRows :: Book -> Book -> IO ()
printStatementRows adjusted closed = do
    beforeClosing <- require (auditLedger (Journal.toAlg adjusted))
    afterClosing <- require (auditLedger (Journal.toAlg closed))
    income <- require (Statements.present incomeContext TrialBalance.BeforeClosing beforeClosing)
    position <- require (Statements.present context TrialBalance.AfterClosing afterClosing)
    incomeRows <- require $ Output.financialStatementRows
        [Statements.RevenueSection, Statements.ExpenseSection] income
    balanceRows <- require $ Output.financialStatementRows
        [ Statements.CurrentAssetsSection
        , Statements.NoncurrentAssetsSection
        , Statements.CurrentLiabilitiesSection
        , Statements.NoncurrentLiabilitiesSection
        , Statements.EquitySection
        ] position
    putStrLn "\nIncome statement for January"
    mapM_ print incomeRows
    putStrLn "\nBalance sheet at January 31"
    mapM_ print balanceRows
-- ANCHOR_END: statement-rows

-- ANCHOR: results
main :: IO ()
main = do
    accepted <- require acceptedJanuary
    let original = Admission.admittedJournal accepted
    adjusted <- require (adjustedJanuary original)
    closed <- require (closedJanuary adjusted)
    putStrLn "January reports (yen)"
    printDatedBooks original
    printTrialAndWorksheet original adjusted closed
    printStatementRows adjusted closed
-- ANCHOR_END: results
