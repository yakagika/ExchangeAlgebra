-- ANCHOR: setup
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar, map)
import qualified ExchangeAlgebra.Algebra.Core as Algebra
import ExchangeAlgebra.Journal ((.|))
import qualified ExchangeAlgebra.Journal as Journal
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import qualified ExchangeAlgebra.Accounting.TrialBalance as TrialBalance

-- | Currency element retained in the source journal.
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

-- | One entry with account and currency elements; the alias adds no validation.
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

-- ANCHOR: display
-- | Print a labeled result, retaining the library's diagnostic constructors.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Stop the example on an unexpected checked failure.
require :: Show e => Either e a -> IO a
require = either (ioError . userError . show) pure

-- | Show gross debit and credit totals in yen, without netting the source.
printTotals :: Entry -> IO ()
printTotals ledger = do
    printNamed "Posting totals (debit, credit)" (norm (decL ledger), norm (decR ledger))
    printNamed "Balanced" (balance ledger)
-- ANCHOR_END: display

-- ANCHOR: trial
-- | Accounts used in the shop, ordered for repeatable output including zero balances.
accounts :: [AccountTitles]
accounts = [Cash, Deposits, PrepaidExpenses, MerchandiseInventory,
            Purchases, RentExpense, Sales, CapitalStock, RetainedEarnings]

-- | Drop the currency element only after checking every posting is in yen.
-- This reporting projection removes the currency element; it does not net postings.
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

-- | Print account turnover, net balances, balance totals, and accounting findings.
printTrial :: TrialBalance.TrialBalanceStage -> Entry -> IO ()
printTrial stage ledger = do
    reporting <- require (auditLedger ledger)
    let balances = TrialBalance.accountBalances reporting
        pairs :: [(MoneyDecimal, MoneyDecimal)]
        pairs = map (TrialBalance.balancePair . (`TrialBalance.balanceFor` balances)) accounts
        totals :: (MoneyDecimal, MoneyDecimal)
        totals = foldr TrialBalance.addPair (0, 0) pairs
        row :: AccountTitles -> IO ()
        row account = do
            let selected = projByAccountTitle account ledger
            printNamed (show account)
                (norm (decL selected), norm (decR selected),
                 TrialBalance.balanceFor account balances)
    putStrLn "Account: (debit turnover, credit turnover, balance)"
    mapM_ row accounts
    printNamed "Balance totals (debit, credit)" totals
    printTotals ledger
    printNamed "Findings" $ TrialBalance.trialBalanceFindings
        (TrialBalance.TrialBalanceInput reporting stage mempty [] mempty)
-- ANCHOR_END: trial

-- ANCHOR: results
-- | Run the January procedure and print its checked results.
main :: IO ()
main = do
    accepted <- require acceptedJanuary
    let journal = Admission.admittedJournal accepted
    printTrial TrialBalance.BeforeClosing (Journal.toAlg journal)
    let withoutOpening = Journal.filterWithNote (\note _ -> note /= EntryNote (2026, 1, 1)
            "Opening capital") journal
    incomplete <- require (Admission.admit withoutOpening)
    putStrLn "Without opening balances:"
    printTrial TrialBalance.BeforeClosing
        $ Journal.toAlg
        $ Admission.admittedJournal incomplete
-- ANCHOR_END: results
