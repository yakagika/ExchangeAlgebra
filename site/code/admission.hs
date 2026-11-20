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
-- | Attach a January date and description after constructing all terms.
record :: Int -> String -> Entry -> Book
record day description algebra = algebra .| EntryNote (2026, 1, day) description

-- | January 1: capital paid into Bank A's savings account.
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

-- | Check the converted journal without changing its notes or term sequences.
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

-- ANCHOR: results
-- | Run the January procedure and print its checked results.
main :: IO ()
main = do
    accepted <- require acceptedJanuary
    let journal = Admission.admittedJournal accepted
    putStrLn "January journal admitted"
    printTotals (Journal.toAlg journal)
    let balances = bar $ Journal.toAlg journal
    printNamed "Balance totals (debit, credit)" (norm (decL balances), norm (decR balances))
    let bad = record 28 "Unbalanced sale"
            (2000 .@ Not :< (Cash, Yen) .+ 1900 .@ Not :< (Sales, Yen))
    case Admission.admit bad of
        Left failure -> printNamed "Rejected" (Admission.failureErrors failure)
        Right _ -> ioError (userError "Expected the unbalanced sale to be rejected")
-- ANCHOR_END: results
