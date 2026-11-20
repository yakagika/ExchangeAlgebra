-- ANCHOR: setup
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar)
import ExchangeAlgebra.Journal ((.|))
import qualified ExchangeAlgebra.Journal as Journal

-- | Currency elements for the withdrawal and exchange records.
data Unit
    = Yen       -- ^ Japanese yen.
    | Dollar    -- ^ US dollars.
    | Unit      -- ^ Inapplicable in an entry; unrestricted in a query.
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Unit where
    wildcard = Unit

-- | Account access for the custom account-and-currency base.
instance ExBaseClass (HatBase (AccountTitles, Unit)) where
    getAccountTitle (_ :< (account, _)) = account
    setAccountTitle (direction :< (_, unit)) account = direction :< (account, unit)
-- ANCHOR_END: setup

-- ANCHOR: note
-- | A year, month, and day, compared in calendar order for the valid dates used here.
-- This alias does not validate dates or distinguish them from other integer tuples.
type EntryDate = (Integer, Int, Int)

-- | Date and description shared by all terms in one journal entry.
data EntryNote = EntryNote
    { entryDate        :: EntryDate -- ^ Calendar date of the transaction.
    , entryDescription :: String    -- ^ Human-readable description.
    } deriving (Eq, Ord, Show, Generic, Hashable)

instance Journal.Note EntryNote where
    plank = EntryNote (0, 0, 0) ""

-- | Metadata for the withdrawal; the blank note is reserved for aggregation.
withdrawalNote :: EntryNote
withdrawalNote = EntryNote (2026, 1, 10) "Withdrawal from Bank A"
-- ANCHOR_END: note

-- ANCHOR: entries
-- | January 10: withdraw 3,000 yen from Bank A's savings account into cash.
withdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
withdrawal = 3000 .@ Not :< (Cash, Yen)
          .+ 3000 .@ Hat :< (Deposits, Yen)

-- | January 15: withdraw another 2,000 yen.
secondWithdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
secondWithdrawal = 2000 .@ Not :< (Cash, Yen)
                .+ 2000 .@ Hat :< (Deposits, Yen)

-- | January 20: buy goods for 500 yen in cash.
cashPayment :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
cashPayment = 500 .@ Hat :< (Cash, Yen)
           .+ 500 .@ Not :< (Purchases, Yen)

-- | January 25: exchange 1,500 yen for 10 dollars without a fee.
currencyExchange :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
currencyExchange = 1500 .@ Hat :< (Cash, Yen)
                .+   10 .@ Not :< (Cash, Dollar)

-- | February 1: buy goods for 400 yen in cash.
februaryPurchase :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
februaryPurchase = 400 .@ Not :< (Purchases, Yen)
                .+ 400 .@ Hat :< (Cash, Yen)

-- | Metadata for the February purchase, excluded by the January date range.
februaryNote :: EntryNote
februaryNote = EntryNote (2026, 2, 1) "Purchase of goods"

-- | January transactions and one February purchase, each with a date and description.
transactions :: Journal.Journal EntryNote MoneyDecimal (HatBase (AccountTitles, Unit))
transactions = (withdrawal .| withdrawalNote)
            .+ (secondWithdrawal .| EntryNote (2026, 1, 15) "Withdrawal from Bank A")
            .+ (cashPayment .| EntryNote (2026, 1, 20) "Purchase of goods")
            .+ (currencyExchange .| EntryNote (2026, 1, 25) "Yen-dollar exchange")
            .+ (februaryPurchase .| februaryNote)
-- ANCHOR_END: entries

-- ANCHOR: queries
-- | Select entries with an inclusive lower and exclusive upper date.
isInJanuary :: EntryNote -> Alg MoneyDecimal (HatBase (AccountTitles, Unit)) -> Bool
isInJanuary note _ = (2026, 1, 1) <= entryDate note && entryDate note < (2026, 2, 1)

-- | Keep the notes while selecting the dates in January.
january :: Journal.Journal EntryNote MoneyDecimal (HatBase (AccountTitles, Unit))
january = Journal.filterWithNote isInJanuary transactions
-- ANCHOR_END: queries

-- ANCHOR: results
-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Ask questions by note, base, and date, then combine notes for net balances.
main :: IO ()
main = do
    printNamed "February 1 entry before date selection"
        (Journal.projWithNote [februaryNote] transactions)
    printNamed "February 1 entry after January selection"
        (Journal.projWithNote [februaryNote] january)
    printNamed "January 10 entry" (Journal.projWithNote [withdrawalNote] transactions)
    printNamed "January 10 cash terms"
        (Journal.projWithNoteBase [withdrawalNote] [HatNot :< (Cash, Yen)] transactions)
    printNamed "January yen cash terms"
        (Journal.projWithBase [HatNot :< (Cash, Yen)] january)
    printNamed "January gross yen cash amount"
        (norm (Journal.projWithBase [HatNot :< (Cash, Yen)] january))
    printNamed "January cash balances by currency"
        (proj [HatNot :< (Cash, (.#))] (bar (Journal.toAlg january)))
    printNamed "Gathered January record" (Journal.gather Journal.plank january)
    printNamed "Bar across notes at plank" (bar january)
-- ANCHOR_END: results

