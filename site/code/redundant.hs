-- | Build a month's posting record from a withdrawal and ask questions about it.
-- Algebra operations retain, select, and explicitly total postings with account-and-unit bases.
-- ANCHOR: unit
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar)

-- | Currency coordinates for the withdrawal and exchange records.
data Unit
    = Yen       -- ^ Japanese yen.
    | Dollar    -- ^ US dollars.
    | Unit      -- ^ Inapplicable in an entry; unrestricted in a query.
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Unit where
    wildcard = Unit
-- ANCHOR_END: unit

-- ANCHOR: withdrawal
-- | January 10: withdraw 3,000 yen from Bank A's savings account into cash.
withdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
withdrawal = 3000 .@ Not :< (Cash, Yen)
          .+ 3000 .@ Hat :< (Deposits, Yen)
-- ANCHOR_END: withdrawal

-- ANCHOR: month
-- | January 15: withdraw another 2,000 yen.
secondWithdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
secondWithdrawal = 2000 .@ Not :< (Cash, Yen)
                .+ 2000 .@ Hat :< (Deposits, Yen)

-- | January 20: buy goods for 500 yen in cash, recorded as Purchases.
cashPayment :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
cashPayment = 500 .@ Hat :< (Cash, Yen)
           .+ 500 .@ Not :< (Purchases, Yen)

-- | January 25: exchange 1,500 yen in cash for 10 dollars, with no fee.
currencyExchange :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
currencyExchange = 1500 .@ Hat :< (Cash, Yen)
                .+   10 .@ Not :< (Cash, Dollar)

-- | Changes recorded during the month, with no opening balances.
month :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
month = withdrawal .+ secondWithdrawal .+ cashPayment .+ currencyExchange
-- ANCHOR_END: month

-- ANCHOR: queries
-- | Remaining cash by currency after canceling changes within each full base.
cashBalances :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
cashBalances = proj [HatNot :< (Cash, (.#))] (bar month)

-- | Original yen postings on every account and in both directions.
yenPostings :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
yenPostings = proj [HatNot :< ((.#), Yen)] month

-- | Original cash postings in yen, before aggregation.
yenCashPostings :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
yenCashPostings = proj [HatNot :< (Cash, Yen)] month
-- ANCHOR_END: queries

-- ANCHOR: results
-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Print the records and the answers to the cash and currency queries.
main :: IO ()
main = do
    printNamed "January 10 withdrawal" withdrawal
    printNamed "Twice the withdrawal" (2 .* withdrawal)
    printNamed "Withdrawal posting total in yen" (norm withdrawal)
    printNamed "Yen cash postings before aggregation" yenCashPostings
    printNamed "Cash balances by currency" cashBalances
    printNamed "Yen postings without cancellation" yenPostings
    printNamed "Yen posting total" (norm yenPostings)
    printNamed "Yen cash totals by direction" (compress yenCashPostings)
    printNamed "Yen cash residual amount" (projNetNorm [HatNot :< (Cash, Yen)] month)
    printNamed "Reverse the withdrawal directions" ((.^) withdrawal)
    printNamed "Withdrawal plus its hat, before bar" (withdrawal .+ (.^) withdrawal)
    printNamed "Withdrawal plus its hat, after bar" (bar (withdrawal .+ (.^) withdrawal))
-- ANCHOR_END: results
