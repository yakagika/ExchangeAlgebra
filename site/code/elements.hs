-- | Add a bank element and query the expanded bases.

-- ANCHOR: program
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar)
-- ANCHOR: query-import
import ExchangeAlgebra.Algebra.Element (matchesQuery)
-- ANCHOR_END: query-import

data Unit = Yen | Dollar | Unit
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Unit where
    wildcard = Unit

-- ANCHOR: bank
-- | The bank where a deposit is held.
data Bank = BankA | BankB | Bank
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Bank where
    wildcard = Bank
-- ANCHOR_END: bank

-- ANCHOR: bank-bases
-- | Cash has no applicable bank.
cashBase :: (AccountTitles, Unit, Bank)
cashBase = (Cash, Yen, Bank)

-- | The deposit is held at Bank A.
depositBase :: (AccountTitles, Unit, Bank)
depositBase = (Deposits, Yen, BankA)
-- ANCHOR_END: bank-bases

-- ANCHOR: bank-withdrawal
-- | The withdrawal using bases with a bank element.
bankWithdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit, Bank))
bankWithdrawal = 3000 .@ Not :< cashBase
              .+ 3000 .@ Hat :< depositBase
-- ANCHOR_END: bank-withdrawal

-- ANCHOR: bank-queries
-- | Select terms on accounts at Bank A.
bankATerms :: Alg MoneyDecimal (HatBase (AccountTitles, Unit, Bank))
bankATerms = proj [HatNot :< ((.#), (.#), BankA)] bankWithdrawal

-- | Check the cash base with an unrestricted or a specific bank query.
cashMatchesAnyBank, cashMatchesBankA :: Bool
cashMatchesAnyBank = matchesQuery (Cash, Yen, (.#)) cashBase
cashMatchesBankA = matchesQuery (Cash, Yen, BankA) cashBase
-- ANCHOR_END: bank-queries

-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Print the deposit terms selected by bank and the cash base checks.
main :: IO ()
main = do
    printNamed "Bank A terms" bankATerms
    printNamed "Any-bank cash query matches cash without a bank" cashMatchesAnyBank
    printNamed "Bank A cash query matches cash without a bank" cashMatchesBankA
-- ANCHOR_END: program
