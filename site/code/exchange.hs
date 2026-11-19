-- ANCHOR: setup
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

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

-- | Account access for the custom account-and-currency base.
instance ExBaseClass (HatBase (AccountTitles, Unit)) where
    getAccountTitle (_ :< (account, _)) = account
    setAccountTitle (direction :< (_, unit)) account = direction :< (account, unit)
-- ANCHOR_END: setup

-- ANCHOR: withdrawal
-- | January 10: withdraw 3,000 yen from Bank A's savings account into cash.
withdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
withdrawal = 3000 .@ Not :< (Cash, Yen)
          .+ 3000 .@ Hat :< (Deposits, Yen)
-- ANCHOR_END: withdrawal

-- ANCHOR: results
-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Recover the withdrawal's debit and credit sides and classify its bases.
main :: IO ()
main = do
    printNamed "Debit postings" (decL withdrawal)
    printNamed "Credit postings" (decR withdrawal)
    printNamed "Debit total in yen" (norm (decL withdrawal))
    printNamed "Credit total in yen" (norm (decR withdrawal))
    printNamed "Withdrawal is balanced" (balance withdrawal)
    printNamed "Debit-credit difference" (diffRL withdrawal)
    printNamed "Cash division" (whatDiv (Not :< (Cash, Yen)))
    printNamed "Cash increase side" (whichSide (Not :< (Cash, Yen)))
    printNamed "Deposit decrease side" (whichSide (Hat :< (Deposits, Yen)))
    printNamed "Cash PIMO class" (whatPIMO (Not :< (Cash, Yen)))
    printNamed "Cash maturity class" (fixedCurrent (Not :< (Cash, Yen)))
-- ANCHOR_END: results

