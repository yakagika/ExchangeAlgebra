-- | Record the January 10 withdrawal and read its debit and credit sides.
-- The elements form a base; the ExBaseClass instance locates its account title.
-- ANCHOR: program
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar)

-- | The unit of an amount.
data Unit = Yen | Dollar | Unit
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Unit where
    wildcard = Unit

-- | Read and replace the account title in a base.
instance ExBaseClass (HatBase (AccountTitles, Unit)) where
    getAccountTitle (_ :< (account, _)) = account
    setAccountTitle (direction :< (_, unit)) account = direction :< (account, unit)

-- | January 10: withdraw 3,000 yen from a savings account at Bank A.
withdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
withdrawal = 3000 .@ Not :< (Cash, Yen)
          .+ 3000 .@ Hat :< (Deposits, Yen)

-- | Print the entry, its debit terms, its credit terms, and the balance check.
main :: IO ()
main = do
    print withdrawal
    print (decL withdrawal)
    print (decR withdrawal)
    print (balance withdrawal)
-- ANCHOR_END: program
