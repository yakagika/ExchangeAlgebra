-- ANCHOR: setup
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar)

-- | Currency elements for the withdrawal and exchange records.
data Unit
    = Yen       -- ^ Japanese yen.
    | Dollar    -- ^ US dollars.
    | Unit      -- ^ Inapplicable in an entry; unrestricted in a query.
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Unit where
    wildcard = Unit
-- ANCHOR_END: setup

-- ANCHOR: owner
-- | Owners of the two accounting records.
data Owner
    = Business  -- ^ The business withdrawing its own deposit.
    | BankA     -- ^ The bank paying out the withdrawal.
    | Owner     -- ^ Inapplicable in an entry; unrestricted in a query.
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Owner where
    wildcard = Owner

-- | Account access for bases with an owner and a currency.
instance ExBaseClass (HatBase (AccountTitles, Owner, Unit)) where
    getAccountTitle (_ :< (account, _, _)) = account
    setAccountTitle (direction :< (_, owner, unit)) account = direction :< (account, owner, unit)
-- ANCHOR_END: owner

-- ANCHOR: records
-- | The business receives cash and reduces its deposit asset.
businessRecord :: Alg MoneyDecimal (HatBase (AccountTitles, Owner, Unit))
businessRecord = 3000 .@ Not :< (Cash, Business, Yen)
              .+ 3000 .@ Hat :< (Deposits, Business, Yen)

-- | Bank A reduces its deposit liability and pays out cash.
bankRecord :: Alg MoneyDecimal (HatBase (AccountTitles, Owner, Unit))
bankRecord = 3000 .@ Hat :< (DepositPayable, BankA, Yen)
          .+ 3000 .@ Hat :< (Cash, BankA, Yen)

-- | Retain both owners' records in one algebra without canceling between owners.
combined :: Alg MoneyDecimal (HatBase (AccountTitles, Owner, Unit))
combined = businessRecord .+ bankRecord
-- ANCHOR_END: records

-- ANCHOR: queries
-- | Select one owner's complete accounting record.
recordOf :: Owner -> Alg MoneyDecimal (HatBase (AccountTitles, Owner, Unit))
recordOf owner = proj [HatNot :< ((.#), owner, (.#))] combined

-- | Select the business deposit asset and the bank's corresponding liability.
depositPair :: Alg MoneyDecimal (HatBase (AccountTitles, Owner, Unit))
depositPair = proj [ HatNot :< (Deposits, Business, Yen)
                   , HatNot :< (DepositPayable, BankA, Yen) ] combined
-- ANCHOR_END: queries

-- ANCHOR: results
-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Inspect each owner and the corresponding cash and deposit movements.
main :: IO ()
main = do
    printNamed "Business record" (recordOf Business)
    printNamed "Bank A record" (recordOf BankA)
    printNamed "Business record is balanced" (balance (recordOf Business))
    printNamed "Bank A record is balanced" (balance (recordOf BankA))
    printNamed "Combined record is balanced" (balance combined)
    printNamed "Bank liability decrease side" (whichSide (Hat :< (DepositPayable, BankA, Yen)))
    printNamed "Corresponding deposit changes" depositPair
    printNamed "Cash changes with owners retained" (proj [HatNot :< (Cash, (.#), Yen)] combined)
    printNamed "Bar retains separate owners" (bar combined)
-- ANCHOR_END: results

