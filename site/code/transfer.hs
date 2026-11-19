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

-- ANCHOR: payment
-- | January 10: withdraw 3,000 yen from Bank A's savings account into cash.
withdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
withdrawal = 3000 .@ Not :< (Cash, Yen)
          .+ 3000 .@ Hat :< (Deposits, Yen)

-- | January 30: pay 1,200 yen for January and February rent, initially expensed.
rentPayment :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
rentPayment = 1200 .@ Not :< (RentExpense, Yen)
           .+ 1200 .@ Hat :< (Cash, Yen)

-- | Recorded changes before deferring the February share of rent.
beforeTransfer :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
beforeTransfer = withdrawal .+ rentPayment
-- ANCHOR_END: payment

-- ANCHOR: table
-- | Move half the rent balance to prepaid expenses, retaining its currency.
rentTable :: Either (TransferTableError (HatBase (AccountTitles, Unit)))
                    (TransferTable (HatBase (AccountTitles, Unit)))
rentTable = mkTransferTable
    [ (RentExpense, (.#)) .-> (PrepaidExpenses, (.#)) |% Portion 1 2 ]
-- ANCHOR_END: table

-- ANCHOR: results
-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Generate and retain the adjusting entry, then inspect the net balances.
main :: IO ()
main = case rentTable of
    Left failure -> ioError (userError (show failure))
    Right table -> case transferEntries table beforeTransfer of
        Left failure -> ioError (userError (show failure))
        Right additions -> do
            printNamed "Rent payment" rentPayment
            printNamed "Additional transfer entry" additions
            printNamed "Transfer entry is balanced" (balance additions)
            printNamed "Record retaining original and added postings" (beforeTransfer .+ additions)
            printNamed "Balances after transfer" (bar (beforeTransfer .+ additions))
            printNamed "Record is balanced" (balance (beforeTransfer .+ additions))
-- ANCHOR_END: results

