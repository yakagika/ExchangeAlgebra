-- | Build hat bases with a bank element and define account access.

-- ANCHOR: program
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar)

data Unit = Yen | Dollar | Unit
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Unit where
    wildcard = Unit

-- | The bank where a deposit is held.
data Bank = BankA | BankB | Bank
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Bank where
    wildcard = Bank

-- ANCHOR: bank-hat-bases
-- | Cash increases with no applicable bank.
cashIncrease :: HatBase (AccountTitles, Unit, Bank)
cashIncrease = Not :< (Cash, Yen, Bank)

-- | Deposits at Bank A decrease.
depositDecrease :: HatBase (AccountTitles, Unit, Bank)
depositDecrease = Hat :< (Deposits, Yen, BankA)
-- ANCHOR_END: bank-hat-bases

-- ANCHOR: bank-instance
-- | Read or replace the title in a base that includes a bank.
instance ExBaseClass (HatBase (AccountTitles, Unit, Bank)) where
    getAccountTitle (_ :< (account, _, _)) = account
    setAccountTitle (direction :< (_, unit, bank)) account = direction :< (account, unit, bank)
-- ANCHOR_END: bank-instance

-- ANCHOR: single-element
-- | Permit Unit itself, as well as tuples containing it, to be a base.
instance BaseClass Unit where

-- | A base with one element, used to compare the instance requirements.
yenOnly :: HatBase Unit
yenOnly = Not :< Yen
-- ANCHOR_END: single-element

-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- ANCHOR: results
-- | Inspect the direction, account, unit, and bank of the example bases.
main :: IO ()
main = do
    printNamed "Cash increase" cashIncrease
    printNamed "Deposit decrease" depositDecrease
    printNamed "Elements of the cash increase" (base cashIncrease)
    printNamed "Direction of the deposit change" (hat depositDecrease)
    printNamed "Reverse the cash direction" (revHat cashIncrease)
    printNamed "Rebuild the deposit decrease"
        (merge (hat depositDecrease) (base depositDecrease) :: HatBase (AccountTitles, Unit, Bank))
    printNamed "Replace the account title" (setAccountTitle depositDecrease Cash)
    printNamed "Single-element yen base" yenOnly
-- ANCHOR_END: results
-- ANCHOR_END: program
