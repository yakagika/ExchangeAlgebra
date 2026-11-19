-- | Add directions to the cash and deposit coordinates of the withdrawal.
-- BaseClass and HatBaseClass supply the base structure used by posting construction.
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

-- ANCHOR: bases
-- | Cash increases in the withdrawal.
cashIncrease :: HatBase (AccountTitles, Unit)
cashIncrease = Not :< (Cash, Yen)

-- | The savings deposit decreases in the withdrawal.
depositDecrease :: HatBase (AccountTitles, Unit)
depositDecrease = Hat :< (Deposits, Yen)
-- ANCHOR_END: bases

-- ANCHOR: single-axis
-- | Permit Unit itself, as well as tuples containing it, to be a base.
instance BaseClass Unit where

-- | A one-axis base used only to contrast the instance requirements.
yenOnly :: HatBase Unit
yenOnly = Not :< Yen
-- ANCHOR_END: single-axis

-- ANCHOR: results
-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Print the withdrawal's bases and inspect their directions.
main :: IO ()
main = do
    printNamed "Cash increase" cashIncrease
    printNamed "Deposit decrease" depositDecrease
    printNamed "Account and unit of the cash increase" (base cashIncrease)
    printNamed "Direction of the deposit change" (hat depositDecrease)
    printNamed "Reverse the cash direction" (revHat cashIncrease)
    printNamed "Rebuild the deposit decrease"
        (merge Hat (Deposits, Yen) :: HatBase (AccountTitles, Unit))
    printNamed "Single-axis yen base" yenOnly
-- ANCHOR_END: results
