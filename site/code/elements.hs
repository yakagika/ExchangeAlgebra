-- | Select the account and currency coordinates of a January 10 withdrawal.
-- The Algebra element instance defines the custom unit axis; main runs coordinate queries.
-- ANCHOR: unit
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar)
import ExchangeAlgebra.Algebra.Element (matchesQuery)

-- | Currency coordinates for the withdrawal and exchange records.
data Unit
    = Yen       -- ^ Japanese yen.
    | Dollar    -- ^ US dollars.
    | Unit      -- ^ Inapplicable in an entry; unrestricted in a query.
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Unit where
    wildcard = Unit
-- ANCHOR_END: unit

-- ANCHOR: coordinates
-- | The two account-and-unit coordinates affected by the withdrawal.
withdrawalCoordinates :: [(AccountTitles, Unit)]
withdrawalCoordinates = [(Cash, Yen), (Deposits, Yen)]

-- | Select cash coordinates without restricting the unit.
cashCoordinates :: [(AccountTitles, Unit)]
cashCoordinates = Prelude.filter (matchesQuery (Cash, (.#))) withdrawalCoordinates

-- | A hypothetical cash coordinate with no applicable currency axis.
cashWithoutUnit :: (AccountTitles, Unit)
cashWithoutUnit = (Cash, Unit)
-- ANCHOR_END: coordinates

-- ANCHOR: results
-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Print the withdrawal coordinates and named query results.
main :: IO ()
main = do
    printNamed "Withdrawal coordinates" withdrawalCoordinates
    printNamed "Cash in any unit" cashCoordinates
    printNamed "Any-unit cash query matches cash without a unit"
        (matchesQuery (Cash, Unit) cashWithoutUnit)
    printNamed "Yen cash query matches cash without a unit"
        (matchesQuery (Cash, Yen) cashWithoutUnit)
-- ANCHOR_END: results
