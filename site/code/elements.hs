-- | Select the account and currency elements of a January 10 withdrawal.
-- The custom currency type has an Element instance. Main queries the bases.
-- ANCHOR: unit
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar)
import ExchangeAlgebra.Algebra.Element (matchesQuery)

-- | Currency elements for the withdrawal and exchange records.
data Unit
    = Yen       -- ^ Japanese yen.
    | Dollar    -- ^ US dollars.
    | Unit      -- ^ Inapplicable in an entry; unrestricted in a query.
    deriving (Eq, Ord, Show, Generic, Hashable)

instance Element Unit where
    wildcard = Unit
-- ANCHOR_END: unit

-- ANCHOR: base-elements
-- | The two account-and-unit bases affected by the withdrawal.
withdrawalElements :: [(AccountTitles, Unit)]
withdrawalElements = [(Cash, Yen), (Deposits, Yen)]

-- | Select cash in any unit.
cashElements :: [(AccountTitles, Unit)]
cashElements = Prelude.filter (matchesQuery (Cash, (.#))) withdrawalElements

-- | A hypothetical cash base with no applicable currency element.
cashWithoutUnit :: (AccountTitles, Unit)
cashWithoutUnit = (Cash, Unit)
-- ANCHOR_END: base-elements

-- ANCHOR: results
-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Print the withdrawal bases and named query results.
main :: IO ()
main = do
    printNamed "Withdrawal base elements" withdrawalElements
    printNamed "Cash in any unit" cashElements
    printNamed "Any-unit cash query matches cash without a unit"
        (matchesQuery (Cash, Unit) cashWithoutUnit)
    printNamed "Yen cash query matches cash without a unit"
        (matchesQuery (Cash, Yen) cashWithoutUnit)
-- ANCHOR_END: results
