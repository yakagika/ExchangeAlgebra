-- ANCHOR: program
module Main where

import Data.Time.Calendar (Day (ModifiedJulianDay))
import ExchangeAlgebra

-- ANCHOR: record-date
-- | Record a cash increase on 1858-11-17.
record :: Alg MoneyDecimal (HatBase (AccountTitles, Wild Day))
record = 100 .@ Not :< (Cash, Exactly (ModifiedJulianDay 0))
-- ANCHOR_END: record-date

-- ANCHOR: date-queries
main :: IO ()
main = do
    putStrLn $ "Recorded term: " ++ show record
    putStrLn $ "Any date: " ++ show (norm (proj [HatNot :< (Cash, Wild)] record))
    putStrLn $ "1858-11-17: "
        ++ show (norm (proj [HatNot :< (Cash, Exactly (ModifiedJulianDay 0))] record))
    putStrLn $ "1858-11-18: "
        ++ show (norm (proj [HatNot :< (Cash, Exactly (ModifiedJulianDay 1))] record))
-- ANCHOR_END: date-queries
-- ANCHOR_END: program
