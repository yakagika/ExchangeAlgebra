-- | Query branch numbers and times of day with Wild.
-- Each group of definitions is included after the matching note in the Reference.
-- ANCHOR: program
module Main where

import Data.Time.LocalTime (TimeOfDay (TimeOfDay), midnight)
import ExchangeAlgebra

-- ANCHOR: int
-- | Cash at branches 1 and 2.
branches :: Alg MoneyDecimal (HatBase (AccountTitles, Wild Int))
branches = 100 .@ Not :< (Cash, Exactly 1)
        .+ 200 .@ Not :< (Cash, Exactly 2)

-- | Total the cash at branch 1 and at any branch.
branchQueries :: IO ()
branchQueries = do
    putStrLn $ "Branch 1: " ++ show (norm (proj [HatNot :< (Cash, Exactly 1)] branches))
    putStrLn $ "Any branch: " ++ show (norm (proj [HatNot :< (Cash, Wild)] branches))
-- ANCHOR_END: int

-- ANCHOR: wild-time
-- | Cash received at 00:00 and at 13:00.
byWildTime :: Alg MoneyDecimal (HatBase (AccountTitles, Wild TimeOfDay))
byWildTime = 100 .@ Not :< (Cash, Exactly midnight)
          .+ 200 .@ Not :< (Cash, Exactly (TimeOfDay 13 0 0))

-- | Total the cash at 00:00, at 13:00, and at any time.
wildTimeQueries :: IO ()
wildTimeQueries = do
    putStrLn $ "Wild TimeOfDay, query Exactly 00:00: "
        ++ show (norm (proj [HatNot :< (Cash, Exactly midnight)] byWildTime))
    putStrLn $ "Wild TimeOfDay, query Exactly 13:00: "
        ++ show (norm (proj [HatNot :< (Cash, Exactly (TimeOfDay 13 0 0))] byWildTime))
    putStrLn $ "Wild TimeOfDay, query Wild: "
        ++ show (norm (proj [HatNot :< (Cash, Wild)] byWildTime))
-- ANCHOR_END: wild-time

main :: IO ()
main = do
    branchQueries
    wildTimeQueries
-- ANCHOR_END: program
