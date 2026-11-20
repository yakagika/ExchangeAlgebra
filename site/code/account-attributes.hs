module Main where

import ExchangeAlgebra
    ( AccountSemantics(..)
    , AccountSpec(..)
    , AccountTitles(..)
    , accountAliases
    , accountSemantics
    , accountSpec
    )
import System.Exit (die)

main :: IO ()
main = do
    describe Cash
    describe AccumulatedDepreciation
    mapM_ putStrLn (map (read . show) (accountAliases Cash) :: [String])

describe :: AccountTitles -> IO ()
describe title = case (accountSpec title, accountSemantics title) of
    (Just spec, Just semantics) ->
        mapM_ putStrLn
            [ show title
            , show (asDivision spec, asClosing spec, asIsContra spec, asFixedCurrent spec)
            , show (asemRoles semantics, asemPostingCapability semantics)
            , show (asemDivisionSemantics semantics, asemHomeSideSemantics semantics)
            , show (asemReportingEligibility semantics)
            ]
    _ -> die "Missing account attributes"
