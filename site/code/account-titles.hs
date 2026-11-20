module Main where

import Control.Monad (unless)
import Data.List (intercalate)
import ExchangeAlgebra
    ( AccountDivision(..)
    , AccountRole(..)
    , AccountSemantics(..)
    , AccountSpec(..)
    , accountSemantics
    , accountSpec
    , concreteAccountTitles
    )
import System.Exit (die)

main :: IO ()
main = do
    let tables =
            [ (anchor, accountRows concreteAccountTitles accountSpec accountSemantics division)
            | (anchor, division) <- divisions
            ]
    unless (sum (map (length . snd) tables) == length concreteAccountTitles) $
        die "The division tables do not cover all concrete account titles."
    mapM_ (uncurry printTable) tables

divisions :: [(String, AccountDivision)]
divisions =
    [ ("assets", Assets)
    , ("liabilities", Liability)
    , ("equity", Equity)
    , ("revenue", Revenue)
    , ("expenses", Cost)
    ]

-- ANCHOR: rows
accountRows
    :: Show title
    => [title]
    -> (title -> Maybe AccountSpec)
    -> (title -> Maybe AccountSemantics)
    -> AccountDivision
    -> [String]
accountRows titles lookupSpec lookupSemantics division =
    [ accountRow title spec semantics
    | title <- titles
    , Just spec <- [lookupSpec title]
    , asDivision spec == division
    , Just semantics <- [lookupSemantics title]
    ]
-- ANCHOR_END: rows

-- Text shows its contents as a String literal. Reading that literal lets the
-- example use the packages exposed by the manual checker.
accountRow :: Show title => title -> AccountSpec -> AccountSemantics -> String
accountRow title spec semantics = "<tr>" ++ concat cells ++ "</tr>"
  where
    cells =
        [ "<td><code>" ++ escapeHtml (show title) ++ "</code></td>"
        , cell (read (show (asNameEn spec)))
        , cell (read (show (asLabelJa spec)))
        , cell (show (asFixedCurrent spec))
        , cell (show (asemHomeSideSemantics semantics))
        , cell (show (asClosing spec))
        , cell (intercalate ", " (map show specialRoles))
        ]
    specialRoles = filter (/= OrdinaryAccount) (asemRoles semantics)
    cell value = "<td>" ++ escapeHtml value ++ "</td>"

escapeHtml :: String -> String
escapeHtml = concatMap escape
  where
    escape '&' = "&amp;"
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '"' = "&quot;"
    escape c   = [c]

printTable :: String -> [String] -> IO ()
printTable anchor rows = do
    putStrLn ("<!-- ANCHOR: " ++ anchor ++ " -->")
    putStrLn "<table>"
    putStrLn ("<thead><tr>" ++ concatMap heading headings ++ "</tr></thead>")
    putStrLn "<tbody>"
    mapM_ putStrLn rows
    putStrLn "</tbody>"
    putStrLn "</table>"
    putStrLn ("<!-- ANCHOR_END: " ++ anchor ++ " -->")
  where
    heading label = "<th>" ++ escapeHtml label ++ "</th>"
    headings =
        [ "Constructor", "English name", "Japanese name", "Fixed/current"
        , "Home side", "Closing rule", "Roles"
        ]
