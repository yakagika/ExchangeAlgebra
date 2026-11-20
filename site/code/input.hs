-- ANCHOR: setup
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (groupBy, sortOn)
import Data.String (IsString, fromString)
import ExchangeAlgebra (AccountTitles(..), Alg, HatBase, MoneyDecimal, Side(..), decL, decR, norm)
import qualified ExchangeAlgebra.IO.Input as Input
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import qualified ExchangeAlgebra.IO.Input.Csv as Csv
import qualified ExchangeAlgebra.Journal as Journal
-- ANCHOR_END: setup

-- ANCHOR: support
-- | Stop the example on an unexpected checked failure.
require :: Show e => Either e a -> IO a
require = either (ioError . userError . show) pure
-- ANCHOR_END: support

-- ANCHOR: grouping
-- | Group the parsed terms by transaction key, keeping the order within each entry.
groupTransactions :: Ord note
                  => [(Side, AccountTitles, MoneyDecimal, note)]
                  -> [(note, [(Side, AccountTitles, MoneyDecimal)])]
groupTransactions rows =
    [ (note, map withoutNote entry)
    | entry@((_, _, _, note) : _) <- groupBy sameNote (sortOn noteOf rows)
    ]
  where
    noteOf (_, _, _, note) = note
    sameNote left right = noteOf left == noteOf right
    withoutNote (side, account, amount, _) = (side, account, amount)

-- | Require a transaction key on every CSV term before grouping by note.
checkNotes :: (Eq note, IsString note)
           => [(Side, AccountTitles, MoneyDecimal, note)] -> Either String ()
checkNotes rows
    | any (\(_, _, _, note) -> note == "") rows = Left "Missing transaction key in note"
    | otherwise = Right ()
-- ANCHOR_END: grouping

-- ANCHOR: admission
main :: IO ()
main = do
    input <- fromString <$> readFile "january-import.csv"
    flat <- require (Csv.parseJournalCsv input :: Either Input.ConvError
        (Alg MoneyDecimal (HatBase AccountTitles)))
    putStrLn $ "Parsed term totals (debit, credit): "
            ++ show (norm (decL flat), norm (decR flat))

    rows <- require (Csv.parseNotedJournalCsv Csv.scientificAmount input)
    require (checkNotes rows)
    journal <- require (Input.checkedJournal (groupTransactions rows))
    let sources = [("Jan 28 Cash sale", 2000), ("Jan 30 January and February rent", 1200)]
        differences = Input.reconcileSources sources journal
    case differences of
        [] -> pure ()
        _ -> ioError (userError (show differences))
    accepted <- require (Admission.admit journal)
    let ledger = Journal.toAlg (Admission.admittedJournal accepted)
    putStrLn $ "Source differences: " ++ show differences
    putStrLn $ "Admitted journal totals (debit, credit): "
            ++ show (norm (decL ledger), norm (decR ledger))
    printDiagnostics
-- ANCHOR_END: admission

-- ANCHOR: diagnostics
-- | Show the results of a misspelled title and a separate amount error.
printDiagnostics :: IO ()
printDiagnostics = do
    putStrLn $ "Normalized title: " ++ show (Input.normalizeTitle "  A/R  ")
    putStrLn $ "Account name: " ++ show (Input.parseAccountTitle "  A/R  ")
    let typoCsv = "side,account,amount\ndebit,Cash,2000\ncredit,Saels,2000\n"
    putStrLn $ "CSV title error: "
        ++ show (Csv.parseJournalCsv typoCsv :: Either Input.ConvError
            (Alg MoneyDecimal (HatBase AccountTitles)))
    let unresolved = Input.certifyJournalText
            [("Jan 28 Cash sale" :: String, [("debit", "Cash", 2000 :: MoneyDecimal),
                                            ("credit", "Saels", 2000)])]
    putStrLn $ "Text certification: " ++ show unresolved
    let unbalanced = Input.checkedJournal
            [("Jan 28 Cash sale" :: String, [(Debit, Cash, 2000 :: MoneyDecimal),
                                            (Credit, Sales, 1900)])]
    putStrLn $ "Entry balance error: " ++ show unbalanced
-- ANCHOR_END: diagnostics
