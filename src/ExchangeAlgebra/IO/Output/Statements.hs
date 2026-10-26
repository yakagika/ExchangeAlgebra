{-# LANGUAGE OverloadedStrings #-}

-- | Render journals, account ledgers, trial balances, and statements.
--
-- Import this module for row layouts and their file-writing wrappers.
-- 'accountLedgerRowsJournal' produces ledger rows when no date is needed.
-- The legacy 'bsRows' settles internally, whereas 'plRows' does not; those
-- functions remain available from "ExchangeAlgebra.Write" during the 0.6
-- migration and are omitted from this interface.
--
-- | Task | Row layout | Writer |
-- | :--- | :--- | :--- |
-- | Journal | 'journalRows' | 'writeJournal' |
-- | Account ledger | 'accountLedgerRows' | 'writeAccountOf' |
-- | Account ledger without dates | 'accountLedgerRowsJournal' | 'writeAccountOfJournal' |
-- | Compound trial balance | 'compoundTrialBalanceRows' | 'writeCompoundTrialBalance' |
-- | Worksheet | 'worksheetRows' | 'writeWorksheet' |
-- | Post-closing trial balance | 'postClosingTrialBalanceRows' | 'writePostClosingTrialBalance' |
-- | Presented financial statements | 'financialStatementRows' | 'writeFinancialStatements' |
module ExchangeAlgebra.IO.Output.Statements
    ( journalRows
    , writeJournal
    , accountLedgerRows
    , writeAccountOf
    , accountLedgerRowsJournal
    , writeAccountOfJournal
    , compoundTrialBalanceRows
    , writeCompoundTrialBalance
    , worksheetRows
    , writeWorksheet
    , postClosingTrialBalanceRows
    , writePostClosingTrialBalance
    , financialStatementRows
    , writeFinancialStatements
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
import ExchangeAlgebra.Write
    ( writeCSV
    , journalRows, writeJournal
    , accountLedgerRows, writeAccountOf
    , accountLedgerRowsJournal, writeAccountOfJournal
    , compoundTrialBalanceRows, writeCompoundTrialBalance
    , worksheetRows, writeWorksheet
    , postClosingTrialBalanceRows, writePostClosingTrialBalance
    )

-- | Rows of presented financial statements: a header, then one row per
-- statement line in presentation order, with the account, label, section,
-- side, and amount ('show' of each field).
financialStatementRows :: (Show v) => Presentation.FinancialStatements v -> [[Text]]
financialStatementRows statements =
    ["account", "label", "section", "side", "amount"]
    : [ [ Text.pack (show (Presentation._lineAccount line))
        , Presentation._lineLabel line
        , Text.pack (show (Presentation._lineSection line))
        , Text.pack (show (Presentation._lineSide line))
        , Text.pack (show (Presentation._lineAmount line))
        ]
      | line <- Presentation._statementLines statements
      ]

-- | Write 'financialStatementRows' as CSV with 'writeCSV' (every cell quoted).
writeFinancialStatements :: (Show v) => FilePath -> Presentation.FinancialStatements v -> IO ()
writeFinancialStatements path = writeCSV path . financialStatementRows
