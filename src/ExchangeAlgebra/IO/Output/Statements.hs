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
    ) where

import ExchangeAlgebra.Write
    ( journalRows, writeJournal
    , accountLedgerRows, writeAccountOf
    , accountLedgerRowsJournal, writeAccountOfJournal
    , compoundTrialBalanceRows, writeCompoundTrialBalance
    , worksheetRows, writeWorksheet
    , postClosingTrialBalanceRows, writePostClosingTrialBalance
    )
