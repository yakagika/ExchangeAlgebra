{-# LANGUAGE OverloadedStrings #-}

-- | Render journals, account ledgers, trial balances, and statements.
--
-- Import this module for row layouts and their file-writing wrappers.
-- 'accountLedgerRowsJournal' produces ledger rows when no date is needed.
-- The legacy 'ExchangeAlgebra.Write.bsRows' settles internally, whereas
-- 'ExchangeAlgebra.Write.plRows' does not; those
-- functions are deprecated in "ExchangeAlgebra.Write" during the 0.6
-- migration and are omitted from this interface. Their successor is
-- 'ExchangeAlgebra.Accounting.Closing.closingEntries' for the balance sheet,
-- then 'ExchangeAlgebra.Accounting.Statements.Presentation.present', then
-- 'financialStatementRows'.
--
-- Statements receive one reporting scope. A ledger with several axes (entity,
-- period, department) is narrowed by the caller and mapped to account titles
-- with 'ExchangeAlgebra.Algebra.Core.mapMaybePosting' or
-- 'ExchangeAlgebra.Algebra.Core.mapPosting' before presentation. The remaining
-- axes are summed, and no axis name appears in the output.
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
    , StatementRowsError(..)
    , financialStatementRows
    , writeFinancialStatements
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import ExchangeAlgebra.Accounting.Statements.Metric (DerivedMetric(..))
import ExchangeAlgebra.Accounting.TrialBalance.Balance (balanceSide, balanceAmount)

import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
import qualified ExchangeAlgebra.Accounting.TrialBalance.Validation as TB
import ExchangeAlgebra.Algebra.Value.Class (HatVal(..))
import ExchangeAlgebra.Write
    ( writeCSV
    , journalRows, writeJournal
    , accountLedgerRows, writeAccountOf
    , accountLedgerRowsJournal, writeAccountOfJournal
    , compoundTrialBalanceRows, writeCompoundTrialBalance
    , worksheetRows, writeWorksheet
    , postClosingTrialBalanceRows, writePostClosingTrialBalance
    )

-- | Why statement rows cannot be rendered for the requested sections.
data StatementRowsError
    = PeriodResultMissing TB.TrialBalanceStage
    -- ^ A revenue or expense section was requested, but the statements carry
    -- no period-result subtotal (for example statements prepared from an
    -- after-closing trial balance).
    deriving (Show, Eq)

-- | Rows of presented financial statements for the requested sections.
--
-- The header is @section, account, label, row, side, amount@. Sections are
-- rendered in the order given; within a section, lines keep presentation
-- order. The @row@ column is @line@ for one statement line (the net balance of
-- one account title) and @subtotal@ for a subtotal row. When a revenue or
-- expense section is requested, the subtotals of the statements follow the
-- requested sections as @subtotal@ rows with empty section and account cells.
-- Amounts use 'ExchangeAlgebra.Algebra.Value.Class.showValue'.
--
-- Subtotal rows must not be summed together with line rows; the @row@ column
-- distinguishes them, but the rendered 'Text' cannot enforce it.
financialStatementRows
    :: HatVal v
    => [Presentation.StatementSection]
    -> Presentation.FinancialStatements v
    -> Either StatementRowsError [[Text]]
financialStatementRows sections statements
    | requestsResult && not hasResult = Left (PeriodResultMissing
        (Presentation._statementTrialBalanceStage statements))
    | otherwise = Right
        (["section", "account", "label", "row", "side", "amount"]
        : lineRows ++ subtotalRows)
  where
    requestsResult = any (`elem` [Presentation.RevenueSection, Presentation.ExpenseSection])
        sections
    subtotals = Presentation._statementSubtotals statements
    hasResult = any ((== PeriodResultMetric) . Presentation._statementSubtotalMetric) subtotals
    lineRows =
        [ [ T.pack (show section)
          , T.pack (show (Presentation._lineAccount line))
          , Presentation._lineLabel line
          , "line"
          , T.pack (show (Presentation._lineSide line))
          , T.pack (showValue (Presentation._lineAmount line))
          ]
        | section <- sections
        , line <- Presentation._statementLines statements
        , Presentation._lineSection line == section
        ]
    subtotalRows =
        [ [ "", "", label, "subtotal", T.pack (show (balanceSide balance))
          , T.pack (showValue (balanceAmount balance)) ]
        | requestsResult
        , Presentation.StatementSubtotal _ label balance <- subtotals
        ]

-- | Write 'financialStatementRows' as CSV with 'writeCSV' (every cell quoted).
-- Nothing is written when the rows cannot be rendered.
writeFinancialStatements
    :: HatVal v
    => FilePath
    -> [Presentation.StatementSection]
    -> Presentation.FinancialStatements v
    -> IO (Either StatementRowsError ())
writeFinancialStatements path sections statements =
    case financialStatementRows sections statements of
        Left issue -> pure (Left issue)
        Right rows -> do
            writeCSV path rows
            pure (Right ())
