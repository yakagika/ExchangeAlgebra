{- |
    Module     : ExchangeAlgebra.Render.Bookkeeping
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Journals, account ledgers, trial balances, and closing worksheets.
    These layouts and their file writers are re-exported from
    "ExchangeAlgebra.IO.Output.Statements". For financial statements, use
    "ExchangeAlgebra.Accounting.Statements.Presentation" followed by
    'ExchangeAlgebra.IO.Output.Statements.financialStatementRows'.
-}

module ExchangeAlgebra.Render.Bookkeeping {-# DEPRECATED "Import ExchangeAlgebra.IO.Output.Statements instead." #-}
    ( -- * Journal and ledgers
      writeJournal
    , journalRows
    , writeAccountOf
    , accountLedgerRows
    , writeAccountOfJournal
    , accountLedgerRowsJournal
      -- * Trial balances and closing documents
    , writeCompoundTrialBalance
    , compoundTrialBalanceRows
    , writeWorksheet
    , worksheetRows
    , writePostClosingTrialBalance
    , postClosingTrialBalanceRows
    ) where

import ExchangeAlgebra.IO.Output.Statements
