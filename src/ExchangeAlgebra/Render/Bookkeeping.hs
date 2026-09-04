{- |
    Module     : ExchangeAlgebra.Render.Bookkeeping
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Bookkeeping documents rendered from an algebra or a journal: balance
    sheet, profit and loss, journal, account ledgers, compound trial
    balance, the eight-column worksheet and the post-closing trial balance.
    Each document comes as a pure @…Rows@ layout and a @write…@ wrapper that
    serialises the rows through 'ExchangeAlgebra.Render.Csv.writeCSV'.

    Every name is re-exported unchanged from "ExchangeAlgebra.Write"; nothing
    is defined here. The row layouts are frozen by the
    @test/fixtures/write-rows-0510@ goldens, so the shim is behaviour-
    identical to the original module by construction. Note that 'bsRows' and
    'plRows' are the legacy presentation (no per-title aggregation, credit-
    balance assets dropped from the sheet); the grouped presentation lives in
    "ExchangeAlgebra.Reporting.Presentation".

    >>> import ExchangeAlgebra.Algebra
    >>> let x = 100 .@ Not :< Cash .+ 100 .@ Not :< Sales :: Alg Double (HatBase AccountTitles)
    >>> balanceOf Cash x
    (Debit,100.0)
-}

module ExchangeAlgebra.Render.Bookkeeping
    ( -- * Balance sheet and profit and loss
      writeBS
    , bsRows
    , writePL
    , plRows
      -- * Journal and ledgers
    , writeJournal
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
      -- * Helpers
    , balanceOf
    , tshow
    , toSameLength
    ) where

import           ExchangeAlgebra.Write
                     ( writeBS
                     , bsRows
                     , writePL
                     , plRows
                     , writeJournal
                     , journalRows
                     , writeAccountOf
                     , accountLedgerRows
                     , writeAccountOfJournal
                     , accountLedgerRowsJournal
                     , writeCompoundTrialBalance
                     , compoundTrialBalanceRows
                     , writeWorksheet
                     , worksheetRows
                     , writePostClosingTrialBalance
                     , postClosingTrialBalanceRows
                     , balanceOf
                     , tshow
                     , toSameLength
                     )
