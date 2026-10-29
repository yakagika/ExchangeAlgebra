{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render journals, account ledgers, trial balances, and statements.
--
-- Import this module for row layouts and their file-writing wrappers.
-- 'accountLedgerRowsJournal' produces ledger rows when no date is needed.
-- For financial statements, close the ledger with
-- 'ExchangeAlgebra.Accounting.Closing.closingEntries' before preparing a
-- balance sheet, then use
-- 'ExchangeAlgebra.Accounting.Statements.Presentation.present' and
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
import ExchangeAlgebra.IO.Output.Csv (writeCSV, csvTranspose)
import qualified ExchangeAlgebra.Algebra.Core as EA
import ExchangeAlgebra.Algebra.Core
import ExchangeAlgebra.Algebra.Base.Representation
import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Accounting.Exchange
import qualified ExchangeAlgebra.Journal.Core as EJ
import ExchangeAlgebra.Journal.Core ((.|))
import qualified ExchangeAlgebra.Accounting.TrialBalance.Accum as Accum
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict as OMap
import Data.Time (Day)

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

-- | Convert a value to a text cell.
--
-- Complexity: O(show cost)
tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

-- | Pad two lists to the same length. Appends empty text to the shorter list.
--
-- Complexity: O(max(|xs|, |ys|))
toSameLength :: [T.Text] -> [T.Text] -> ([T.Text],[T.Text])
toSameLength xs ys =
    case compare lx ly of
        EQ -> (xs, ys)
        LT -> (xs ++ replicate (ly - lx) T.empty, ys)
        GT -> (xs, ys ++ replicate (lx - ly) T.empty)
  where
    lx = Prelude.length xs
    ly = Prelude.length ys

-- | Build the rows of a journal (仕訳帳), as a pure value (the part
-- 'writeJournal' renders to CSV).
--
-- Entries are grouped by date (via @f@) into a deterministic, deduplicated,
-- ascending day sequence; within a day the debit and credit postings are
-- listed by 'decL'\/'decR' and padded to equal length with @toSameLength@ --
-- no aggregation\/@bar@ is applied, so a day with more debit than credit
-- postings (or vice versa) simply gets blank cells on the shorter side.
--
-- ==== __Examples__
--
-- Three days: a capital contribution, a cash sale, and a day with two debit
-- postings against one credit posting (the debit column is one line longer,
-- and the credit/date cells of that extra line are padded blank):
--
-- >>> import Data.Time (fromGregorian)
-- >>> type T = Alg Double (HatBase (AccountTitles, Day))
-- >>> let d1 = fromGregorian 2024 4 1
-- >>> let d2 = fromGregorian 2024 4 2
-- >>> let d3 = fromGregorian 2024 4 3
-- >>> let getDay (_ :< (_, d)) = d
-- >>> let alg = (100 .@ Not:<(Cash,d1)) .+ (100 .@ Not:<(CapitalStock,d1)) .+ (50 .@ Not:<(Cash,d2)) .+ (50 .@ Not:<(Sales,d2)) .+ (30 .@ Not:<(Cash,d3)) .+ (10 .@ Not:<(AccountsReceivable,d3)) .+ (40 .@ Not:<(Sales,d3)) :: T
-- >>> mapM_ print (journalRows alg getDay)
-- ["Day","Debit","Amount","Credit","Amount"]
-- ["2024-04-01","Cash","100.0","CapitalStock","100.0"]
-- ["2024-04-02","Cash","50.0","Sales","50.0"]
-- ["2024-04-03","AccountsReceivable","10.0","Sales","40.0"]
-- ["","Cash","30.0","",""]
--
-- Complexity: O(s * log d) (s = number of entries, d = number of distinct dates)
journalRows :: (HatVal n, HatBaseClass b, ExBaseClass b)
            => Alg n b
            -> (b -> Day)
            -> [[T.Text]]
journalRows alg f = csvTranspose [ds, dt, dv, ct, cv]
  where
    days = L.sort $ Set.toList . Set.fromList $ L.map (f . _hatBase) $ EA.toList alg
    rows = L.map perDay days
    perDay d =
        let da = EA.filter (\y -> (f . _hatBase) y == d) alg
            dl = decL da
            dr = decR da
            dlTexts = L.map (tshow . getAccountTitle . _hatBase) (EA.toList dl)
            drTexts = L.map (tshow . getAccountTitle . _hatBase) (EA.toList dr)
            dlValues = L.map (tshow . _val) (EA.toList dl)
            drValues = L.map (tshow . _val) (EA.toList dr)
            (dt', ct') = toSameLength dlTexts drTexts
            (dv', cv') = toSameLength dlValues drValues
            (ds', _) = toSameLength [tshow d] cv'
        in (ds', dt', dv', ct', cv')
    ds = [T.pack "Day"] ++ concatMap (\(a,_,_,_,_) -> a) rows
    dt = [T.pack "Debit"] ++ concatMap (\(_,a,_,_,_) -> a) rows
    dv = [T.pack "Amount"] ++ concatMap (\(_,_,a,_,_) -> a) rows
    ct = [T.pack "Credit"] ++ concatMap (\(_,_,_,a,_) -> a) rows
    cv = [T.pack "Amount"] ++ concatMap (\(_,_,_,_,a) -> a) rows

-- | Output journal entries in CSV format.
-- Groups by date and records the debit/credit account titles and amounts for each day.
-- Pure layout is delegated to 'journalRows'; this function only writes the file.
--
-- Complexity: O(s * log d) (s = number of entries, d = number of distinct dates; see 'journalRows')
writeJournal :: (HatVal n, HatBaseClass b, ExBaseClass b)
             => FilePath
             -> Alg n b
             -> (b -> Day)
             -> IO ()
writeJournal path alg f = writeCSV path (journalRows alg f)


-- | Build the rows of a general ledger (総勘定元帳 / T-account) for the given
-- account titles, as a pure value (the part 'writeAccountOf' renders to CSV).
--
-- The ledger is the audit-trail view of an algebra: for each requested account
-- title, every posting touching that title is listed __individually in date
-- order, with no aggregation__ — the redundant sequence (seq) of the algebra is
-- preserved verbatim. (Contrast with 'writeCompoundTrialBalance', which is the
-- aggregated, @bar@\/@norm@-netted view.) This is the showcase of the algebra's
-- redundancy: each historical posting is kept as a separate ledger line.
--
-- For each title the layout is a two-sided T-account:
--
-- > <Title>
-- > Date | Debit | Date | Credit
--
-- Debit-side postings (@'whichSide' == 'Debit'@) go on the left, credit-side
-- postings on the right; the two columns are padded to equal length. Within a
-- side, postings are sorted by date and otherwise keep their original order
-- (so the line count equals the number of postings — no netting).
--
-- ==== __Examples__
--
-- Two separate debit postings to Cash are kept as two ledger lines (the seq is
-- /not/ aggregated into a single 30; the count of postings is preserved):
--
-- >>> import Data.Time (fromGregorian)
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let led = (10 .@ Not:<Cash) .+ (20 .@ Not:<Cash) .+ (5 .@ Hat:<Cash) :: T
-- >>> let d = fromGregorian 2024 4 1
-- >>> mapM_ print (accountLedgerRows [Cash] led (const d))
-- ["Cash","","",""]
-- ["Date","Debit","Date","Credit"]
-- ["2024-04-01","20.0","2024-04-01","5.0"]
-- ["2024-04-01","10.0","",""]
--
-- Complexity: O(t * s) (t = number of titles, s = number of entries).
accountLedgerRows :: (HatVal n, HatBaseClass b, ExBaseClass b)
                  => [AccountTitles]
                  -> Alg n b
                  -> (b -> Day)
                  -> [[T.Text]]
accountLedgerRows titles alg f =
    concatMap titleBlock titles
  where
    titleBlock t =
        let xs       = projByAccountTitle t alg
            debits   = sortByDay (EA.toList (decL xs))
            credits  = sortByDay (EA.toList (decR xs))
            dDates   = L.map (tshow . f . _hatBase) debits
            dVals    = L.map (tshow . _val)          debits
            cDates   = L.map (tshow . f . _hatBase) credits
            cVals    = L.map (tshow . _val)          credits
            (dDates', cDates') = toSameLength dDates cDates
            (dVals',  cVals')  = toSameLength dVals  cVals
            header   = [tshow t, T.empty, T.empty, T.empty]
            sub      = [T.pack "Date", T.pack "Debit", T.pack "Date", T.pack "Credit"]
            body     = L.zipWith4 (\a b c d -> [a,b,c,d]) dDates' dVals' cDates' cVals'
         in header : sub : body
    sortByDay = L.sortBy (\x y -> compare ((f . _hatBase) x) ((f . _hatBase) y))

-- | Output general ledgers (総勘定元帳) for the given account titles in CSV
-- format. Pure layout is delegated to 'accountLedgerRows'; this function only
-- performs the file write.
--
-- Each requested title is rendered as a two-sided T-account whose postings are
-- listed __individually, without aggregation__ (the seq redundancy is the audit
-- trail — see 'accountLedgerRows').
--
-- Complexity: O(t * s) (t = number of titles, s = number of entries).
writeAccountOf :: (HatVal n, HatBaseClass b, ExBaseClass b)
             => [AccountTitles]
             -> FilePath
             -> Alg n b
             -> (b -> Day)
             -> IO ()
writeAccountOf titles path alg f = writeCSV path (accountLedgerRows titles alg f)

-- | Build the rows of general ledgers (総勘定元帳) from a 'EJ.Journal', as a
-- pure value (the part 'writeAccountOfJournal' renders to CSV), carrying the
-- per-posting note (摘要) as an extra column.
--
-- As with 'accountLedgerRows', postings are listed __individually, without
-- aggregation__ — the redundant sequence is the audit trail. Because each
-- posting carries its own note, this version uses a flat detail layout rather
-- than the two-sided T-account:
--
-- > <Title>
-- > Note | Debit | Credit
-- > <note> | <amount> |          -- debit-side posting
-- > <note> |          | <amount> -- credit-side posting
--
-- The note is rendered with 'show'. Postings are emitted in note order
-- (the 'EJ.Journal' is keyed by note); within a note the algebra's own seq
-- order is preserved. No 'EA.bar' \/ aggregation is applied.
--
-- ==== __Examples__
--
-- Two Cash postings under different notes; note order ("pay" < "sale")
-- determines the row order, not posting order:
--
-- >>> let jrn = ((100 .@ Not:<Cash) .| "sale") .+ ((40 .@ Hat:<Cash) .| "pay") :: EJ.Journal String Double (HatBase AccountTitles)
-- >>> mapM_ print (accountLedgerRowsJournal [Cash] jrn)
-- ["Cash","",""]
-- ["Note","Debit","Credit"]
-- ["\"pay\"","","40.0"]
-- ["\"sale\"","100.0",""]
--
-- Complexity: O(t * s) (t = number of titles, s = number of postings).
accountLedgerRowsJournal :: (HatVal n, HatBaseClass b, ExBaseClass b, EJ.Note note)
                         => [AccountTitles]
                         -> EJ.Journal note n b
                         -> [[T.Text]]
accountLedgerRowsJournal titles j =
    concatMap titleBlock titles
  where
    pairs = L.sortBy (\(a,_) (b,_) -> compare a b) (Map.toList (EJ.toMap j))
    titleBlock t =
        let header = [tshow t, T.empty, T.empty]
            sub    = [T.pack "Note", T.pack "Debit", T.pack "Credit"]
            body   = concatMap (noteRows t) pairs
         in header : sub : body
    noteRows t (note, alg) =
        let xs = projByAccountTitle t alg
            mkRow x =
                let amt = tshow (_val x)
                in if (whichSide . _hatBase) x == Debit
                     then [tshow note, amt, T.empty]
                     else [tshow note, T.empty, amt]
         in L.map mkRow (EA.toList (EA.filter (\x -> x /= EA.Zero) xs))

-- | Output general ledgers (総勘定元帳) from a 'EJ.Journal' in CSV format,
-- carrying the per-posting note (摘要) as an extra column. Pure layout is
-- delegated to 'accountLedgerRowsJournal'; this function only writes the
-- file. See 'accountLedgerRowsJournal' for the flat note-detail layout.
--
-- Complexity: O(t * s) (t = number of titles, s = number of postings; see
-- 'accountLedgerRowsJournal').
writeAccountOfJournal :: (HatVal n, HatBaseClass b, ExBaseClass b, EJ.Note note)
                      => [AccountTitles]
                      -> FilePath
                      -> EJ.Journal note n b
                      -> IO ()
writeAccountOfJournal titles path j =
    writeCSV path (accountLedgerRowsJournal titles j)


------------------------------------------------------------------
-- Trial-balance aggregation (non-negative single-pass)
------------------------------------------------------------------

-- | Per-account gross debit/credit totals, accumulated in a __single pass__ over
-- the algebra's scalar entries (O(s)), keyed by 'AccountTitles' in 'Ord' order.
--
-- Each entry contributes its (non-negative) value to either the debit or the
-- credit total of its account title, classified by 'whichSide' (which already
-- folds in the Hat\/Not reversal) — exactly the @Debit@\/@Credit@ partition that
-- 'decL'\/'decR' produce. Summing per side reproduces @'norm' . 'decL'@ (debit
-- gross) and @'norm' . 'decR'@ (credit gross) without any signed netting, so the
-- value-domain invariant ($\\mathbb{R}_0^+$) is preserved: only non-negative
-- magnitudes are stored.
--
-- 'foldEntries' already skips zero values, matching the @x /= 'Zero'@ guard in
-- 'decL'\/'decR'. The resulting 'OMap.Map' iterates titles in ascending order,
-- matching the previous @L.sort . Set.toList . Set.fromList@ title enumeration.
--
-- Exact gross sums are retained until the caller has netted or combined accounts.
-- Each displayed amount is then rounded once.
--
-- Complexity: O(s) (s = number of scalar entries).
accountGrossTotals :: (HatVal n, ExBaseClass b)
                   => Alg n b -> OMap.Map AccountTitles (Accum n, Accum n)
accountGrossTotals = EA.foldEntries step OMap.empty
  where
    step totals value base =
        let (!debit, !credit) = case whichSide base of
                Debit -> (addAccum value emptyAccum, emptyAccum)
                Credit -> (emptyAccum, addAccum value emptyAccum)
                Side -> (emptyAccum, emptyAccum)
        in OMap.insertWith merge (getAccountTitle base) (debit, credit) totals
    merge (!debit, !credit) (!otherDebit, !otherCredit) =
        let !debitTotal = mergeAccum debit otherDebit
            !creditTotal = mergeAccum credit otherCredit
        in (debitTotal, creditTotal)

-- | Read the exact difference before rounding its non-negative magnitude once.
-- NaN retains the legacy debit-column placement.
readAccumBalance :: HatVal n => (Accum n, Accum n) -> (Side, n)
readAccumBalance (debit, credit) =
    Accum.readAccountAccum Debit (mergeAccum debit (negateAccum credit))


-- | Build the rows of a Compound Trial Balance (合計残高試算表), as a pure
-- value (the part 'writeCompoundTrialBalance' renders to CSV). Calculates the
-- debit total, credit total, and balance for each account title, aggregated
-- from exact gross accumulators. Each gross total and net balance is rounded
-- once; netting compares the accumulators before rounding, without tolerance.
-- Displayed rows need not sum to the displayed total. No implicit @bar@. Layout:
--
-- > Debit Balance | Debit Total | Account Title | Credit Total | Credit Balance
--
-- A debit-heavy account's balance is in the __Debit Balance__ (leftmost)
-- column and a credit-heavy account's balance is in the __Credit Balance__
-- (rightmost) column, as in 'worksheetRows' and
-- 'postClosingTrialBalanceRows'. The Total row sums each column. Releases up
-- to 0.5.3.1 placed each balance in the opposite column.
--
-- ==== __Examples__
--
-- Cash is debit-heavy (gross debit 100, credit 0); CapitalStock and
-- LoansPayable are credit-heavy:
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = (100 .@ Not:<Cash) .+ (60 .@ Not:<LoansPayable) .+ (40 .@ Not:<CapitalStock) :: T
-- >>> mapM_ print (compoundTrialBalanceRows alg)
-- ["Debit Balance","Debit Total","Account Title","Credit Total","Credit Balance"]
-- ["100.0","100.0","Cash","0.0",""]
-- ["","0.0","CapitalStock","40.0","40.0"]
-- ["","0.0","LoansPayable","60.0","60.0"]
-- ["100.0","100.0","Total","100.0","100.0"]
--
-- Complexity: O(s) (single pass over s scalar entries; see @accountGrossTotals@)
compoundTrialBalanceRows :: (HatVal n, HatBaseClass b, ExBaseClass b)
                          => Alg n b -> [[T.Text]]
compoundTrialBalanceRows alg =
    header : lines' ++ [totalLine]
  where
    header = [T.pack "Debit Balance"
             ,T.pack "Debit Total"
             ,T.pack "Account Title"
             ,T.pack "Credit Total"
             ,T.pack "Credit Balance"]
    -- Single pass (O(s)): gross debit/credit totals per title, in Ord order.
    accounts = OMap.toList (accountGrossTotals alg)
    (lines', debitBalanceTotal, debitTotal, creditBalanceTotal, creditTotal) =
        L.foldl' step ([], emptyAccum, emptyAccum, emptyAccum, emptyAccum) accounts
    totalLine = [ tshow (roundAccum debitBalanceTotal)
                , tshow (roundAccum creditTotal)
                , T.pack "Total"
                , tshow (roundAccum debitTotal)
                , tshow (roundAccum creditBalanceTotal)
                ]
    step (accLines, dbt, dt, cbt, ct) (a, gross) =
        let xl = roundAccum (fst gross)   -- norm (decL xs) : debit gross
            xr = roundAccum (snd gross)   -- norm (decR xs) : credit gross
            difference = mergeAccum (fst gross) (negateAccum (snd gross))
            (dc, diff) = readAccumBalance gross
            -- The exact difference returns the wildcard v'Side' with a zero
            -- difference when an account nets to zero (e.g. a fully-cleared
            -- suspense account). Treat that as no balance on either side
            -- (cf. @sideCells@).
            (dbt', cbt') = case dc of
                Credit -> (dbt, mergeAccum cbt (negateAccum difference))
                Debit  -> (mergeAccum dbt difference, cbt)
                Side   -> (dbt, cbt)
            line = case dc of
                Debit  -> [ tshow diff
                          , tshow xl
                          , tshow a
                          , tshow xr
                          , T.empty
                          ]
                Credit -> [ T.empty
                          , tshow xl
                          , tshow a
                          , tshow xr
                          , tshow diff
                          ]
                Side   -> [ T.empty
                          , tshow xl
                          , tshow a
                          , tshow xr
                          , T.empty
                          ]
         in (accLines ++ [line], dbt', mergeAccum dt (snd gross), cbt', mergeAccum ct (fst gross))

-- | Output a Compound Trial Balance in CSV format.
-- Calculates the debit total, credit total, and balance for each account
-- title and outputs as a table. Pure layout is delegated to
-- 'compoundTrialBalanceRows'; this function only writes the file. See
-- 'compoundTrialBalanceRows' for the column layout.
--
-- Complexity: O(s) (single pass over s scalar entries; see
-- 'compoundTrialBalanceRows')
writeCompoundTrialBalance :: (HatVal n, HatBaseClass b, ExBaseClass b)
                           => FilePath
                           -> Alg n b
                           -> IO ()
writeCompoundTrialBalance path alg =
    writeCSV path (compoundTrialBalanceRows alg)


-- | Place a @(side, magnitude)@ balance into a (debit, credit) text column pair.
-- A zero magnitude produces two empty cells.
sideCells :: (HatVal n) => (Side, n) -> (T.Text, T.Text)
sideCells (side, mag)
    | mag == zeroValue = (T.empty, T.empty)
    -- 'diffRL' returns the wildcard v'Side' only when the net magnitude is zero,
    -- which the guard above has already handled; a non-zero balance is always
    -- 'Debit' or 'Credit'. The non-exhaustive @case@ is by design (audited).
    | otherwise = case side of
        Debit  -> (tshow mag, T.empty)
        Credit -> (T.empty, tshow mag)

-- | Build the rows of an 8-column worksheet (8 桁精算表), as a pure value
-- (the part 'writeWorksheet' renders to CSV).
--
-- Columns (matching the lecture's 精算表): per account title there are four
-- debit\/credit column pairs —
--
-- > Account Title
-- >   | Trial Balance (Debit, Credit)   -- 残高試算表
-- >   | Adjustments   (Debit, Credit)   -- 整理記入
-- >   | Profit & Loss (Debit, Credit)   -- 損益計算書
-- >   | Balance Sheet (Debit, Credit)   -- 貸借対照表
--
-- For each account title:
--
--   * the __Trial Balance__ pair is the net balance of the
--     /pre-adjustment ledger/ (@diffRL (projByAccountTitle title pre)@ — an explicit
--     @diffRL@ netting, no implicit @bar@);
--   * the __Adjustments__ pair is the net balance of the /adjustment entries/
--     (@diffRL (projByAccountTitle title adj)@);
--   * the __final balance__ is the net balance of @pre '.+' adj@, routed by
--     'whatDiv': 'Cost'\/'Revenue' titles go to the __Profit & Loss__ columns,
--     'Assets'\/'Liability'\/'Equity' titles go to the __Balance Sheet__
--     columns.
--
-- The penultimate row is the column totals of the trial-balance, P\/L and B\/S
-- column pairs. The final row is the profit\/loss balancing figure
-- (当期純利益\/純損失): it is the amount that makes each of the P\/L and B\/S
-- column pairs balance. By construction (the homomorphism @norm@) the P\/L
-- imbalance equals the B\/S imbalance — that equality is the worksheet's own
-- self-check (精算表の貸借差額の一致). __This function does not enforce the
-- equality__; if the inputs are inconsistent it still emits both figures so the
-- discrepancy is visible (rather than raising an error).
--
-- Account titles are listed in 'Ord' order so the output is deterministic.
--
-- ==== __Examples__
--
-- Opening capital 100 + a cash sale of 50, with no adjustments. Cash (150) and
-- CapitalStock (100) go to the Balance Sheet; Sales (50) goes to Profit & Loss.
-- The net income of 50 appears on the P\/L debit and B\/S credit, making both
-- statement column pairs balance at 50 and 150 respectively:
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let pre = (150 .@ Not:<Cash) .+ (100 .@ Not:<CapitalStock) .+ (50 .@ Not:<Sales) :: T
-- >>> mapM_ print (worksheetRows pre (zeroValue .@ Not:<Cash))
-- ["Account Title","Trial Balance","","Adjustments","","Profit & Loss","","Balance Sheet",""]
-- ["","Debit","Credit","Debit","Credit","Debit","Credit","Debit","Credit"]
-- ["Cash","150.0","","","","","","150.0",""]
-- ["CapitalStock","","100.0","","","","","","100.0"]
-- ["Sales","","50.0","","","","50.0","",""]
-- ["Subtotal","150.0","150.0","","","0.0","50.0","150.0","100.0"]
-- ["Net Income","","","","","50.0","","","50.0"]
-- ["Total","","","","","50.0","50.0","150.0","150.0"]
--
-- Complexity: O(s) (single pass per algebra over s scalar entries;
-- see @accountGrossTotals@).
worksheetRows :: (HatVal n, HatBaseClass b, ExBaseClass b)
              => Alg n b   -- ^ pre-adjustment ledger (決算整理前残高)
              -> Alg n b   -- ^ adjustment entries     (決算整理仕訳)
              -> [[T.Text]]
worksheetRows pre adj =
    header : sub : bodyRows ++ [tbTotalRow, netRow, grandTotalRow]
  where
    header =
        [ T.pack "Account Title"
        , T.pack "Trial Balance", T.empty
        , T.pack "Adjustments",   T.empty
        , T.pack "Profit & Loss", T.empty
        , T.pack "Balance Sheet", T.empty ]
    sub =
        [ T.empty
        , T.pack "Debit", T.pack "Credit"
        , T.pack "Debit", T.pack "Credit"
        , T.pack "Debit", T.pack "Credit"
        , T.pack "Debit", T.pack "Credit" ]
    combined = pre .+ adj
    -- Single pass per algebra (O(s)): gross debit/credit totals per title.
    -- @diffRL (projByAccountTitle t alg)@ is reproduced as
    -- the exact difference of the per-title gross accumulators; a title absent from a map
    -- (e.g. only in 'adj') contributes the empty sums.
    preTotals = accountGrossTotals pre
    adjTotals = accountGrossTotals adj
    combTotals = accountGrossTotals combined
    grossOf m t = OMap.findWithDefault (emptyAccum, emptyAccum) t m
    -- Title enumeration: the keys of 'combined' in Ord order (= the previous
    -- 'L.sort . Set.toList . Set.fromList' over 'EA.toList combined').
    titles = OMap.keys combTotals
    -- per-title row + accumulate the four debit/credit column totals
    (bodyRows, tbD, tbC, plD, plC, bsD, bsC) =
        L.foldl' step
            ([], emptyAccum, emptyAccum, emptyAccum, emptyAccum, emptyAccum, emptyAccum) titles
    step (rows, tbd, tbc, pld, plc, bsd, bsc) t =
        let tb              = readAccumBalance (grossOf preTotals t)
            (tbDc, tbCc)    = sideCells tb
            adjPair         = readAccumBalance (grossOf adjTotals t)
            (adjDc, adjCc)  = sideCells adjPair
            finalPair = readAccumBalance (grossOf combTotals t)
            div'            = classifyAccountDivision t
            isPL            = div' == Cost || div' == Revenue
            (plDc, plCc, bsDc, bsCc)
              | isPL      = let (d,c) = sideCells finalPair in (d, c, T.empty, T.empty)
              | otherwise = let (d,c) = sideCells finalPair in (T.empty, T.empty, d, c)
            row = [ tshow t, tbDc, tbCc, adjDc, adjCc, plDc, plCc, bsDc, bsCc ]
            -- column totals
            addCol (debit, credit) d c = case signAccum difference of
                Just GT -> (mergeAccum d difference, c)
                Just LT -> (d, mergeAccum c (negateAccum difference))
                Just EQ -> (d, c)  -- balanced to zero: adds nothing
                Nothing -> (mergeAccum d difference, c)  -- NaN follows the debit cell
              where
                difference = mergeAccum debit (negateAccum credit)
            (ntbd, ntbc) = addCol (grossOf preTotals t) tbd tbc
            (npld, nplc) | isPL      = addCol (grossOf combTotals t) pld plc
                         | otherwise = (pld, plc)
            (nbsd, nbsc) | not isPL  = addCol (grossOf combTotals t) bsd bsc
                         | otherwise = (bsd, bsc)
         in (rows ++ [row], ntbd, ntbc, npld, nplc, nbsd, nbsc)
    -- trial-balance totals row (整理記入 totals are intentionally omitted: the
    -- adjustment column does not have a meaningful single grand total in the
    -- lecture's layout; the self-check is on P/L and B/S).
    tbTotalRow =
        [ T.pack "Subtotal"
        , tshow (roundAccum tbD), tshow (roundAccum tbC)
        , T.empty, T.empty
        , tshow (roundAccum plD), tshow (roundAccum plC)
        , tshow (roundAccum bsD), tshow (roundAccum bsC) ]
    -- profit/loss balancing figure (当期純利益 / 純損失).
    -- P/L imbalance: if credit (revenue) side exceeds debit (cost) side, the
    -- difference is net income, recorded on the P/L debit side and the B/S
    -- credit side (it increases equity). The reverse is a net loss.
    plDiff = snd (readAccumBalance (plD, plC))
    bsDiff = snd (readAccumBalance (bsD, bsC))
    netProfit = signAccum (mergeAccum plC (negateAccum plD)) == Just GT
    (netLabel, netPlD, netPlC, netBsD, netBsC)
        | signAccum (mergeAccum plC (negateAccum plD)) == Just EQ =
            (T.pack "Net Income/Loss", T.empty, T.empty, T.empty, T.empty)
        | netProfit  = (T.pack "Net Income", tshow plDiff, T.empty, T.empty, tshow bsDiff)
        | otherwise  = (T.pack "Net Loss",   T.empty, tshow plDiff, tshow bsDiff, T.empty)
    netRow =
        [ netLabel
        , T.empty, T.empty
        , T.empty, T.empty
        , netPlD, netPlC
        , netBsD, netBsC ]
    -- grand totals after adding the balancing figure (both sides now equal).
    plTotal = if netProfit then plC else plD
    bsTotal = case signAccum (mergeAccum bsD (negateAccum bsC)) of
        Just GT -> bsD
        _ -> bsC
    grandTotalRow =
        [ T.pack "Total"
        , T.empty, T.empty
        , T.empty, T.empty
        , tshow (roundAccum plTotal), tshow (roundAccum plTotal)
        , tshow (roundAccum bsTotal), tshow (roundAccum bsTotal) ]

-- | Output an 8-column worksheet (8 桁精算表) in CSV format.
-- Pure layout is delegated to 'worksheetRows'; this function only writes the
-- file. See 'worksheetRows' for the column structure and the self-check
-- (P\/L vs B\/S balancing figure) semantics.
--
-- Complexity: O(s) (single pass per algebra; see 'worksheetRows').
writeWorksheet :: (HatVal n, HatBaseClass b, ExBaseClass b)
               => FilePath
               -> Alg n b   -- ^ pre-adjustment ledger
               -> Alg n b   -- ^ adjustment entries
               -> IO ()
writeWorksheet path pre adj = writeCSV path (worksheetRows pre adj)

-- | Build the rows of a post-closing trial balance (繰越試算表), as a pure
-- value (the part 'writePostClosingTrialBalance' renders to CSV).
--
-- After the closing transfers, only the /real/ (permanent) accounts remain on
-- the ledger. This lists the net balance of every 'Assets'\/'Liability'\/
-- 'Equity' account (the carried-forward balances, 次期繰越); 'Cost'\/'Revenue'
-- (nominal) accounts are __excluded by construction__, since they have been
-- closed out to the income summary. Layout:
--
-- > Debit | Account Title | Credit
--
-- The net balance per title is computed with an explicit @diffRL@ netting
-- (the same aggregation as 'writeCompoundTrialBalance'); no implicit @bar@.
-- The final row gives the debit and credit totals (which must agree).
--
-- Titles are listed in 'Ord' order for deterministic output.
--
-- ==== __Examples__
--
-- A ledger with cash, a loan (liability) and a sale (revenue). Only the real
-- accounts (Cash, LoansPayable) survive; the nominal Sales account is dropped.
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let led = (100 .@ Not:<Cash) .+ (40 .@ Hat:<Cash) .+ (60 .@ Not:<LoansPayable) .+ (100 .@ Not:<Sales) :: T
-- >>> mapM_ print (postClosingTrialBalanceRows led)
-- ["Debit","Account Title","Credit"]
-- ["60.0","Cash",""]
-- ["","LoansPayable","60.0"]
-- ["60.0","Total","60.0"]
--
-- Complexity: O(s) (single pass over s scalar entries; see @accountGrossTotals@).
postClosingTrialBalanceRows :: (HatVal n, HatBaseClass b, ExBaseClass b)
                            => Alg n b -> [[T.Text]]
postClosingTrialBalanceRows alg =
    header : bodyRows ++ [totalRow]
  where
    header = [T.pack "Debit", T.pack "Account Title", T.pack "Credit"]
    -- Single pass (O(s)): gross debit/credit totals per title, in Ord order;
    -- keep only real (Assets/Liability/Equity) titles.
    totals = accountGrossTotals alg
    titles = L.filter isReal (OMap.keys totals)
    isReal t = let d = classifyAccountDivision t
               in d == Assets || d == Liability || d == Equity
    (bodyRows, debitTotal, creditTotal) =
        L.foldl' step ([], emptyAccum, emptyAccum) titles
    step (rows, dt, ct) t =
        let (side, mag) = readAccumBalance
                (OMap.findWithDefault (emptyAccum, emptyAccum) t totals)
            (debit, credit) = OMap.findWithDefault (emptyAccum, emptyAccum) t totals
            difference = mergeAccum debit (negateAccum credit)
            (dCell, cCell) = sideCells (side, mag)
            (dt', ct') = case side of
                Debit -> (mergeAccum dt difference, ct)
                Credit -> (dt, mergeAccum ct (negateAccum difference))
                _                          -> (dt, ct)
            row = [dCell, tshow t, cCell]
         in (rows ++ [row], dt', ct')
    totalRow = [tshow (roundAccum debitTotal), T.pack "Total", tshow (roundAccum creditTotal)]

-- | Output a post-closing trial balance (繰越試算表) in CSV format.
-- Pure layout is delegated to 'postClosingTrialBalanceRows'; this function only
-- writes the file. Only 'Assets'\/'Liability'\/'Equity' (real) accounts appear;
-- 'Cost'\/'Revenue' accounts are excluded — see 'postClosingTrialBalanceRows'.
--
-- Complexity: O(s) (single pass; see 'postClosingTrialBalanceRows').
writePostClosingTrialBalance :: (HatVal n, HatBaseClass b, ExBaseClass b)
                             => FilePath
                             -> Alg n b
                             -> IO ()
writePostClosingTrialBalance path alg =
    writeCSV path (postClosingTrialBalanceRows alg)

