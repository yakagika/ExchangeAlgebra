{-# LANGUAGE FlexibleContexts #-}
{- |
    Module     : ExchangeAlgebra.Write
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

-}

module ExchangeAlgebra.Write
    ( -- * CSV utilities
      writeCSV
    , csvTranspose
      -- * Balance Sheet / P&L / Journal output
    , writeBS
    , writePL
    , writeJournal
    , writeAccountOf
    , writeAccountOfJournal
    , writeCompoundTrialBalance
      -- * Closing documents (決算書類)
    , writeWorksheet
    , worksheetRows
    , writePostClosingTrialBalance
    , postClosingTrialBalanceRows
    , accountLedgerRows
      -- * Simulation output
    , writeTermIO
    , writeIOMatrix
      -- * Spill / Restore
    , restoreJournalFromBinarySpill
      -- * Helpers
    , tshow
    , toSameLength
    ) where

import qualified    ExchangeAlgebra.Algebra     as EA
import              ExchangeAlgebra.Algebra
import qualified    ExchangeAlgebra.Journal     as EJ

import qualified    ExchangeAlgebra.Algebra.Transfer    as ET

import              ExchangeAlgebra.Simulate

import qualified    Data.List                   as L
import qualified    Data.Text                   as T
import qualified    Data.Binary                 as Binary

import              Control.Monad
import qualified    Data.Set as Set
import qualified    Data.HashMap.Strict as Map
import              Data.Array.IO
import              Data.Time           (Day)
import              System.IO           (openFile, IOMode(WriteMode), hClose)
import qualified    Data.Text.IO        as TIO

-- | Transpose a matrix of Text, padding shorter rows with empty Text.
csvTranspose :: [[T.Text]] -> [[T.Text]]
csvTranspose [] = []
csvTranspose mx = [ [ getCell r i | r <- mx ] | i <- [0 .. maxLen - 1] ]
  where
    maxLen = L.maximum (L.map L.length mx)
    getCell row i
        | i < L.length row = row !! i
        | otherwise        = T.empty

-- | Write a matrix of Text as a CSV file. Each cell is quoted.
writeCSV :: FilePath -> [[T.Text]] -> IO ()
writeCSV path rows = do
    h <- openFile path WriteMode
    mapM_ (TIO.hPutStrLn h . toCsvLine) rows
    hClose h
  where
    toCsvLine = T.intercalate (T.pack ",") . L.map quoteCell
    quoteCell t = T.concat [T.pack "\"", T.replace (T.pack "\"") (T.pack "\"\"") t, T.pack "\""]

-- | Helper to convert from Show to Text.
--
-- Complexity: O(show cost)
tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

-- | Output a Balance Sheet in CSV format.
-- Internally applies @finalStockTransfer@, then decomposes into assets, liabilities, and equity for output.
--
-- Complexity: O(s) (s = total number of scalar entries)
writeBS :: (HatVal n, HatBaseClass b, ExBaseClass b) => FilePath -> Alg n b -> IO ()
writeBS path alg = writeCSV path result
  where
    transferred = ET.finalStockTransfer alg
    debitSide = decR transferred
    creditSide = decL transferred
    assets = creditSide
    liability = EA.filter (\x -> whatDiv (_hatBase x) == Liability) debitSide
    equity = EA.filter (\x -> whatDiv (_hatBase x) == Equity) debitSide
    debitTotal = tshow (EA.norm debitSide)
    creditTotal = tshow (EA.norm creditSide)
    assetsText = L.map (tshow . getAccountTitle . _hatBase) (EA.toList assets)
    assetsValue = L.map (tshow . _val) (EA.toList assets)
    liabilityText = L.map (tshow . getAccountTitle . _hatBase) (EA.toList liability)
    liabilityValue = L.map (tshow . _val) (EA.toList liability)
    equityText = L.map (tshow . getAccountTitle . _hatBase) (EA.toList equity)
    equityValue = L.map (tshow . _val) (EA.toList equity)
    result = csvTranspose
      [ [T.pack "Asset"] ++ assetsText ++ [T.pack "Total"]
      , [T.empty] ++ assetsValue ++ [creditTotal]
      , [T.pack "Liability"] ++ liabilityText ++ [T.pack "Equity"] ++ equityText ++ [T.pack "Total"]
      , [T.empty] ++ liabilityValue ++ [T.empty] ++ equityValue ++ [debitTotal]
      ]

-- | Output a Profit and Loss Statement in CSV format.
-- Decomposes into costs and revenues for output.
--
-- Complexity: O(s) (s = total number of scalar entries)
writePL :: (HatVal n, HatBaseClass b, ExBaseClass b) => FilePath -> Alg n b -> IO ()
writePL path alg = writeCSV path result
  where
    debitSide = decR alg
    creditSide = decL alg
    cost = EA.filter (\x -> whatDiv (_hatBase x) == Cost) creditSide
    revenue = EA.filter (\x -> whatDiv (_hatBase x) == Revenue) debitSide
    debitTotal = tshow (EA.norm cost)
    creditTotal = tshow (EA.norm revenue)
    costText = L.map (tshow . getAccountTitle . _hatBase) (EA.toList cost)
    costValue = L.map (tshow . _val) (EA.toList cost)
    revenueText = L.map (tshow . getAccountTitle . _hatBase) (EA.toList revenue)
    revenueValue = L.map (tshow . _val) (EA.toList revenue)
    (ct, rt) = toSameLength costText revenueText
    (cv, rv) = toSameLength costValue revenueValue
    result = csvTranspose
      [ [T.pack "Cost"] ++ ct ++ [T.pack "Total"]
      , [T.empty] ++ cv ++ [creditTotal]
      , [T.pack "Revenue"] ++ rt ++ [T.pack "Total"]
      , [T.empty] ++ rv ++ [debitTotal]
      ]

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

-- | Output journal entries in CSV format.
-- Groups by date and records the debit/credit account titles and amounts for each day.
--
-- Complexity: O(s * log d) (s = number of entries, d = number of distinct dates)
writeJournal :: (HatVal n, HatBaseClass b, ExBaseClass b)
             => FilePath
             -> Alg n b
             -> (b -> Day)
             -> IO ()
writeJournal path alg f = do
    let days = L.sort $ Set.toList . Set.fromList $ L.map (f . _hatBase) $ EA.toList alg
    rows <- forM days $ \d -> do
        let da = EA.filter (\y -> (f . _hatBase) y == d) alg
        let dl = decL da
        let dr = decR da
        let dlTexts = L.map (tshow . getAccountTitle . _hatBase) (EA.toList dl)
        let drTexts = L.map (tshow . getAccountTitle . _hatBase) (EA.toList dr)
        let dlValues = L.map (tshow . _val) (EA.toList dl)
        let drValues = L.map (tshow . _val) (EA.toList dr)
        let (dt', ct') = toSameLength dlTexts drTexts
        let (dv', cv') = toSameLength dlValues drValues
        let (ds', _) = toSameLength [tshow d] cv'
        pure (ds', dt', dv', ct', cv')
    let ds = [T.pack "Day"] ++ concatMap (\(a,_,_,_,_) -> a) rows
    let dt = [T.pack "Debit"] ++ concatMap (\(_,a,_,_,_) -> a) rows
    let dv = [T.pack "Amount"] ++ concatMap (\(_,_,a,_,_) -> a) rows
    let ct = [T.pack "Credit"] ++ concatMap (\(_,_,_,a,_) -> a) rows
    let cv = [T.pack "Amount"] ++ concatMap (\(_,_,_,_,a) -> a) rows
    writeCSV path (csvTranspose [ds, dt, dv, ct, cv])


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

-- | Output general ledgers (総勘定元帳) from a 'EJ.Journal' in CSV format,
-- carrying the per-posting note (摘要) as an extra column.
--
-- As with 'writeAccountOf', postings are listed __individually, without
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
-- Complexity: O(t * s) (t = number of titles, s = number of postings).
writeAccountOfJournal :: (HatVal n, HatBaseClass b, ExBaseClass b, EJ.Note note)
                      => [AccountTitles]
                      -> FilePath
                      -> EJ.Journal note n b
                      -> IO ()
writeAccountOfJournal titles path j =
    writeCSV path (concatMap titleBlock titles)
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


-- | Output a Compound Trial Balance in CSV format.
-- Calculates the debit total, credit total, and balance for each account title and outputs as a table.
--
-- Complexity: O(s * a) (s = number of entries, a = number of distinct account titles)
writeCompoundTrialBalance :: (HatVal n, HatBaseClass b, ExBaseClass b)
                           => FilePath
                           -> Alg n b
                           -> IO ()
writeCompoundTrialBalance path alg = do
    let header = [T.pack "Debit Balance"
                 ,T.pack "Debit Total"
                 ,T.pack "Account Title"
                 ,T.pack "Credit Total"
                 ,T.pack "Credit Balance"]
    let accounts = L.sort
                 $ Set.toList . Set.fromList
                 $ L.map (getAccountTitle . _hatBase)
                 $ EA.toList alg
    let (lines', debitBalanceTotal, debitTotal, creditBalanceTotal, creditTotal) =
            L.foldl' step ([], zeroValue, zeroValue, zeroValue, zeroValue) accounts
    let totalLine = [ tshow debitBalanceTotal
                    , tshow creditTotal
                    , T.pack "Total"
                    , tshow debitTotal
                    , tshow creditBalanceTotal
                    ]
    writeCSV path (header : lines' ++ [totalLine])
  where
    step (accLines, dbt, dt, cbt, ct) a =
        let xs = projByAccountTitle a alg
            xr = norm (decR xs)
            xl = norm (decL xs)
            (dc, diff) = diffRL xs
            (dbt', cbt') = case dc of
                Credit -> (dbt + diff, cbt)
                Debit  -> (dbt, cbt + diff)
            line = case dc of
                Credit -> [ tshow diff
                          , tshow xl
                          , tshow a
                          , tshow xr
                          , T.empty
                          ]
                Debit  -> [ T.empty
                          , tshow xl
                          , tshow a
                          , tshow xr
                          , tshow diff
                          ]
         in (accLines ++ [line], dbt', dt + xr, cbt', ct + xl)


------------------------------------------------------------------
-- Closing documents (決算書類): worksheet & post-closing trial balance
------------------------------------------------------------------

-- | Per-account net balance: @('diffRL' . 'projByAccountTitle' title)@ on a
-- single-title restriction returns @(side, magnitude)@ where @side@ is the
-- balance side (Debit or Credit) after netting and @magnitude@ is the
-- (non-negative) amount. This is the explicit aggregation used by both the
-- worksheet trial-balance column and the post-closing trial balance (the
-- analogous netting that 'writeCompoundTrialBalance' performs).
balanceOf :: (HatVal n, HatBaseClass b, ExBaseClass b)
          => AccountTitles -> Alg n b -> (Side, n)
balanceOf t = diffRL . projByAccountTitle t

-- | Place a @(side, magnitude)@ balance into a (debit, credit) text column pair.
-- A zero magnitude produces two empty cells.
sideCells :: (HatVal n) => (Side, n) -> (T.Text, T.Text)
sideCells (side, mag)
    | mag == zeroValue = (T.empty, T.empty)
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
--     /pre-adjustment ledger/ (@'balanceOf' title pre@ — an explicit
--     @diffRL@ netting, no implicit @bar@);
--   * the __Adjustments__ pair is the net balance of the /adjustment entries/
--     (@'balanceOf' title adj@);
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
-- Complexity: O(a * s) (a = distinct titles, s = number of entries).
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
    titles = L.sort
           $ Set.toList . Set.fromList
           $ L.map (getAccountTitle . _hatBase)
           $ EA.toList combined
    -- per-title row + accumulate the four debit/credit column totals
    (bodyRows, tbD, tbC, plD, plC, bsD, bsC) =
        L.foldl' step ([], zeroValue, zeroValue, zeroValue, zeroValue, zeroValue, zeroValue) titles
    step (rows, tbd, tbc, pld, plc, bsd, bsc) t =
        let tb              = balanceOf t pre
            (tbDc, tbCc)    = sideCells tb
            adjPair         = balanceOf t adj
            (adjDc, adjCc)  = sideCells adjPair
            finalPair@(finalSide, finalMag) = balanceOf t combined
            div'            = classifyAccountDivision t
            isPL            = div' == Cost || div' == Revenue
            (plDc, plCc, bsDc, bsCc)
              | isPL      = let (d,c) = sideCells finalPair in (d, c, T.empty, T.empty)
              | otherwise = let (d,c) = sideCells finalPair in (T.empty, T.empty, d, c)
            row = [ tshow t, tbDc, tbCc, adjDc, adjCc, plDc, plCc, bsDc, bsCc ]
            -- column totals
            addCol side mag d c = case side of
                Debit  -> (d + mag, c)
                Credit -> (d, c + mag)
                _      -> (d, c)   -- wildcard 'Side' (balanced to zero): adds nothing
            (ntbd, ntbc) = let (s,m) = tb in addCol s m tbd tbc
            (npld, nplc) | isPL      = addCol finalSide finalMag pld plc
                         | otherwise = (pld, plc)
            (nbsd, nbsc) | not isPL  = addCol finalSide finalMag bsd bsc
                         | otherwise = (bsd, bsc)
         in (rows ++ [row], ntbd, ntbc, npld, nplc, nbsd, nbsc)
    -- trial-balance totals row (整理記入 totals are intentionally omitted: the
    -- adjustment column does not have a meaningful single grand total in the
    -- lecture's layout; the self-check is on P/L and B/S).
    tbTotalRow =
        [ T.pack "Subtotal"
        , tshow tbD, tshow tbC
        , T.empty, T.empty
        , tshow plD, tshow plC
        , tshow bsD, tshow bsC ]
    -- profit/loss balancing figure (当期純利益 / 純損失).
    -- P/L imbalance: if credit (revenue) side exceeds debit (cost) side, the
    -- difference is net income, recorded on the P/L debit side and the B/S
    -- credit side (it increases equity). The reverse is a net loss.
    plDiff = absDiff plD plC
    bsDiff = absDiff bsD bsC
    netProfit = plC > plD   -- revenue side larger => profit
    (netLabel, netPlD, netPlC, netBsD, netBsC)
        | plC == plD = (T.pack "Net Income/Loss", T.empty, T.empty, T.empty, T.empty)
        | netProfit  = (T.pack "Net Income", tshow plDiff, T.empty, T.empty, tshow bsDiff)
        | otherwise  = (T.pack "Net Loss",   T.empty, tshow plDiff, tshow bsDiff, T.empty)
    netRow =
        [ netLabel
        , T.empty, T.empty
        , T.empty, T.empty
        , netPlD, netPlC
        , netBsD, netBsC ]
    -- grand totals after adding the balancing figure (both sides now equal).
    plTotal = max plD plC
    bsTotal = max bsD bsC
    grandTotalRow =
        [ T.pack "Total"
        , T.empty, T.empty
        , T.empty, T.empty
        , tshow plTotal, tshow plTotal
        , tshow bsTotal, tshow bsTotal ]
    absDiff a b = if a >= b then a - b else b - a

-- | Output an 8-column worksheet (8 桁精算表) in CSV format.
-- Pure layout is delegated to 'worksheetRows'; this function only writes the
-- file. See 'worksheetRows' for the column structure and the self-check
-- (P\/L vs B\/S balancing figure) semantics.
--
-- Complexity: O(a * s) (a = distinct titles, s = number of entries).
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
-- Complexity: O(a * s) (a = distinct titles, s = number of entries).
postClosingTrialBalanceRows :: (HatVal n, HatBaseClass b, ExBaseClass b)
                            => Alg n b -> [[T.Text]]
postClosingTrialBalanceRows alg =
    header : bodyRows ++ [totalRow]
  where
    header = [T.pack "Debit", T.pack "Account Title", T.pack "Credit"]
    titles = L.filter isReal
           $ L.sort
           $ Set.toList . Set.fromList
           $ L.map (getAccountTitle . _hatBase)
           $ EA.toList alg
    isReal t = let d = classifyAccountDivision t
               in d == Assets || d == Liability || d == Equity
    (bodyRows, debitTotal, creditTotal) =
        L.foldl' step ([], zeroValue, zeroValue) titles
    step (rows, dt, ct) t =
        let (side, mag) = balanceOf t alg
            (dCell, cCell) = sideCells (side, mag)
            (dt', ct') = case side of
                Debit  | mag /= zeroValue -> (dt + mag, ct)
                Credit | mag /= zeroValue -> (dt, ct + mag)
                _                          -> (dt, ct)
            row = [dCell, tshow t, cCell]
         in (rows ++ [row], dt', ct')
    totalRow = [tshow debitTotal, T.pack "Total", tshow creditTotal]

-- | Output a post-closing trial balance (繰越試算表) in CSV format.
-- Pure layout is delegated to 'postClosingTrialBalanceRows'; this function only
-- writes the file. Only 'Assets'\/'Liability'\/'Equity' (real) accounts appear;
-- 'Cost'\/'Revenue' accounts are excluded — see 'postClosingTrialBalanceRows'.
--
-- Complexity: O(a * s) (a = distinct titles, s = number of entries).
writePostClosingTrialBalance :: (HatVal n, HatBaseClass b, ExBaseClass b)
                             => FilePath
                             -> Alg n b
                             -> IO ()
writePostClosingTrialBalance path alg =
    writeCSV path (postClosingTrialBalanceRows alg)

------------------------------------------------------------------
-- Write Functions for Simulation
------------------------------------------------------------------

-- | Output the Input-Output Table for a specified term in CSV format.
-- Outputs a slice of the specified term from a 3D array (term, row industry, column industry).
--
-- Complexity: O(r * c) (r = number of rows, c = number of columns)
writeTermIO :: (HatVal n,BaseClass b, StateTime t, Ix b, Ix t, Enum b)
            => FilePath -> t -> IOArray (t, b, b) n  -> IO ()
writeTermIO path t arr = do
    ((_, c1Min, c2Min), (_, c1Max, c2Max)) <- getBounds arr
    let rows = [c1Min .. c1Max]
    let cols = [c2Min .. c2Max]
    body <- forM rows $ \r -> do
        vals <- forM cols $ \c -> tshow <$> readArray arr (t, r, c)
        pure (tshow r : vals)
    writeCSV path ((T.pack "" : L.map tshow cols) : body)

-- | Output a 2D IOArray (Input-Output Table or ripple effect matrix) in CSV format.
--
-- Complexity: O(r * c) (r = number of rows, c = number of columns)
writeIOMatrix :: FilePath -> IOArray (Int, Int) Double -> IO ()
writeIOMatrix path arr = do
    ((r1, c1), (r2, c2)) <- getBounds arr
    let rows = [r1 .. r2]
    let cols = [c1 .. c2]
    body <- forM rows $ \r -> do
        vals <- forM cols $ \c -> tshow <$> readArray arr (r, c)
        pure (tshow r : vals)
    writeCSV path ((T.pack "" : L.map tshow cols) : body)

------------------------------------------------------------------
-- Spill Restore Utilities
------------------------------------------------------------------

-- | Restore a complete Journal from spilled binary chunks and the current in-memory Journal.
-- The in-memory portion is narrowed to only terms after the last spill range,
-- so duplicate terms are not double-counted.
--
-- Complexity: O(file size + number of chunks * union cost)
restoreJournalFromBinarySpill
    :: ( Binary.Binary t
       , Ord t
       , Binary.Binary (EJ.Journal n v b)
       , EJ.Note n
       , HatVal v
       , HatBaseClass b
       )
    => FilePath
    -> (n -> t)
    -> EJ.Journal n v b
    -> IO (EJ.Journal n v b)
restoreJournalFromBinarySpill spillPath noteToTerm currentLedger = do
    chunks <- readBinarySpillFile spillPath
    let spilled = L.foldl' (\acc (_, j) -> acc .+ j) mempty chunks
        latestEnd = L.foldl'
            (\acc ((_, tEnd), _) ->
                case acc of
                    Nothing -> Just tEnd
                    Just x -> Just (max x tEnd)
            )
            Nothing
            chunks
        remainder = case latestEnd of
            Nothing -> currentLedger
            Just tEnd ->
                EJ.filterWithNote (\n _ -> noteToTerm n > tEnd) currentLedger
    pure (spilled .+ remainder)

------------------------------------------------------------------
