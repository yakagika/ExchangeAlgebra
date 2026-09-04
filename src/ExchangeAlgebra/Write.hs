{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns    #-}
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
    , bsRows
    , writePL
    , plRows
    , writeJournal
    , journalRows
    , writeAccountOf
    , writeAccountOfJournal
    , accountLedgerRowsJournal
    , writeCompoundTrialBalance
    , compoundTrialBalanceRows
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
    , restoreJournalFromBinarySpillChecked
      -- * Helpers
    , balanceOf
    , tshow
    , toSameLength
    ) where

import qualified    ExchangeAlgebra.Algebra     as EA
import              ExchangeAlgebra.Algebra
import qualified    ExchangeAlgebra.Journal     as EJ
import              ExchangeAlgebra.Journal     ((.|))

import qualified    ExchangeAlgebra.Algebra.Transfer    as ET
import qualified    ExchangeAlgebra.Reporting.Group     as RG

import              ExchangeAlgebra.Simulate.Spill
                    ( restoreJournalFromBinarySpill
                    , restoreJournalFromBinarySpillChecked )

import qualified    Data.List                   as L
import qualified    Data.Text                   as T

import              Control.Monad
import qualified    Data.Set as Set
import qualified    Data.HashMap.Strict as Map
import qualified    Data.Map.Strict as OMap
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

-- | Render one 'RG.RelativeAmount' as a statement cell. The magnitude is
-- always non-negative (see "ExchangeAlgebra.Reporting.Group"); a deduction, or
-- a net that its deductions pushed past zero, is shown with a leading @-@.
-- The minus sign exists only here, in the rendered text — never in a value.
--
-- Complexity: O(show cost)
renderRelative :: (HatVal n) => RG.RelativeAmount n -> T.Text
renderRelative amount
    | RG.raBelowZero amount = T.cons '-' magnitude
    | otherwise             = magnitude
  where
    magnitude = tshow (RG.raMagnitude amount)

-- | Flatten presentation blocks into @(label, value)@ statement cells.
-- Gross and deduction rows are labelled by their account title, subtotal and
-- net rows by the group's label.
--
-- Complexity: O(r) (r = number of rows in the given blocks)
groupCells :: (HatVal n)
           => [RG.PresentationGroupDef]
           -> [(RG.PresentationGroupDef, [RG.GroupRow n])]
           -> [(T.Text, T.Text)]
groupCells defs blocks =
    [ (label (RG.grKind row), renderRelative (RG.grAmount row))
    | (_, rows) <- blocks, row <- rows ]
  where
    label kind = case kind of
        RG.GrossRow t      -> tshow t
        RG.DeductionRow t  -> tshow t
        RG.SubgroupRow key -> groupLabel key
        RG.NetRow key      -> groupLabel key
    groupLabel key = maybe (tshow key) RG.pgLabel (RG.lookupGroupDef defs key)

-- | Build the rows of a Balance Sheet, as a pure value (the part 'writeBS'
-- renders to CSV).
--
-- Internally applies @'ET.finalStockTransfer'@ -- the only netting on the
-- /algebra/ this function does -- which closes every 'Cost'\/'Revenue'
-- account into 'RetainedEarnings' via @('.-')@\/@bar@. The closed algebra is
-- then partitioned by 'whichSide'\/'whatDiv' into assets (debit side) and
-- liability\/equity (credit side, further split by division);
-- 'decL'\/'decR'\/'EA.filter' only select entries, they do not aggregate them.
--
-- __Contra accounts (Definition 7 amendment, Land 3).__ Contra assets
-- (@whatDiv == Assets && isContra@, e.g. 貸倒引当金\/減価償却累計額) sit on
-- the credit side, so a plain side partition would either drop them from the
-- statement or file them under Liability. They are instead collected into the
-- presentation groups of "ExchangeAlgebra.Reporting.Group" and shown as a
-- real deduction — @gross lines → deduction lines → net line@ — inside the
-- Asset column, replacing Land 2's temporary placement in the Liability
-- column. A
-- group is formed only when one of its contra accounts actually carries gross
-- activity, so a chart that contains no contra posting keeps its ordinary
-- rows. Deduction
-- and negative-net cells carry a leading @-@; values themselves stay in
-- \(\mathbb{R}_0^+\).
--
-- Column totals use the ordinary cells plus each outermost group's net, so
-- nested subtotal rows are not counted twice. __Known limitation:__ an
-- ungrouped non-contra asset whose net balance is
-- on the credit side (an abnormal balance) is still not displayed, and is now
-- excluded from the totals as well; the side-versus-division placement of
-- abnormal balances is a separate pre-existing issue, not part of the contra
-- amendment.
-- Presentation groups are financial-statement aggregates keyed by account
-- title. With a multi-axis base, grouped titles therefore aggregate across
-- the remaining axes; ungrouped titles retain the legacy per-entry layout.
--
-- Layout:
--
-- > Asset | <titles...> | Total
-- >       | <values...> | <asset total>
-- > Liability | <titles...> | Equity | <titles...> | Total
-- >           | <values...> |        | <values...> | <liability+equity total>
--
-- ==== __Examples__
--
-- Cash 100 (asset), a loan 60 (liability), capital 40 (equity); no
-- cost\/revenue accounts, so @'ET.finalStockTransfer'@ is a no-op, and no
-- contra account is present, so no presentation group is formed:
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = (100 .@ Not:<Cash) .+ (60 .@ Not:<LoansPayable) .+ (40 .@ Not:<CapitalStock) :: T
-- >>> mapM_ print (bsRows alg)
-- ["Asset","","Liability",""]
-- ["Cash","100.0","LoansPayable","60.0"]
-- ["Total","100.0","Equity",""]
-- ["","","CapitalStock","40.0"]
-- ["","","Total","100.0"]
--
-- Receivables 1000 with an allowance of 100 against them, and capital 900.
-- The allowance is deducted from the receivables and the asset total is the
-- net 900, not the gross 1000:
--
-- >>> let contra = (1000 .@ Not:<AccountsReceivable) .+ (100 .@ Not:<AllowanceForDoubtfulAccounts) .+ (900 .@ Not:<CapitalStock) :: T
-- >>> mapM_ print (bsRows contra)
-- ["Asset","","Liability",""]
-- ["AccountsReceivable","1000.0","Equity",""]
-- ["AllowanceForDoubtfulAccounts","-100.0","CapitalStock","900.0"]
-- ["TradeReceivablesNet","900.0","Total","900.0"]
-- ["Total","900.0","",""]
--
-- Complexity: O(s) (s = total number of scalar entries)
bsRows :: (HatVal n, HatBaseClass b, ExBaseClass b) => Alg n b -> [[T.Text]]
bsRows alg = result
  where
    transferred = ET.finalStockTransfer alg
    grouping = RG.groupingForDivisions [Assets, Liability, Equity]
                                       RG.defaultPresentationGrouping
    grouped = RG.presentGroups grouping (accountGrossTotals transferred)
    consumed = RG.gpConsumed grouped
    ungrouped = EA.filter
        (\x -> not (Set.member (getAccountTitle (_hatBase x)) consumed)) transferred
    creditSide = decR ungrouped
    debitSide = decL ungrouped
    assets = debitSide
    liability = EA.filter (\x -> whatDiv (_hatBase x) == Liability) creditSide
    equity = EA.filter (\x -> whatDiv (_hatBase x) == Equity) creditSide
    blocksIn divisions =
        [block | block@(def, _) <- RG.gpBlocks grouped
               , RG.pgDivision def `elem` divisions]
    rootOf division = OMap.findWithDefault (zeroValue, zeroValue) division
        (RG.gpRootTotals grouped)
    -- Totals are the sum of the displayed cells: the ungrouped entries of the
    -- column plus each group's net (counted once, at its outermost block).
    assetGross = RG.addGross (EA.norm assets, zeroValue) (rootOf Assets)
    creditGross = RG.addGross
        (zeroValue, EA.norm liability + EA.norm equity)
        (RG.addGross (rootOf Liability) (rootOf Equity))
    assetTotal = renderRelative (RG.relativeTo Debit assetGross)
    creditTotal = renderRelative (RG.relativeTo Credit creditGross)
    (assetGroupText, assetGroupValue) =
        unzip (groupCells grouping (blocksIn [Assets]))
    (liabilityGroupText, liabilityGroupValue) =
        unzip (groupCells grouping (blocksIn [Liability]))
    (equityGroupText, equityGroupValue) =
        unzip (groupCells grouping (blocksIn [Equity]))
    assetsText = L.map (tshow . getAccountTitle . _hatBase) (EA.toList assets)
                 ++ assetGroupText
    assetsValue = L.map (tshow . _val) (EA.toList assets) ++ assetGroupValue
    liabilityText = L.map (tshow . getAccountTitle . _hatBase) (EA.toList liability)
                    ++ liabilityGroupText
    liabilityValue = L.map (tshow . _val) (EA.toList liability) ++ liabilityGroupValue
    equityText = L.map (tshow . getAccountTitle . _hatBase) (EA.toList equity)
                 ++ equityGroupText
    equityValue = L.map (tshow . _val) (EA.toList equity) ++ equityGroupValue
    result = csvTranspose
      [ [T.pack "Asset"] ++ assetsText ++ [T.pack "Total"]
      , [T.empty] ++ assetsValue ++ [assetTotal]
      , [T.pack "Liability"] ++ liabilityText ++ [T.pack "Equity"] ++ equityText ++ [T.pack "Total"]
      , [T.empty] ++ liabilityValue ++ [T.empty] ++ equityValue ++ [creditTotal]
      ]

-- | Output a Balance Sheet in CSV format. Pure layout is delegated to
-- 'bsRows'; this function only writes the file. See 'bsRows' for the
-- @'ET.finalStockTransfer'@ closing semantics and the column layout.
--
-- Complexity: O(s) (s = total number of scalar entries; see 'bsRows')
writeBS :: (HatVal n, HatBaseClass b, ExBaseClass b) => FilePath -> Alg n b -> IO ()
writeBS path alg = writeCSV path (bsRows alg)

-- | Build the rows of a Profit and Loss Statement, as a pure value (the part
-- 'writePL' renders to CSV).
--
-- No closing is applied here (contrast 'bsRows', which applies
-- @'ET.finalStockTransfer'@) -- this decomposes the algebra /as given/ into
-- cost and revenue entries by 'whichSide'\/'whatDiv' ('decL'\/'decR'\/'EA.filter'
-- only select, they do not aggregate).
--
-- __Contra accounts (Definition 7 amendment, Land 3).__ A contra revenue
-- (売上割戻) sits on the debit side and a contra cost (仕入割戻, 還付法人税等)
-- on the credit side, so the side partition above would drop all three from
-- the statement entirely. They are instead collected into the presentation
-- groups of "ExchangeAlgebra.Reporting.Group" and shown as a real deduction
-- (@gross lines → deduction lines → net line@) inside their own column, on
-- the same terms as 'bsRows'. A group is formed only when one of its contra
-- accounts carries gross activity, so a statement containing no contra
-- posting keeps its ordinary rows. The column totals keep their historical cross-placement
-- (the Cost column's total cell states the revenue total and vice versa), but
-- are now the sum of the cells displayed in the other column.
--
-- Layout:
--
-- > Cost | <titles...> | Total
-- >      | <values...> | <revenue total>
-- > Revenue | <titles...> | Total
-- >         | <values...> | <cost total>
--
-- ==== __Examples__
--
-- A single sale of 500 (revenue) against its cost of 300; no contra account,
-- so no presentation group is formed:
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = (500 .@ Not:<Sales) .+ (300 .@ Not:<SalesCost) :: T
-- >>> mapM_ print (plRows alg)
-- ["Cost","","Revenue",""]
-- ["SalesCost","300.0","Sales","500.0"]
-- ["Total","500.0","Total","300.0"]
--
-- The same sale with a rebate of 50 granted on it. Gross sales stay visible,
-- the rebate is deducted, and net sales carry into the total:
--
-- >>> let rebated = alg .+ (50 .@ Not:<SalesRebates) :: T
-- >>> mapM_ print (plRows rebated)
-- ["Cost","","Revenue",""]
-- ["SalesCost","300.0","Sales","500.0"]
-- ["","","SalesRebates","-50.0"]
-- ["","","NetSales","450.0"]
-- ["Total","450.0","Total","300.0"]
--
-- Complexity: O(s) (s = total number of scalar entries)
plRows :: (HatVal n, HatBaseClass b, ExBaseClass b) => Alg n b -> [[T.Text]]
plRows alg = result
  where
    grouping = RG.groupingForDivisions [Cost, Revenue] RG.defaultPresentationGrouping
    grouped = RG.presentGroups grouping (accountGrossTotals alg)
    consumed = RG.gpConsumed grouped
    ungrouped = EA.filter
        (\x -> not (Set.member (getAccountTitle (_hatBase x)) consumed)) alg
    creditSide = decR ungrouped
    debitSide = decL ungrouped
    cost = EA.filter (\x -> whatDiv (_hatBase x) == Cost) debitSide
    revenue = EA.filter (\x -> whatDiv (_hatBase x) == Revenue) creditSide
    blocksIn division =
        [block | block@(def, _) <- RG.gpBlocks grouped
               , RG.pgDivision def == division]
    rootOf division = OMap.findWithDefault (zeroValue, zeroValue) division
        (RG.gpRootTotals grouped)
    costGross = RG.addGross (EA.norm cost, zeroValue) (rootOf Cost)
    revenueGross = RG.addGross (zeroValue, EA.norm revenue) (rootOf Revenue)
    -- Historical cross-placement preserved: the Cost column's total cell
    -- states the revenue total, and the Revenue column's the cost total.
    debitTotal = renderRelative (RG.relativeTo Debit costGross)
    creditTotal = renderRelative (RG.relativeTo Credit revenueGross)
    (costGroupText, costGroupValue) = unzip (groupCells grouping (blocksIn Cost))
    (revenueGroupText, revenueGroupValue) =
        unzip (groupCells grouping (blocksIn Revenue))
    costText = L.map (tshow . getAccountTitle . _hatBase) (EA.toList cost)
               ++ costGroupText
    costValue = L.map (tshow . _val) (EA.toList cost) ++ costGroupValue
    revenueText = L.map (tshow . getAccountTitle . _hatBase) (EA.toList revenue)
                  ++ revenueGroupText
    revenueValue = L.map (tshow . _val) (EA.toList revenue) ++ revenueGroupValue
    (ct, rt) = toSameLength costText revenueText
    (cv, rv) = toSameLength costValue revenueValue
    result = csvTranspose
      [ [T.pack "Cost"] ++ ct ++ [T.pack "Total"]
      , [T.empty] ++ cv ++ [creditTotal]
      , [T.pack "Revenue"] ++ rt ++ [T.pack "Total"]
      , [T.empty] ++ rv ++ [debitTotal]
      ]

-- | Output a Profit and Loss Statement in CSV format. Pure layout is
-- delegated to 'plRows'; this function only writes the file.
--
-- Complexity: O(s) (s = total number of scalar entries; see 'plRows')
writePL :: (HatVal n, HatBaseClass b, ExBaseClass b) => FilePath -> Alg n b -> IO ()
writePL path alg = writeCSV path (plRows alg)

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
-- listed by 'decL'\/'decR' and padded to equal length with 'toSameLength' --
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
-- Complexity: O(s) (s = number of scalar entries).
accountGrossTotals :: (HatVal n, HatBaseClass b, ExBaseClass b)
                   => Alg n b -> OMap.Map AccountTitles (n, n)
accountGrossTotals = EA.foldEntries step OMap.empty
  where
    -- (debit gross, credit gross)
    step acc v b =
        let !t   = getAccountTitle b
            !pair = case whichSide b of
                Debit  -> (v, zeroValue)
                Credit -> (zeroValue, v)
                Side   -> (zeroValue, zeroValue)
        in OMap.insertWith addPair t pair acc
    addPair (d1, c1) (d2, c2) = (d1 + d2, c1 + c2)

-- | Net a @(debit gross, credit gross)@ pair into a @(Side, magnitude)@ balance,
-- reproducing 'diffRL' exactly. 'diffRL' compares @r = 'norm' . 'decR'@ (credit)
-- against @l = 'norm' . 'decL'@ (debit) with the scale-aware tolerance, so the
-- same comparison is applied here: near-equal sides report @(v'Side', 0)@,
-- otherwise the larger side wins with the non-negative difference.
--
-- Complexity: O(1).
netGross :: (HatVal n) => (n, n) -> (Side, n)
netGross (l, r)   -- l = debit gross, r = credit gross
    | nearlyEqScaled r l = (Side, zeroValue)
    | r > l              = (Credit, r - l)
    | otherwise          = (Debit, l - r)

-- | Build the rows of a Compound Trial Balance (合計残高試算表), as a pure
-- value (the part 'writeCompoundTrialBalance' renders to CSV). Calculates the
-- debit total, credit total, and balance for each account title, aggregated
-- via the single-pass @accountGrossTotals@\/@netGross@ (the same
-- @diffRL@-equivalent netting as 'balanceOf'; no implicit @bar@). Layout:
--
-- > Debit Balance | Debit Total | Account Title | Credit Total | Credit Balance
--
-- __Legacy column-placement quirk (preserved verbatim):__ unlike
-- 'worksheetRows'\/'postClosingTrialBalanceRows' (which route a @(side,mag)@
-- balance through @sideCells@, putting a Debit balance in the Debit cell and
-- a Credit balance in the Credit cell), this layout places the balance
-- figure in the column pair /opposite/ the netted side: a debit-heavy
-- account's balance lands in the __Credit Balance__ (rightmost) column, and a
-- credit-heavy account's balance lands in the __Debit Balance__ (leftmost)
-- column — see the example below. Reusing @sideCells@ here would require
-- flipping 'Debit'\/'Credit' first, which is no clearer than the explicit
-- case analysis in @step@ below, so this was kept as-is rather than
-- consolidated (design-review C7) to guarantee output is unchanged.
--
-- ==== __Examples__
--
-- Cash is debit-heavy (gross debit 100, credit 0); CapitalStock and
-- LoansPayable are credit-heavy. Note where each balance figure lands:
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = (100 .@ Not:<Cash) .+ (60 .@ Not:<LoansPayable) .+ (40 .@ Not:<CapitalStock) :: T
-- >>> mapM_ print (compoundTrialBalanceRows alg)
-- ["Debit Balance","Debit Total","Account Title","Credit Total","Credit Balance"]
-- ["","100.0","Cash","0.0","100.0"]
-- ["40.0","0.0","CapitalStock","40.0",""]
-- ["60.0","0.0","LoansPayable","60.0",""]
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
        L.foldl' step ([], zeroValue, zeroValue, zeroValue, zeroValue) accounts
    totalLine = [ tshow debitBalanceTotal
                , tshow creditTotal
                , T.pack "Total"
                , tshow debitTotal
                , tshow creditBalanceTotal
                ]
    step (accLines, dbt, dt, cbt, ct) (a, gross) =
        let xl = fst gross   -- norm (decL xs) : debit gross
            xr = snd gross   -- norm (decR xs) : credit gross
            (dc, diff) = netGross gross
            -- 'netGross'/'diffRL' returns the wildcard v'Side' with a zero
            -- difference when an account nets to zero (e.g. a fully-cleared
            -- suspense account). Treat that as no balance on either side
            -- (cf. 'sideCells').
            (dbt', cbt') = case dc of
                Credit -> (dbt + diff, cbt)
                Debit  -> (dbt, cbt + diff)
                Side   -> (dbt, cbt)
            -- See the Haddock above: the Credit case places 'diff' in the
            -- *Debit Balance* column (position 0) and the Debit case places
            -- it in the *Credit Balance* column (position 4) -- opposite of
            -- 'sideCells' -- a legacy layout kept verbatim.
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
                Side   -> [ T.empty
                          , tshow xl
                          , tshow a
                          , tshow xr
                          , T.empty
                          ]
         in (accLines ++ [line], dbt', dt + xr, cbt', ct + xl)

-- | Output a Compound Trial Balance in CSV format.
-- Calculates the debit total, credit total, and balance for each account
-- title and outputs as a table. Pure layout is delegated to
-- 'compoundTrialBalanceRows'; this function only writes the file. See
-- 'compoundTrialBalanceRows' for the column layout (including the legacy
-- Debit\/Credit Balance placement quirk).
--
-- Complexity: O(s) (single pass over s scalar entries; see
-- 'compoundTrialBalanceRows')
writeCompoundTrialBalance :: (HatVal n, HatBaseClass b, ExBaseClass b)
                           => FilePath
                           -> Alg n b
                           -> IO ()
writeCompoundTrialBalance path alg =
    writeCSV path (compoundTrialBalanceRows alg)


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
    -- 'balanceOf t alg' = 'diffRL (projByAccountTitle t alg)' is reproduced as
    -- 'netGross' of the per-title gross pair; a title absent from a map (e.g.
    -- only in 'adj', not 'pre') yields '(0,0)', i.e. 'diffRL Zero = (Side, 0)'.
    preTotals = accountGrossTotals pre
    adjTotals = accountGrossTotals adj
    combTotals = accountGrossTotals combined
    grossOf m t = OMap.findWithDefault (zeroValue, zeroValue) t m
    -- Title enumeration: the keys of 'combined' in Ord order (= the previous
    -- 'L.sort . Set.toList . Set.fromList' over 'EA.toList combined').
    titles = OMap.keys combTotals
    -- per-title row + accumulate the four debit/credit column totals
    (bodyRows, tbD, tbC, plD, plC, bsD, bsC) =
        L.foldl' step ([], zeroValue, zeroValue, zeroValue, zeroValue, zeroValue, zeroValue) titles
    step (rows, tbd, tbc, pld, plc, bsd, bsc) t =
        let tb              = netGross (grossOf preTotals t)
            (tbDc, tbCc)    = sideCells tb
            adjPair         = netGross (grossOf adjTotals t)
            (adjDc, adjCc)  = sideCells adjPair
            finalPair@(finalSide, finalMag) = netGross (grossOf combTotals t)
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
                _      -> (d, c)   -- wildcard v'Side' (balanced to zero): adds nothing
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
        L.foldl' step ([], zeroValue, zeroValue) titles
    step (rows, dt, ct) t =
        let (side, mag) = netGross (OMap.findWithDefault (zeroValue, zeroValue) t totals)
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
-- Complexity: O(s) (single pass; see 'postClosingTrialBalanceRows').
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
writeTermIO :: (HatVal n,BaseClass b, Ix t, Ix b, Enum b)
            => FilePath -> t -> IOArray (t, b, b) n  -> IO ()
writeTermIO path t arr = do
    ((_, c1Min, c2Min), (_, c1Max, c2Max)) <- getBounds arr
    let rows = [c1Min .. c1Max]
    let cols = [c2Min .. c2Max]
    body <- forM rows $ \r -> do
        cells <- forM cols $ \c -> tshow <$> readArray arr (t, r, c)
        pure (tshow r : cells)
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
        cells <- forM cols $ \c -> tshow <$> readArray arr (r, c)
        pure (tshow r : cells)
    writeCSV path ((T.pack "" : L.map tshow cols) : body)
