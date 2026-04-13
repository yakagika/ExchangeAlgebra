{-# LANGUAGE FlexibleContexts #-}
{- |
    Module     : ExchangeAlgebra
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi15@cs.dis.titech.ac.jp

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

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
    , writeCompoundTrialBalance
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


-- | Output account ledgers in CSV format.
--
-- __Note__: Not yet implemented. Calling this will raise an exception.
writeAccountOf :: (HatVal n, HatBaseClass b, ExBaseClass b)
             => [AccountTitles]
             -> FilePath
             -> Alg n b
             -> (b -> Day)
             -> IO ()
writeAccountOf _ _ _ _ = undefined


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
