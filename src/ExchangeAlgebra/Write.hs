{- |
    Module     : ExchangeAlgebra
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi15@cs.dis.titech.ac.jp

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}

module ExchangeAlgebra.Write where

import qualified    ExchangeAlgebra.Algebra     as EA
import              ExchangeAlgebra.Algebra

import qualified    ExchangeAlgebra.Algebra.Transfer    as ET

import              ExchangeAlgebra.Simulate

import qualified    CSV.Text                    as CSV
import qualified    Data.List                   as L
import qualified    Data.Text                   as T

import              Control.Monad
import qualified    Data.Set as Set
import              Data.Array.IO
import              Data.Time           (Day)

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

-- | BalanceSheet貸借対照表の形でCSVで出力する
writeBS :: (HatVal n, HatBaseClass b, ExBaseClass b) => FilePath -> Alg n b -> IO ()
writeBS path alg = CSV.writeCSV path result
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
    result = CSV.transpose
      [ [T.pack "Asset"] ++ assetsText ++ [T.pack "Total"]
      , [T.empty] ++ assetsValue ++ [creditTotal]
      , [T.pack "Liability"] ++ liabilityText ++ [T.pack "Equity"] ++ equityText ++ [T.pack "Total"]
      , [T.empty] ++ liabilityValue ++ [T.empty] ++ equityValue ++ [debitTotal]
      ]

-- | Profit and Loss Statement 損益計算書の形でCSVで出力する
writePL :: (HatVal n, HatBaseClass b, ExBaseClass b) => FilePath -> Alg n b -> IO ()
writePL path alg = CSV.writeCSV path result
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
    result = CSV.transpose
      [ [T.pack "Cost"] ++ ct ++ [T.pack "Total"]
      , [T.empty] ++ cv ++ [creditTotal]
      , [T.pack "Revenue"] ++ rt ++ [T.pack "Total"]
      , [T.empty] ++ rv ++ [debitTotal]
      ]

-- | 同じ長さのリストに揃える関数
toSameLength :: [T.Text] -> [T.Text] -> ([T.Text],[T.Text])
toSameLength xs ys =
    case compare lx ly of
        EQ -> (xs, ys)
        LT -> (xs ++ replicate (ly - lx) T.empty, ys)
        GT -> (xs, ys ++ replicate (lx - ly) T.empty)
  where
    lx = Prelude.length xs
    ly = Prelude.length ys

-- | Journal Entry 仕訳
-- 日付,勘定科目,金額を借方,貸方別に記録
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
    CSV.writeCSV path (CSV.transpose [ds, dt, dv, ct, cv])


-- | Account 勘定口座
writeAccountOf :: (HatVal n, HatBaseClass b, ExBaseClass b)
             => [AccountTitles]
             -> FilePath
             -> Alg n b
             -> (b -> Day)
             -> IO ()
writeAccountOf _ _ _ _ = undefined


-- | 合計残高試算表
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
    CSV.writeCSV path (header : lines' ++ [totalLine])
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

-- | 複数年の産業連関表
writeTermIO :: (HatVal n,BaseClass b, StateTime t, Ix b, Ix t, Enum b)
            => FilePath -> t -> IOArray (t, b, b) n  -> IO ()
writeTermIO path t arr = do
    ((_, c1Min, c2Min), (_, c1Max, c2Max)) <- getBounds arr
    let rows = [c1Min .. c1Max]
    let cols = [c2Min .. c2Max]
    body <- forM rows $ \r -> do
        vals <- forM cols $ \c -> tshow <$> readArray arr (t, r, c)
        pure (tshow r : vals)
    CSV.writeCSV path ((T.pack "" : L.map tshow cols) : body)

-- | 与えられたInput-Output TableをCSVとして出力する
-- CSVとして出力する関数
-- 与えられた波及効果をCSVとして出力する
writeIOMatrix :: FilePath -> IOArray (Int, Int) Double -> IO ()
writeIOMatrix path arr = do
    ((r1, c1), (r2, c2)) <- getBounds arr
    let rows = [r1 .. r2]
    let cols = [c1 .. c2]
    body <- forM rows $ \r -> do
        vals <- forM cols $ \c -> tshow <$> readArray arr (r, c)
        pure (tshow r : vals)
    CSV.writeCSV path ((T.pack "" : L.map tshow cols) : body)

------------------------------------------------------------------
