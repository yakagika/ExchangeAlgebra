

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

module ExchangeAlgebraMap.Write where

import qualified    ExchangeAlgebraMap.Algebra     as EA
import              ExchangeAlgebraMap.Algebra

import qualified    ExchangeAlgebraMap.Transfer    as ET
import qualified    CSV.Text                    as CSV
import qualified    Data.List                   as L
import qualified    Data.Text                   as T

import              Data.IORef
import              Control.Monad
import qualified    Data.Set as Set

-- Day
import qualified    Data.Time           as Time
import              Data.Time



-- | BalanceSheet貸借対照表の形でCSVで出力する
writeBS :: (HatVal n, HatBaseClass b, ExBaseClass b) => FilePath -> Alg n b -> IO ()
writeBS path alg
    =  let transferd       = ET.finalStockTransferKeepWiledcard alg
    in let debitSide       = decR transferd
    in let creditSide      = decL transferd
    in let assets          = creditSide
    in let liability       = EA.filter (\x -> whatDiv (_hatBase x) == Liability) debitSide
    in let equity          = EA.filter (\x -> whatDiv (_hatBase x) == Equity) debitSide
    in let debitTotal      = T.pack $ show $ EA.norm debitSide
    in let creditTotal     = T.pack $ show $ EA.norm creditSide
    in let assetsText      = L.map (T.pack . show) $ L.map (getAccountTitle . _hatBase) (EA.toList assets)
    in let assetsValue     = L.map (T.pack . show) $ L.map _val (EA.toList assets)
    in let liabilityText   = L.map (T.pack . show) $ L.map (getAccountTitle . _hatBase) (EA.toList liability)
    in let liabilityValue  = L.map (T.pack . show) $ L.map _val (EA.toList liability)
    in let equityText      = L.map (T.pack . show) $ L.map (getAccountTitle . _hatBase) (EA.toList equity)
    in let equityValue     = L.map (T.pack . show) $ L.map _val (EA.toList equity)
    in let result          = CSV.transpose
                           [[T.pack "Asset"]       ++ assetsText     ++ [T.pack "Total"]
                           ,[T.empty]              ++ assetsValue    ++ [creditTotal]
                           ,[T.pack "Liability"]   ++ liabilityText  ++ [T.pack "Equity"] ++ equityText  ++ [T.pack "Total"]
                           ,[T.empty]              ++ liabilityValue ++ [T.empty]         ++ equityValue ++ [debitTotal]]
    in CSV.writeCSV path result

-- | Profit and Loss Statement 損益計算書の形でCSVで出力する
writePL :: (HatVal n, HatBaseClass b, ExBaseClass b) => FilePath -> Alg n b -> IO ()
writePL path alg
    =  let debitSide       = decR alg
    in let creditSide      = decL alg
    in let cost            = EA.filter (\x -> whatDiv (_hatBase x) == Cost) creditSide
    in let revenue         = EA.filter (\x -> whatDiv (_hatBase x) == Revenue) debitSide
    in let debitTotal      = T.pack $ show $ EA.norm cost
    in let creditTotal     = T.pack $ show $ EA.norm revenue
    in let costText        = L.map (T.pack . show) $ L.map (getAccountTitle . _hatBase) (EA.toList cost)
    in let costValue       = L.map (T.pack . show) $ L.map _val (EA.toList cost)
    in let revenueText     = L.map (T.pack . show) $ L.map (getAccountTitle . _hatBase) (EA.toList revenue)
    in let revenueValue    = L.map (T.pack . show) $ L.map _val (EA.toList revenue)
    in let (ct,rt) = toSameLength costText revenueText
    in let (cv,rv) = toSameLength costValue revenueValue
    in let result          = CSV.transpose
                           [[T.pack "Cost"]       ++ ct ++ [T.pack "Total"]
                           ,[T.empty]             ++ cv ++ [creditTotal]
                           ,[T.pack "Revenue"]    ++ rt ++ [T.pack "Total"]
                           ,[T.empty]             ++ rv ++ [debitTotal]]
    in CSV.writeCSV path result

-- | 同じ長さのリストに揃える関数
toSameLength :: [T.Text] -> [T.Text] -> ([T.Text],[T.Text])
toSameLength xs ys
    = case (compare (Prelude.length xs) (Prelude.length ys)) of
        EQ -> (xs,ys)
        LT -> (xs ++ (take ((Prelude.length ys) - (Prelude.length xs)) (repeat T.empty)),ys)
        GT -> (xs,ys ++ (take ((Prelude.length xs) - (Prelude.length ys)) (repeat T.empty)))

-- | Journal Entry 仕訳
-- 日付,勘定科目,金額を借方,貸方別に記録
writeJournal :: (HatVal n, HatBaseClass b, ExBaseClass b)
             => FilePath
             -> Alg n b
             -> (b -> Day)
             -> IO ()
writeJournal path alg f = do
    let debitSide       = decR alg
    let creditSide      = decL alg
    let debitTotal      = T.pack $ show $ EA.norm debitSide
    let creditTotal     = T.pack $ show $ EA.norm creditSide
    let days     =  L.sort $ Set.toList . Set.fromList $ L.map (f . _hatBase) $ EA.toList alg
    ds <- newIORef [(T.pack "Day")]
    dt <- newIORef [(T.pack "Debit")]
    dv <- newIORef [(T.pack "Amount")]
    ct <- newIORef [(T.pack "Credit")]
    cv <- newIORef [(T.pack "Amount")]
    forM_ days $ \d -> do
        let da = EA.filter (\y -> (f . _hatBase) y == d) alg
        let dl = decL da
        let dr = decR da
        let dlTexts = L.map (T.pack . show) $ L.map (getAccountTitle . _hatBase) $ EA.toList dl
        let drTexts = L.map (T.pack . show) $ L.map (getAccountTitle . _hatBase) $ EA.toList dr
        let dlValues = L.map (T.pack . show) $ L.map _val $ EA.toList dl
        let drValues = L.map (T.pack . show) $ L.map _val $ EA.toList dr
        let (dt',ct') = toSameLength dlTexts drTexts
        let (dv',cv') = toSameLength dlValues drValues
        let (ds',_) = toSameLength [(T.pack . show) d] cv'
        modifyIORef ds (++ ds')
        modifyIORef dt (++ dt')
        modifyIORef dv (++ dv')
        modifyIORef ct (++ ct')
        modifyIORef cv (++ cv')
    ds' <- readIORef ds
    dt' <- readIORef dt
    dv' <- readIORef dv
    ct' <- readIORef ct
    cv' <- readIORef cv

    let result = CSV.transpose [ds',dt',dv',ct',cv']
    CSV.writeCSV path result


-- | Account 勘定口座
writeAccountOf :: (HatVal n, HatBaseClass b, ExBaseClass b)
             => [AccountTitles]
             -> FilePath
             -> Alg n b
             -> (b -> Day)
             -> IO ()
writeAccountOf path alg f = undefined


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
    result             <- newIORef ([header] :: [[T.Text]])
    debitBalanceTotal  <- newIORef zeroValue
    debitTotal         <- newIORef zeroValue
    creditBalanceTotal <- newIORef zeroValue
    creditTotal        <- newIORef zeroValue
    ------------------------------------------------------------------
    forM_ accounts $ \a -> do
        let xs = projByAccountTitle a alg
        let xr = norm $ decR xs
        modifyIORef debitTotal (+ xr)
        let xl = norm $ decL xs
        modifyIORef creditTotal (+ xl)
        let (dc,diff) = diffRL xs
        case dc of
            Credit -> modifyIORef debitBalanceTotal (+ diff)
            Debit  -> modifyIORef creditBalanceTotal (+ diff)

        let line  = case dc of
                Credit  -> [(T.pack . show) diff
                           ,(T.pack . show) xl
                           ,(T.pack . show) a
                           ,(T.pack . show) xr
                           ,T.empty]
                Debit   -> [T.empty
                           ,(T.pack . show) xl
                           ,(T.pack . show) a
                           ,(T.pack . show) xr
                           ,(T.pack . show) diff]
        modifyIORef result (\x -> x ++ [line])
    ------------------------------------------------------------------
    debitBalanceTotal'  <- readIORef debitBalanceTotal
    debitTotal'         <- readIORef debitTotal
    creditBalanceTotal' <- readIORef creditBalanceTotal
    creditTotal'        <- readIORef creditTotal
    modifyIORef result  $ \x -> x ++ [[(T.pack . show) debitBalanceTotal'
                                      ,(T.pack . show) creditTotal'
                                      ,T.pack "Total"
                                      ,(T.pack . show) debitTotal'
                                      ,(T.pack . show) creditBalanceTotal']]
    result' <- readIORef result
    CSV.writeCSV path result'



