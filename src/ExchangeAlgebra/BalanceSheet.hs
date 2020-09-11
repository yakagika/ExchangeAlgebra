

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

module ExchangeAlgebra.BalanceSheet where

import qualified    ExchangeAlgebra             as EA
import              ExchangeAlgebra
import qualified    ExchangeAlgebra.Transfer    as ET
import qualified    CSV.Text                    as CSV
import qualified    Data.List                   as L
import qualified    Data.Text                   as T


-- | BalanceSheetの形でCSVで出力する
writeBS :: (HatVal n, HatBaseClass b, ExBaseClass b) => FilePath -> Alg n b -> IO ()
writeBS path alg
    =  let transferd       = ET.finalStockTransfer alg
    in let debitSide       = decR transferd
    in let creditSide      = decL transferd
    in let assets          = creditSide
    in let liability       = EA.filter (\x -> whatDiv (_hatBase x) == Liability) debitSide
    in let equity          = EA.filter (\x -> whatDiv (_hatBase x) == Equity) debitSide
    in let debitTotal      = T.pack $ show $ EA.norm debitSide
    in let creditTotal     = T.pack $ show $ EA.norm creditSide
    in let assetsText      = L.map (T.pack . show) $ L.map (getAccountTitle . _hatBase) (EA.toList assets)
    in let assetsValue     = L.map (T.pack . show) $ EA.vals assets
    in let liabilityText   = L.map (T.pack . show) $ L.map (getAccountTitle . _hatBase) (EA.toList liability)
    in let liabilityValue  = L.map (T.pack . show) $ EA.vals liability
    in let equityText      = L.map (T.pack . show) $ L.map (getAccountTitle . _hatBase) (EA.toList equity)
    in let equityValue     = L.map (T.pack . show) $ EA.vals equity
    in let result          = CSV.transpose
                           [[T.pack "Asset"]       ++ assetsText     ++ [T.pack "Total"]
                           ,[T.empty]              ++ assetsValue    ++ [creditTotal]
                           ,[T.pack "Liability"]   ++ liabilityText  ++ [T.pack "Equity"] ++ equityText  ++ [T.pack "Total"]
                           ,[T.empty]              ++ liabilityValue ++ [T.empty]         ++ equityValue ++ [debitTotal]]
    in CSV.writeCSV path result





