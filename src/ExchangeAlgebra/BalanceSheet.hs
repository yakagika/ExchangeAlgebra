

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

import qualified    ExchangeAlgebra as EA
import              ExchangeAlgebra



-- | BalanceSheetの形でCSVで出力する
outputBS :: (HatVal n, HatBaseClass b, ExBaseClass b) => Alg n b -> IO ()
outputBS alg =  let debitSide  = decR ((.-) alg)
             in let creditSide = decL ((.-) alg)
             in let assets     = creditSide
             in let liability  = EA.filter (\x -> whatDiv (_hatBase x) == Liability) debitSide
             in let equity     = EA.filter (\x -> whatDiv (_hatBase x) == Equity) debitSide
             in return ()
