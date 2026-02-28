{- |
    Module     : ExchangeAlgebra.Transfer
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}


{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternGuards      #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE PostfixOperators   #-}



module ExchangeAlgebra.Journal.Transfer
    ( TransTable (..)
    , isNullTable
    , table
    , TransTableParts
    , (.->)
    , (|%)
    , ExchangeAlgebra.Journal.Transfer.transfer
    , ExchangeAlgebra.Journal.Transfer.incomeSummaryAccount
    , ExchangeAlgebra.Journal.Transfer.netIncomeTransfer
    , ExchangeAlgebra.Journal.Transfer.grossProfitTransfer
    , ExchangeAlgebra.Journal.Transfer.ordinaryProfitTransfer
    , ExchangeAlgebra.Journal.Transfer.retainedEarningTransfer
    , ExchangeAlgebra.Journal.Transfer.finalStockTransfer
    ) where

import qualified    ExchangeAlgebra.Algebra as EA
import              ExchangeAlgebra.Algebra hiding (map)
import qualified    ExchangeAlgebra.Algebra.Transfer as EAT
import              ExchangeAlgebra.Algebra.Transfer (TransTable (..)
                                                    , isNullTable
                                                    , table
                                                    , TransTableParts
                                                    , (.->)
                                                    , (|%)
                                                    , finalStockTransferStep
                                                    , retainedEarningTransfer
                                                    , ordinaryProfitTransfer
                                                    , grossProfitTransfer)
import qualified    ExchangeAlgebra.Journal as EJ
import              ExchangeAlgebra.Journal hiding ()

import qualified    Number.NonNegative  as NN       ( Double
                                                    , fromNumber
                                                    , toNumber,T) -- 非負の実数
import qualified    Data.Maybe          as Maybe
import              Text.Show.Unicode               ( ushow)
import              GHC.Exts                        ( reallyUnsafePtrEquality#
                                                    , isTrue#
                                                    , build
                                                    , lazy)
import              Data.Semigroup                  ( Semigroup(stimes)
                                                    , stimesIdempotentMonoid)
import              Data.Monoid                     ( Monoid(..))
import qualified    Data.Foldable       as Foldable
import              Data.Foldable                   ( Foldable())
import              Data.Bits                       ( shiftL
                                                    , shiftR)
import              Utils.Containers.Internal.StrictPair
import              Debug.Trace



-- | タプルの内, ワイルドカードは変換しない
{-# INLINE transfer #-}
transfer :: (HatVal v, HatBaseClass b, Note n)
                      => Journal n v b -> TransTable v b -> Journal n v b
transfer js tb = EJ.map (\x ->  EAT.transfer x tb) js

createTransfer :: (Note n, HatVal v, ExBaseClass b)
               => [(b,b,(v -> v))] -> (Journal n v b -> Journal n v b)
createTransfer tt = \ts -> transfer ts $ EAT.table tt

-- * 決算振替仕訳

-- | Income Summary Account 当期純利益の算定
incomeSummaryAccount :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
incomeSummaryAccount js =  let (dc,diff) = diffRL js
                         in let x = case dc of
                                        Debit  -> diff :@ (toNot wiledcard) .~ NetIncome
                                        Credit -> diff :@ (toNot wiledcard) .~ NetLoss
                         in js .+  ( x .| plank)

-- | 当期純利益の振替
netIncomeTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
netIncomeTransfer = EJ.map EAT.netIncomeTransfer

-- **  仕分け

-- | Gross Profit Transfer
grossProfitTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
grossProfitTransfer = EJ.map EAT.grossProfitTransfer

-- | Ordinary Profit Transfer
--
-- >>> type Test = Journal String Double (HatBase (CountUnit, AccountTitles))
-- >>> x = 2279.0:@Not:<(Yen,Depreciation) .| "A" :: Test
-- >>> y = 500475.0:@Not:<(Yen,InterestEarned) .| "B" :: Test
-- >>> ExchangeAlgebra.Journal.Transfer.ordinaryProfitTransfer ( x .+ y)
-- 2279.00:@Hat:<(Yen,OrdinaryProfit).|"A" .+ 500475.00:@Not:<(Yen,OrdinaryProfit).|"B"

ordinaryProfitTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
ordinaryProfitTransfer = EJ.map EAT.ordinaryProfitTransfer

-- | Retained Earning Transfer
retainedEarningTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
retainedEarningTransfer = EJ.map EAT.retainedEarningTransfer

-- | Final Stock Transfer (損益勘定)
finalStockTransfer ::(Note n, HatVal v, ExBaseClass b) =>  Journal n v b -> Journal n v b
finalStockTransfer = (.-) . EJ.map finalStockTransferStep

