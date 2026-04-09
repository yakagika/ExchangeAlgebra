{- |
    Module     : ExchangeAlgebra.Transfer
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

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
                                                    , toNumber,T) -- Non-negative real numbers
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



-- | Apply transfer transformations to each Note entry in a Journal.
-- Wildcard portions within tuples are not transformed and retain their original values.
--
-- Complexity: O(j * s) (j = number of Notes, s = number of scalar entries per Note)
{-# INLINE transfer #-}
transfer :: (HatVal v, HatBaseClass b, Note n)
                      => Journal n v b -> TransTable v b -> Journal n v b
transfer js tb = EJ.map (\x ->  EAT.transfer x tb) js

createTransfer :: (Note n, HatVal v, ExBaseClass b)
               => [(b,b,(v -> v))] -> (Journal n v b -> Journal n v b)
createTransfer tt = \ts -> transfer ts $ EAT.table tt

-- * Closing transfer entries

-- | Compute net income for the current period (Income Summary Account).
-- Calculate the debit-credit difference and add it as NetIncome or NetLoss to the plank Note.
--
-- Complexity: O(s) (s = total number of scalar entries)
incomeSummaryAccount :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
incomeSummaryAccount js =  let (dc,diff) = diffRL js
                         in let x = case dc of
                                        Credit  -> diff :@ (toNot wiledcard) .~ NetIncome
                                        Debit -> diff :@ (toNot wiledcard) .~ NetLoss
                         in js .+  ( x .| plank)

-- | Net income transfer (Journal version). Transfer NetIncome/NetLoss to RetainedEarnings for each Note.
--
-- Complexity: O(j * s) (j = number of Notes, s = number of scalar entries per Note)
netIncomeTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
netIncomeTransfer = EJ.map EAT.netIncomeTransfer

-- ** Journalizing

-- | Gross profit transfer (Journal version). Aggregate sales and cost accounts into GrossProfit for each Note.
--
-- Complexity: O(j * s)
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

-- | Retained earnings transfer (Journal version). Transfer OrdinaryProfit to RetainedEarnings for each Note.
--
-- Complexity: O(j * s)
retainedEarningTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
retainedEarningTransfer = EJ.map EAT.retainedEarningTransfer

-- | Income summary account (Journal version). Transfer all cost and revenue accounts to RetainedEarnings, then offset using the Bar operation.
--
-- Complexity: O(j * s)
finalStockTransfer ::(Note n, HatVal v, ExBaseClass b) =>  Journal n v b -> Journal n v b
finalStockTransfer = (.-) . EJ.map finalStockTransferStep

