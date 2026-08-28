{- |
    Module     : ExchangeAlgebra.Journal.Transfer
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

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
    , ExchangeAlgebra.Journal.Transfer.createTransfer
    , ExchangeAlgebra.Journal.Transfer.incomeSummaryAccount
    , ExchangeAlgebra.Journal.Transfer.netIncomeTransfer
    , ExchangeAlgebra.Journal.Transfer.grossProfitTransfer
    , ExchangeAlgebra.Journal.Transfer.ordinaryProfitTransfer
    , ExchangeAlgebra.Journal.Transfer.retainedEarningTransfer
    , ExchangeAlgebra.Journal.Transfer.finalStockTransfer
    ) where

import              ExchangeAlgebra.Algebra hiding (map)
import qualified    ExchangeAlgebra.Algebra.Transfer as EAT
import              ExchangeAlgebra.Algebra.Transfer (TransTable (..)
                                                    , isNullTable
                                                    , table
                                                    , TransTableParts
                                                    , (.->)
                                                    , (|%)
                                                    , finalStockTransferStep)
import qualified    ExchangeAlgebra.Journal as EJ
import              ExchangeAlgebra.Journal hiding ()



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
-- When the ledger is balanced (credit == debit, net income is zero), @diffRL@ reports the
-- wildcard 'Side'; in that case the journal is returned unchanged (balanced ledger =
-- identity). Appending @Zero .| plank@ is not an identity for t'Journal' because @(.|)@ builds
-- a @Map.singleton plank Zero@ and drives version/compaction, so the input is returned directly.
-- The result contains a legacy NetIncome/NetLoss balancing coordinate and is
-- an intermediate closing state, not input for reporting presentation.  New
-- reporting code should derive the result from a validated before-closing
-- trial balance with "ExchangeAlgebra.Reporting.Metric".
--
-- Complexity: O(s) (s = total number of scalar entries)
incomeSummaryAccount :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
incomeSummaryAccount js =  let (dc,diff) = diffRL js
                         in case dc of
                                Credit -> js .+ ((diff :@ (toNot wildcard) .~ NetIncome) .| plank)
                                Debit  -> js .+ ((diff :@ (toNot wildcard) .~ NetLoss)   .| plank)
                                Side   -> js

-- | Net income transfer (Journal version). Transfer NetIncome/NetLoss to RetainedEarnings for each Note.
--
-- Complexity: O(j * s) (j = number of Notes, s = number of scalar entries per Note)
netIncomeTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
netIncomeTransfer = EJ.map EAT.netIncomeTransfer

-- ** Journalizing

-- | Historical SNA/simulation gross-profit transfer (Journal version).
-- This delegates to the legacy fixed-list rule and is not a JGAAP subtotal.
--
-- Complexity: O(j * s)
grossProfitTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
grossProfitTransfer = EJ.map EAT.grossProfitTransfer

-- | Historical SNA/simulation ordinary-profit transfer (Journal version).
-- The fixed list predates the JCCI chart and is not a JGAAP subtotal.
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
