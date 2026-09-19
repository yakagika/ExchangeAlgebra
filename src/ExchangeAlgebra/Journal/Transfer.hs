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

    This module provides the legacy transfer API. New code should import
    "ExchangeAlgebra.Journal.Transfer.Rule" qualified.

    The legacy tree-based transfer walker is correct only under all of these
    preconditions:

    (P1) Every @from@ pattern places wildcards in the same tuple positions.

    (P2) The @from@ patterns are pairwise non-overlapping.

    (P3) Ledger bases contain no wildcards.

    (P4) Base tuples are not nested.

    (P5) Value transformations do not return zero.

    Rules with mixed wildcard positions violate P1 and can make the tree
    search miss matching entries.

-}


{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternGuards      #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE PostfixOperators   #-}



module ExchangeAlgebra.Journal.Transfer
    ( TransTable
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
    , ExchangeAlgebra.Journal.Transfer.finalStockTransferAggregated
    , ExchangeAlgebra.Journal.Transfer.finalStockTransfer
    ) where

import              ExchangeAlgebra.Algebra hiding (map)
import qualified    ExchangeAlgebra.Algebra.Transfer as EAT
import              ExchangeAlgebra.Algebra.Transfer (TransTable
                                                    , isNullTable
                                                    , table
                                                    , TransTableParts
                                                    , (.->)
                                                    , (|%)
                                                    , finalStockTransferStep)
import qualified    ExchangeAlgebra.Journal as EJ
import              ExchangeAlgebra.Journal hiding ()



-- | Apply legacy transfer transformations to each Note entry in a Journal.
-- Wildcard portions within tuples are not transformed and retain their original values.
-- See the module header for the legacy API's preconditions. New code should
-- use qualified "ExchangeAlgebra.Journal.Transfer.Rule".
--
-- Complexity: O(j * s) (j = number of Notes, s = number of scalar entries per Note)
{-# INLINE transfer #-}
transfer :: (HatVal v, HatBaseClass b, Note n)
                      => Journal n v b -> TransTable v b -> Journal n v b
transfer js tb = EJ.map (\x ->  EAT.transfer x tb) js

-- | Build a legacy Journal transfer from a list of rule triples.
-- See the module header for the legacy API's preconditions. New code should
-- use qualified "ExchangeAlgebra.Journal.Transfer.Rule".
createTransfer :: (Note n, HatVal v, ExBaseClass b)
               => [(b,b,(v -> v))] -> (Journal n v b -> Journal n v b)
createTransfer tt = \ts -> transfer ts $ EAT.table tt

-- * Closing transfer entries

-- | Compute net income for the current period (Income Summary Account).
-- Calculate the debit-credit difference and add it as NetIncome or NetLoss to the plank Note.
-- When the ledger is balanced (credit == debit, net income is zero), @diffRL@ reports the
-- wildcard v'Side'; in that case the journal is returned unchanged (balanced ledger =
-- identity). Appending @Zero .| plank@ is not an identity for t'Journal' because @(.|)@ builds
-- a @Map.singleton plank Zero@ and drives version/compaction, so the input is returned directly.
-- The result contains a legacy NetIncome/NetLoss balancing coordinate and is
-- an intermediate closing state, not input for reporting presentation.  New
-- reporting code should derive the result from a validated before-closing
-- trial balance with "ExchangeAlgebra.Reporting.Metric".
-- This is a legacy named transfer; see the module header for its preconditions.
-- New transfer code should use qualified "ExchangeAlgebra.Journal.Transfer.Rule".
--
-- Complexity: O(s) (s = total number of scalar entries)
incomeSummaryAccount :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
incomeSummaryAccount js =  let (dc,diff) = diffRL js
                         in case dc of
                                Credit -> js .+ ((diff :@ (toNot wildcard) .~ NetIncome) .| plank)
                                Debit  -> js .+ ((diff :@ (toNot wildcard) .~ NetLoss)   .| plank)
                                Side   -> js

-- | Legacy net income transfer (Journal version). Transfer NetIncome/NetLoss to RetainedEarnings for each Note.
-- See the module header for the legacy API's preconditions. New code should
-- use qualified "ExchangeAlgebra.Journal.Transfer.Rule".
--
-- Complexity: O(j * s) (j = number of Notes, s = number of scalar entries per Note)
netIncomeTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
netIncomeTransfer = EJ.map EAT.netIncomeTransfer

-- ** Journalizing

-- | Legacy SNA/simulation gross-profit transfer (Journal version).
-- This delegates to the legacy fixed-list rule and is not a JGAAP subtotal.
-- See the module header for the legacy API's preconditions. New code should
-- use qualified "ExchangeAlgebra.Journal.Transfer.Rule".
--
-- Complexity: O(j * s)
grossProfitTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
grossProfitTransfer = EJ.map EAT.grossProfitTransfer

-- | Legacy SNA/simulation ordinary-profit transfer (Journal version).
-- The fixed list predates the JCCI chart and is not a JGAAP subtotal.
-- See the module header for the legacy API's preconditions. New code should
-- use qualified "ExchangeAlgebra.Journal.Transfer.Rule".
--
-- >>> type Test = Journal String Double (HatBase (CountUnit, AccountTitles))
-- >>> x = 2279.0:@Not:<(Yen,Depreciation) .| "A" :: Test
-- >>> y = 500475.0:@Not:<(Yen,InterestEarned) .| "B" :: Test
-- >>> ExchangeAlgebra.Journal.Transfer.ordinaryProfitTransfer ( x .+ y)
-- (2279.00:@Hat:<(Yen,OrdinaryProfit) .| "A") .+ (500475.00:@Not:<(Yen,OrdinaryProfit) .| "B")

ordinaryProfitTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
ordinaryProfitTransfer = EJ.map EAT.ordinaryProfitTransfer

-- | Legacy retained earnings transfer (Journal version). Transfer OrdinaryProfit to RetainedEarnings for each Note.
-- See the module header for the legacy API's preconditions. New code should
-- use qualified "ExchangeAlgebra.Journal.Transfer.Rule".
--
-- Complexity: O(j * s)
retainedEarningTransfer :: (Note n, HatVal v, ExBaseClass b) => Journal n v b -> Journal n v b
retainedEarningTransfer = EJ.map EAT.retainedEarningTransfer

-- | Apply the legacy Algebra-level closing to every Note and then fold the Note axis
-- onto the plank via the Journal's '(.-)'. The per-Note lift without folding is
-- @EJ.map EAT.finalStockTransfer@.
-- See the module header for the legacy API's preconditions. New code should
-- use qualified "ExchangeAlgebra.Journal.Transfer.Rule".
--
-- Complexity: O(j * s)
finalStockTransferAggregated ::(Note n, HatVal v, ExBaseClass b) =>  Journal n v b -> Journal n v b
finalStockTransferAggregated = (.-) . EJ.map finalStockTransferStep

-- | Compatibility name for 'finalStockTransferAggregated'.
-- Its behaviour is unchanged: it closes each Note and folds the Note axis onto
-- the plank.
-- This is a legacy named transfer; see the module header for its preconditions.
-- New code should use qualified "ExchangeAlgebra.Journal.Transfer.Rule".
--
-- Complexity: O(j * s)
finalStockTransfer ::(Note n, HatVal v, ExBaseClass b) =>  Journal n v b -> Journal n v b
finalStockTransfer = finalStockTransferAggregated
