{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : ExchangeAlgebra.Accounting.Closing
Description : Additional closing entries and ordered settlement pairs.

This Accounting layer selects closing accounts and constructs additional
entries using generic Algebra operations. Start with 'closingEntries' for
an existing ledger; use 'settleEntries' for signed nets already computed
by the caller. Both preserve the non-account coordinates of each source.

Import this module qualified alongside "ExchangeAlgebra.Algebra.Core":

> import qualified ExchangeAlgebra.Accounting.Closing as Closing
> import ExchangeAlgebra.Algebra.Core ((.+))

'closingEntries' returns only additional entries. For @Right entries =
Closing.closingEntries ledger@, add them with @ledger .+ entries@.
Supply only postings through the closing date, including earlier closing
entries. For a journal, first use @toAlg@ from "ExchangeAlgebra.Journal.Core",
then attach the closing note to the returned entries with @(.|)@.

The legacy @incomeSummaryAccount@ / @netIncomeTransfer@ chain and
@finalStockTransfer@ return transformed ledgers. Use 'closingEntries' for
additional entries that retain the original audit trail. Its compatibility
with the legacy result has the preconditions stated in L2.
The posting pairs implement Definition 9.

== Laws

The observation @obs@ is the map from complete bases (including Hat\/Not) to
summed values after @bar@; it does not observe the singleton versus composite
constructor or sequence order. Relative tolerance means a per-base difference
of at most @1e-9 * max (abs x) (abs y)@, treating absent bases as zero.

=== L2: closing

* Subject: 'closingEntries' and legacy @finalStockTransfer@.
* Preconditions: concrete ledger bases, valid Hat\/Not postings and integer values
  in @1..1000000@; the ledger includes only entries up to the closing date.
  Closing returns @Right entries@. Concrete bases are needed only for
  equivalence with legacy @finalStockTransfer@ and its symmetric matching;
  'closingEntries' groups by each actual base, including ledger wildcards
  as values.
* Relation: @obs (a .+ entries) ~= obs (finalStockTransfer a)@.
* Observation: @obs@, including 'RetainedEarnings' and all retained axes.
* Tolerance: relative @1e-9@ within the stated range. Large historical
  cancellations are deliberately outside this compatibility law.
* Instances: 'Double' and @MoneyDecimal@ with 'ExBaseClass' bases.

=== L3: balance

* Subject: generated transfer and closing entries.
* Preconditions: valid postings, concrete accounts, @Relabel@ rules whose
  actual target and source have equal 'whichSide'; finite totals. The
  transfer or closing operation being observed returns @Right entries@.
* Relation: debit total equals credit total. Closing entries also satisfy
  this relation, because 'closingSide' follows the account's PIMO direction.
* Observation: @norm (decL entries)@ and @norm (decR entries)@.
* Tolerance: relative @1e-9@ for 'Double'; exact for @MoneyDecimal@.
* Instances: 'Double' and @MoneyDecimal@ with 'ExBaseClass' bases.

A relabel from @Not :< Cash@ to @Not :< Sales@ violates L3's side
precondition: its cancellation and destination are both credits.

=== L5: non-negativity

* Subject: generated transfer and closing entries.
* Preconditions: non-negative valid input values; the transfer or closing
  operation being observed returns @Right entries@.
* Relation: every generated value is greater than or equal to zero.
* Observation: posting values, before any @bar@.
* Tolerance: none.
* Instances: all lawful 'HatVal' and 'HatBaseClass' instances (closing also
  requires 'ExBaseClass'). Hat reversal is never numeric negation.

-}
module ExchangeAlgebra.Accounting.Closing ( ClosingSide(..)
                                          , closingSide
                                          , closingEntries
                                          , SettleRule
                                          , retainedEarningsRule
                                          , SettlementBatch
                                          , SignedNet
                                          , SettleError(..)
                                          , settleEntries
                                          , settlementSteps
                                          ) where

import Data.Binary (Binary(..))
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import ExchangeAlgebra.Algebra.Core ( Alg(..)
                                    , Redundant((.+))
                                    , (.@)
                                    , foldEntries
                                    )
import ExchangeAlgebra.Algebra.Base.Representation (HatBaseClass(..), Hat(..))
import ExchangeAlgebra.Algebra.Value.Class (HatVal(..))
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import ExchangeAlgebra.Accounting.Account ( AccountTitles(..)
                                          , accountSpec
                                          , asClosing
                                          , ClosingRule(..)
                                          , classifyAccountDivision
                                          , classifyAccountContra
                                          , pimoFromDivision
                                          , pimoFlip
                                          , PIMO(..)
                                          )
import ExchangeAlgebra.Algebra.Transfer.Representation (TransferApplyError(..))

-- * Closing entries

-- | The retained-earnings side selected by a closing account's PIMO direction.
data ClosingSide
    = ClosingKeep -- ^ IN: retain Hat/Not (revenue or contra cost).
    | ClosingFlip -- ^ OUT: reverse Hat/Not (cost or contra revenue).
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Binary ClosingSide

instance Hashable ClosingSide

-- | Public closing classification used by the legacy final stock transfer.
-- Registry NoClose accounts, including NetIncome and NetLoss, are excluded.
closingSide :: AccountTitles -> Maybe ClosingSide
closingSide title = case accountSpec title of
    Nothing -> Nothing
    Just spec -> case asClosing spec of
        NoClose -> Nothing
        CloseByDivision -> case direction of
            IN -> Just ClosingKeep
            OUT -> Just ClosingFlip
            _ -> Nothing
  where
    direction
        | classifyAccountContra title = pimoFlip ordinaryDirection
        | otherwise = ordinaryDirection
    ordinaryDirection = pimoFromDivision (classifyAccountDivision title)

-- | Generate closing entries from each eligible base's sequential side totals.
-- Supply only postings through the closing date. Hat and Not totals are
-- compared without a tolerance; their non-negative difference is closed.
-- This deliberately folds the source sequences (audit detail) per base,
-- but preserves separate target postings from distinct source accounts.
-- It does not call @bar@: small rounding residues can become closing entries.
-- All non-account axes are retained, and no note is attached.
-- Input values must satisfy the ordinary non-negative, finite posting contract.
-- Finite inputs normally yield 'Right'; 'Left' occurs only when a side's sum
-- exceeds the value type's range. Both side totals and their difference are
-- checked before generating postings, even when both totals compare equal.
-- 'NonFiniteBalance' reports the first base in ascending 'Ord' order after
-- normalizing Hat/Not to Not. No partial result or split balance is returned.
-- Balance collection is O(s log b), for s postings and b eligible bases;
-- output construction uses the ordinary algebra addition operation.
closingEntries :: (HatVal v, ExBaseClass b)
               => Alg v b -> Either (TransferApplyError v b) (Alg v b)
closingEntries = Map.foldlWithKey' close (Right Zero) . foldEntries collect Map.empty
  where
    collect balances value source = case (hat source, closingSide (getAccountTitle source)) of
        (Hat, Just _) -> Map.insertWith addTotals (toNot source) (value, zeroValue) balances
        (Not, Just _) -> Map.insertWith addTotals source (zeroValue, value) balances
        _ -> balances
    addTotals (hatValue, notValue) (hatTotal, notTotal) =
        (hatValue + hatTotal, notValue + notTotal)
    close result source (hatTotal, notTotal) = do
        entries <- result
        additions <- netEntries source hatTotal notTotal
        pure (entries .+ additions)
    netEntries source hatTotal notTotal
        | isErrorValue hatTotal || isErrorValue notTotal = Left (NonFiniteBalance source)
        | hatTotal == notTotal = Right Zero
        | hatTotal > notTotal = checkedPair source (hatTotal - notTotal) (toHat source)
        | otherwise = checkedPair source (notTotal - hatTotal) source
    checkedPair source value balanceBase
        | isErrorValue value = Left (NonFiniteBalance source)
        | otherwise = case closingSide (getAccountTitle balanceBase) of
            Nothing   -> Right Zero
            Just side -> Right
                (closingPairBy (targetSide side) RetainedEarnings value balanceBase)
    targetSide side = case side of
        ClosingKeep -> id
        ClosingFlip -> revHat

-- | A closing rule containing only its destination account title.
-- The private constructor restricts destinations to accounts compatible with
-- the closing directions.
newtype SettleRule = SettleRule AccountTitles

-- | Close eligible accounts into 'RetainedEarnings', preserving all other axes.
retainedEarningsRule :: SettleRule
retainedEarningsRule = SettleRule RetainedEarnings

-- | Settlement pairs in strictly ascending source-base order.
-- A model returns only @Posting@ built with @entry@ and 'Monoid'. Settlement
-- magnitudes can exceed the @Posted@ bound, so this type has no conversion to
-- @Posting@ and no 'Semigroup' or 'Monoid' instance.
--
-- Record each pair separately in source-base order. Combining all pairs first
-- can change the order of additions to a shared destination. For example,
-- sequential increments @T, 1, -T@ with @T = 2^53@ give 0 in 'Double', whereas
-- adding @T, -T, 1@ gives 1.
newtype SettlementBatch b = SettlementBatch [(BasePart b, Alg Double b)]

-- | A signed Not-minus-Hat net. This is not a non-negative posting magnitude.
-- As a type synonym, it does not enforce finiteness or any numeric range.
type SignedNet = Double

-- | The first non-finite input net, identified by its complete base coordinates.
data SettleError b = NonFiniteNet (BasePart b)

deriving instance Eq (BasePart b) => Eq (SettleError b)
deriving instance Show (BasePart b) => Show (SettleError b)

-- | Construct one reversal and destination pair per eligible source base.
-- Input values are signed Not-minus-Hat nets. Zero nets, accounts without a
-- closing side, and destination bases produce no pair. Every pair has two
-- finite, non-negative magnitudes equal to the absolute input net; the source
-- reversal cancels that net exactly. Other base axes are preserved.
--
-- A non-finite input, including at an excluded key, returns 'Left' with the
-- first key in ascending order. Finite magnitudes above
-- 'ExchangeAlgebra.Algebra.Posting.postedUpperBound' are accepted, without passing
-- through 'ExchangeAlgebra.Algebra.Posting.posted'. No implicit @bar@ or @compress@ is
-- applied. Complexity: O(b) for b input bases.
-- Law: subject: each generated settlement pair; preconditions: all nets finite.
-- Relation: each reversal cancels its source net. For each destination base,
-- its increment is the sum of @direction * net@ over the source bases, where
-- @direction@ is +1 for 'ClosingKeep' and -1 for 'ClosingFlip'.
-- Observation: sums of @decL@ and @decR@ lifted to Rational for each pair.
-- Tolerance: exact. Instances: 'ExBaseClass' bases with Double posting values.
settleEntries :: forall b. ExBaseClass b
              => SettleRule
              -> Map (BasePart b) SignedNet
              -> Either (SettleError b) (SettlementBatch b)
settleEntries (SettleRule destination) amounts
    = case mapMaybe nonFinite (Map.toAscList amounts) of
        first : _ -> Left (NonFiniteNet first)
        []        -> Right (SettlementBatch (mapMaybe close (Map.toAscList amounts)))
  where
    finite amount = not (isNaN amount || isInfinite amount)
    nonFinite (coordinates, amount)
        | finite amount = Nothing
        | otherwise     = Just coordinates
    close (coordinates, amount)
        | amount == 0 = Nothing
        | coordinates == base (setAccountTitle source destination) = Nothing
        | otherwise = case closingSide (getAccountTitle source) of
            Nothing   -> Nothing
            Just side -> Just
                (coordinates, closingPairBy (targetSide side) destination (abs amount) source)
      where
        source = merge (sourceSide amount) coordinates :: b
    sourceSide amount
        | amount < 0 = Hat
        | otherwise  = Not
    targetSide side = case side of
        ClosingKeep -> id
        ClosingFlip -> revHat

-- | Read the pairs in strictly ascending source-base order, without constraints
-- on the base type. Record each pair before proceeding to the next source.
-- Complexity: O(1) to expose the list; O(b) to consume b pairs.
settlementSteps :: SettlementBatch b -> [(BasePart b, Alg Double b)]
settlementSteps (SettlementBatch steps) = steps

-- | Reverse a closing balance and post it to the given destination account.
-- The first argument transforms the source to the target side: @id@ retains
-- Hat/Not, and 'revHat' reverses it. The caller must classify the source,
-- and the value must satisfy the non-negative,
-- finite posting contract of '.@'.
closingPairBy :: (HatVal v, ExBaseClass b)
              => (b -> b)
              -> AccountTitles
              -> v
              -> b
              -> Alg v b
closingPairBy targetSide targetAccount value source
    =  (value .@ revHat source)
    .+ (value .@ setAccountTitle (targetSide source) targetAccount)
