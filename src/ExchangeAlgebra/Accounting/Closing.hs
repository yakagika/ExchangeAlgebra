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
by the caller. Both preserve the base components other than the account title of each source.

Both close into the 'closingTarget' of the base's title vocabulary
('RetainedEarnings' for 'AccountTitles'). Before reading any input, both check
that target with 'closingTargetViolation' and reject a vocabulary that breaks
the closing-target law, so a model's own title type cannot close into a
revenue, cost, or contra title.

Import this module qualified alongside "ExchangeAlgebra.Algebra.Core":

> import qualified ExchangeAlgebra.Accounting.Closing as Closing
> import ExchangeAlgebra.Algebra.Core ((.+))

'closingEntries' returns only additional entries. For @Right entries =
Closing.closingEntries ledger@, add them with @ledger .+ entries@.
Supply only postings through the closing date, including earlier closing
entries. For a journal, first use @toAlg@ from "ExchangeAlgebra.Journal.Core",
then attach the closing note to the returned entries with @(.|)@.

The posting pairs implement Definition 9 and retain the original audit trail.

== Laws

The observation @obs@ is the map from complete bases (including Hat\/Not) to
summed values after @bar@; it does not observe the singleton versus composite
constructor or sequence order. Relative tolerance means a per-base difference
of at most @1e-9 * max (abs x) (abs y)@, treating absent bases as zero.

=== L2: closing

* Subject: 'closingEntries'.
* Preconditions: valid Hat\/Not postings; the ledger includes only entries
  through the closing date and the call returns @Right entries@.
* Relation: adding the entries closes each eligible complete base exactly.
  Each reversal has an equal term at the vocabulary's 'closingTarget' on
  the side selected by 'closingSide'. It keeps the source's base components
  other than the account title. For each combination of those components, the
  exact net of @x .+ entries@ is zero at every closed source. At the closing target it is
  the net of @x@ there plus the sum of @direction * net@ over the closed
  sources, where @direction@ is +1 for 'ClosingKeep' and -1 for
  'ClosingFlip'; every other base keeps its net in @x@.
* Observation: exact Not-minus-Hat net per base without Hat\/Not, including
  the closing target.
* Tolerance: none; the 'HatVal' accumulator and 'splitAccum' preserve exact nets.
* Instances: 'Double' and @MoneyDecimal@ with 'ExBaseClass' bases whose
  vocabulary satisfies the closing-target law.

=== L2v: vocabulary renaming

* Subject: 'closingEntries' and 'settleEntries'.
* Preconditions: bases @b@ and @b'@ whose vocabularies satisfy the
  closing-target law, and an injective map @g :: BasePart b -> BasePart b'@
  that applies a title map @f@ and keeps every other base component, with
  @closingSide (f x) == closingSide x@ for every title @x@ and
  @f closingTarget == closingTarget@. 'ExchangeAlgebra.Algebra.Core.mapBasePart'
  applies @g@ to a ledger.
* Relation: @fmap (mapBasePart g) (closingEntries x)@ and
  @closingEntries (mapBasePart g x)@ are both @Right@ or both @Left@, and the
  @Right@ entries are equal under the observation. For 'settleEntries' with
  nets @m@ and @m'@ = @m@ with keys mapped by @g@, both results are @Right@ or
  both @Left@; for @Right@, the pairs are matched by source key: the pair of
  @g k@ in the second equals the pair of @k@ in the first mapped by
  @mapBasePart g@. The order of the pairs follows each base type's 'Ord'
  and is not preserved unless @g@ is strictly monotone; for @Left@, the
  reported base can differ for the same reason.
* Observation: @obs@; for 'settleEntries', @obs@ of each matched pair.
* Tolerance: none.
* Instances: 'AccountTitles' and a vocabulary that renames its closing target.

=== L3: balance

* Subject: generated transfer and closing entries.
* Preconditions: valid postings, concrete accounts, @Relabel@ rules whose
  actual target and source have equal 'whichSide'; finite totals. The
  transfer or closing operation being observed returns @Right entries@.
  For closing, the base's 'whatDiv' and 'isContra' agree with the title's
  vocabulary ('vocabDivision' and 'vocabContra'), as the 'ExBaseClass'
  defaults do: 'closingSide' reads the vocabulary, while @decL@ and @decR@
  read 'whichSide' of the base.
* Relation: debit total equals credit total. Closing entries also satisfy
  this relation, because 'closingSide' follows the account's PIMO direction.
* Observation: @norm (decL entries)@ and @norm (decR entries)@.
* Tolerance: relative @1e-9@ for 'Double'; exact for @MoneyDecimal@.
* Instances: 'Double' and @MoneyDecimal@ with 'StandardBase' bases, and
  with 'ExBaseClass' bases that keep the 'whatDiv' and 'isContra' defaults.

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
                                          , ClosingTargetViolation(..)
                                          , closingTargetViolation
                                          , ClosingError(..)
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
import ExchangeAlgebra.Algebra.Value.Class (HatVal(..), isErrorValue)
import ExchangeAlgebra.Algebra.Element.Representation (Element(..))
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import ExchangeAlgebra.Accounting.Account ( AccountTitles(..)
                                          , AccountDivision(..)
                                          , AccountSpec(..)
                                          , AccountVocabulary(..)
                                          , ClosingRule(..)
                                          , pimoFromDivision
                                          , pimoFlip
                                          , PIMO(..)
                                          )
import ExchangeAlgebra.Algebra.Transfer.Representation (TransferApplyError(..))

-- * Closing entries

-- | The closing-target side selected by a closing account's PIMO direction.
data ClosingSide
    = ClosingKeep -- ^ IN: retain Hat/Not (revenue or contra cost).
    | ClosingFlip -- ^ OUT: reverse Hat/Not (cost or contra revenue).
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Binary ClosingSide

instance Hashable ClosingSide

-- | Select the closing-target direction for an account eligible for closing.
-- The direction follows the title's division in 'vocabSpec', flipped for a
-- contra account ('vocabContra'). Three kinds of title are excluded: titles
-- whose specification has 'NoClose' (for 'AccountTitles', NetIncome and
-- NetLoss among others); titles with 'CloseByDivision' whose direction is
-- neither IN nor OUT (asset, liability, and equity titles, including
-- 'RetainedEarnings', whose direction is PS or MS); and titles without a
-- specification, including the wildcard.
-- For 'AccountTitles' the result is the same as before the generalization.
{-# INLINABLE closingSide #-}
{-# SPECIALIZE closingSide :: AccountTitles -> Maybe ClosingSide #-}
closingSide :: AccountVocabulary t => t -> Maybe ClosingSide
closingSide title = case vocabSpec title of
    Nothing -> Nothing
    Just spec -> case asClosing spec of
        NoClose -> Nothing
        CloseByDivision -> case direction spec of
            IN -> Just ClosingKeep
            OUT -> Just ClosingFlip
            _ -> Nothing
  where
    direction spec
        | vocabContra title = pimoFlip (pimoFromDivision (asDivision spec))
        | otherwise = pimoFromDivision (asDivision spec)

-- | The first check that a title fails as a closing target.
data ClosingTargetViolation
    = ClosingTargetIsWildcard
      -- ^ The title is the vocabulary's wildcard ('isWildcard').
    | ClosingTargetWithoutSpec
      -- ^ 'vocabSpec' returns 'Nothing' for the title.
    | ClosingTargetNotEquity AccountDivision
      -- ^ The title's division, which is not 'Equity'.
    | ClosingTargetIsContra
      -- ^ The title is an equity contra account ('vocabContra').
    deriving (Eq, Show)

-- | Check a title against the closing-target law of 'AccountVocabulary'.
-- The checks run in this order: the title is not the wildcard
-- ('isWildcard', never the symmetric '.=='), it has a specification, its
-- division is 'Equity', and it is not a contra account. The result is the
-- first failed check, or 'Nothing' when the title can receive closing
-- balances. 'closingEntries' and 'settleEntries' apply it to 'closingTarget'.
--
-- >>> closingTargetViolation RetainedEarnings
-- Nothing
-- >>> closingTargetViolation Sales
-- Just (ClosingTargetNotEquity Revenue)
-- >>> closingTargetViolation AccountTitle
-- Just ClosingTargetIsWildcard
{-# INLINABLE closingTargetViolation #-}
closingTargetViolation :: AccountVocabulary t => t -> Maybe ClosingTargetViolation
closingTargetViolation title
    | isWildcard title = Just ClosingTargetIsWildcard
    | otherwise = case vocabSpec title of
        Nothing -> Just ClosingTargetWithoutSpec
        Just spec
            | asDivision spec /= Equity -> Just (ClosingTargetNotEquity (asDivision spec))
            | vocabContra title -> Just ClosingTargetIsContra
            | otherwise -> Nothing

-- | Failures of 'closingEntries'.
data ClosingError v b
    = InvalidClosingTarget (TitleOf b) ClosingTargetViolation
      -- ^ The vocabulary's 'closingTarget' and its first failed check.
      -- Reported before the ledger is read, so every ledger, including the
      -- empty one, gives this result.
    | ClosingTransferError (TransferApplyError v b)
      -- ^ A balance that cannot be closed: 'NonFiniteBalance' with the base.

deriving instance (Show v, Show b, Show (TitleOf b)) => Show (ClosingError v b)

-- | Generate closing entries that close each eligible base exactly.
-- Supply only postings through the closing date. For each base whose account
-- has a 'closingSide', the Hat and Not values are accumulated without
-- rounding ('HatVal' accumulator); the exact net (Not minus Hat) decides the
-- winning side. A zero net generates nothing. Otherwise 'splitAccum' splits
-- the exact magnitude into finite positive values @d1 >= d2 >= ... >= dk@
-- whose exact sum is the net, and each @di@ becomes one pair: the reversal of
-- the base and the posting to the vocabulary's 'closingTarget'
-- ('RetainedEarnings' for 'AccountTitles'). @k = 1@ whenever the net is
-- representable in the value type. The pairs are not compressed back into one
-- pair, so the exact net of every closed base after the entries is zero.
-- This folds the source sequences (audit detail) per base, but preserves
-- separate target postings from distinct source accounts. It does not call
-- @bar@. All base components other than the account title are retained, and no note is attached.
--
-- The closing target is checked first: when @closingTargetViolation
-- closingTarget == Just r@, the result is @Left (InvalidClosingTarget
-- closingTarget r)@ for every ledger, including the empty one.
-- @'ClosingTransferError' ('NonFiniteBalance' base)@ is returned when an
-- eligible base has a posting value
-- for which 'valueError' is 'Just' (non-finite or negative), or when its net
-- cannot be split ('splitAccum' returns 'Nothing': the rounded net is not
-- finite). Side sums beyond the value type's range succeed when the exact
-- net is in range. The reported base is the first in ascending 'Ord' order
-- after normalizing Hat/Not to Not. No partial result is returned.
-- 'NonFiniteResult' is never returned.
--
-- For 'AccountTitles' bases, @Right@ results are equal to those before the
-- generalization, and each former @Left e@ is now @Left (ClosingTransferError e)@.
--
-- Laws (exact arithmetic; observed with 'exactAccum'): for every closed base
-- the exact net of @x .+ c@ is zero, where @Right c = closingEntries x@;
-- the entries of @c@ have equal exact debit and credit sums; and
-- @closingEntries (x .+ c)@ returns @Right z@ with @isZero z@.
-- Balance collection is O(s log b), for s postings and b eligible bases.
{-# INLINABLE closingEntries #-}
closingEntries :: forall v b. (HatVal v, ExBaseClass b)
               => Alg v b -> Either (ClosingError v b) (Alg v b)
closingEntries ledger = case closingTargetViolation target of
    Just violation -> Left (InvalidClosingTarget target violation)
    Nothing -> either (Left . ClosingTransferError) Right
        (Map.foldlWithKey' close (Right Zero) (foldEntries collect Map.empty ledger))
  where
    target :: TitleOf b
    target = closingTarget

    collect :: Map b (Bool, Accum v, Accum v) -> v -> b
            -> Map b (Bool, Accum v, Accum v)
    collect balances value source = case (hat source, closingSide (getAccountTitle source)) of
        (Hat, Just _) -> Map.insertWith addTotals (toNot source)
            (isErrorValue value, addAccum value emptyAccum, emptyAccum) balances
        (Not, Just _) -> Map.insertWith addTotals source
            (isErrorValue value, emptyAccum, addAccum value emptyAccum) balances
        _ -> balances

    addTotals :: (Bool, Accum v, Accum v) -> (Bool, Accum v, Accum v)
              -> (Bool, Accum v, Accum v)
    addTotals (invalid, hatValue, notValue) (failed, hatTotal, notTotal) =
        (invalid || failed, mergeAccum hatValue hatTotal, mergeAccum notValue notTotal)

    close :: Either (TransferApplyError v b) (Alg v b)
          -> b
          -> (Bool, Accum v, Accum v)
          -> Either (TransferApplyError v b) (Alg v b)
    close result source (invalid, hatTotal, notTotal) = do
        entries   <- result
        additions <- netEntries source invalid (mergeAccum notTotal (negateAccum hatTotal))
        pure (entries .+ additions)

    netEntries :: b -> Bool -> Accum v -> Either (TransferApplyError v b) (Alg v b)
    netEntries source invalid amount
        | invalid = Left (NonFiniteBalance source)
        | otherwise = case signAccum amount of
            Just EQ -> Right Zero
            Just LT -> splitPairs source (toHat source) (negateAccum amount)
            Just GT -> splitPairs source source amount
            Nothing -> Left (NonFiniteBalance source)

    splitPairs :: b -> b -> Accum v -> Either (TransferApplyError v b) (Alg v b)
    splitPairs source balanceBase amount = case splitAccum amount of
        Nothing -> Left (NonFiniteBalance source)
        Just values -> case closingSide (getAccountTitle balanceBase) of
            Nothing   -> Right Zero
            Just side -> Right $ mconcat
                [ closingPairBy (targetSide side) target value balanceBase
                | value <- values ]

    targetSide :: ClosingSide -> b -> b
    targetSide side = case side of
        ClosingKeep -> id
        ClosingFlip -> revHat

-- | A rule that closes eligible accounts into the vocabulary's 'closingTarget'.
-- It carries no title: 'settleEntries' reads the destination from the base
-- type it is called at and checks it with 'closingTargetViolation'. The
-- constructor is private.
data SettleRule = SettleRule

-- | Close eligible accounts into the vocabulary's 'closingTarget'
-- ('RetainedEarnings' for 'AccountTitles'), preserving all other base components.
retainedEarningsRule :: SettleRule
retainedEarningsRule = SettleRule

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

-- | Failures of 'settleEntries'.
data SettleError b
    = NonFiniteNet (BasePart b)
      -- ^ The first non-finite input net, identified by its
      -- complete base.
    | InvalidSettleTarget (TitleOf b) ClosingTargetViolation
      -- ^ The vocabulary's 'closingTarget' and its first failed check.
      -- Reported before any net is read, so every input map, including the
      -- empty one, gives this result.

deriving instance (Eq (BasePart b), Eq (TitleOf b)) => Eq (SettleError b)
deriving instance (Show (BasePart b), Show (TitleOf b)) => Show (SettleError b)

-- | Construct one reversal and destination pair per eligible source base.
-- Input values are signed Not-minus-Hat nets. Zero nets, accounts without a
-- closing side, and destination bases produce no pair. Every pair has two
-- finite, non-negative magnitudes equal to the absolute input net; the source
-- reversal cancels that net exactly. Other base components are preserved.
--
-- The destination is the vocabulary's 'closingTarget'. It is checked first:
-- when @closingTargetViolation closingTarget == Just r@, the result is
-- @Left (InvalidSettleTarget closingTarget r)@ for every input map.
-- A non-finite input, including at an excluded key, returns 'NonFiniteNet' with
-- the first key in ascending order. Finite magnitudes above
-- 'ExchangeAlgebra.Algebra.Value.postingUpperBound' are accepted, without passing
-- through 'ExchangeAlgebra.Algebra.Posting.posted'. No implicit @bar@ or @compress@ is
-- applied. Complexity: O(b) for b input bases.
-- Law: subject: each generated settlement pair; preconditions: all nets finite.
-- Relation: each reversal cancels its source net. For each destination base,
-- its increment is the sum of @direction * net@ over the source bases, where
-- @direction@ is +1 for 'ClosingKeep' and -1 for 'ClosingFlip'.
-- Observation: sums of @decL@ and @decR@ lifted to Rational for each pair.
-- Tolerance: exact. Instances: 'ExBaseClass' bases with Double posting values.
-- For 'AccountTitles' bases the results are equal to those before the
-- generalization.
{-# INLINABLE settleEntries #-}
settleEntries :: forall b. ExBaseClass b
              => SettleRule
              -> Map (BasePart b) SignedNet
              -> Either (SettleError b) (SettlementBatch b)
settleEntries SettleRule amounts = case closingTargetViolation destination of
    Just violation -> Left (InvalidSettleTarget destination violation)
    Nothing -> case mapMaybe nonFinite (Map.toAscList amounts) of
        first : _ -> Left (NonFiniteNet first)
        []        -> Right (SettlementBatch (mapMaybe close (Map.toAscList amounts)))
  where
    destination :: TitleOf b
    destination = closingTarget
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
              -> TitleOf b
              -> v
              -> b
              -> Alg v b
closingPairBy targetSide targetAccount value source
    =  (value .@ revHat source)
    .+ (value .@ setAccountTitle (targetSide source) targetAccount)
