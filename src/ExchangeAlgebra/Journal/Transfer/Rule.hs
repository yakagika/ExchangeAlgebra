{- |
Module      : ExchangeAlgebra.Journal.Transfer.Rule
Description : Note-preserving transfer and carry entries across notes.

Use a qualified import to distinguish these functions from the Algebra API:

> import qualified ExchangeAlgebra.Journal.Transfer.Rule as Transfer

The additional-entry operations return entries to add to the input journal.
'carryBefore' instead returns the complete journal after replacing selected
notes. Transfer lifts Definition 9 over notes; closing reads the combined
ledger and returns an unannotated algebra through 'Either'. Read the
additional-entry section before the complete-result section.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExchangeAlgebra.Journal.Transfer.Rule
    ( -- * Additional entries
      transferEntries
    , closingEntries
    , carryEntries
      -- * Complete results
    , carryBefore
      -- * Side totals
    , SideTotals(..)
    , sideTotal
    , CarryError(..)
    , sideTotalsBy
    ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict as OrderedMap
import ExchangeAlgebra.Algebra.Core ( Alg( (:@) )
                                    , foldEntries
                                    , (.@)
                                    , Redundant((.^))
                                    )
import qualified ExchangeAlgebra.Algebra.Core as Algebra
import ExchangeAlgebra.Algebra.Transfer.Representation (TransferRules, TransferApplyError)
import qualified ExchangeAlgebra.Algebra.Transfer.Representation as Rule
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import ExchangeAlgebra.Algebra.Base.Representation (Hat(..), HatBaseClass(..))
import ExchangeAlgebra.Algebra.Value.Class (HatVal(..), isErrorValue)
import qualified ExchangeAlgebra.Algebra.Value.Class as Value
import ExchangeAlgebra.Algebra (ExBaseClass)
import ExchangeAlgebra.Journal.Core ( Journal
                                    , Note
                                    , (.|)
                                    , toMap
                                    , fromMap
                                    , toAlg
                                    )
import qualified ExchangeAlgebra.Journal.Core as Journal
import           ExchangeAlgebra.Algebra.Posting (PostSide(..))

-- * Additional entries

{-# DEPRECATED transferEntries "Use transferEntriesBy of ExchangeAlgebra.Accounting.Transfer, which reads the exact balance per target note." #-}

-- | Generate additional entries under the same notes as their sources.
-- Each note is processed in the traversal order of 'toMap'. The first failure
-- aborts the whole operation; no partial Journal is returned. This order is
-- an implementation traversal, not a chronological ordering of notes.
-- Input posting values must be valid and finite. No @bar@ is applied.
transferEntries :: (Note n, HatVal v, HatBaseClass b)
                => TransferRules v b
                -> Journal n v b
                -> Either (TransferApplyError v b) (Journal n v b)
transferEntries rules journal = fromMap . Map.fromList
                             <$> traverse apply (Map.toList (toMap journal))
  where
    apply (note, algebra) = do
        entries <- Rule.transferEntries rules algebra
        pure (note, entries)

-- | Read net balances across all notes and return unannotated closing entries.
-- The input must include only postings through the closing date; 'toAlg'
-- does not select a period. Earlier periods must already have their closing
-- entries included. Attach the settlement note with @(.|)@ at the call site.
-- Source sequences are accumulated exactly per base by the Algebra operation,
-- without a tolerance or implicit @bar@. Each nonzero net is split into finite
-- positive values and each value produces a reversal and retained-earnings pair.
-- Invalid eligible postings or an unsplittable net return 'Left', reporting
-- the first failing base in ascending order with Hat/Not normalized to Not.
-- Side totals beyond the value type's range can succeed through cancellation.
-- No partial result is returned.
closingEntries :: (Note n, HatVal v, ExBaseClass b)
               => Journal n v b -> Either (TransferApplyError v b) (Alg v b)
closingEntries = Closing.closingEntries . toAlg

-- | Round selected complete-base nets once and attach the carry note.
-- Each complete base's Not and Hat values are accumulated exactly with the
-- 'HatVal' accumulator; the net is rounded once. Failures are reported as
-- 'CarryError' (see 'carryEntries').
carryNetEntries :: forall n v b. (Note n, HatVal v, HatBaseClass b)
                => n
                -> Journal n v b
                -> Either CarryError (Journal n v b)
carryNetEntries carryNote journal = case validationError of
    Just failure -> Left failure
    Nothing -> (.| carryNote) . mconcat <$> traverse finish (OrderedMap.toAscList totals)
  where
    (validationError, totals) = Map.foldl' (foldEntries scanEntry)
        (Nothing, OrderedMap.empty) (toMap journal)

    scanEntry :: (Maybe CarryError, OrderedMap.Map (BasePart b) (Accum v))
              -> v
              -> b
              -> (Maybe CarryError, OrderedMap.Map (BasePart b) (Accum v))
    scanEntry (!failure, !acc) value coordinates
        | Just current <- classify value coordinates = (Just (prefer failure current), acc)
        | otherwise = (failure, OrderedMap.insertWith mergeAccum (base coordinates) amount acc)
      where
        amount = case hat coordinates of
            Hat -> negateAccum (addAccum value emptyAccum)
            _   -> addAccum value emptyAccum

    classify :: v -> b -> Maybe CarryError
    classify value coordinates
        | hat coordinates == HatNot = Just WildcardSide
        | Just Value.NonFiniteValue <- valueError value = Just NonFiniteValue
        | Just Value.NegativeValue <- valueError value = Just NegativeValue
        | otherwise = Nothing

    prefer :: Maybe CarryError -> CarryError -> CarryError
    prefer (Just WildcardSide) _ = WildcardSide
    prefer _ WildcardSide = WildcardSide
    prefer (Just NonFiniteValue) _ = NonFiniteValue
    prefer _ NonFiniteValue = NonFiniteValue
    prefer (Just NegativeValue) _ = NegativeValue
    prefer _ NegativeValue = NegativeValue
    prefer _ ResultOutOfRange = ResultOutOfRange

    finish :: (BasePart b, Accum v) -> Either CarryError (Alg v b)
    finish (coordinates, amount) = case signAccum amount of
        Just EQ -> Right mempty
        Just LT -> roundedPosting Hat coordinates (negateAccum amount)
        _       -> roundedPosting Not coordinates amount

    roundedPosting :: Hat -> BasePart b -> Accum v -> Either CarryError (Alg v b)
    roundedPosting side coordinates amount
        | isErrorValue value = Left ResultOutOfRange
        | otherwise = Right (value .@ merge side coordinates)
      where
        value = roundAccum amount

-- | Generate carry entries while retaining every original posting.
-- Select source notes with the predicate, reverse the Hat or Not side of each
-- selected scalar under its original note, and append one rounded exact net
-- per complete base under the carry note. An exact zero net adds no entry.
-- The net accumulates the selected original scalars exactly with the 'HatVal'
-- accumulator and rounds once per complete base with the value type's
-- 'ExchangeAlgebra.Algebra.Value.Class.roundAccum'; no @bar@ or intermediate
-- rounding is applied.
--
-- Failures return 'Left' with 'CarryError' and no partial journal. Every
-- selected posting is checked before any net is rounded, and the
-- highest-priority failure wins regardless of traversal order:
-- 'WildcardSide' (a selected nonzero HatNot posting), 'NonFiniteValue',
-- 'NegativeValue', then 'ResultOutOfRange' (a rounded net is not finite).
-- A selected raw zero HatNot posting, which ordinary construction never
-- produces, is not reported: it contributes nothing to the net and is
-- omitted from the reversal, since 'revHat' is undefined for HatNot.
--
-- Inputs require concrete Hat or Not sides and finite, non-negative values.
-- Applying the returned entries cancels selected notes mathematically. The
-- net read out by the 'HatVal' accumulator succeeds on the resulting journal,
-- but the 0.5.x @*Exact@ readers can fail on it: reversing Not and Hat
-- entries of @2^1023@ at one base makes each side total @2^1024@. For the
-- same reason 'sideTotalsBy' reports 'ResultOutOfRange' on such a journal,
-- because it rounds each side total rather than the net.
--
-- Law: subject: 'carryEntries'. Preconditions: the input meets the side and
-- value contract above and every rounded net is finite.
-- Relation: for each base @b@, the Rational Not-minus-Hat balance obeys
-- @exact_b(j <> e) - exact_b(j) = RN(s_b) - s_b@, where @e@ is the generated
-- journal, @s_b@ is the selected exact net, and @RN@ is one rounding by
-- 'ExchangeAlgebra.Algebra.Value.Class.roundAccum' of the value type (the
-- identity for t'ExchangeAlgebra.Algebra.Value.MoneyDecimal' within its
-- range, nearest-even for Double).
-- Observation: Rational balances of original scalar entries, including zero
-- bases. Tolerance: exact Rational equality. Instances: 'Note' and
-- 'HatBaseClass' with any 'HatVal' value type. For Double, on every input
-- for which the 0.5.x implementation returned 'Right', the results are
-- bit-identical; some inputs it rejected because a side sum left the Double
-- range now succeed, because only the net is rounded.
carryEntries :: forall n v b. (Note n, HatVal v, HatBaseClass b)
             => (n -> Bool)
             -> n
             -> Journal n v b
             -> Either CarryError (Journal n v b)
carryEntries selectedNote carryNote journal = do
    carried <- carryNetEntries carryNote selected
    pure (Journal.map ((.^) . Algebra.filter reversible) selected <> carried)
  where
    selected = Journal.filterWithNote (\note _ -> selectedNote note) journal

    reversible :: Alg v b -> Bool
    reversible (value :@ coordinates) =
        not (isZeroValue value && hat coordinates == HatNot)
    reversible _ = True

-- * Complete results

-- | Replace selected notes with one rounded exact net per complete base.
-- Other entries retain their notes, bases, sides, and values. Entries already
-- under the carry note remain when that note is not selected. This returns
-- the entire resulting journal: selected audit entries are discarded, rather
-- than retained with cancellation entries. Positive nets become Not entries,
-- negative nets become Hat entries, and exact zero nets add nothing.
--
-- For selected entries @S@, unselected entries @U@, and rounded carried
-- entries @Q@, this function constructs @U <> Q@ directly. Its result has
-- the same nonzero scalar-entry multiset as adding 'carryEntries' to the
-- original journal and then forgetting notes selected by the predicate.
-- This decomposition requires @not (selectedNote carryNote)@, concrete Hat
-- or Not sides, finite non-negative values, and successful exact aggregation.
-- It does not claim equality for zero entries, empty notes, or sequence order.
-- Existing entries under the carry note remain and are not netted again.
-- When @selectedNote carryNote@ is true, this function still carries as
-- described, but the decomposition does not hold because forgetting the
-- selected notes would also remove @Q@. Selected nonzero HatNot entries lie
-- outside the contract.
--
-- The selected net is accumulated exactly and rounded once per complete base,
-- with the same 'CarryError' failures as 'carryEntries'. Structurally, 'carryEntries' resembles
-- 'ExchangeAlgebra.Algebra.Transfer.Rule.collapseNetEntries': both generate
-- reversals and moved nets. The latter uses @bar@ after rewriting and has a
-- different numeric contract. Complexity: O(e log(k + 1)) for e selected
-- scalars and k distinct selected complete bases, plus filtering and merge.
-- Law: subject: 'carryBefore'. Preconditions: the decomposition conditions
-- above. Relation: the nonzero scalar-entry multiset equals that obtained
-- from @forgetNotes p (j <> e)@ for @Right e = carryEntries p n j@.
-- Observation: nonzero scalar-entry multiset. Tolerance: exact.
-- Instances: 'Note' and 'HatBaseClass' with any 'HatVal' value type.
carryBefore :: (Note n, HatVal v, HatBaseClass b)
            => (n -> Bool)
            -> n
            -> Journal n v b
            -> Either CarryError (Journal n v b)
carryBefore selectedNote carryNote journal = do
    carried <- carryNetEntries carryNote selected
    pure (retained <> carried)
  where
    selected = Journal.filterWithNote (\note _ -> selectedNote note) journal
    retained = Journal.filterWithNote (\note _ -> not (selectedNote note)) journal

-- * Side totals

-- | Pre-cancellation totals for one key. An absent side has positive zero.
data SideTotals = SideTotals
    { notTotal :: !Double -- ^ The Not sum, rounded once to nearest even.
    , hatTotal :: !Double -- ^ The Hat sum, rounded once to nearest even.
    } deriving (Eq, Show)

-- | Select a pre-cancellation total by posting side.
sideTotal :: PostSide -> SideTotals -> Double
sideTotal HatSide = hatTotal
sideTotal NotSide = notTotal

-- | Failures of 'carryEntries', 'carryBefore', and 'sideTotalsBy', in
-- descending priority. This is the successor of 0.5.x t'ExchangeAlgebra.Algebra.Exact.ExactSumError'
-- for these functions.
data CarryError
    = WildcardSide   -- ^ A posting uses HatNot, including a retained zero posting.
    | NonFiniteValue -- ^ A posting is NaN or infinite.
    | NegativeValue  -- ^ A posting has a negative value.
    | ResultOutOfRange -- ^ A rounded side total or net is not finite.
    deriving (Eq, Show)

-- | Accumulate one side without retaining its source postings.
data SideAccum = SideAccum !(Accum Double)

-- | Keep Not and Hat independent until both sides have been rounded.
data SideAccums = SideAccums !SideAccum !SideAccum

emptySide :: SideAccum
emptySide = SideAccum emptyAccum

addSideValue :: Double -> SideAccum -> SideAccum
addSideValue value (SideAccum total) = SideAccum (addAccum value total)

roundSide :: SideAccum -> Either CarryError Double
roundSide (SideAccum total)
    | isInfinite value = Left ResultOutOfRange
    | value == 0       = Right 0
    | otherwise        = Right value
  where
    value = roundAccum total

-- | Sum every posting by a key derived from its 'BasePart', without netting
-- Hat against Not or combining source postings. Each side is the exact sum of
-- its finite, non-negative input values rounded once to nearest-even Double.
-- Results, including zero, have a positive sign. The empty journal gives an
-- empty map; a key with one side absent has positive zero for that side.
-- Reordering postings, regrouping @(.+)@, or reassigning notes leaves each
-- output's bits unchanged. Every posting is validated independently of the
-- accumulator. The highest-priority failure wins regardless of traversal:
-- 'WildcardSide', 'NonFiniteValue', 'NegativeValue', then 'ResultOutOfRange'.
-- A finite rounded total succeeds even if its exact sum exceeds the largest
-- finite Double; for example, maximum finite plus the least subnormal rounds
-- back to maximum finite. No partial map is returned on failure.
-- A raw zero @(:@)@ posting is inspected, including HatNot; ordinary algebra
-- construction discards zero postings before they reach this function.
-- Complexity: one traversal of all entries with O(log(k + 1)) map updates
-- per entry for k distinct keys, followed by one rounding per side and key.
--
-- Law: subject: 'sideTotalsBy'. Preconditions: all postings have concrete
-- sides and finite, non-negative values, and all rounded side sums are finite.
-- Relation: each output side equals nearest-even rounding of the Rational sum
-- of that key's postings on that side, before Hat/Not cancellation.
-- Observation: Double bits for each key and side. Tolerance: bit-identical.
-- Instances: 'Note' and 'HatBaseClass' with Double values and ordered keys.
sideTotalsBy :: forall n b k. (Note n, HatBaseClass b, Ord k)
             => (BasePart b -> k)
             -> Journal n Double b
             -> Either CarryError (OrderedMap.Map k SideTotals)
sideTotalsBy keyOf journal = case validationError of
    Just failure -> Left failure
    Nothing -> traverse finish totals
  where
    (validationError, totals) = Map.foldl' (scanAlgebra scanEntry)
        (Nothing, OrderedMap.empty)
        (toMap journal)

    scanAlgebra :: (state -> Double -> b -> state) -> state -> Alg Double b -> state
    scanAlgebra step !state algebra = case algebra of
        value :@ coordinates -> step state value coordinates
        _ -> foldEntries step state algebra

    scanEntry :: (Maybe CarryError, OrderedMap.Map k SideAccums)
              -> Double
              -> b
              -> (Maybe CarryError, OrderedMap.Map k SideAccums)
    scanEntry (!failure, !acc) value coordinates =
        case classify value coordinates of
            Just current -> (Just (prefer failure current), acc)
            Nothing ->
                let !key = keyOf (base coordinates)
                    !updated = OrderedMap.alter (Just . addToSide (hat coordinates) value
                        . maybe (SideAccums emptySide emptySide) id) key acc
                in (failure, updated)

    classify :: Double -> b -> Maybe CarryError
    classify value coordinates
        | hat coordinates == HatNot = Just WildcardSide
        | isNaN value || isInfinite value = Just NonFiniteValue
        | value < 0 = Just NegativeValue
        | otherwise = Nothing

    prefer :: Maybe CarryError -> CarryError -> CarryError
    prefer (Just WildcardSide) _ = WildcardSide
    prefer _ WildcardSide = WildcardSide
    prefer (Just NonFiniteValue) _ = NonFiniteValue
    prefer _ NonFiniteValue = NonFiniteValue
    prefer (Just NegativeValue) _ = NegativeValue
    prefer _ NegativeValue = NegativeValue
    prefer _ ResultOutOfRange = ResultOutOfRange

    addToSide :: Hat -> Double -> SideAccums -> SideAccums
    addToSide Hat value (SideAccums notSide hatSide) =
        SideAccums notSide (addSideValue value hatSide)
    addToSide Not value (SideAccums notSide hatSide) =
        SideAccums (addSideValue value notSide) hatSide
    addToSide HatNot _ sides = sides

    finish :: SideAccums -> Either CarryError SideTotals
    finish (SideAccums notSide hatSide) =
        SideTotals <$> roundSide notSide <*> roundSide hatSide
