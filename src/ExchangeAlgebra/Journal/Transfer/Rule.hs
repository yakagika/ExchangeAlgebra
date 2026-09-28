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
import qualified Data.Set as Set
import ExchangeAlgebra.Algebra.Core ( Alg( (:@) )
                                    , foldEntries
                                    , (.@)
                                    , Redundant((.^))
                                    )
import           ExchangeAlgebra.Algebra.Exact (ExactSum(..))
import ExchangeAlgebra.Algebra.Transfer.Representation (TransferRules, TransferApplyError)
import qualified ExchangeAlgebra.Algebra.Transfer.Representation as Rule
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import ExchangeAlgebra.Algebra.Base.Representation (Hat(..), HatBaseClass(..))
import ExchangeAlgebra.Algebra.Value.Class (HatVal)
import ExchangeAlgebra.Algebra (ExBaseClass)
import ExchangeAlgebra.Journal.Core ( Journal
                                    , Note
                                    , (.|)
                                    , toMap
                                    , fromMap
                                    , toAlg
                                    )
import qualified ExchangeAlgebra.Journal.Core as Journal
import           ExchangeAlgebra.Journal.Exact (ExactSumError(..))
import qualified ExchangeAlgebra.Journal.Exact as Exact
import           ExchangeAlgebra.Algebra.Posting (PostSide(..))

-- * Additional entries

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
-- Source sequences are folded per base by the Algebra operation, without
-- a tolerance or implicit @bar@; small floating-point residues can remain.
-- Finite, non-negative inputs normally yield 'Right'; 'Left' occurs only
-- when a side's sum exceeds the value type's range. The Algebra operation
-- checks totals and differences and reports the first failing base in
-- ascending order, with Hat/Not normalized to Not. No partial result is returned.
closingEntries :: (Note n, HatVal v, ExBaseClass b)
               => Journal n v b -> Either (TransferApplyError v b) (Alg v b)
closingEntries = Closing.closingEntries . toAlg

-- | Round selected complete-base nets once and attach the carry note.
carryNetEntries :: (Note n, HatBaseClass b)
                => n
                -> Journal n Double b
                -> Either ExactSumError (Journal n Double b)
carryNetEntries carryNote selected = do
    balances <- Exact.balanceMapByExact Just selected
    pure (OrderedMap.foldlWithKey' append mempty balances)
  where
    append result coordinates (direction, value) = case direction of
        EQ -> result
        GT -> result <> ((value .@ merge Not coordinates) .| carryNote)
        LT -> result <> ((value .@ merge Hat coordinates) .| carryNote)

-- | Generate carry entries while retaining every original posting.
-- Select source notes with the predicate, reverse the Hat or Not side of each
-- selected scalar under its original note, and append one rounded exact net
-- per complete base under the carry note. An exact zero net adds no entry.
-- The net uses 'Exact.balanceMapByExact' on the selected original scalars;
-- no @bar@ or intermediate rounding is applied. An aggregation failure
-- returns 'Left' with 'ExactSumError'.
--
-- Inputs require concrete Hat or Not sides and finite, non-negative values.
-- Selected nonzero HatNot entries lie outside this contract. Applying the
-- returned entries cancels selected notes mathematically, but reading the
-- resulting journal with Exact can fail: reversing Not and Hat entries of
-- @2^1023@ at one base makes each side total @2^1024@.
--
-- Law: subject: 'carryEntries'. Preconditions: selected exact aggregation
-- succeeds and the input meets the side and value contract above.
-- Relation: for each base @b@, the Rational Not-minus-Hat balance obeys
-- @exact_b(j <> e) - exact_b(j) = RN(s_b) - s_b@, where @e@ is the generated
-- journal, @s_b@ is the selected exact net, and @RN@ rounds once to Double.
-- Observation: Rational balances of original scalar entries, including zero
-- bases. Tolerance: exact Rational equality. Instances: 'Note' and
-- 'HatBaseClass' with Double values.
carryEntries :: (Note n, HatBaseClass b)
             => (n -> Bool)
             -> n
             -> Journal n Double b
             -> Either ExactSumError (Journal n Double b)
carryEntries selectedNote carryNote journal = do
    carried <- carryNetEntries carryNote selected
    pure (Journal.map (.^) selected <> carried)
  where
    selected = Journal.filterWithNote (\note _ -> selectedNote note) journal

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
-- The selected net is computed by 'Exact.balanceMapByExact' and rounded once
-- per complete base. An aggregation failure returns 'Left' with
-- 'ExactSumError'. Structurally, 'carryEntries' resembles
-- 'ExchangeAlgebra.Algebra.Transfer.Rule.collapseNetEntries': both generate
-- reversals and moved nets. The latter uses @bar@ after rewriting and has a
-- different numeric contract. Complexity: O(e log(k + 1)) for e selected
-- scalars and k distinct selected complete bases, plus filtering and merge.
-- Law: subject: 'carryBefore'. Preconditions: the decomposition conditions
-- above. Relation: the nonzero scalar-entry multiset equals that obtained
-- from @forgetNotes p (j <> e)@ for @Right e = carryEntries p n j@.
-- Observation: nonzero scalar-entry multiset. Tolerance: exact.
-- Instances: 'Note' and 'HatBaseClass' with Double values.
carryBefore :: (Note n, HatBaseClass b)
            => (n -> Bool)
            -> n
            -> Journal n Double b
            -> Either ExactSumError (Journal n Double b)
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

-- | Failures of 'sideTotalsBy', in descending priority.
-- In P3 this type is planned to succeed 'ExactSumError'. It currently belongs
-- only to 'sideTotalsBy'; 'carryBefore' still reports 'ExactSumError'.
data CarryError
    = WildcardSide   -- ^ A posting uses HatNot, including a retained zero posting.
    | NonFiniteValue -- ^ A posting is NaN or infinite.
    | NegativeValue  -- ^ A posting has a negative value.
    | ResultOutOfRange -- ^ A rounded side total is infinite.
    deriving (Eq, Show)

-- | Accumulate one side without retaining its source postings.
data SideAccum = SideAccum !(Accum Double)

-- | Keep Not and Hat independent until both sides have been rounded.
data SideAccums = SideAccums !SideAccum !SideAccum

emptySide :: SideAccum
emptySide = SideAccum emptyAccum

addSideValue :: Double -> SideAccum -> SideAccum
addSideValue value (SideAccum total) = SideAccum (addAccum value total)

roundSide :: SideAccum -> Either ExactSumError Double
roundSide (SideAccum total) = roundAccum total

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
-- per entry for k distinct keys. Only when a side exceeds ExactSum's range,
-- a second traversal of all entries recomputes those sides as Rational sums.
--
-- Law: subject: 'sideTotalsBy'. Preconditions: all postings have concrete
-- sides and finite, non-negative values, and all rounded side sums are finite.
-- Relation: each output side equals nearest-even rounding of the Rational sum
-- of that key's postings on that side, before Hat/Not cancellation.
-- Observation: Double bits for each key and side. Tolerance: bit-identical.
-- Instances: 'Note' and 'HatBaseClass' with Double values and ordered keys.
sideTotalsBy :: (Note n, HatBaseClass b, Ord k)
             => (BasePart b -> k)
             -> Journal n Double b
             -> Either CarryError (OrderedMap.Map k SideTotals)
sideTotalsBy keyOf journal = case validationError of
    Just failure -> Left failure
    Nothing -> OrderedMap.traverseWithKey finish rounded
  where
    (validationError, totals) = Map.foldl' (scanAlgebra scanEntry)
        (Nothing, OrderedMap.empty)
        (toMap journal)

    scanAlgebra step !state algebra = case algebra of
        value :@ coordinates -> step state value coordinates
        _ -> foldEntries step state algebra

    scanEntry (!failure, !acc) value coordinates =
        case classify value coordinates of
            Just current -> (Just (prefer failure current), acc)
            Nothing ->
                let !key = keyOf (base coordinates)
                    !updated = OrderedMap.alter (Just . addToSide (hat coordinates) value
                        . maybe (SideAccums emptySide emptySide) id) key acc
                in (failure, updated)

    classify value coordinates
        | hat coordinates == HatNot = Just WildcardSide
        | isNaN value || isInfinite value = Just NonFiniteValue
        | value < 0 = Just NegativeValue
        | otherwise = Nothing

    prefer (Just WildcardSide) _ = WildcardSide
    prefer _ WildcardSide = WildcardSide
    prefer (Just NonFiniteValue) _ = NonFiniteValue
    prefer _ NonFiniteValue = NonFiniteValue
    prefer (Just NegativeValue) _ = NegativeValue
    prefer _ NegativeValue = NegativeValue
    prefer _ ResultOutOfRange = ResultOutOfRange

    addToSide Hat value (SideAccums notSide hatSide) =
        SideAccums notSide (addSideValue value hatSide)
    addToSide Not value (SideAccums notSide hatSide) =
        SideAccums (addSideValue value notSide) hatSide
    addToSide HatNot _ sides = sides

    rounded = OrderedMap.map (\(SideAccums notSide hatSide) ->
        (roundSide notSide, roundSide hatSide)) totals

    overflowing = OrderedMap.foldlWithKey' collectOverflow Set.empty rounded
    collectOverflow !keys key (notResult, hatResult) =
        let !withNot = if notResult == Left SumOutOfRange
                then Set.insert (key, NotSide) keys else keys
        in if hatResult == Left SumOutOfRange
           then Set.insert (key, HatSide) withNot else withNot

    fallbackTotals
        | Set.null overflowing = OrderedMap.empty
        | otherwise = Map.foldl' (scanAlgebra scanFallback) OrderedMap.empty
            (toMap journal)

    scanFallback !acc value coordinates =
        let !side = if hat coordinates == Hat then HatSide else NotSide
            !target = (keyOf (base coordinates), side)
        in if Set.member target overflowing
           then OrderedMap.insertWith (+) target (toRational value) acc
           else acc

    finish key (notResult, hatResult) =
        SideTotals <$> finishSide (key, NotSide) notResult
                   <*> finishSide (key, HatSide) hatResult

    finishSide _ (Right value) = Right (if value == 0 then 0 else value)
    finishSide target (Left SumOutOfRange) =
        let !value = fromRational (OrderedMap.findWithDefault 0 target fallbackTotals)
                :: Double
        in if isInfinite value then Left ResultOutOfRange
           else Right (if value == 0 then 0 else value)
    -- The first traversal validates every input, so ExactSum cannot report
    -- NonFiniteInput or NegativeInput here.
    finishSide _ (Left _) = Left ResultOutOfRange
