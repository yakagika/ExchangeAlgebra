{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : ExchangeAlgebra.Accounting.Transfer
Description : Accounting transfers from exact balances (Definition 9).

An accounting transfer reads the exact balance of each selected base and
returns the additional entries that move it: a cancellation of the source
and a posting to the destination, of equal value. Bases here are complete
bases without Hat\/Not ('BasePart'); a balance is Not minus Hat of one base.
The destination keeps the balance on the same debit or credit side: when the
source and destination accounts have the same home side
(@whichSide (Not :< b)@), the destination keeps the balance's Hat\/Not;
otherwise it reverses it. Rules do not state Hat\/Not.

Write a table in the order the rows apply:

> import ExchangeAlgebra.Accounting.Transfer
>
> table = mkTransferTable
>     [ (RentExpense, (.#)) .-> (PrepaidExpenses, (.#)) |% Portion 8 12 ]

Each row reads the balances left by the rows before it, so @[A .-> B |%
Relabel, B .-> C |% Relabel]@ moves A to C in one application. A table in
which a row can move a balance into the source of the same or an earlier row
is rejected ('BackwardFlow'); this includes @A .-> A@ and cycles.

'transferEntries' returns only the additional entries @d@; add them with
@x .+ d@. It does not call @bar@. The input ledger keeps every posting.

* 'Relabel' moves the whole exact balance. When the balance is not a value
  of the value type, it is split into several finite positive values
  ('splitAccum'), each one pair, so the source closes exactly also for
  'Double'.
* @'Portion' n d@ (@0 < n <= d@) moves the exact balance times @n/d@, rounded
  once with the given 'Rounding' (@roundScaled@). Applying it twice moves a
  share of the remainder; it is not idempotent.

Reading the result of a journal transfer: base projections of a
"ExchangeAlgebra.Journal.Core" journal cancel only within a note. When the
transfer entries sit under a different note from the original postings,
combine the notes with @toAlg@ (or @gather@) before reading a balance. Pass
the transfer notes of the same range to closing and carrying, and apply a
transfer to the ledger after any per-period archiving.

Conversion between units (quantity to money) is not a transfer: use
'ExchangeAlgebra.Algebra.Core.mapPosting' (or @map@ of a journal) and keep the
ledger in quantities.

For generic per-posting coordinate rules, import
"ExchangeAlgebra.Algebra.Transfer" qualified. The umbrella modules export
the accounting transfer names defined here.

== Laws

Bases are flat @HatBase@ tuples with two or more coordinates and every
registry account; value types are 'Double' and @MoneyDecimal@. The exact
balance of a base is Not minus Hat read with 'exactAccum' as a 'Rational'.
@d = transferEntries t x@ with @Right d@ unless stated.

=== T1: balance

* Subject: 'transferEntries'.
* Preconditions: valid input; the call returns @Right d@.
* Relation: the exact debit and credit sums of @d@ are equal; the
  cancellation and destination of each pair have the same value.
* Observation: 'exactAccum' of @decL d@ and @decR d@, and of each pair.
* Tolerance: none. Instances: as above.

=== T2: shape

* Subject: 'transferEntries'.
* Preconditions: as T1.
* Relation: every posting of @d@ is finite and positive. A 'Relabel' row
  makes @k >= 1@ pairs per base with a nonzero balance (@k = 1@ when the
  balance is a value of the type); a 'Portion' row makes at most one pair per
  base, and none when the rounded amount is zero.
* Observation: postings of @d@ before any @bar@. Tolerance: none.

=== T3: closure

* Subject: a 'Relabel' row.
* Preconditions: as T1.
* Relation: after the row applies, the exact balance of every base that
  matches its source is zero.
* Observation: exact balances of @x .+ d@. Tolerance: none, also for 'Double'.

=== T4: idempotence

* Subject: tables whose rows are all 'Relabel'.
* Preconditions: as T1.
* Relation: @transferEntries t (x .+ d) == Right Zero@.
* Observation: 'Eq' of the result. Tolerance: none.

=== T5: agreement with rewriting

* Subject: 'Relabel' tables.
* Preconditions: as T1.
* Relation: the exact balances of @x .+ d@ equal those of @x@ rewritten by
  the table row by row, each posting keeping its debit or credit side.
* Observation: exact balance of every base. Posting sequences and counts are
  not compared. Tolerance: none.

=== T6: Hat\/Not of the destination

* Subject: each generated pair.
* Relation: the destination's Hat\/Not is the opposite of the cancellation's
  when the source and destination have the same home side, and equal to it
  otherwise.
* Observation: Hat\/Not of the pair. Instances: every pair of registry accounts.

=== T7: closing

* Subject: 'transferEntries' and 'ExchangeAlgebra.Accounting.Closing.closingEntries'.
* Preconditions: @t@ has one 'Relabel' row @(a, #, ...) .-> (RetainedEarnings,
  #, ...)@ for each account @a@ with a @closingSide@.
* Relation: @closingEntries x@ and @transferEntries t x@ have the same
  multiset of postings; for every such account the @closingSide@ decision
  agrees with the comparison of home sides.
* Observation: sorted postings. Tolerance: none.

=== T8: portion

* Subject: a @'Portion' n d@ row.
* Relation: the moved amount is @roundScaled rule (n % d) |s|@ for the exact
  balance @s@; @|s| - q@ remains on the source exactly.
* Observation: exact balances. Tolerance: none. With @MoneyDecimal@ a
  balance of 1 and @Portion 1 3@ fails with 'UnroundableAmount' under
  'Nearest' and moves 0.33 under @ToMultipleOf 0.01@; a balance of 3 moves 1.

=== T9: order

* Subject: 'mkTransferTable'.
* Relation: @[B .-> C, A .-> B]@ is 'BackwardFlow'; @[A .-> B, B .-> C]@ is
  accepted and moves the balance of A to C. 'tableRows' returns the rows in
  the order written.

=== T10: failures

* Subject: 'transferEntries'.
* Relation: a NaN, negative or nonzero @HatNot@ posting on a selected base
  returns the corresponding 'Left'; permuting the postings does not change the
  result (the failure order below is total).

=== J1-J3: journals

* J1: the entries of 'transferEntriesBy' under target note @g@ equal
  @transferEntries t (toAlg (filterWithNote (\\n _ -> f n == g) j))@; other
  notes are empty.
* J2: if an input note @m@ is a target and @f m /= m@, the result is
  @Left (TransferNoteMoves m (f m))@; a target absent from the input (the
  next period) is accepted.
* J3: for all-'Relabel' tables and @f@ that maps every target to itself,
  @transferEntriesBy f t (j .+ r)@ is empty, where @Right r@ is the first result.
-}
module ExchangeAlgebra.Accounting.Transfer
    ( -- * Tables
      TransferShare(..)
    , TransferEnds
    , TransferRow
    , TransferTable
    , (.->)
    , (|%)
    , mkTransferTable
    , tableRows
    , TransferTableError(..)
      -- * Additional entries
    , TransferError(..)
    , transferEntries
    , transferEntriesWith
      -- * Journals
    , TransferJournalError(..)
    , transferEntriesBy
    , transferEntriesByWith
    ) where

import Data.List (find, inits)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isNothing)
import Data.Ratio ((%))
import ExchangeAlgebra.Algebra.Core (Alg(..), Redundant((.+)), (.@), foldEntries)
import ExchangeAlgebra.Algebra.Base.Representation (HatBaseClass(..), Hat(..))
import ExchangeAlgebra.Algebra.Element (ignoreWildcard)
import ExchangeAlgebra.Algebra.Transfer.Representation (matches, patternsOverlap)
import ExchangeAlgebra.Algebra.Value.Class
    ( HatVal(..)
    , ValueError(..)
    , Rounding(..)
    , roundScaled
    )
import ExchangeAlgebra.Accounting.Account (accountSpec)
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import ExchangeAlgebra.Journal.Core (Journal, Note)
import qualified ExchangeAlgebra.Journal.Core as Journal

-- * Tables

-- | How much of a balance a row moves.
data TransferShare
    = Relabel
      -- ^ The whole exact balance.
    | Portion !Integer !Integer
      -- ^ @Portion n d@ moves @n/d@ of the exact balance, rounded once;
      -- @0 < n <= d@, checked by 'mkTransferTable'.
    deriving (Eq, Show)

-- | A source and a destination, written @from .-> to@. Both are complete bases
-- without Hat\/Not; a wildcard in the source selects any coordinate and a
-- wildcard in the destination keeps the source's coordinate.
data TransferEnds b = TransferEnds (BasePart b) (BasePart b)

deriving instance Eq (BasePart b) => Eq (TransferEnds b)

-- | One row of a table, written @from .-> to |% share@.
data TransferRow b = TransferRow (BasePart b) (BasePart b) TransferShare

deriving instance Eq (BasePart b) => Eq (TransferRow b)

instance Show (BasePart b) => Show (TransferRow b) where
    showsPrec d (TransferRow from to share) = showParen (d > 7)
        $ showsPrec 9 from . showString " .-> " . showsPrec 9 to
        . showString " |% " . showsPrec 8 share

-- | Validated rows in the order written. The constructor is private; build
-- tables with 'mkTransferTable'.
newtype TransferTable b = TransferTable [TransferRow b]

deriving instance Eq (BasePart b) => Eq (TransferTable b)
deriving instance Show (BasePart b) => Show (TransferTable b)

infixr 8 .->
infixr 7 |%

-- | Pair a source with a destination.
(.->) :: BasePart b -> BasePart b -> TransferEnds b
(.->) = TransferEnds

-- | Attach the share to move.
(|%) :: TransferEnds b -> TransferShare -> TransferRow b
(|%) (TransferEnds from to) = TransferRow from to

-- | A table that 'mkTransferTable' rejects. Checks run in the order of the
-- constructors; within each, rows are compared in the order written.
data TransferTableError b
    = InvalidPortion (TransferRow b)
      -- ^ The first row with @n <= 0@, @d <= 0@ or @n > d@.
    | OverlappingRows (TransferRow b) (TransferRow b)
      -- ^ The first pair of rows @(i, j)@, @i < j@, whose sources overlap
      -- (ordered by @j@, then @i@). Duplicates overlap.
    | BackwardFlow (TransferRow b) (TransferRow b)
      -- ^ The first pair @(i, j)@, @i <= j@, in the same order, where row @j@
      -- can move a balance into the source of row @i@: the destination
      -- pattern of row @j@ (its wildcards filled from its source pattern)
      -- overlaps the source of row @i@. @i == j@ is a row to itself.

deriving instance Eq (BasePart b) => Eq (TransferTableError b)
deriving instance Show (BasePart b) => Show (TransferTableError b)

-- | Validate a table and keep the rows in the order written.
-- Matching is one-way: a wildcard in the pattern matches any coordinate,
-- including a wildcard in the other base. Complexity: O(r^2) for r rows.
mkTransferTable :: forall b. HatBaseClass b
                => [TransferRow b]
                -> Either (TransferTableError b) (TransferTable b)
mkTransferTable rows = case find invalidPortion rows of
    Just row -> Left (InvalidPortion row)
    Nothing -> case [(first, second)
                    | (before, second) <- zip (inits rows) rows
                    , first@(TransferRow source _ _) <- before
                    , let TransferRow other _ _ = second
                    , patternsOverlap source other] of
        (first, second) : _ -> Left (OverlappingRows first second)
        [] -> case [(first, second)
                   | (before, second@(TransferRow source target _)) <- zip (inits rows) rows
                   , first@(TransferRow previous _ _) <- before ++ [second]
                   , patternsOverlap previous (ignoreWildcard source target)] of
            (first, second) : _ -> Left (BackwardFlow first second)
            [] -> Right (TransferTable rows)
  where
    invalidPortion :: TransferRow b -> Bool
    invalidPortion (TransferRow _ _ Relabel) = False
    invalidPortion (TransferRow _ _ (Portion numerator denominator)) =
        numerator <= 0 || denominator <= 0 || numerator > denominator

-- | The rows in the order written. Complexity: O(1)
tableRows :: TransferTable b -> [TransferRow b]
tableRows (TransferTable rows) = rows

-- * Additional entries

-- | A ledger that 'transferEntries' cannot transfer. Only postings whose base
-- matches the source of some row are checked. The first failure is reported
-- in the order of the constructors; within a constructor, by ascending base
-- (Hat\/Not normalized to Not), and for 'InvalidPosting' on the same base a
-- non-finite value before a negative one. No partial result is returned.
data TransferError b
    = WildcardPosting b
      -- ^ A posting with a nonzero value and side @HatNot@.
    | InvalidPosting b ValueError
      -- ^ A non-finite or negative value.
    | UnclassifiedAccount b
      -- ^ A source or destination account without an account division, so
      -- its home side is unknown. Checked for every selected base (from the
      -- ledger or from an earlier row's destinations) and its destination,
      -- whatever the balance, so the result does not depend on the amounts.
    | UnroundableAmount (TransferRow b) b
      -- ^ The rounding rule cannot represent the moved amount, or the
      -- rounded amount is infinite. Reported for the first row in table
      -- order, then the first base.

deriving instance (Eq b, Eq (BasePart b)) => Eq (TransferError b)
deriving instance (Show b, Show (BasePart b)) => Show (TransferError b)

-- | 'transferEntriesWith' rounding 'Portion' rows to the nearest value.
transferEntries :: (HatVal v, ExBaseClass b)
                => TransferTable b -> Alg v b -> Either (TransferError b) (Alg v b)
transferEntries = transferEntriesWith Nearest

-- | Generate the additional entries of a table, rounding 'Portion' rows with
-- the given rule. The ledger is read once: per selected base, Hat and Not are
-- accumulated exactly, rows apply in table order, and each row's destinations
-- feed the balances of later rows. A base with a zero exact balance, and a
-- 'Portion' whose rounded amount is zero, add nothing. Wildcards in ledger
-- bases are values. No @bar@ is applied.
-- Complexity: O(s log b + r * b) for s postings, b selected bases, r rows.
transferEntriesWith :: forall v b. (HatVal v, ExBaseClass b)
                    => Rounding v
                    -> TransferTable b
                    -> Alg v b
                    -> Either (TransferError b) (Alg v b)
transferEntriesWith rounding (TransferTable rows) ledger =
    case Set.lookupMin wildcards of
        Just source -> Left (WildcardPosting source)
        Nothing -> case Map.lookupMin invalid of
            Just (source, failure) -> Left (InvalidPosting source failure)
            Nothing -> case process rows balances of
                (unclassified, failures, entries) -> case Set.lookupMin unclassified of
                    Just source -> Left (UnclassifiedAccount source)
                    Nothing -> case failures of
                        failure : _ -> Left failure
                        [] -> Right entries
  where
    -- Only selected input postings enter these exact, separate side totals.
    (_, wildcards, invalid, balances) =
        foldEntries collect (Map.empty, Set.empty, Map.empty, Map.empty) ledger

    collect :: (Map.Map b Bool, Set.Set b, Map.Map b ValueError, Map.Map b (Accum v, Accum v))
            -> v
            -> b
            -> (Map.Map b Bool, Set.Set b, Map.Map b ValueError, Map.Map b (Accum v, Accum v))
    collect (selection, wildcardBases, invalidBases, totals) value posting
        | not isSelected = (nextSelection, wildcardBases, invalidBases, totals)
        | otherwise = (nextSelection, nextWildcards, nextInvalid, nextTotals)
      where
        source = merge Not (base posting)
        isSelected :: Bool
        isSelected = Map.findWithDefault (any selects rows) source selection
        nextSelection :: Map.Map b Bool
        nextSelection = Map.insert source isSelected selection

        selects :: TransferRow b -> Bool
        selects (TransferRow patternBase _ _) = matches patternBase (base posting)

        nextWildcards :: Set.Set b
        nextWildcards
            | hat posting == HatNot && value /= zeroValue = Set.insert source wildcardBases
            | otherwise = wildcardBases

        nextInvalid :: Map.Map b ValueError
        nextInvalid = case valueError value of
            Nothing -> invalidBases
            Just failure -> Map.insertWith preferNonFinite source failure invalidBases

        nextTotals :: Map.Map b (Accum v, Accum v)
        nextTotals = case (hat posting, valueError value) of
            (Hat, Nothing) -> addTotal source (addAccum value emptyAccum, emptyAccum) totals
            (Not, Nothing) -> addTotal source (emptyAccum, addAccum value emptyAccum) totals
            (HatNot, Nothing) -> addTotal source (emptyAccum, emptyAccum) totals
            _ -> totals

    preferNonFinite :: ValueError -> ValueError -> ValueError
    preferNonFinite NonFiniteValue _ = NonFiniteValue
    preferNonFinite _ NonFiniteValue = NonFiniteValue
    preferNonFinite _ _ = NegativeValue

    addTotal :: b
             -> (Accum v, Accum v)
             -> Map.Map b (Accum v, Accum v)
             -> Map.Map b (Accum v, Accum v)
    addTotal = Map.insertWith mergeTotals

    mergeTotals :: (Accum v, Accum v) -> (Accum v, Accum v) -> (Accum v, Accum v)
    mergeTotals (firstHat, firstNot) (secondHat, secondNot) =
        (mergeAccum firstHat secondHat, mergeAccum firstNot secondNot)

    -- Continue after a rounding failure so an unclassified account anywhere
    -- in the table still has priority. No partial entries escape this function.
    process :: [TransferRow b]
            -> Map.Map b (Accum v, Accum v)
            -> (Set.Set b, [TransferError b], Alg v b)
    process [] _ = (Set.empty, [], Zero)
    process (row@(TransferRow patternBase targetPattern share) : remaining) totals =
        let selected :: [(b, (Accum v, Accum v))]
            selected = [(source, amounts)
                       | (source, amounts) <- Map.toAscList totals
                       , matches patternBase (base source)]
            (unclassified, failures, entries, nextTotals) =
                foldl' move (Set.empty, [], Zero, totals) selected
            (laterUnclassified, laterFailures, laterEntries) = process remaining nextTotals
        in ( Set.union unclassified laterUnclassified
           , failures ++ laterFailures
           , entries .+ laterEntries )
      where
        move :: (Set.Set b, [TransferError b], Alg v b, Map.Map b (Accum v, Accum v))
             -> (b, (Accum v, Accum v))
             -> (Set.Set b, [TransferError b], Alg v b, Map.Map b (Accum v, Accum v))
        move (unclassified, failures, entries, current) (source, (hatTotal, notTotal))
            | not (null unknown) =
                (Set.union unclassified (Set.fromList unknown), failures, entries, current)
            | otherwise = case signAccum net of
                Just EQ -> (unclassified, failures, entries, current)
                Just LT -> emit (toHat source) (negateAccum net)
                Just GT -> emit source net
                Nothing -> (unclassified, failures ++ [UnroundableAmount row source], entries,
                            current)
          where
            target :: b
            target = merge Not (ignoreWildcard (base source) targetPattern)
            unknown :: [b]
            unknown = [postingBase | postingBase <- [source, target]
                                  , isNothing (accountSpec (getAccountTitle postingBase))]
            net :: Accum v
            net = mergeAccum notTotal (negateAccum hatTotal)

            emit :: b
                 -> Accum v
                 -> (Set.Set b, [TransferError b], Alg v b, Map.Map b (Accum v, Accum v))
            emit balanceBase amount = case movedValues of
                Nothing -> (unclassified, failures ++ [UnroundableAmount row source], entries,
                            current)
                Just values ->
                    let additions :: Alg v b
                        additions = mconcat [ (value .@ revHat balanceBase)
                                          .+ (value .@ destination)
                                           | value <- values, value > zeroValue ]
                        next :: Map.Map b (Accum v, Accum v)
                        next = foldl' feed current values
                    in (unclassified, failures, entries .+ additions, next)
              where
                destination :: b
                destination
                    | whichSide source == whichSide target = merge (hat balanceBase) (base target)
                    | otherwise = merge (hat (revHat balanceBase)) (base target)

                movedValues :: Maybe [v]
                movedValues = case share of
                    Relabel -> splitAccum amount
                    Portion numerator denominator ->
                        fmap (: []) (roundScaled rounding (numerator % denominator) amount)

                feed :: Map.Map b (Accum v, Accum v) -> v -> Map.Map b (Accum v, Accum v)
                feed accumulated value
                    | value == zeroValue || not (any selectsDestination remaining) = accumulated
                    | hat destination == Hat =
                        addTotal target (addAccum value emptyAccum, emptyAccum) accumulated
                    | otherwise =
                        addTotal target (emptyAccum, addAccum value emptyAccum) accumulated

                selectsDestination :: TransferRow b -> Bool
                selectsDestination (TransferRow nextPattern _ _) =
                    matches nextPattern (base destination)

-- * Journals

-- | A journal that 'transferEntriesBy' cannot transfer.
data TransferJournalError n b
    = TransferFailed n (TransferError b)
      -- ^ The first failing target note in ascending order.
    | TransferNoteMoves n n
      -- ^ @(m, f m)@: an input note @m@ is a target but @f m /= m@. Checked
      -- for all notes, ascending, before any transfer is computed.

deriving instance (Eq n, Eq b, Eq (BasePart b)) => Eq (TransferJournalError n b)
deriving instance (Show n, Show b, Show (BasePart b)) => Show (TransferJournalError n b)

-- | 'transferEntriesByWith' rounding to the nearest value.
transferEntriesBy :: (Note n, HatVal v, ExBaseClass b)
                  => (n -> n)
                  -> TransferTable b
                  -> Journal n v b
                  -> Either (TransferJournalError n b) (Journal n v b)
transferEntriesBy = transferEntriesByWith Nearest

-- | Transfer per target note. @f@ maps each input note to the note that holds
-- its transfer entries. The notes with the same target are combined, the
-- table is applied once to their combined ledger (rounding once per target),
-- and the entries are placed under the target. Only the additional entries
-- are returned; add them with @j .+ r@. A target with no entries creates no
-- note. See the module header for reading balances across notes.
transferEntriesByWith :: forall n v b. (Note n, HatVal v, ExBaseClass b)
                      => Rounding v
                      -> (n -> n)
                      -> TransferTable b
                      -> Journal n v b
                      -> Either (TransferJournalError n b) (Journal n v b)
transferEntriesByWith rounding targetOf table journal = case movingNotes of
    (note, target) : _ -> Left (TransferNoteMoves note target)
    [] -> fmap mconcat $ traverse transferGroup (Map.toAscList groups)
  where
    notes :: [(n, Alg v b)]
    notes = HashMap.toList (Journal.toMap journal)
    targets :: Set.Set n
    targets = Set.fromList [targetOf note | (note, _) <- notes]
    movingNotes :: [(n, n)]
    movingNotes = [(note, targetOf note)
                  | note <- Set.toAscList (Set.fromList (map fst notes))
                  , Set.member note targets
                  , targetOf note /= note]
    groups :: Map.Map n (Journal n v b)
    groups = Map.fromListWith (.+)
        [(targetOf note, entries Journal..| note) | (note, entries) <- notes]

    transferGroup :: (n, Journal n v b) -> Either (TransferJournalError n b) (Journal n v b)
    transferGroup (target, group) = case transferEntriesWith rounding table (Journal.toAlg group) of
        Left failure -> Left (TransferFailed target failure)
        Right additions -> Right (additions Journal..| target)
