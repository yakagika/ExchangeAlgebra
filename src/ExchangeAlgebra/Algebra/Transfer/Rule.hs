{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : ExchangeAlgebra.Algebra.Transfer.Rule
Description : Data-defined transfer rules and the entries they generate.

Definition 9 describes a transfer by adding source cancellations and target
postings to the original algebra. For @Right entries = transferEntries rules a@,
@a .+ entries@ is precisely that expression. This module returns only the
additional entries, preserving the original audit trail. Only
'collapseNetEntries' applies @bar@ to rewritten entries. 'closingEntries'
uses sequential side totals from the source entries, while 'settleEntries'
accepts signed nets already computed by its caller.

== Laws

The observation @obs@ is the map from complete bases (including Hat\/Not) to
summed values after @bar@; it does not observe the singleton versus composite
constructor or sequence order. Relative tolerance means a per-base difference
of at most @1e-9 * max (abs x) (abs y)@, treating absent bases as zero.

=== L1: compatibility

* Subject: 'transferEntries' and legacy @transfer@.
* Preconditions: P1, all source patterns have the same wildcard positions;
  P2, patterns are disjoint; P3, ledger bases contain no wildcards; P4, axes
  are not nested tuples; P5, transformed values are nonzero. The legacy table
  translates 'Relabel', 'MulBy' and 'DivBy' to @id@, @(* p)@ and @(/ p)@,
  respectively, and applying the new rules returns @Right entries@. P3 is
  needed only for equivalence with legacy @transfer@, whose matching is
  symmetric. 'transferEntries' matches one way and treats ledger wildcards
  as values.
* Relation: @obs (a .+ entries) ~= obs (transfer a table)@.
* Observation: @obs@ as defined above, including every target base.
* Tolerance: relative @1e-9@; tested values are integers in @1..1000000@,
  and coefficients are @2@, @3@, @0.5@ and @4@.
* Instances: 'Double' and @MoneyDecimal@ with flat @HatBase@ tuples.

=== L2: closing

* Subject: 'closingEntries' and legacy @finalStockTransfer@.
* Preconditions: concrete ledger bases, valid Hat\/Not postings and values
  in the L1 range; the ledger includes only entries up to the closing date.
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
* Preconditions: valid postings, concrete accounts, 'Relabel' rules whose
  actual target and source have equal 'whichSide'; finite totals. The
  transfer or closing operation being observed returns @Right entries@.
* Relation: debit total equals credit total. Closing entries also satisfy
  this relation, because 'closingSide' follows the account's PIMO direction.
* Observation: @norm (decL entries)@ and @norm (decR entries)@.
* Tolerance: relative @1e-9@ for 'Double'; exact for @MoneyDecimal@.
* Instances: 'Double' and @MoneyDecimal@ with 'ExBaseClass' bases.

A relabel from @Not :< Cash@ to @Not :< Sales@ violates L3's side
precondition: its cancellation and destination are both credits.

=== L4: canonical form

* Subject: 'mkTransferRules'.
* Preconditions: construction succeeds; the second input permutes the first.
* Relation: both constructions return equal rule sets.
* Observation: 'Eq' of t'TransferRules', or 'rulesToList'.
* Tolerance: exact equality. Errors (including NaN) are outside this law.
* Instances: all lawful 'HatVal' and 'HatBaseClass' instances.

=== L5: non-negativity

* Subject: generated transfer and closing entries.
* Preconditions: non-negative valid input values; the transfer or closing
  operation being observed returns @Right entries@.
* Relation: every generated value is greater than or equal to zero.
* Observation: posting values, before any @bar@.
* Tolerance: none.
* Instances: all lawful 'HatVal' and 'HatBaseClass' instances (closing also
  requires 'ExBaseClass'). Hat reversal is never numeric negation.

=== L6: coordinate collapse

* Subject: 'collapseEntries' and 'collapseNetEntries'.
* Preconditions: non-negative valid postings and an exact additive value type.
* Relation: @bar (x .+ collapseEntries p f x) ==
  bar (x .+ collapseNetEntries p f x)@. The raw form has twice as many
  postings as @proj p x@ and twice its norm; the net form calls 'bar' after
  rewriting the base parts.
* Observation: net ledger and raw posting count and norm.
* Tolerance: exact.
* Instances: @MoneyDecimal@ with 'HatBaseClass' bases.
-}
module ExchangeAlgebra.Algebra.Transfer.Rule
    ( TransferScale(..)
    , TransferRule(..)
    , TransferRules
    , TransferRuleError(..)
    , TransferApplyError(..)
    , mkTransferRules
    , rulesToList
    , relabel
    , scaleBy
    , divideBy
      -- * Additional entries
    , transferEntries
    , collapseEntries
    , collapseNetEntries
      -- * Closing entries
    , ClosingSide(..)
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

import           Data.Binary (Binary(..))
import           Data.Hashable (Hashable)
import           Data.List (find, sortOn, tails)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           GHC.Generics (Generic)
import           ExchangeAlgebra.Algebra
                     ( Alg(..), HatVal(..), HatBaseClass(..), ExBaseClass(..)
                     , Hat(..), HatBase(..), CountUnit(..), Element(..)
                     , AccountTitles(..), Redundant((.+), (.^), bar, norm)
                     , (.@)
                     , ignoreWildcard, foldEntries, mapBasePart, proj
                     , postFromNetBy, vals
                     , accountSpec, asClosing, ClosingRule(..)
                     , classifyAccountDivision, classifyAccountContra
                     , pimoFromDivision, pimoFlip, PIMO(..) )
import ExchangeAlgebra.Algebra.Transfer.Closing (closingPairBy)

-- | How a rule changes the value it moves.
data TransferScale v
    = Relabel  -- ^ Keep the value.
    | MulBy v  -- ^ Multiply by a positive finite coefficient.
    | DivBy v  -- ^ Divide by a positive finite coefficient.
    deriving (Eq, Ord, Show, Generic)

instance Binary v => Binary (TransferScale v)

instance Hashable v => Hashable (TransferScale v)

-- | A source pattern, target template and value operation.
data TransferRule v b = TransferRule
    { ruleFrom  :: b                -- ^ One-way matching pattern.
    , ruleTo    :: b                -- ^ Wildcards retain source coordinates.
    , ruleScale :: TransferScale v  -- ^ Value operation, validated at construction.
    } deriving (Eq, Ord, Show, Generic)

instance (Binary v, Binary b) => Binary (TransferRule v b)

instance (Hashable v, Hashable b) => Hashable (TransferRule v b)

-- | Validated, pairwise-disjoint rules, sorted by source pattern.
-- No 'Semigroup' instance: combine 'rulesToList' values and validate again.
-- Serialize the list of rules and use 'mkTransferRules' after decoding;
-- decoding revalidates the list to preserve this type's invariant.
newtype TransferRules v b = TransferRules [TransferRule v b]
    deriving (Eq, Show, Generic)

instance (HatVal v, HatBaseClass b, Binary v, Binary b) => Binary (TransferRules v b) where
    put = put . rulesToList
    get = do
        rules <- get
        case mkTransferRules rules of
            Left failure    -> fail (show failure)
            Right validated -> pure validated

instance (Hashable v, Hashable b) => Hashable (TransferRules v b)

-- | A construction failure. The first invalid coefficient in input order
-- precedes overlap checks; overlaps report the first pair @(i,j)@, @i < j@.
data TransferRuleError v b
    = OverlappingRules (TransferRule v b) (TransferRule v b) -- ^ Including duplicates.
    | InvalidCoefficient (TransferRule v b)                -- ^ Zero, negative or non-finite.
    deriving (Eq, Show)

-- | A value operation produced a non-finite (or otherwise invalid) value.
data TransferApplyError v b
    = NonFiniteResult (TransferRule v b) v b -- ^ Rule, source value, source base.
    | NonFiniteBalance b                   -- ^ Closing base, normalized to Not.
    deriving (Show)

-- | Validate coefficients, reject all overlapping source patterns, and sort.
-- Matching is one-way: a wildcard in the ledger is a value, not a pattern.
-- Overlap uses wildcard substitution recursively, including nested tuples;
-- symmetric wildcard equality is insufficient. Complexity: O(r^2).
mkTransferRules :: (HatVal v, HatBaseClass b)
                => [TransferRule v b]
                -> Either (TransferRuleError v b) (TransferRules v b)
mkTransferRules rules = case find invalidCoefficient rules of
    Just rule -> Left (InvalidCoefficient rule)
    Nothing -> case find overlaps pairs of
        Just (first, second) -> Left (OverlappingRules first second)
        Nothing -> Right (TransferRules (sortOn ruleFrom rules))
  where
    invalidCoefficient rule = case ruleScale rule of
        Relabel -> False
        MulBy coefficient -> invalid coefficient
        DivBy coefficient -> invalid coefficient
    invalid coefficient = coefficient <= zeroValue || isErrorValue coefficient
    pairs = [(first, second) | first : rest <- tails rules, second <- rest]
    overlaps (first, second) =
        matches (ruleFrom first)
                (ignoreWildcard (ruleFrom first) (ruleFrom second))

-- | Extract the canonical list, sorted by 'ruleFrom'.
rulesToList :: TransferRules v b -> [TransferRule v b]
rulesToList (TransferRules rules) = rules

-- | Construct a value-preserving rule; validate with 'mkTransferRules'.
relabel :: b -> b -> TransferRule v b
relabel source target = TransferRule source target Relabel

-- | Construct a multiplication rule; the coefficient must be positive and finite.
scaleBy :: b -> b -> v -> TransferRule v b
scaleBy source target coefficient = TransferRule source target (MulBy coefficient)

-- | Construct a division rule. Division is not reciprocal multiplication:
-- the latter need not round the same way for floating-point values.
divideBy :: b -> b -> v -> TransferRule v b
divideBy source target coefficient = TransferRule source target (DivBy coefficient)

-- | Match only the pattern's wildcards, preserving literal ledger wildcards.
matches :: HatBaseClass b => b -> b -> Bool
matches patternBase entry = ignoreWildcard entry patternBase == entry

-- * Additional entries

-- | Generate cancellation and destination entries, without the input ledger.
-- Input values must satisfy the ordinary non-negative, finite posting contract.
-- 'Relabel'-only rules cannot fail. Scaled overflow returns 'NonFiniteResult',
-- and no partial result is returned. Source HatNot postings are unmatched.
-- A relabel to the identical base generates nothing; a zero scaled value
-- generates only the cancellation. No implicit @bar@ or legacy one-to-one
-- map is used. Complexity: O(s*r), with linear rule lookup per posting.
transferEntries :: (HatVal v, HatBaseClass b)
                => TransferRules v b
                -> Alg v b
                -> Either (TransferApplyError v b) (Alg v b)
transferEntries (TransferRules rules) = foldEntries step (Right Zero)
  where
    step result value source = do
        entries <- result
        additions <- apply value source
        pure (entries .+ additions)
    apply value source = case hat source of
        HatNot -> Right Zero
        _ -> case find (\rule -> matches (ruleFrom rule) source) rules of
            Nothing -> Right Zero
            Just rule -> generate rule value source
    generate rule value source
        | ruleScale rule == Relabel && target == source = Right Zero
        | isErrorValue moved = Left (NonFiniteResult rule value source)
        | isZeroValue moved = Right cancellation
        | otherwise = Right (cancellation .+ (moved :@ target))
      where
        target = ignoreWildcard source (ruleTo rule)
        moved = case ruleScale rule of
            Relabel -> value
            MulBy coefficient -> value * coefficient
            DivBy coefficient -> value / coefficient
        cancellation = value :@ revHat source

-- | Move selected entries to new base coordinates while retaining every posting.
-- Query patterns use one-way matching: only a pattern wildcard matches any
-- coordinate. The function rewrites each selected 'BasePart' with the supplied
-- function, so callers can replace an axis with its wildcard. A transfer
-- rule's target wildcard instead keeps the source coordinate; it cannot turn
-- a concrete coordinate into a wildcard.
--
-- The result contains only the added entries: a Hat-reversed copy of each
-- selected posting and its rewritten copy. Add it to the ledger with @(.+)@.
-- Values remain non-negative, and this function does not call 'bar', so it
-- retains redundant audit detail. 'collapseNetEntries' nets the rewritten
-- entries instead. 'postFromNetBy' generates new postings for each netted
-- classification; both collapse functions move the coordinates of the same
-- entries.
-- On an axis-preserving ledger, @norm . bar@ cannot cancel across axes.
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> x = 10 .@ Not :< Yen .+ 4 .@ Hat :< Dollar :: T
-- >>> let moved = collapseEntries [HatNot :< wildcard] (const wildcard) x
-- >>> norm moved
-- 28.0
-- >>> length (vals moved)
-- 4
collapseEntries :: (HatVal v, HatBaseClass b)
                => [b] -> (BasePart b -> BasePart b) -> Alg v b -> Alg v b
collapseEntries pats f x = (.^) selected .+ mapBasePart f selected
  where
    selected = proj pats x

-- | Move selected entries to new base coordinates and net the rewritten side.
-- The result contains only added entries: a Hat-reversed copy of the selected
-- postings plus @bar (mapBasePart f selected)@. This function calls 'bar'
-- internally after rewriting, so opposite sides from distinct original axes
-- can cancel when the new base parts coincide. Add the result to the original
-- ledger with @(.+)@. It leaves the original audit entries in place.
--
-- Query wildcards match one way. A wildcard in a transfer rule's target
-- preserves the source coordinate; use this function to replace a concrete
-- coordinate with a wildcard. 'collapseEntries' retains all rewritten
-- postings. 'postFromNetBy' generates new postings for each netted
-- classification, while this function moves the coordinates of the same
-- entries. On an axis-preserving ledger, @norm . bar@ does not cancel across
-- axes.
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> x = 10 .@ Not :< Yen .+ 4 .@ Hat :< Dollar :: T
-- >>> norm (bar (x .+ collapseNetEntries [HatNot :< wildcard] (const wildcard) x))
-- 6.0
collapseNetEntries :: (HatVal v, HatBaseClass b)
                   => [b] -> (BasePart b -> BasePart b) -> Alg v b -> Alg v b
collapseNetEntries pats f x = (.^) selected .+ bar (mapBasePart f selected)
  where
    selected = proj pats x

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
-- 'ExchangeAlgebra.Posting.postedUpperBound' are accepted, without passing
-- through 'ExchangeAlgebra.Posting.posted'. No implicit @bar@ or @compress@ is
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
