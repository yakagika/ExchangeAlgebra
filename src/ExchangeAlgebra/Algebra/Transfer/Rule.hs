{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : ExchangeAlgebra.Algebra.Transfer.Rule
Description : Data-defined transfer rules and the entries they generate.

Definition 9 describes a transfer by adding source cancellations and target
postings to the original algebra. For @Right entries = transferEntries rules a@,
@a .+ entries@ is precisely that expression. This module returns only the
additional entries, preserving the original audit trail. It never applies
@bar@. 'closingEntries' explicitly nets each closing account first.

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
  respectively, and applying the new rules returns @Right entries@.
* Relation: @obs (a .+ entries) ~= obs (transfer a table)@.
* Observation: @obs@ as defined above, including every target base.
* Tolerance: relative @1e-9@; tested values are integers in @1..1000000@,
  and coefficients are @2@, @3@, @0.5@ and @4@.
* Instances: 'Double' and @MoneyDecimal@ with flat @HatBase@ tuples.

=== L2: closing

* Subject: 'closingEntries' and legacy @finalStockTransfer@.
* Preconditions: concrete ledger bases, valid Hat\/Not postings and values
  in the L1 range; the ledger includes only entries up to the closing date.
  Closing returns @Right entries@.
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
    , transferEntries
    , ClosingSide(..)
    , closingSide
    , closingEntries
    ) where

import           Data.Binary (Binary(..))
import           Data.Hashable (Hashable)
import           Data.List (find, sortOn, tails)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           ExchangeAlgebra.Algebra
                     ( Alg(..), HatVal(..), HatBaseClass(..), ExBaseClass(..)
                     , Hat(..), AccountTitles(..), Redundant((.+))
                     , ignoreWildcard, foldEntries
                     , accountSpec, asClosing, ClosingRule(..)
                     , classifyAccountDivision, classifyAccountContra
                     , pimoFromDivision, pimoFlip, PIMO(..) )

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

-- | Generate closing entries from each eligible base's exact net balance.
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
        | otherwise = Right (closingPair value balanceBase)
    closingPair value source = case closingSide (getAccountTitle source) of
        Nothing -> Zero
        Just side ->
            let targetSource = case side of
                    ClosingKeep -> source
                    ClosingFlip -> revHat source
            in (value :@ revHat source)
                .+ (value :@ setAccountTitle targetSource RetainedEarnings)
