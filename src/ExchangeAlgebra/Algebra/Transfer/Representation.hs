{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : ExchangeAlgebra.Algebra.Transfer.Representation
Description : Generic transfer rules and additional entries.

This Algebra layer owns transfer rules, their validation, and base
collapse. It uses generic bases and values and supplies the public
"ExchangeAlgebra.Algebra.Transfer" entry point.
Start with 'mkTransferRules', then 'transferEntries' or the collapse functions.
Definition 9 describes a transfer by adding source cancellations and target
postings: for @Right entries = transferEntries rules ledger@, use
@ledger .+ entries@. Only 'collapseNetEntries' applies @bar@ to rewritten entries.

== Laws

The observation @obs@ is the map from complete bases (including Hat\/Not) to
summed values after @bar@; it does not observe the singleton versus composite
constructor or sequence order. Relative tolerance means a per-base difference
of at most @1e-9 * max (abs x) (abs y)@, treating absent bases as zero.

=== L1: base rewriting

* Subject: 'transferEntries'.
* Preconditions: disjoint source patterns, valid non-negative postings and
  a successful application returning @Right entries@. A source wildcard
  matches any stored element; a concrete source matches only itself.
* Relation: @obs (a .+ entries)@ equals the observation of independently
  moving each matched term to its rule's target with its original value;
  unmatched postings remain unchanged.
* Observation: @obs@ as defined above, including every target base.
* Tolerance: relative @1e-9@; tested values are integers in @1..1000000@.
* Instances: 'Double' and @MoneyDecimal@ with flat @HatBase@ tuples.

=== L4: canonical form

* Subject: 'mkTransferRules'.
* Preconditions: construction succeeds; the second input permutes the first.
* Relation: both constructions return equal rule sets.
* Observation: 'Eq' of t'TransferRules', or 'rulesToList'.
* Tolerance: exact equality. Errors (including NaN) are outside this law.
* Instances: all lawful 'HatVal' and 'HatBaseClass' instances.

=== L6: base collapse

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
module ExchangeAlgebra.Algebra.Transfer.Representation ( TransferScale(..)
                                                       , TransferRule(..)
                                                       , TransferRules
                                                       , TransferRuleError(..)
                                                       , TransferApplyError(..)
                                                       , mkTransferRules
                                                       , rulesToList
                                                       , transferEntries
                                                       , collapseEntries
                                                       , collapseNetEntries
                                                         -- * Shared with the accounting transfer
                                                       , matches
                                                       , patternsOverlap
                                                       ) where

import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Hashable (Hashable(..))
import Data.List (find, sortOn, tails)
import GHC.Generics (Generic)
import ExchangeAlgebra.Algebra.Core.Representation (Alg(Single))
import ExchangeAlgebra.Algebra.Core ( Alg(..)
                                    , Redundant((.+), (.^), bar, norm)
                                    , (.@)
                                    , foldEntries
                                    , mapBasePart
                                    , proj
                                    , postFromNetBy
                                    , vals
                                    )
import ExchangeAlgebra.Algebra.Base.Representation (HatBaseClass(..), Hat(..), HatBase(..))
import ExchangeAlgebra.Algebra.Element (CountUnit(..), Element(..), ignoreWildcard)
import ExchangeAlgebra.Algebra.Value.Class (HatVal(..), isErrorValue)

-- | How a rule changes the value it moves.
data TransferScale v
    = Relabel  -- ^ Keep the value.
    deriving (Eq, Ord, Show, Generic)

-- Keep the original wire tag for the supported operation.
instance Binary v => Binary (TransferScale v) where
    put Relabel = putWord8 0
    get = do
        tag <- getWord8
        case tag of
            0 -> pure Relabel
            _ -> fail "TransferScale: unknown or removed operation"

-- Preserve the hash inputs for the retained operation in the old sum type.
instance Hashable v => Hashable (TransferScale v) where
    hashWithSalt salt Relabel = hashWithSalt (hashWithSalt salt (0 :: Int)) (0 :: Int)

-- | A source pattern, target template and value operation.
data TransferRule v b = TransferRule
    { ruleFrom  :: b                -- ^ One-way matching pattern.
    , ruleTo    :: b                -- ^ Wildcards retain source elements.
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

-- | A construction failure. Overlaps report the first pair @(i,j)@, @i < j@.
data TransferRuleError v b
    = OverlappingRules (TransferRule v b) (TransferRule v b) -- ^ Including duplicates.
    deriving (Eq, Show)

-- | A value operation produced a non-finite (or otherwise invalid) value.
data TransferApplyError v b
    = NonFiniteResult (TransferRule v b) v b -- ^ Rule, source value, source base.
    | NonFiniteBalance b                   -- ^ Closing base, normalized to Not.
    deriving (Show)

-- | Reject all overlapping source patterns and sort.
-- Matching is one-way: a wildcard in the ledger is a value, not a pattern.
-- Overlap uses wildcard substitution recursively, including nested tuples;
-- symmetric wildcard equality is insufficient. Complexity: O(r^2).
mkTransferRules :: (HatVal v, HatBaseClass b)
                => [TransferRule v b]
                -> Either (TransferRuleError v b) (TransferRules v b)
mkTransferRules rules = case find overlaps pairs of
    Just (first, second) -> Left (OverlappingRules first second)
    Nothing -> Right (TransferRules (sortOn ruleFrom rules))
  where
    pairs = [(first, second) | first : rest <- tails rules, second <- rest]
    overlaps (first, second) =
        patternsOverlap (ruleFrom first) (ruleFrom second)

-- | Extract the canonical list, sorted by 'ruleFrom'.
rulesToList :: TransferRules v b -> [TransferRule v b]
rulesToList (TransferRules rules) = rules

-- | Match only the pattern's wildcards, preserving literal ledger wildcards.
-- Also used by "ExchangeAlgebra.Accounting.Transfer" on 'BasePart'.
matches :: Element b => b -> b -> Bool
matches patternBase entry = ignoreWildcard entry patternBase == entry

-- | Whether some base matches both patterns, with wildcard substitution at each component position.
patternsOverlap :: Element b => b -> b -> Bool
patternsOverlap first second = matches first (ignoreWildcard first second)

-- * Additional entries

-- | Generate cancellation and destination entries, without the input ledger.
-- Input values must satisfy the ordinary non-negative, finite posting contract.
-- Source 'HatNot' terms are unmatched. A rule with an identical
-- source and destination generates no entries. The result retains all
-- generated terms. Complexity: O(s*r), with linear rule lookup per term.
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
        | target == source = Right Zero
        | isErrorValue value = Left (NonFiniteResult rule value source)
        | isZeroValue value = Right cancellation
        | otherwise = Right (cancellation .+ (value `Single` target))
      where
        target = ignoreWildcard source (ruleTo rule)
        cancellation = value `Single` revHat source

-- | Move selected entries to new bases while retaining every posting.
-- Query patterns use one-way matching: only a pattern wildcard matches any
-- element. The function rewrites each selected 'BasePart' with the supplied
-- function, so callers can replace a component with its wildcard. A transfer
-- rule's target wildcard instead keeps the source element; it cannot turn
-- a concrete element into a wildcard.
--
-- The result contains only the added entries: a Hat-reversed copy of each
-- selected posting and its rewritten copy. Add it to the ledger with @(.+)@.
-- Values remain non-negative, and this function does not call 'bar', so it
-- retains redundant audit detail. 'collapseNetEntries' nets the rewritten
-- entries instead. 'postFromNetBy' generates new postings for each netted
-- classification; both collapse functions rewrite the bases of the same
-- entries.
-- On a ledger that retains complete bases, @norm . bar@ cannot cancel across different bases.
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

-- | Move selected entries to new bases and net the rewritten side.
-- The result contains only added entries: a Hat-reversed copy of the selected
-- postings plus @bar (mapBasePart f selected)@. This function calls 'bar'
-- internally after rewriting, so opposite sides from different original bases
-- can cancel when the new base parts coincide. Add the result to the original
-- ledger with @(.+)@. It leaves the original audit entries in place.
--
-- Query wildcards match one way. A wildcard in a transfer rule's target
-- preserves the source element; use this function to replace a concrete
-- element with a wildcard. 'collapseEntries' retains all rewritten
-- postings. 'postFromNetBy' generates new postings for each netted
-- classification, while this function rewrites the bases of the same
-- entries. On a ledger that retains complete bases, @norm . bar@ cancels
-- only among terms with matching base parts.
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
