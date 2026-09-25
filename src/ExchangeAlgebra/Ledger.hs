{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Index checked postings by component while retaining their journal as the
-- source of truth. This layer builds on "ExchangeAlgebra.Ledger.Posting" and
-- "ExchangeAlgebra.Journal" for evaluators that read one component at a time.
-- Define a 'Partition', start with 'emptyLedger', and append with 'post'.
-- Read balances and flows through the indexes, or inspect 'journal' for the
-- original entries and notes.
--
-- == Numeric contract and laws
--
-- Indexes use sequential 'Double' difference updates, a separate layer from
-- exact journal readouts. For each base, the E1 absolute error bound is
-- @n * u * sum (abs x)@, where @u = 2 ** (-53)@ and @n@ counts the nonzero
-- scalar entries for that base. The sums and exact readouts must remain finite.
-- Updates follow posting order and, within each posting, 'foldEntries' order.
-- No cancellation or compression is implicit. List readouts sort their keys;
-- callers folding a 'component' or 'sidesIn' map must likewise sort its keys
-- when floating-point reproducibility matters.
--
-- These laws apply to every lawful 'Partition' and 'Note' instance, without
-- carryover. Let @b(p, beta)@ be the Not-minus-Hat amount of base @beta@ in @p@.
--
-- * IX-1: @netAt (post note p l) beta == netAt l beta + Signed (b(p, beta))@
--   holds exactly for integer inputs with total absolute magnitude below @2^53@.
-- * IX-2: 'netAt' differs by at most E1 from
--   @ExchangeAlgebra.Journal.Exact.balanceMapByExact Just (journal l)@,
--   converting @GT@ to a positive magnitude, @LT@ to a negative magnitude,
--   and @EQ@ to zero.
-- * IX-3: components are disjoint, cover all indexed bases, and agree with
--   'netAt' exactly, including retained zero balances.
-- * IX-4: 'flowIn' is the sequential sum for its note and side since the last
--   'clearFlows'. Clearing flows empties that index and preserves all other
--   indexes and the journal exactly.
-- * IX-8b: 'queryIn' agrees within E1 with the exact balance map of the same
--   component's journal projected by @projWithBase [merge HatNot (base q)]@.
-- * IX-10: 'componentsOf' lists exactly the components ever posted to in its
--   group, in ascending order. Cancellation never removes a component.
--
-- 'journal' preserves the multiset of all posted entries under each note;
-- compare each note's algebra with @toASCList@, rather than structural equality.
module ExchangeAlgebra.Ledger ( -- * Partition and ledger
                             Partition(..)
                             , Ledger
                             , emptyLedger
                             -- * Indexed readouts
                             , netAt
                             , component
                             , componentsOf
                             , queryIn
                             , sidesIn
                             , flowIn
                             -- * Journal and updates
                             , journal
                             , clearFlows
                             , post
                             ) where

import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))

import ExchangeAlgebra.Algebra (foldEntries)
import ExchangeAlgebra.Algebra.Base (Element(ignoreWildcard), Hat(..), HatBaseClass(..))
import ExchangeAlgebra.Journal (Journal, Note, (.|))
import ExchangeAlgebra.Ledger.Posting (PostSide(..), Posting, Signed(..))
import qualified ExchangeAlgebra.Ledger.Posting as Posting

-- * Partition and ledger

-- | Choose a component from base coordinates and a group from a component.
-- The type fixes both mappings; no mapping function is stored in a ledger.
--
-- Invariant: each base has one stable component and each component has one
-- stable group. Equal inputs must produce equal keys, with lawful 'Eq', 'Ord',
-- and 'Hashable' instances. These requirements give the disjoint partition and
-- group membership laws IX-3 and IX-10. Complexity claims assume constant-cost
-- mappings, hashing, comparisons, and a fixed number of base axes.
class ( HatBaseClass b
      , Hashable (BasePart b), Ord (BasePart b)
      , Hashable (PartKey b), Ord (PartKey b)
      , Hashable (Group b), Ord (Group b)
      ) => Partition b where
    -- | Key identifying one component.
    type PartKey b
    -- | Select the unique component for these coordinates.
    partKey :: Proxy b -> BasePart b -> PartKey b
    -- | Key identifying a group of components.
    type Group b
    -- | Select the unique group for this component.
    groupOf :: Proxy b -> PartKey b -> Group b

-- | A journal and its component, side, flow, and membership indexes.
--
-- Invariant: only 'post' adds entries and updates all indexes together.
-- 'clearFlows' resets only the flow index. Zero-valued input entries are absent;
-- indexed bases and components remain after their balances reach zero.
data Ledger n b = Ledger
    { ledgerJournal :: !(Journal n Double b)
    , netIndex      :: !(HashMap (PartKey b) (HashMap (BasePart b) Double))
    , sideIndex     :: !(HashMap (PartKey b) (HashMap (BasePart b) (Double, Double)))
    , flowIndex     :: !(HashMap n
                            (HashMap (PartKey b) (HashMap PostSide (HashMap (BasePart b) Double))))
    , groupIndex    :: !(HashMap (Group b) (HashMap (PartKey b) ()))
    }

-- | Force every index, including keys and both side totals, and the journal
-- through its existing 'NFData' instance. Derived journal caches and reserved
-- algebra fields are not forced. Complexity: linear in the stored structures.
instance ( NFData n
         , NFData (BasePart b)
         , NFData (PartKey b)
         , NFData (Group b)
         ) => NFData (Ledger n b) where
    rnf (Ledger recorded nets sides flows groups) =
        rnf recorded `seq` rnf nets `seq` rnf sides `seq` rnf flows `seq` rnf groups

-- | Construct an empty journal with empty indexes. Every 'netAt' readout is
-- zero and every collection readout is empty. Complexity: O(1).
emptyLedger :: (Note n, Partition b) => Ledger n b
emptyLedger = Ledger mempty HashMap.empty HashMap.empty HashMap.empty HashMap.empty

-- * Indexed readouts

-- | Read Not minus Hat for one base, returning zero if it has never been posted.
-- Complexity: expected O(1), using the base's component and base key.
netAt :: forall n b. (Note n, Partition b) => Ledger n b -> BasePart b -> Signed
netAt ledger coordinates = Signed $
    HashMap.lookupDefault 0 coordinates
        (HashMap.lookupDefault HashMap.empty key (netIndex ledger))
  where
    key = partKey (Proxy :: Proxy b) coordinates

-- | Read all indexed balances of a component, including zero balances.
-- An absent component returns an empty map. Complexity: expected O(1) lookup
-- plus O(m) to wrap the m balances in 'Signed'. Map order is unspecified.
component :: (Note n, Partition b) => Ledger n b -> PartKey b -> HashMap (BasePart b) Signed
component ledger key = HashMap.map Signed $
    HashMap.lookupDefault HashMap.empty key (netIndex ledger)

-- | List the components ever posted to in a group, in ascending key order.
-- Zero-only postings introduce no components. An absent group returns @[]@.
-- Complexity: expected O(1) lookup plus O(c log c) for c component keys.
componentsOf :: (Note n, Partition b) => Ledger n b -> Group b -> [PartKey b]
componentsOf ledger group = sort $ HashMap.keys $
    HashMap.lookupDefault HashMap.empty group (groupIndex ledger)

-- | Select matching keys with the same one-way predicate as algebra projection.
-- Only query wildcards are patterns; stored wildcards are ordinary values.
-- Complexity: O(m + r log r) for m stored keys and r matches.
selectAscending :: Element a => a -> HashMap a value -> [(a, value)]
selectAscending query = sortOn fst . filter matches . HashMap.toList
  where
    matches (stored, _) = ignoreWildcard stored query == stored

-- | Resolve a query within one component's net index, in ascending base order.
-- Only wildcards in @base q@ match arbitrary coordinates; stored wildcards are
-- literal values. The hat of @q@ is ignored because net amounts combine sides.
-- Missing components and queries with no matches return @[]@; matched zero
-- balances remain in the result.
-- Complexity: O(m + r log r), where m is the component's base count and r is
-- the number of matches. The journal and other components are not scanned.
queryIn :: (Note n, Partition b) => Ledger n b -> PartKey b -> b -> [(BasePart b, Signed)]
queryIn ledger key query =
    [(coordinates, Signed value) | (coordinates, value) <- selectAscending (base query) totals]
  where
    totals = HashMap.lookupDefault HashMap.empty key (netIndex ledger)

-- | Read the sequential (Not, Hat) totals of current journal entries in one
-- component. An absent component returns an empty map. Complexity: expected
-- O(1) lookup; consuming the result takes O(m) for m bases. Map order is unspecified.
sidesIn :: (Note n, Partition b)
        => Ledger n b -> PartKey b -> HashMap (BasePart b) (Double, Double)
sidesIn ledger key = HashMap.lookupDefault HashMap.empty key (sideIndex ledger)

-- | Read a note's flow on one side since the last 'clearFlows', restricted to
-- one component and sorted by base. Matching is one-way as in 'queryIn'; the
-- query's hat is ignored and the 'PostSide' argument selects the side.
-- An absent note, component, side, or match returns @[]@.
-- Complexity: O(m + r log r), where m is the component's base count in this
-- note-side index and r is the number of matches. No journal scan is performed.
flowIn :: (Note n, Partition b)
       => Ledger n b
       -> n
       -> PartKey b
       -> PostSide
       -> b
       -> [(BasePart b, Double)]
flowIn ledger note key side query = selectAscending (base query) totals
  where
    byComponent = HashMap.lookupDefault HashMap.empty note (flowIndex ledger)
    bySide      = HashMap.lookupDefault HashMap.empty key byComponent
    totals      = HashMap.lookupDefault HashMap.empty side bySide

-- * Journal and updates

-- | Read the source journal with all original entries and notes. Complexity: O(1).
journal :: (Note n, Partition b) => Ledger n b -> Journal n Double b
journal = ledgerJournal

-- | Empty the flow index, leaving the journal and all other indexes unchanged.
-- Subsequent 'post' calls accumulate new flows from zero. Complexity: O(1).
clearFlows :: (Note n, Partition b) => Ledger n b -> Ledger n b
clearFlows ledger = ledger { flowIndex = HashMap.empty }

-- | Update one nested strict map, starting from an empty map when absent.
-- Complexity: expected O(1) plus the supplied update.
updateNested :: (Eq key, Hashable key)
             => key
             -> (HashMap inner value -> HashMap inner value)
             -> HashMap key (HashMap inner value)
             -> HashMap key (HashMap inner value)
updateNested key update = HashMap.alter (Just . update . fromMaybe HashMap.empty) key

-- | Add one positive magnitude to a (Not, Hat) pair, forcing both fields.
-- Complexity: O(1).
addSide :: PostSide -> Double -> (Double, Double) -> (Double, Double)
addSide PNot value (!nots, !hats) = let !total = nots + value in (total, hats)
addSide PHat value (!nots, !hats) = let !total = hats + value in (nots, total)

-- | Update the four indexes for one scalar in the posting fold.
-- Invariant: 'Posting' supplies only 'Hat' or 'Not', never 'HatNot'.
-- Complexity: expected O(1).
indexEntry :: forall n b. (Note n, Partition b)
           => Proxy b
           -> n
           -> Ledger n b
           -> Double
           -> b
           -> Ledger n b
indexEntry proxy note ledger value postingBase = case hat postingBase of
    Not    -> add PNot value
    Hat    -> add PHat (negate value)
    HatNot -> error "Ledger.indexEntry: Posting concrete-side invariant violated"
  where
    coordinates = base postingBase
    key         = partKey proxy coordinates
    group       = groupOf proxy key
    add side delta = ledger
        { netIndex   = updateNested key
            (HashMap.insertWith (flip (+)) coordinates delta) (netIndex ledger)
        , sideIndex  = updateNested key
            (HashMap.alter (Just . addSide side value . fromMaybe (0, 0)) coordinates)
            (sideIndex ledger)
        , flowIndex  = updateNested note
            (updateNested key (updateNested side
                (HashMap.insertWith (flip (+)) coordinates value))) (flowIndex ledger)
        , groupIndex = updateNested group (HashMap.insert key ()) (groupIndex ledger)
        }

-- | Append a checked posting under its note and update all four indexes.
-- The journal receives @Posting.toAlg p .| note@ through its 'Semigroup'.
-- Each nonzero scalar updates the indexes sequentially in 'foldEntries' order;
-- no implicit @bar@ or @compress@ is applied.
--
-- Complexity: index updates take expected O(s), where s is the posting's
-- scalar entry count. Journal addition separately has the cost of its existing
-- 'Semigroup': amortized O(size(rhs)), with O(n) internal map compaction when
-- its delta crosses the threshold, as documented in "ExchangeAlgebra.Journal".
post :: forall n b. (Note n, Partition b) => n -> Posting b -> Ledger n b -> Ledger n b
post note posting ledger = indexed
    { ledgerJournal = ledgerJournal ledger <> (algebra .| note) }
  where
    algebra = Posting.toAlg posting
    indexed = foldEntries (indexEntry (Proxy :: Proxy b) note) ledger algebra
