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
-- Read balances and flows through the indexes, or inspect 'journal' for its
-- current entries and notes, including explicit carryover and settlement.
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
-- These laws apply to every lawful 'Partition' and 'Note' instance. Let
-- @b(p, beta)@ be the Not-minus-Hat amount of base @beta@ in @p@.
--
-- * IX-1: @netAt (post note p l) beta == netAt l beta + Signed (b(p, beta))@
--   holds exactly for integer inputs with total absolute magnitude below @2^53@.
-- * IX-2: 'netAt' differs by at most E1 from
--   @ExchangeAlgebra.Journal.Exact.balanceMapByExact Just original@,
--   converting @GT@ to a positive magnitude, @LT@ to a negative magnitude,
--   and @EQ@ to zero. @original@ contains all postings before carryover.
-- * IX-3: components are disjoint, cover all indexed bases, and agree with
--   'netAt' exactly, including retained zero balances.
-- * IX-4: 'flowIn' is the sequential sum for its note and side since the last
--   'clearFlows'. Clearing flows empties that index and preserves all other
--   indexes and the journal exactly.
-- * IX-5 and IX-9: 'carryBefore' preserves net and flow bits and rebuilds
--   affected components' side totals with one rounding per base and side.
-- * IX-6: 'Binary' restores stored indexes without recalculating them.
-- * IX-8b: 'queryIn' agrees within E1 + E2 with the exact balance map of the same
--   component's journal projected by @projWithBase [merge HatNot (base q)]@.
--   E2 is the sum of carry-entry rounding errors for that base, zero before
--   the first carryover.
-- * IX-10: 'componentsOf' lists exactly the components ever posted to in its
--   group, in ascending order. Cancellation and carryover never remove one.
-- * IX-11: 'settle' cancels selected source flows and transfers their signed
--   nets to retained earnings; its integer-input law is stated at 'settle'.
--
-- 'journal' preserves posted entries until explicit carryover replaces them;
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
                             , carryBefore
                             , SettleRule
                             , retainedEarningsRule
                             , settle
                             ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (replicateM)
import Data.Binary (Binary(..), Get, Put)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (foldl', sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))

import ExchangeAlgebra.Algebra (Alg, foldEntries, (.@))
import qualified ExchangeAlgebra.Algebra as Algebra
import ExchangeAlgebra.Algebra.Base (Element(ignoreWildcard), Hat(..), HatBaseClass(..))
import ExchangeAlgebra.Algebra.Base (AccountTitles(RetainedEarnings), ExBaseClass(..))
import ExchangeAlgebra.Algebra.Transfer.Closing (closingPairBy)
import ExchangeAlgebra.Algebra.Transfer.Rule (ClosingSide(..), closingSide)
import ExchangeAlgebra.Journal (Journal, Note, (.|))
import qualified ExchangeAlgebra.Journal as Journal
import qualified ExchangeAlgebra.Journal.Exact as Exact
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

-- | Pending postings in reverse posting order. Strict fields keep appending
-- from retaining a chain of unevaluated ledger updates.
data PendingPostings n b
    = NoPendingPostings                              -- ^ No unmerged entries.
    | PendingPosting !n !(Alg Double b) !(PendingPostings n b)
      -- ^ One nonzero posting and the previously recorded postings.

-- | A merged journal, pending postings, and component, side, flow, and
-- membership indexes. The journal and pending postings together are the
-- source of truth; 'journal' materializes their combined entries.
--
-- Invariant: 'post' and 'settle' add entries and update all indexes together.
-- 'carryBefore' replaces expired entries and rebuilds affected side totals.
-- 'clearFlows' resets only the flow index. Zero-valued input entries are absent;
-- net-index bases and component memberships remain after balances reach zero.
-- Rebuilt side maps contain only bases with current journal entries.
data Ledger n b = Ledger
    { ledgerJournal :: !(Journal n Double b)
    , ledgerPending :: !(PendingPostings n b)
    , netIndex      :: !(HashMap (PartKey b) (HashMap (BasePart b) Double))
    , sideIndex     :: !(HashMap (PartKey b) (HashMap (BasePart b) (Double, Double)))
    , flowIndex     :: !(HashMap n
                            (HashMap (PartKey b) (HashMap PostSide (HashMap (BasePart b) Double))))
    , groupIndex    :: !(HashMap (Group b) (HashMap (PartKey b) ()))
    }

-- | Force every index, including keys and both side totals, the pending spine
-- and notes, and all stored journals and algebras through their existing
-- 'NFData' instances. Derived caches and reserved algebra fields are not
-- forced. Complexity: linear in the stored structures.
instance ( NFData n
         , NFData (BasePart b)
         , NFData (PartKey b)
         , NFData (Group b)
         ) => NFData (Ledger n b) where
    rnf (Ledger recorded pending nets sides flows groups) =
        rnf recorded `seq` forcePending pending `seq`
        rnf nets `seq` rnf sides `seq` rnf flows `seq` rnf groups
      where
        forcePending NoPendingPostings = ()
        forcePending (PendingPosting note algebra previous) =
            rnf note `seq` rnf algebra `seq` forcePending previous

-- | Write a map as a length-prefixed list in ascending key order.
-- Nested maps use this same encoding at every level.
putMapWith :: (Ord key, Binary key)
           => (value -> Put) -> HashMap key value -> Put
putMapWith putValue values = do
    put (HashMap.size values :: Int)
    mapM_ (\(key, value) -> put key >> putValue value) (sortOn fst (HashMap.toList values))

-- | Read a map without recalculating values; reject negative lengths and
-- keys that are duplicated or out of ascending order.
getMapWith :: (Ord key, Hashable key, Binary key)
           => Get value -> Get (HashMap key value)
getMapWith getValue = do
    count <- get
    readPairs count
  where
    readPairs count
        | count < (0 :: Int) = fail "Ledger Binary: negative map length"
        | otherwise = do
            pairs <- replicateM count ((,) <$> get <*> getValue)
            validate pairs
    validate pairs
        | and (zipWith (<) keys (drop 1 keys)) = pure (HashMap.fromList pairs)
        | otherwise = fail "Ledger Binary: map keys are not strictly ascending"
      where
        keys = map fst pairs

-- | Merge pending postings and store the existing journal encoding, then net, side, flow, and group
-- indexes. Every index map is a length-prefixed ascending list, recursively.
-- Decoding restores the stored floating-point values without recalculating
-- indexes (IX-6); the journal decoder rebuilds only its own note-axis cache.
-- Index maps are written in ascending key order. The journal uses its existing
-- 'Journal' and 'Alg' encoding, whose bytes are deterministic within one build;
-- byte equality across hashable versions is not guaranteed.
instance ( Note n, Partition b
         , Binary n, Binary b, Binary (BasePart b)
         , Binary (PartKey b), Binary (Group b)
         ) => Binary (Ledger n b) where
    put ledger@(Ledger _ _ nets sides flows groups) = do
        put (journal ledger)
        putMapWith (putMapWith put) nets
        putMapWith (putMapWith put) sides
        putMapWith (putMapWith (putMapWith (putMapWith put))) flows
        putMapWith (putMapWith put) groups
    get = Ledger <$> get
                 <*> pure NoPendingPostings
                 <*> getMapWith (getMapWith get)
                 <*> getMapWith (getMapWith get)
                 <*> getMapWith (getMapWith (getMapWith (getMapWith get)))
                 <*> getMapWith (getMapWith get)

-- | A closing rule containing only its destination account title.
-- The constructor is private so that closing directions remain compatible
-- with the destination's accounting side.
newtype SettleRule = SettleRule AccountTitles

-- | Close eligible accounts into 'RetainedEarnings', preserving other axes.
-- An arbitrary destination would break accounting sides: closing @Not Sales 20@
-- into @Cash@ would create two debit entries.
retainedEarningsRule :: SettleRule
retainedEarningsRule = SettleRule RetainedEarnings

-- | Construct an empty journal with empty indexes. Every 'netAt' readout is
-- zero and every collection readout is empty. Complexity: O(1).
emptyLedger :: (Note n, Partition b) => Ledger n b
emptyLedger = Ledger mempty NoPendingPostings
    HashMap.empty HashMap.empty HashMap.empty HashMap.empty

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

-- | Read the (Not, Hat) totals of current journal entries in one
-- component. An absent component returns an empty map. Complexity: expected
-- O(1) lookup; consuming the result takes O(m) for m bases. Map order is unspecified.
-- Updates are sequential between carryovers; 'carryBefore' rebuilds affected
-- components from exact side sums, rounded once per side and base.
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

-- | Group pending algebras in posting order without repeatedly merging them.
-- Complexity: expected O(k) for k pending postings.
pendingByNote :: Note n => PendingPostings n b -> HashMap n [Alg Double b]
pendingByNote = collect HashMap.empty
  where
    collect !grouped NoPendingPostings = grouped
    collect !grouped (PendingPosting note algebra previous) =
        collect (HashMap.alter (Just . (algebra :) . fromMaybe []) note grouped) previous

-- | Read current entries and notes, including carryover and settlement.
-- The merged journal and pending postings together are the source of truth.
-- Pending entries are grouped by note and built in bulk, without cancellation
-- or compression. The returned journal preserves each note's posting multiset;
-- within-base sequence order can differ from incremental journal addition.
--
-- Complexity: O(1) with no pending postings; otherwise the cost of grouping
-- and merging the pending entries with the stored journal. This pure accessor
-- does not update the ledger, so each call can pay that merge cost again.
journal :: (Note n, Partition b) => Ledger n b -> Journal n Double b
journal ledger = case ledgerPending ledger of
    NoPendingPostings -> ledgerJournal ledger
    pending -> Journal.fromMap $ HashMap.unionWith (<>)
        (Journal.toMap (ledgerJournal ledger))
        (HashMap.map mergePostings (pendingByNote pending))
  where
    -- Flatten first so the bulk builder inserts singletons, rather than
    -- repeatedly unioning a growing Liner with each small posting's map.
    mergePostings = Algebra.unionsMerge . concatMap Algebra.toList

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
-- The posting is retained until 'journal', 'carryBefore', or serialization
-- merges it with the recorded journal.
-- Each nonzero scalar updates the indexes sequentially in 'foldEntries' order;
-- no implicit @bar@ or @compress@ is applied.
--
-- Complexity: index updates take expected O(s), where s is the posting's
-- scalar entry count. Retaining the posting takes O(1), independently of the
-- stored journal's size. Reading 'journal' separately pays the merge cost.
post :: forall n b. (Note n, Partition b) => n -> Posting b -> Ledger n b -> Ledger n b
post note posting = postAlgebra note (Posting.toAlg posting)

-- | Append internally constructed finite, non-negative concrete-side entries
-- through the same journal and index update path as checked external postings.
postAlgebra :: forall n b. (Note n, Partition b)
            => n -> Alg Double b -> Ledger n b -> Ledger n b
postAlgebra note algebra ledger
    | Algebra.isZero algebra = ledger
    | otherwise = indexed
        { ledgerPending = PendingPosting note algebra (ledgerPending ledger) }
  where
    indexed = foldEntries (indexEntry (Proxy :: Proxy b) note) ledger algebra

-- | Extract an exact result under the finite-sum invariant of carryover.
-- Invariant: checked postings and evaluator count bounds keep all sums in range.
exactCarry :: Either Exact.ExactSumError value -> value
exactCarry = either
    (error . ("Ledger.carryBefore: finite exact-sum invariant violated: " ++) . show) id

-- | Recompute side totals for every component from all current entries,
-- with one rounding per base and side. Empty components retain empty maps.
rebuildSides :: forall n b. (Note n, Partition b)
             => Journal n Double b
             -> HashMap (PartKey b) (HashMap (BasePart b) (Double, Double))
             -> HashMap (PartKey b) (HashMap (BasePart b) (Double, Double))
rebuildSides recorded previous = HashSet.foldl' replace HashMap.empty allKeys
  where
    proxy = Proxy :: Proxy b
    allKeys = HashSet.union (HashSet.fromList (HashMap.keys previous))
        (HashSet.fromList (HashMap.keys states))
    states = foldEntries collect HashMap.empty (Journal.toAlg recorded)
    collect totals value postingBase = updateNested key
        (HashMap.alter (Just . accumulate . fromMaybe (Exact.emptyAccum, Exact.emptyAccum))
            (base postingBase)) totals
      where
        key = partKey proxy (base postingBase)
        accumulate (nots, hats) =
          case hat postingBase of
            Not -> let !next = Exact.addAccum value nots in (next, hats)
            Hat -> let !next = Exact.addAccum value hats in (nots, next)
            HatNot -> error "Ledger.carryBefore: concrete-side invariant violated"
    rounded (nots, hats) = let
        !notTotal = exactCarry (Exact.roundAccum nots)
        !hatTotal = exactCarry (Exact.roundAccum hats)
        in (notTotal, hatTotal)
    replace totals key = HashMap.insert key
        (HashMap.map rounded (HashMap.lookupDefault HashMap.empty key states)) totals

-- | Replace expired entries, selected by the predicate, with one entry per
-- base under the supplied note. Each Not-minus-Hat sum is exact and rounded
-- once; an exactly zero sum produces no entry. Entries inside the window stay
-- unchanged. This is explicit carryover, with no implicit @bar@ or @compress@.
--
-- Net balances, flows, and component membership retain their bits (IX-5).
-- Every component's side totals are rebuilt from retained and carried
-- entries, rounding each exact side sum once (IX-9). The exact journal balance
-- can change by one carry-entry rounding per base (E2); repeated carryovers
-- accumulate those errors. Compare the net index with the original postings
-- using E1, and with the current journal using E1 + E2 (IX-2 and IX-8b).
--
-- Invariant: external values satisfy the 'Posting' contract, with at most
-- @2^50@ external scalar entries in the run, @2^52@ entries per base including
-- internal entries, and @2^31@ periods. Settlement uses current-period flows
-- once per base per period. Exact sums outside the value type's range are
-- outside this contract and cause 'error'. No evaluator count checks are run.
-- Complexity: a journal scan plus exact aggregation and component-map updates.
carryBefore :: forall n b. (Note n, Partition b)
            => (n -> Bool) -> n -> Ledger n b -> Ledger n b
carryBefore expired carryNote ledger = ledger
    { ledgerJournal = recorded
    , ledgerPending = NoPendingPostings
    , sideIndex = rebuildSides recorded (sideIndex ledger)
    }
  where
    current = journal ledger
    selected = Journal.filterWithNote (\note _ -> expired note) current
    retained = Journal.filterWithNote (\note _ -> not (expired note)) current
    balances = exactCarry (Exact.balanceMapByExact Just selected)
    carried = Map.foldlWithKey' append mempty balances
    append previous coordinates (direction, value) = case direction of
        EQ -> previous
        GT -> previous <> ((value .@ merge Not coordinates) .| carryNote)
        LT -> previous <> ((value .@ merge Hat coordinates) .| carryNote)
    recorded = retained <> carried

-- | Close selected bases using the pre-call flow index and append both the
-- reversal and destination entries under the supplied note. Bases whose
-- account has no closing direction, destination bases, and zero nets are
-- skipped. Every generated magnitude is non-negative. No @bar@ or @compress@
-- is applied, and all journal and index updates use the 'post' update path.
--
-- Bases are processed in ascending order. For each base, selected notes are
-- read in ascending order, sequentially adding each note's Not-minus-Hat
-- flow. Generated entries never feed back into those pre-call inputs.
-- Generated entries also enter the flow index under the settlement note;
-- exclude that note when observing pre-settlement flows afterward.
-- For integer inputs with total selected absolute flow below @2^53@, each
-- selected source flow plus its reversal is exactly zero; target entries sum
-- to the source nets with the closing direction's sign (IX-11).
--
-- One call increases the journal's sum of absolute entry values by at most
-- @2 * (1 + u)^k@ times the sum of absolute current-period flows of the closed
-- bases, where @u = 2^-53@ and @k@ counts additions along a value's history.
-- The finite-input and evaluator count assumptions of 'carryBefore' apply.
-- This operation returns a ledger directly, without a failure result.
-- Complexity: O(b log b + t log t + b*t) lookups for b bases and t notes,
-- plus the ordinary journal and index cost of generated postings.
settle :: forall n b. (Note n, Partition b, ExBaseClass b)
       => SettleRule
       -> (n -> Bool)
       -> HashSet (BasePart b)
       -> n
       -> Ledger n b
       -> Ledger n b
settle (SettleRule destination) selected requested settlementNote ledger =
    foldl' close ledger sources
  where
    proxy = Proxy :: Proxy b
    destinationOf coordinates = base (setAccountTitle (merge Not coordinates :: b) destination)
    destinations = HashSet.map destinationOf requested
    sources = sort (HashSet.toList (requested `HashSet.difference` destinations))
    notes = sortOn fst (filter (selected . fst) (HashMap.toList (flowIndex ledger)))
    net coordinates = foldl' (addNote coordinates) 0 notes
    addNote coordinates total (_, byComponent) = total + (side PNot - side PHat)
      where
        bySide = HashMap.lookupDefault HashMap.empty (partKey proxy coordinates) byComponent
        side postingSide = HashMap.lookupDefault 0 coordinates
            (HashMap.lookupDefault HashMap.empty postingSide bySide)
    close current coordinates = case closingSide (getAccountTitle source) of
        Nothing -> current
        Just side
            | amount == 0 -> current
            | otherwise -> postAlgebra settlementNote
                (closingPairBy (side == ClosingKeep) destination (abs amount) source) current
      where
        amount = net coordinates
        source = merge direction coordinates :: b
        direction
            | amount < 0 = Hat
            | otherwise = Not
