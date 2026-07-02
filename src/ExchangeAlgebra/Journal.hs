{- |
    Module     : ExchangeAlgebra.Journal
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>


-}

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}

module ExchangeAlgebra.Journal
    ( module ExchangeAlgebra.Algebra.Base
    , HatVal(..)
    , HatBaseClass(..)
    , Redundant(..)
    , Exchange(..)
    , pattern (:@)
    , (.@)
    , Note(..)
    , NoteAxisKey(..)
    , NoteAxisPosting
    , Journal(..)
    , pattern ExchangeAlgebra.Journal.Zero
    , (.|)
    , toAlg
    , toMap
    , fromMap
    , fromList
    , sigma
    , sigma2When
    , sigmaOn
    , sigmaOnFromMap
    , decTo
    , sigmaM
    , map
    , insert
    , projWithNote
    , projWithBase
    , projWithNoteBase
    , projWithBaseNetNorm
    , projWithNoteBaseNetNorm
    , projWithBaseNorm
    , projWithNoteNorm
    , filterWithNote
    , filterByAxis
    , gather
    ) where

import qualified    ExchangeAlgebra.Algebra as EA
import              ExchangeAlgebra.Algebra.Base
import              ExchangeAlgebra.Algebra ( HatVal(..)
                                            , Alg(..)
                                            , Redundant(..)
                                            , Exchange(..)
                                            , pattern (:@)
                                            , (.@))
import              Prelude                 hiding (map, filter)
import qualified    Data.HashMap.Strict     as Map
import qualified    Data.IntMap.Strict      as IntMap
import              Data.IntMap.Strict      (IntMap)
import qualified    Data.HashSet            as HSet
import              Data.HashSet            (HashSet)
import              Control.Parallel.Strategies (NFData)
import              Control.DeepSeq             (rnf)
import qualified    Data.Set                as S
import qualified    Data.List               as L
import qualified    Data.Map.Strict         as M
import qualified    Data.Text               as T
import qualified    Control.Monad           as CM
import qualified    Data.Binary             as Binary
import              Data.Typeable           (Typeable, cast, typeOf)

------------------------------------------------------------------
-- * NoteAxisKey
------------------------------------------------------------------

-- | An existential type that holds each axis of a Note with its type erased.
-- Used to decompose multi-dimensional Note types (tuples) into per-axis keys
-- for indexing, mirroring how @AxisKey@ works for basis elements in 'Alg'.
data NoteAxisKey = forall a. (Eq a, Hashable a, Typeable a) => NoteAxisKey !a

instance Eq NoteAxisKey where
    NoteAxisKey x == NoteAxisKey y = case cast y of
        Nothing -> False
        Just y' -> x == y'

instance Hashable NoteAxisKey where
    hashWithSalt salt (NoteAxisKey x) = salt `hashWithSalt` typeOf x `hashWithSalt` x

------------------------------------------------------------------
-- * NoteAxisPosting
------------------------------------------------------------------

-- | Per-axis index for Note keys.
-- Maps axis_number -> axis_value -> set of Notes.
-- Mirrors @AxisPosting@ in 'Alg'.
type NoteAxisPosting n = IntMap (Map.HashMap NoteAxisKey (HashSet n))

{-# INLINE emptyNoteAxisPosting #-}
emptyNoteAxisPosting :: NoteAxisPosting n
emptyNoteAxisPosting = IntMap.empty

{-# INLINE insertNoteAxisPosting #-}
-- | Complexity: O(d) where d is the number of axes in the Note
insertNoteAxisPosting :: (Eq n, Hashable n) => [NoteAxisKey] -> n -> NoteAxisPosting n -> NoteAxisPosting n
insertNoteAxisPosting !keys !note !idx =
    snd $ L.foldl' step (0 :: Int, idx) keys
  where
    step (!axis, !acc) !k =
        let !axisMap = IntMap.findWithDefault Map.empty axis acc
            !notes0 = Map.lookupDefault HSet.empty k axisMap
            !notes1 = HSet.insert note notes0
            !axisMap' = Map.insert k notes1 axisMap
            !acc' = IntMap.insert axis axisMap' acc
        in (axis + 1, acc')

{-# INLINE deleteNoteAxisPosting #-}
-- | Complexity: O(d) where d is the number of axes in the Note
deleteNoteAxisPosting :: (Eq n, Hashable n) => [NoteAxisKey] -> n -> NoteAxisPosting n -> NoteAxisPosting n
deleteNoteAxisPosting !keys !note !idx =
    snd $ L.foldl' step (0 :: Int, idx) keys
  where
    step (!axis, !acc) !k =
        case IntMap.lookup axis acc of
            Nothing -> (axis + 1, acc)
            Just axisMap ->
                case Map.lookup k axisMap of
                    Nothing -> (axis + 1, acc)
                    Just notes0 ->
                        let !notes1 = HSet.delete note notes0
                            !axisMap' = if HSet.null notes1
                                        then Map.delete k axisMap
                                        else Map.insert k notes1 axisMap
                            !acc' = if Map.null axisMap'
                                    then IntMap.delete axis acc
                                    else IntMap.insert axis axisMap' acc
                        in (axis + 1, acc')

{-# INLINE queryNoteAxisPosting #-}
-- | Query the NoteAxisPosting index for a single axis.
-- Returns the set of Notes whose value matches on the specified axis.
--
-- Complexity: O(1) (two map lookups)
queryNoteAxisPosting :: Int -> NoteAxisKey -> NoteAxisPosting n -> HashSet n
queryNoteAxisPosting !axis !key !idx =
    case IntMap.lookup axis idx of
        Nothing -> HSet.empty
        Just axisMap -> Map.lookupDefault HSet.empty key axisMap

------------------------------------------------------------------
-- * Note
------------------------------------------------------------------

-- | Type class for journal annotations (notes attached to postings).
-- @plank@ represents a blank note (analogous to @mempty@ in @Monoid@).
--
-- @toNoteAxisKeys@ decomposes a Note into per-axis keys for AxisPosting
-- indexing, mirroring how @toAxisKeys@ works for basis elements.
-- For tuple Note types, each component becomes a separate axis.
-- The default returns a single axis containing the Note itself.
--
-- == Prefer an ADT note over @String@
--
-- A note's /event/ axis is best modelled as a small enumeration (an ADT)
-- rather than a @String@. A @String@ tag is a __stringly-typed semantic key__:
-- the same literal must appear at the write site (@.| (\"trade\", t)@) and at
-- every read site (@projWithNote [(\"trade\", t)]@), and a typo on either side
-- still type-checks — the projection just /silently matches nothing/. With an
-- ADT a mistyped constructor is a __compile error__, so reads and writes can
-- never drift apart. Add 'plank' as its own explicit constructor (the note is a
-- pointed set, so the blank tag is a distinguished element, not the empty
-- string):
--
-- @
-- data MTag = PlankTag | Trade | Production | Report | Closing | Carryover
--   deriving (Show, Eq, Ord, Enum, Bounded, Generic)
-- instance Hashable MTag
-- instance Note MTag where plank = PlankTag
-- type MNote = (MTag, Int)   -- the tuple instance keeps the (event, term) index
-- @
--
-- (The @marketEx1@ example and the @SimEvent@ note in the test suite follow
-- this pattern.) If the ledger is ever spilled\/restored, also give the tag a
-- @Binary@ instance (structurally derivable from @Generic@).
class (Show a, Eq a, Ord a, Hashable a, Typeable a) => Note a where
    plank :: a
    isPlank :: a -> Bool
    isPlank x = x == plank
    -- | Decompose a Note into per-axis keys for AxisPosting indexing.
    -- Default: single axis with the Note itself.
    toNoteAxisKeys :: a -> [NoteAxisKey]
    toNoteAxisKeys a = [NoteAxisKey a]

-- | Default instance for using @Int@ as a time axis (Term).
-- @plank = -1@ is distinguished from non-negative term numbers.
--
-- In simulations, typically used as @type Term = Int@.
--
-- >>> (plank :: Int)
-- -1
--
-- >>> isPlank (0 :: Int)
-- False
--
-- >>> isPlank (-1 :: Int)
-- True
instance Note Int where
    plank = -1

instance Note String where
    plank = ""

instance Note T.Text where
    plank = ""

instance (Note a, Note b) => Note (a, b) where
    plank = (plank, plank)
    toNoteAxisKeys (a, b) = [NoteAxisKey a, NoteAxisKey b]

instance (Note a, Note b, Note c) => Note (a, b, c) where
    plank = (plank, plank, plank)
    toNoteAxisKeys (a, b, c) = [NoteAxisKey a, NoteAxisKey b, NoteAxisKey c]

instance (Note a, Note b, Note c, Note d) => Note (a, b, c, d) where
    plank = (plank, plank, plank, plank)
    toNoteAxisKeys (a, b, c, d) = [NoteAxisKey a, NoteAxisKey b, NoteAxisKey c, NoteAxisKey d]

------------------------------------------------------------------
-- * Journal
------------------------------------------------------------------

-- | Transaction data with annotations.
--   Stored in a base + delta two-layer structure with per-axis indices.
--   Base index is lazy (built on first axis query), while delta index is updated incrementally.
--   Updates are appended only to delta and periodically compacted into base.
--
--   __Invariants (do not hand-construct t'Journal').__ The constructor exposes
--   internal cache\/index fields @_jBaseAxis@ and @_jDeltaAxis@, which must be
--   exactly the Note axis indices (@buildNoteAxisPosting@) of @_jBase@ and
--   @_jDelta@ respectively. The axis-filtered query path ('filterByAxis') reads
--   those indices, so a value whose indices disagree with its maps yields wrong
--   answers silently (not an exception). Always build journals via 'fromMap',
--   @mkJournal@, '(.|)', or 'fromList' — never by applying @Journal@ directly.
data Journal n v b where
     Journal :: (Note n, HatVal v, HatBaseClass b)
            => { _jBase      :: !(Map.HashMap n (Alg v b))
               , _jDelta     :: !(Map.HashMap n (Alg v b))
               , _jBaseAxis  :: NoteAxisPosting n
               , _jDeltaAxis :: !(NoteAxisPosting n)
               } -> Journal n v b

deltaCompactThreshold :: Int
deltaCompactThreshold = 128

-- | Build Note axis index from map keys.
{-# INLINE buildNoteAxisPosting #-}
buildNoteAxisPosting :: Note n => Map.HashMap n a -> NoteAxisPosting n
buildNoteAxisPosting =
    Map.foldlWithKey'
        (\acc n _ -> insertNoteAxisPosting (toNoteAxisKeys n) n acc)
        emptyNoteAxisPosting

-- | Smart constructor for Journal.
-- Base axis index is lazy; delta axis index is built eagerly.
{-# INLINE mkJournal #-}
mkJournal :: (Note n, HatVal v, HatBaseClass b)
          => Map.HashMap n (Alg v b) -> Map.HashMap n (Alg v b) -> Journal n v b
mkJournal bs delta = Journal bs delta baseIdx deltaIdx
  where
    ~baseIdx = buildNoteAxisPosting bs
    !deltaIdx = buildNoteAxisPosting delta

-- | Construct a Journal from a HashMap.
--
-- Complexity: O(1). Base axis index is built lazily on first axis query.
{-# INLINE fromMap #-}
fromMap :: (HatVal v, HatBaseClass b, Note n)
        => Map.HashMap n (Alg v b) -> Journal n v b
fromMap m = mkJournal m Map.empty

-- | Retrieve all entries of a Journal as a HashMap.
-- Merges the base and delta layers.
--
-- Complexity: O(n) where n is the number of Notes
{-# INLINE toMap #-}
toMap :: (HatVal v, HatBaseClass b, Note n)
      => Journal n v b -> Map.HashMap n (Alg v b)
toMap = materializeMap

{-# INLINE materializeMap #-}
-- Short-circuits when one layer is empty: returns the other layer verbatim
-- instead of building a fresh map via 'Map.unionWith'. This is the common
-- case for @fromMap@ products (delta empty — the Lite @sigma msgs id@ shape)
-- and for freshly @(.|)@-built journals (base empty). Values are untouched,
-- so the result is identical to the unconditional union
-- (audit R5 / ROAD_MAP P1b).
materializeMap :: (HatVal v, HatBaseClass b, Note n)
               => Journal n v b -> Map.HashMap n (Alg v b)
materializeMap (Journal bs delta _ _)
    | Map.null delta = bs
    | Map.null bs    = delta
    | otherwise      = Map.unionWith (.+) bs delta

{-# INLINE lookupNote #-}
lookupNote :: (HatVal v, HatBaseClass b, Note n)
           => n -> Journal n v b -> Maybe (Alg v b)
lookupNote n (Journal bs delta _ _) =
    case (Map.lookup n delta, Map.lookup n bs) of
        (Nothing, Nothing) -> Nothing
        (Just d, Nothing)  -> Just d
        (Nothing, Just b)  -> Just b
        (Just d, Just b)   -> Just (b .+ d)

{-# INLINE compactIfNeeded #-}
compactIfNeeded :: (HatVal v, HatBaseClass b, Note n)
                => Journal n v b -> Journal n v b
compactIfNeeded j@(Journal bs delta _ _)
    | Map.size delta < deltaCompactThreshold = j
    | otherwise = mkJournal (Map.unionWith (.+) bs delta) Map.empty

{-# INLINE appendMap #-}
appendMap :: (HatVal v, HatBaseClass b, Note n)
          => Map.HashMap n (Alg v b) -> Journal n v b -> Journal n v b
appendMap rhs j@(Journal bs delta baseAxis deltaAxis)
    | Map.null rhs = j
    | otherwise = compactIfNeeded $ Journal bs delta' baseAxis deltaAxis'
  where
    (delta', deltaAxis') = Map.foldlWithKey' step (delta, deltaAxis) rhs

    step (!dAcc, !idxAcc) !k !v =
        case Map.lookup k dAcc of
            -- New note key: index it (insert) unless the value is zero.
            Nothing
                | EA.isZero v -> (dAcc, idxAcc)
                | otherwise   ->
                    ( Map.insert k v dAcc
                    , insertNoteAxisPosting (toNoteAxisKeys k) k idxAcc )
            -- Existing note key: 'k' is already in the delta axis index
            -- (the index tracks exactly the keys present in delta, and the
            -- note IS the key, so its axis keys are unchanged). The
            -- re-insert was therefore idempotent — skip it. Only a result
            -- that collapses to zero changes the index, via delete
            -- (audit R5 / ROAD_MAP P1b).
            Just dv ->
                let !dMerged = dv .+ v
                in if EA.isZero dMerged
                    then ( Map.delete k dAcc
                         , deleteNoteAxisPosting (toNoteAxisKeys k) k idxAcc )
                    else ( Map.insert k dMerged dAcc, idxAcc )

instance ( Note n
         , HatVal v
         , HatBaseClass b
         , Binary.Binary n
         , Binary.Binary (Alg v b)
         ) => Binary.Binary (Journal n v b) where
    {-# INLINABLE put #-}
    {-# INLINABLE get #-}
    put j = do
        let !m = toMap j
        Binary.put (Map.size m :: Int)
        Map.foldrWithKey
            (\n alg k -> Binary.put n >> Binary.put alg >> k)
            (pure ())
            m
    get = do
        n <- Binary.get :: Binary.Get Int
        fromMap <$> go n Map.empty
      where
        go !remaining !acc
            | remaining <= 0 = pure acc
            | otherwise = do
                k <- Binary.get
                v <- Binary.get
                go (remaining - 1) (Map.insert k v acc)

-- | Test whether the Journal is empty (zero).
--
-- Complexity: O(1)
isZero :: (HatVal v, HatBaseClass b, Note n)
       => Journal n v b -> Bool
isZero (Journal bs delta _ _) = Map.null bs && Map.null delta

pattern Zero :: (HatVal v, HatBaseClass b, Note n) => Journal n v b
pattern Zero <- (isZero -> True)
    where
        Zero = mkJournal Map.empty Map.empty

-- | Smart constructor that attaches a Note (annotation) to an algebra element to build a Journal.
--
-- Complexity: O(1)
(.|) :: (HatVal v, HatBaseClass b, Note n)
      => Alg v b -> n -> Journal n v b
(.|) alg n = mkJournal Map.empty (Map.singleton n alg)

infixr 2 .|

------------------------------------------------------------------
-- Show
------------------------------------------------------------------
instance (HatVal v, HatBaseClass b, Note n) => Show (Journal n v b) where
    show js
        | Map.null m = "0"
        | otherwise  = Map.foldrWithKey f "" m
      where
        m = toMap js
        f k a t
            | isPlank k = if t == "" then show a else t ++ " .+ " ++ show a
            | otherwise = foldr (\x y -> if y == ""
                                        then show x ++ ".|" ++ show k
                                        else y ++ " .+ " ++ show x ++ ".|" ++ show k)
                                t
                                (EA.toASCList a)
------------------------------------------------------------------

instance (HatVal v, HatBaseClass b, Note n) => Semigroup (Journal n v b) where
    {-# INLINE (<>) #-}
    (<>) = addJournal

-- | Journal addition. Appends right-hand entries to the left-hand side.
--
-- Complexity: Amortized O(size(rhs)); O(n) compaction when the delta exceeds the threshold
--
-- >>> type Test = Journal String Double (HatBase AccountTitles)
-- >>> x = 20.00:@Not:<Cash .+ 20.00:@Hat:<Deposits .| "Withdrawal" :: Test
-- >>> y = 10.00:@Hat:<Cash .+ 10.00:@Not:<Deposits .| "Deposits" :: Test
-- >>> x .+ y
-- 10.00:@Not:<Deposits.|"Deposits" .+ 10.00:@Hat:<Cash.|"Deposits" .+ 20.00:@Hat:<Deposits.|"Withdrawal" .+ 20.00:@Not:<Cash.|"Withdrawal"
addJournal :: (HatVal v, HatBaseClass b, Note n)
           => Journal n v b -> Journal n v b -> Journal n v b
addJournal lhs rhs = appendMap (toMap rhs) lhs

instance (HatVal v, HatBaseClass b, Note n) => Monoid (Journal n v b) where
    mempty = mkJournal Map.empty Map.empty
    mappend = (<>)

-- | Shallow-structural 'NFData', mirroring the @'Alg' v b@ instance in
-- "ExchangeAlgebra.Algebra": it forces the two-layer @base@ / @delta@ maps to
-- WHNF on their spines and forces every contained 'Alg' (via the 'Alg' 'NFData'
-- instance), but does not touch the lazily built axis indices (@_jBaseAxis@ /
-- @_jDeltaAxis@), which are derived caches. This is enough for
-- 'Control.Parallel.Strategies.rdeepseq' to fully evaluate journal "messages"
-- before merging them in parallel.
instance NFData (Journal n v b) where
    rnf (Journal bs delta _ _) =
        Map.foldr  (\alg acc -> rnf alg `seq` acc)
                   (Map.foldr (\alg acc -> rnf alg `seq` acc)
                              ()
                              bs)
                   delta

instance (HatVal v, HatBaseClass b, Note n) => Redundant (Journal n) v b where
    (.^) = map (.^)
    (.+) = mappend
    (.*) x  = map ((.*) x)
    norm = norm . toAlg
    (.-) x = map (.-) (gather plank x)
    compress = map compress

instance (Note n, HatVal v, ExBaseClass b) => Exchange (Journal n) v b where
    decR js = map (EA.filter (\x -> x /= EA.Zero && (whichSide . EA._hatBase) x == Credit)) js
    decL xs = map (EA.filter (\x -> x /= EA.Zero && (whichSide . EA._hatBase) x == Debit)) xs
    decP xs = map (EA.filter (\x -> x /= EA.Zero && (isHat . EA._hatBase) x)) xs
    decM xs = map (EA.filter (\x -> x /= EA.Zero && (not . isHat . EA._hatBase) x)) xs

    -- scale-aware tolerance (WI-12), consistent with Alg's Exchange instance
    balance xs = EA.nearlyEqScaled ((norm . decR) xs) ((norm . decL) xs)

    diffRL xs
        | EA.nearlyEqScaled r l = (Side, 0)
        | r > l                 = (Credit, r - l)
        | otherwise             = (Debit, l - r)
      where
        r = (norm . decR) xs
        l = (norm . decL) xs

------------------------------------------------------------------
-- | Build a t'Journal' from a list of postings. @O(N)@ via a strict left fold
-- (@L.foldl' (.+) mempty@).
--
-- == Why a strict left fold
--
-- This was previously a lazy right fold (@foldr (.+) mempty@), kept that way to
-- freeze the same-base accumulation order for 'Double' (whose addition is
-- non-associative, so reordering shifts the last-ULP of 'norm'). The strict left
-- fold is far cheaper at scale — the lazy right fold builds a deep right-nested
-- thunk that is expensive to force (≈40x at N=20000, ~15x at N=10000 in the core
-- benchmark). The fold direction was switched to the fast version once the
-- audited/exact path moved to 'ExchangeAlgebra.Value.MoneyDecimal'; see
-- plans/in-progress/LAZY_EVAL_AUDIT.md and SELECTABLE_VALUE_TYPE_PLAN.md.
--
-- == Behaviour contract
--
-- 'fromList' preserves the /multiset/ of postings exactly (no posting is added,
-- dropped, or merged across bases that would otherwise be kept apart). When two
-- postings collide on the /same note key and same base/ (and therefore land in
-- one 'ExchangeAlgebra.Algebra.Alg' sequence) the strict left fold orders that
-- @Seq@ opposite to the old lazy right fold. That order is observable through
-- 'Eq' \/ 'Show' \/ 'toAlg' \/ @Binary@, and for 'Double' through the last-ULP of
-- 'norm' \/ 'bar' (IEEE-754 addition is non-associative). Postings that differ in
-- note or base land in separate map entries and are unaffected.
--
-- For an /exact/ value type ('ExchangeAlgebra.Value.MoneyDecimal') addition is
-- associative, so 'norm' \/ 'bar' \/ balance are independent of construction order
-- (the fold direction here, parallel merges, etc.). Use
-- 'ExchangeAlgebra.Value.MoneyDecimal' when you need
-- deterministic, auditable totals.
--
-- >>> type Test = Journal String Double (HatBase AccountTitles)
-- >>> x = [(1.00:@Hat:<Cash .| z) | z <- ["Loan Payment","Purchace Apple"]] :: [Test]
-- >>> fromList x
-- 1.00:@Hat:<Cash.|"Purchace Apple" .+ 1.00:@Hat:<Cash.|"Loan Payment"
fromList :: (HatVal v, HatBaseClass b, Note n)
         => [Journal n v b] -> Journal n v b
fromList = L.foldl' (.+) mempty

------------------------------------------------------------------
{-# INLINE mergeJournalMap #-}
mergeJournalMap :: (HatVal v, HatBaseClass b, Note n)
                => Map.HashMap n (Alg v b)
                -> Journal n v b
                -> Map.HashMap n (Alg v b)
mergeJournalMap !acc (Journal bs delta _ _)
    | Map.null bs && Map.null delta = acc
    | otherwise =
        let !acc1 = Map.foldlWithKey' mergeOne acc bs
        in Map.foldlWithKey' mergeOne acc1 delta
  where
    mergeOne !m !n !alg
        | EA.isZero alg = m
        | otherwise = Map.insertWith (.+) n alg m

{-# INLINE mergeJournalMapIfNonZero #-}
mergeJournalMapIfNonZero :: (HatVal v, HatBaseClass b, Note n)
                         => Map.HashMap n (Alg v b)
                         -> Journal n v b
                         -> Map.HashMap n (Alg v b)
mergeJournalMapIfNonZero !acc js
    | isZero js = acc
    | otherwise = mergeJournalMap acc js

-- | Summation function that applies a function to each list element and sums the resulting Journals.
--
-- Complexity: O(|xs| * union cost)
{-# INLINE sigma #-}
sigma :: (HatVal v, HatBaseClass b, Note n)
      => [a] -> (a -> Journal n v b) -> Journal n v b
sigma xs f = fromMap $ L.foldl' step Map.empty xs
  where
    step !acc !x = mergeJournalMapIfNonZero acc (f x)

-- | Conditional summation over a double loop (Journal version).
-- Applies the function only to pairs that satisfy the condition across all combinations of two lists, and sums the results.
--
-- Complexity: O(|xs| * |ys| * union cost)
{-# INLINE sigma2When #-}
sigma2When :: (HatVal v, HatBaseClass b, Note n)
           => [a]
           -> [c]
           -> (a -> c -> Bool)
           -> (a -> c -> Journal n v b)
           -> Journal n v b
sigma2When xs ys cond f =
    fromMap $ L.foldl' outer Map.empty xs
  where
    outer !acc !x = L.foldl' (inner x) acc ys
    inner !x !acc !y
        | cond x y = mergeJournalMapIfNonZero acc (f x y)
        | otherwise = acc

-- | Sum each list element as an Alg on the specified Note and store the result in a Journal.
-- Returns an empty Journal if the result is zero.
--
-- Complexity: O(|xs| * union cost)
{-# INLINE sigmaOn #-}
sigmaOn :: (HatVal v, HatBaseClass b, Note n)
        => n
        -> [a]
        -> (a -> Alg v b)
        -> Journal n v b
sigmaOn n xs f =
    let !alg = EA.sigma xs f
    in if EA.isZero alg
        then mempty
        else alg .| n

-- | Sum Alg values from Map keys and values on the specified Note and store the result in a Journal.
-- Map version of 'sigmaOn'.
--
-- Complexity: O(|map| * union cost)
{-# INLINE sigmaOnFromMap #-}
sigmaOnFromMap :: (HatVal v, HatBaseClass b, Note n, Ord k)
               => n
               -> M.Map k v
               -> (k -> v -> Alg v b)
               -> Journal n v b
sigmaOnFromMap n kvs f =
    let !alg = EA.sigmaFromMap kvs f
    in if EA.isZero alg
        then mempty
        else alg .| n

-- | Quotient decomposition into the Journal (dec_κ landing on the graded
-- carrier): partition an 'Alg' along the classes induced by a classifier and
-- return the family as a t'Journal' keyed by the class 'Note'.
--
-- A t'Journal' is exactly a finite map @Note → Alg@ (paper Definition 12), i.e.
-- the library's native \"keyed family of algebras\" — so the decomposition
-- stays inside the algebra vocabulary (no external container in the result).
-- Each note's entry is the redundancy-preserving restriction of the input to
-- that class (same guarantees as 'EA.decBy'): no 'bar', no aggregation, and
-- @norm (decTo keyOf x) + norm residual == norm x@ (norm additivity of the
-- decomposition).
--
-- Entries classified to @Nothing@ or to 'plank' are dropped as residual
-- ('plank' is the blank note and cannot carry a class).
--
-- Complexity: O(m) single pass (via 'EA.decBy') + O(k) journal construction.
--
-- >>> type TJ = Journal String Double (HatBase AccountTitles)
-- >>> let alg = 100 :@ Not:<Cash .+ 30 :@ Hat:<Cash .+ 50 :@ Not:<Deposits :: Alg Double (HatBase AccountTitles)
-- >>> let j = decTo (\(_ :< a) -> Just (if a == Cash then "cash" else "other")) alg :: TJ
-- >>> norm j
-- 180.0
--
-- >>> norm (projWithNote ["cash"] j)
-- 130.0
{-# INLINE decTo #-}
decTo :: (HatVal v, HatBaseClass b, Note n)
      => (b -> Maybe n)
      -> Alg v b
      -> Journal n v b
decTo kf x =
    fromMap $ Map.fromList
        [ (n, alg)
        | (n, alg) <- M.toList (EA.decBy kf x)
        , not (isPlank n) ]

-- | Summation in a monadic context. Applies a monadic function to each element and mconcats the results.
--
-- Complexity: O(|xs| * cost(f))
--
-- NOTE: kept as @mconcat <$> forM xs f@ deliberately. A strict @foldM@ left fold
-- was tried (plan WI-3) but it changes the '<>' association order, which for
-- 'Alg'/t'Journal' reorders the audit-trail sequence and (via non-associative
-- 'Double' addition) shifts 'norm' results. Although the 'Monoid' laws make the
-- value equal in exact arithmetic, it is observably different under floating point.
-- See plans/in-progress/LAZY_EVAL_AUDIT.md (WI-3).
sigmaM :: (Monoid m, Monad m0) => [a] -> (a -> m0 m) -> m0 m
sigmaM xs f = mconcat <$> CM.forM xs f

------------------------------------------------------------------
-- | Combine entries from all Notes in a Journal into a single Alg.
--
-- Complexity: O(total number of base keys across all Notes)
toAlg :: (HatVal v, HatBaseClass b, Note n)
      => Journal n v b -> Alg v b
toAlg (Journal bs delta _ _) =
    -- Fold base's elements directly onto delta's element list instead of
    -- @Map.elems base ++ Map.elems delta@, which avoids materializing the
    -- separate @Map.elems base@ list and the @(++)@ traversal.
    EA.unionsMerge (Map.foldr (:) (Map.elems delta) bs)

------------------------------------------------------------------
-- | Apply function f to the entry of each Note in the Journal.
-- Applies to merged Note entries (base + delta), preserving semantics.
--
-- Complexity: O(j * cost(f)) where j is the number of Notes
map :: (HatVal v, HatBaseClass b, Note n)
    => (Alg v b -> Alg v b) -> Journal n v b -> Journal n v b
map f = fromMap . Map.map f . toMap

-- NB. The unused 'parallelMap'\/'parMap' helpers (a 'Control.Parallel.Strategies'
-- based variant of 'map') were removed as dead code: neither was exported nor
-- called. Reintroduce from history if a parallel journal map is needed.

-- | Insert x into y. If x's Note already exists in y, it is overwritten with x's value.
--
-- >>> type Test = Journal String Double (HatBase AccountTitles)
-- >>> x = 10.00:@Not:<Cash .| "A" :: Test
-- >>> y = 20.00:@Not:<Cash .| "B" :: Test
-- >>> z = 30.00:@Hat:<Cash .| "A" :: Test
-- >>> insert z (x .+ y)
-- 20.00:@Not:<Cash.|"B" .+ 30.00:@Hat:<Cash.|"A"
insert :: (HatVal v, HatBaseClass b, Note n)
        => Journal n v b -> Journal n v b -> Journal n v b
-- Complexity: O(n + m) where n, m are the number of Notes in each Journal
insert x y = fromMap (Map.union (toMap x) (toMap y))

------------------------------------------------------------------
-- | projWithNote
-- Projecting with Note.
--
-- >>> type Test = Journal String Double (HatBase CountUnit)
-- >>> x = 1.00:@Hat:<Yen .+ 1.00:@Not:<Amount .| "cat"  :: Test
-- >>> y = 2.00:@Hat:<Yen .+ 2.00:@Not:<Amount .| "dog"  :: Test
-- >>> z = 3.00:@Hat:<Yen .+ 3.00:@Not:<Amount .| "fish" :: Test
-- >>> projWithNote ["dog","cat"] (x .+ y .+ z)
-- 1.00:@Not:<Amount.|"cat" .+ 1.00:@Hat:<Yen.|"cat" .+ 2.00:@Not:<Amount.|"dog" .+ 2.00:@Hat:<Yen.|"dog"
projWithNote :: (HatVal v, HatBaseClass b, Note n)
             => [n] -> Journal n v b -> Journal n v b
projWithNote ns js
    | any isPlank ns = js
projWithNote [n] js = fromMap $ case lookupNote n js of
    Nothing -> Map.empty
    Just a  -> Map.singleton n a
projWithNote ns js =
    fromMap $
      S.foldl'
        (\acc n -> case lookupNote n js of
            Nothing -> acc
            Just a  -> Map.insert n a acc)
        Map.empty
        (S.fromList ns)

------------------------------------------------------------------
-- | projWithBase
-- Projecting with Base.
--
-- >>> type Test = Journal String Double (HatBase CountUnit)
-- >>> x = 1.00:@Hat:<Yen .+ 1.00:@Not:<Amount .| "cat"  :: Test
-- >>> y = 2.00:@Not:<Yen .+ 2.00:@Hat:<Amount .| "dog"  :: Test
-- >>> z = 3.00:@Hat:<Yen .+ 3.00:@Not:<Amount .| "fish" :: Test
-- >>> projWithBase [Not:<Amount] (x .+ y .+ z)
-- 3.00:@Not:<Amount.|"fish" .+ 1.00:@Not:<Amount.|"cat"
projWithBase :: (HatVal v, HatBaseClass b, Note n)
             => [b] -> Journal n v b -> Journal n v b
{-# INLINE projWithBase #-}
projWithBase [] _ = mempty
projWithBase bs js = fromMap $ Map.map (EA.proj bs) (toMap js)

-- | Directly compute the /bar-netted/ norm of a base projection, without
-- constructing an intermediate Journal. Per note this applies 'EA.projNetNorm',
-- which nets each projected base's hat and not sides (the positive-part
-- normalization); hence
--
-- @projWithBaseNetNorm bs js == norm (map bar (projWithBase bs js))@
--
-- which is __not__ the same as @norm (projWithBase bs js)@ when a query
-- (e.g. a @HatNot@ wildcard, or a list selecting both sides of one base)
-- selects both the hat and the not side of a base: the un-netted norm sums
-- both sides, the netted one cancels them. See 'EA.projNetNorm'.
--
-- Complexity: O(j * proj cost) where j is the number of Notes
projWithBaseNetNorm :: (HatVal v, HatBaseClass b, Note n)
                 => [b] -> Journal n v b -> v
projWithBaseNetNorm [] _ = 0
projWithBaseNetNorm bs js =
    Map.foldl' (\acc alg -> acc + EA.projNetNorm bs alg) 0 (toMap js)

------------------------------------------------------------------
-- | projWithNoteBase
-- Projecting with Note and Base.
--
-- >>> type Test = Journal String Double (HatBase CountUnit)
-- >>> x = 1.00:@Hat:<Yen .+ 1.00:@Not:<Amount .| "cat"  :: Test
-- >>> y = 2.00:@Not:<Yen .+ 2.00:@Hat:<Amount .| "dog"  :: Test
-- >>> z = 3.00:@Hat:<Yen .+ 3.00:@Not:<Amount .| "fish" :: Test
-- >>> projWithNoteBase ["dog","fish"] [Not:<Amount] (x .+ y .+ z)
-- 3.00:@Not:<Amount.|"fish"
projWithNoteBase :: (HatVal v, HatBaseClass b, Note n)
                 => [n] -> [b] -> Journal n v b -> Journal n v b
{-# INLINE projWithNoteBase #-}
projWithNoteBase _ [] _ = mempty
projWithNoteBase ns bs js
    | any isPlank ns = projWithBase bs js
projWithNoteBase [n] bs js = fromMap $ case lookupNote n js of
    Nothing -> Map.empty
    Just a  -> Map.singleton n (EA.proj bs a)
projWithNoteBase [] bs js = projWithBase bs js
projWithNoteBase ns bs js =
    fromMap $
      S.foldl'
        (\acc n -> case lookupNote n js of
            Nothing -> acc
            Just a  -> Map.insert n (EA.proj bs a) acc)
        Map.empty
        (S.fromList ns)

-- | Directly compute the /bar-netted/ norm of a note-and-base projection,
-- without constructing an intermediate Journal. Like 'projWithBaseNetNorm' this
-- goes through 'EA.projNetNorm', so per note each projected base is netted
-- (positive-part normalization):
--
-- @projWithNoteBaseNetNorm ns bs js == norm (map bar (projWithNoteBase ns bs js))@
--
-- which is __not__ the same as @norm (projWithNoteBase ns bs js)@ when a query
-- selects both sides of one base (see 'projWithBaseNetNorm').
--
-- Complexity: O(|ns| * proj cost)
projWithNoteBaseNetNorm :: (HatVal v, HatBaseClass b, Note n)
                 => [n] -> [b] -> Journal n v b -> v
projWithNoteBaseNetNorm _ [] _ = 0
projWithNoteBaseNetNorm ns bs js
    | any isPlank ns = projWithBaseNetNorm bs js
projWithNoteBaseNetNorm [n] bs js = case lookupNote n js of
    Nothing -> 0
    Just a  -> EA.projNetNorm bs a
projWithNoteBaseNetNorm [] bs js = projWithBaseNetNorm bs js
projWithNoteBaseNetNorm ns bs js =
    S.foldl'
        (\acc n -> case lookupNote n js of
            Nothing -> acc
            Just a  -> acc + EA.projNetNorm bs a)
        0
        (S.fromList ns)

-- NB. Two RULES that rewrote @norm (projWithBase bs js)@ to 'projWithBaseNetNorm'
-- (and the note-base analogue) were REMOVED here: the equation is false whenever
-- a query selects both sides of one base (e.g. a @HatNot@ wildcard) — the
-- left-hand side sums both sides, the right-hand side bar-nets them (verified:
-- 14.0 vs 6.0 on a both-sided base). A rewrite rule must be semantics-preserving;
-- callers who want the fused netted read-out call 'projWithBaseNetNorm' \/
-- 'projWithNoteBaseNetNorm' explicitly.

{-# DEPRECATED projWithBaseNorm "renamed to 'projWithBaseNetNorm': the result is bar-netted per base, which the old name concealed. Will be removed in 0.6" #-}
-- | Deprecated alias for 'projWithBaseNetNorm' (renamed in 0.5.0.0 so the name
-- states the bar-netting).
projWithBaseNorm :: (HatVal v, HatBaseClass b, Note n)
                 => [b] -> Journal n v b -> v
projWithBaseNorm = projWithBaseNetNorm

{-# DEPRECATED projWithNoteNorm "renamed to 'projWithNoteBaseNetNorm': it takes note AND base queries and the result is bar-netted per base — both were missing from the old name. Will be removed in 0.6" #-}
-- | Deprecated alias for 'projWithNoteBaseNetNorm' (renamed in 0.5.0.0 so the
-- name states both the base argument and the bar-netting).
projWithNoteNorm :: (HatVal v, HatBaseClass b, Note n)
                 => [n] -> [b] -> Journal n v b -> v
projWithNoteNorm = projWithNoteBaseNetNorm

------------------------------------------------------------------
-- | Filter by a predicate on Note-entry pairs.
-- Applies the filter to both the base and delta layers.
--
-- Complexity: O(n) where n is the number of Notes
filterWithNote :: (HatVal v, HatBaseClass b, Note n)
               => (n -> Alg v b -> Bool) -> Journal n v b -> Journal n v b
filterWithNote f (Journal bs delta _ _) =
    let !base' = Map.filterWithKey f bs
        !delta' = Map.filterWithKey f delta
    in mkJournal base' delta'

-- | Efficiently filter a Journal to entries whose Note matches on the specified axis.
-- Uses base/delta NoteAxisPosting indices for O(|result|) retrieval after index construction.
--
-- Axis numbers are 0-indexed. For a Note type @(EventName, Term)@:
--
--   * axis 0 corresponds to EventName
--   * axis 1 corresponds to Term
--
-- For non-tuple Note types, axis 0 is the only valid axis.
--
-- Complexity: O(|result|) after index construction; O(n) for first base-axis query on a Journal value
--
-- >>> type Test = Journal (String, Int) Double (HatBase AccountTitles)
-- >>> x = 10.00:@Not:<Cash .| ("A", 1) :: Test
-- >>> y = 20.00:@Hat:<Cash .| ("B", 1) :: Test
-- >>> z = 30.00:@Not:<Cash .| ("A", 2) :: Test
-- >>> filterByAxis 0 (NoteAxisKey "A") (x .+ y .+ z)
-- 10.00:@Not:<Cash.|("A",1) .+ 30.00:@Not:<Cash.|("A",2)
--
-- >>> filterByAxis 1 (NoteAxisKey (1 :: Int)) (x .+ y .+ z)
-- 10.00:@Not:<Cash.|("A",1) .+ 20.00:@Hat:<Cash.|("B",1)
{-# INLINE filterByAxis #-}
filterByAxis :: (HatVal v, HatBaseClass b, Note n)
             => Int -> NoteAxisKey -> Journal n v b -> Journal n v b
filterByAxis axis key j@(Journal _ _ baseIdx deltaIdx) =
    let !matched = HSet.union
            (queryNoteAxisPosting axis key baseIdx)
            (queryNoteAxisPosting axis key deltaIdx)
        !result = HSet.foldl'
            (\acc n -> case lookupNote n j of
                Nothing  -> acc
                Just alg -> Map.insert n alg acc)
            Map.empty
            matched
    in fromMap result

------------------------------------------------------------------
-- | gather
-- Gathers all Alg into one on the given Note.
--
-- >>> type Test = Journal String Double (EA.HatBase EA.AccountTitles)
-- >>> x = 20.00:@Not:<Cash .+ 20.00:@Hat:<Deposits .| "Withdrawal" :: Test
-- >>> y = 10.00:@Hat:<Cash .+ 10.00:@Not:<Deposits .| "Deposits" :: Test
-- >>> gather "A" (x .+ y)
-- 10.00:@Not:<Deposits.|"A" .+ 20.00:@Hat:<Deposits.|"A" .+ 20.00:@Not:<Cash.|"A" .+ 10.00:@Hat:<Cash.|"A"
gather :: (HatVal v, HatBaseClass b, Note n)
       => n -> Journal n v b -> Journal n v b
gather n js = (toAlg js) .| n
