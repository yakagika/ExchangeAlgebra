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
{-# LANGUAGE TypeFamilies               #-}

-- | Generic journals, note indices, and redundancy-preserving operations.
-- This Journal layer owns the representation and instances for Definitions 10-12.
-- It uses generic Algebra owners; "ExchangeAlgebra.Journal.Core" exposes the
-- public operations. Read the note definitions before the journal operations.
module ExchangeAlgebra.Journal.Core.Representation ( Note(..)
                                                   , HasTermAxis(..)
                                                   , NoteAxisKey(..)
                                                   , NoteAxisPosting
                                                   , Journal
                                                   , pattern Zero
                                                   , mkJournal
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
                                                   , replaceNotes
                                                   , projWithNote
                                                   , projWithBase
                                                   , projWithNoteBase
                                                   , projWithBaseNetNorm
                                                   , projWithNoteBaseNetNorm
                                                   , filterWithNote
                                                   , filterByAxis
                                                   , postFromNetBy
                                                   , gather
                                                   ) where

import qualified ExchangeAlgebra.Algebra.Core.Representation as EA
import qualified ExchangeAlgebra.Algebra.Base.Representation as EA
import qualified ExchangeAlgebra.Algebra.Element.Representation as EA
import qualified ExchangeAlgebra.Algebra.Value.Class as EA
import ExchangeAlgebra.Algebra.Base.Representation
import ExchangeAlgebra.Algebra.Element.Representation
import ExchangeAlgebra.Algebra.Value.Class (HatVal(..))
import ExchangeAlgebra.Algebra.Value (MoneyDouble, MoneyDecimal)
import ExchangeAlgebra.Algebra.Core.Representation (Alg, pattern (:@), Redundant(..), (.@))
import              Prelude                 hiding (map, filter)
import qualified    Prelude
import qualified    Data.HashMap.Strict     as Map
import qualified    Data.IntMap.Strict      as IntMap
import              Data.IntMap.Strict      (IntMap)
import qualified    Data.HashSet            as HSet
import              Data.HashSet            (HashSet)
import              Control.DeepSeq             (NFData, rnf)
import qualified    Data.Set                as S
import qualified    Data.List               as L
import qualified    Data.Map.Strict         as M
import qualified    Data.Text               as T
import qualified    Control.Monad           as CM
import qualified    Data.Binary             as Binary
import              Data.Typeable           (Typeable, cast, typeOf)

-- $setup
-- >>> :set -XOverloadedStrings

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
-- A note's /event/ axis is best modeled as a small enumeration (an ADT)
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
-- (The @SimEvent@ note in the test suite follows this pattern.)
-- If the ledger is ever spilled\/restored, also give the tag a
-- @Binary@ instance (structurally derivable from @Generic@).
class (Show a, Eq a, Ord a, Hashable a, Typeable a) => Note a where
    plank :: a
    isPlank :: a -> Bool
    isPlank x = x == plank
    -- | Decompose a Note into per-axis keys for AxisPosting indexing.
    -- Default: single axis with the Note itself.
    toNoteAxisKeys :: a -> [NoteAxisKey]
    toNoteAxisKeys a = [NoteAxisKey a]

-- | The convention that fixes /which/ part of a 'Note' is the simulation term.
--
-- A simulation policy or a multi-period ledger maps a Note to its term to
-- decide which entries belong to a period. Since a tuple Note has no intrinsic "term" component, the rule
-- is fixed here: __the term is the last component of the Note__. The shipped
-- instances follow this rule; a custom Note type provides a one-line instance.
--
-- >>> termOf ("buy" :: T.Text, 7 :: Int)
-- 7
--
-- >>> termOf ("buy" :: T.Text, "shopA" :: T.Text, 3 :: Int)
-- 3
class (Note n, Ord (TermOf n)) => HasTermAxis n where
  -- | The term type extracted from the Note.
  type TermOf n
  -- | Extract the term (last Note component, by the library convention).
  termOf :: n -> TermOf n

-- | Pair Note: the term is the second (last) component.
instance (Note e, Note t) => HasTermAxis (e, t) where
  type TermOf (e, t) = t
  termOf = snd

-- | Triple Note: the term is the third (last) component.
instance (Note e1, Note e2, Note t) => HasTermAxis (e1, e2, t) where
  type TermOf (e1, e2, t) = t
  termOf (_, _, t) = t

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
--   __Invariants.__ The constructor is not exported; build with 'mkJournal',
--   '(.|)' or 'fromList'. Its internal cache\/index fields @_jBaseAxis@ and
--   @_jDeltaAxis@ must be
--   exactly the Note axis indices (@buildNoteAxisPosting@) of @_jBase@ and
--   @_jDelta@ respectively. The axis-filtered query path ('filterByAxis') reads
--   those indices, so a value whose indices disagree with its maps yields wrong
--   answers silently (not an exception).
--
--   __Normal form.__ Neither layer holds a note whose algebra is zero
--   ('EA.isZero'). The redundant '(.+)' never cancels terms, so the sum of two
--   nonzero algebras is nonzero, and the merged entry of a note ('toMap') is
--   never zero either. Every public operation returns a journal in this form:
--   the operations that can produce a zero note ('map', 'projWithBase',
--   'projWithNoteBase') drop it while they traverse the notes, and
--   'mkJournal', 'fromMap' and the @Binary@ decoder drop the zero notes they
--   are given. Hence a journal without postings always matches 'Zero', and
--   the keys of 'toMap', 'Show' and @Binary@ do not depend on how a journal
--   was built. Postings, balances and readouts are unaffected, since a zero
--   note holds no posting.
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

-- | The only safe way to build a t'Journal' from a base layer and a delta
-- layer. It keeps the Note-axis indices consistent with both layers and
-- returns the normal form; see the invariants note on t'Journal'.
--
-- A note whose algebra is zero ('EA.isZero') is dropped from the layer that
-- holds it. Both maps are read once; a map without a zero note is used as
-- given, and only a map that holds one is rebuilt without it.
--
-- Complexity: O(n) where n is the number of notes in both layers; each note's
-- algebra is evaluated to WHNF. The base axis index is lazy, while the delta
-- axis index is built eagerly.
{-# INLINE mkJournal #-}
mkJournal :: (Note n, HatVal v, HatBaseClass b)
          => Map.HashMap n (Alg v b) -> Map.HashMap n (Alg v b) -> Journal n v b
mkJournal bs delta = mkJournalNonZero base' delta'
  where
    base'
        | any EA.isZero bs = Map.filter (not . EA.isZero) bs
        | otherwise       = bs
    delta'
        | any EA.isZero delta = Map.filter (not . EA.isZero) delta
        | otherwise          = delta

-- Internal constructor for callers that already hold the normal form (see the
-- invariants note on t'Journal'): neither map may hold a note whose algebra is
-- zero. It does not check this. It builds the Note-axis indices as
-- 'mkJournal' does: the base axis index lazily, the delta axis index eagerly.
{-# INLINE mkJournalNonZero #-}
mkJournalNonZero :: (Note n, HatVal v, HatBaseClass b)
                 => Map.HashMap n (Alg v b) -> Map.HashMap n (Alg v b) -> Journal n v b
mkJournalNonZero bs delta = Journal bs delta baseIdx deltaIdx
  where
    ~baseIdx = buildNoteAxisPosting bs
    !deltaIdx = buildNoteAxisPosting delta

-- | Construct a Journal from a HashMap. A note whose algebra is zero is
-- dropped (see 'mkJournal').
--
-- Complexity: O(n) where n is the number of notes; the map is rebuilt only
-- when it holds a zero note. Base axis index is built lazily on first axis
-- query.
--
-- >>> case fromMap (Map.singleton "empty" EA.Zero) :: Journal T.Text Double (HatBase CountUnit) of { Zero -> True; _ -> False }
-- True
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
    | otherwise = mkJournalNonZero (Map.unionWith (.+) bs delta) Map.empty

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

-- | Serialize a journal as its note count followed by each note and its
-- algebra. Decoding checks each algebra as the 'Alg' instance does and fails
-- on a repeated note. The encoder never writes a note whose algebra is zero
-- (see the normal form on t'Journal'); the decoder drops one written by an
-- older encoder instead of rejecting the bytes, so they still decode.
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
        count <- Binary.get :: Binary.Get Int
        fromMap <$> go count Map.empty
      where
        go :: (Note note, Binary.Binary note, Binary.Binary algebra)
           => Int -> Map.HashMap note algebra -> Binary.Get (Map.HashMap note algebra)
        go remaining !acc
            | remaining <= 0 = pure acc
            | otherwise = do
                note    <- Binary.get
                algebra <- Binary.get
                CM.when (Map.member note acc) (fail "Journal: repeated note")
                go (remaining - 1) (Map.insert note algebra acc)

-- | Test whether the Journal is empty (zero).
--
-- Complexity: O(1)
isZero :: (HatVal v, HatBaseClass b, Note n)
       => Journal n v b -> Bool
isZero (Journal bs delta _ _) = Map.null bs && Map.null delta

-- | Construct an empty journal or match a journal with no postings.
pattern Zero :: (HatVal v, HatBaseClass b, Note n) => Journal n v b
pattern Zero <- (isZero -> True)
    where
        Zero = mkJournalNonZero Map.empty Map.empty

-- | Smart constructor that attaches a Note (annotation) to an algebra element to build a Journal.
-- Returns an empty Journal when the algebra element is zero, consistent with
-- the zero-value handling in @appendMap@ and 'sigmaOn'.
--
-- Complexity: O(1)
(.|) :: (HatVal v, HatBaseClass b, Note n)
      => Alg v b -> n -> Journal n v b
(.|) alg n
    | EA.isZero alg = mempty
    | otherwise    = mkJournalNonZero Map.empty (Map.singleton n alg)

infixr 2 .|

------------------------------------------------------------------
-- Show
------------------------------------------------------------------
-- | Render each non-blank note as the sum of its algebra entries followed by
-- one '(.|)'. A single group needs no parentheses because '(.+)' binds more
-- tightly than '(.|)'; multiple groups are parenthesized individually so the
-- result can be pasted back as an expression with the current fixities.
--
-- The blank note remains invisible when it is the only group. In a mixed
-- journal it is rendered explicitly, so every summand is one complete group.
--
-- >>> type ShowTest = Journal T.Text Double (HatBase CountUnit)
-- >>> (mempty :: ShowTest)
-- 0
-- >>> (5.00.@Not:<Yen .| plank :: ShowTest)
-- 5.00:@Not:<Yen
-- >>> one = 20.00.@Not:<Yen .+ 20.00.@Hat:<Dollar .| "N" :: ShowTest
-- >>> one
-- 20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen .| "N"
-- >>> toMap (20.00.@Hat:<Dollar .+ 20.00.@Not:<Yen .| "N" :: ShowTest) == toMap one
-- True
-- >>> two = 10.00.@Hat:<Yen .| "M" :: ShowTest
-- >>> show (one .+ two) `elem` ["(10.00:@Hat:<Yen .| \"M\") .+ (20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen .| \"N\")", "(20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen .| \"N\") .+ (10.00:@Hat:<Yen .| \"M\")"]
-- True
-- >>> toMap ((10.00.@Hat:<Yen .| "M") .+ (20.00.@Hat:<Dollar .+ 20.00.@Not:<Yen .| "N") :: ShowTest) == toMap (one .+ two)
-- True
-- >>> blank = 5.00.@Not:<Yen .| plank :: ShowTest
-- >>> show (one .+ blank) `elem` ["(20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen .| \"N\") .+ (5.00:@Not:<Yen .| \"\")", "(5.00:@Not:<Yen .| \"\") .+ (20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen .| \"N\")"]
-- True
instance (HatVal v, HatBaseClass b, Note n) => Show (Journal n v b) where
    showsPrec d js =
        case groups of
            [] -> showString "0"
            [(k, a)]
                | isPlank k -> showsPrec d a
                | otherwise -> showParen (d > 2) (showsGroup k a)
            _ -> showParen (d > 3) (showsGroups groups)
      where
        m = toMap js
        groups = reverse $ Map.foldrWithKey collectGroup [] m

        collectGroup k a acc
            | not (isPlank k) && Prelude.null (EA.toASCList a) = acc
            | otherwise = (k, a) : acc

        showsGroups [] = id
        showsGroups [(k, a)] = showParen True (showsGroup k a)
        showsGroups ((k, a):rest) =
            showParen True (showsGroup k a)
            . showString " .+ "
            . showsGroups rest

        showsGroup k a =
            showsGroupAlg k a
            . showString " .| "
            . showsPrec 3 k

        showsGroupAlg k a
            | isPlank k = showsPrec 3 a
            | otherwise = showsAlgEntries (reverse (EA.toASCList a))

        showsAlgEntries [] = showString "0"
        showsAlgEntries [x] = showsPrec 3 x
        showsAlgEntries (x:xs) =
            showsPrec 3 x
            . showString " .+ "
            . showsAlgEntries xs
------------------------------------------------------------------

instance (HatVal v, HatBaseClass b, Note n) => Semigroup (Journal n v b) where
    {-# INLINE (<>) #-}
    (<>) = addJournal

-- | Journal addition. Appends right-hand entries to the left-hand side.
--
-- Complexity: Amortized O(size(rhs)); O(n) compaction when the delta exceeds the threshold
--
-- >>> type Test = Journal T.Text Double (HatBase CountUnit)
-- >>> x = 20.00.@Not:<Yen .+ 20.00.@Hat:<Dollar .| "Withdrawal" :: Test
-- >>> y = 10.00.@Hat:<Yen .+ 10.00.@Not:<Dollar .| "Deposits" :: Test
-- >>> toMap (x .+ y) == toMap ((10.00.@Not:<Dollar .+ 10.00.@Hat:<Yen .| "Deposits") .+ (20.00.@Hat:<Dollar .+ 20.00.@Not:<Yen .| "Withdrawal") :: Test)
-- True
addJournal :: (HatVal v, HatBaseClass b, Note n)
           => Journal n v b -> Journal n v b -> Journal n v b
addJournal lhs rhs = appendMap (toMap rhs) lhs

instance (HatVal v, HatBaseClass b, Note n) => Monoid (Journal n v b) where
    mempty = mkJournalNonZero Map.empty Map.empty
    mappend = (<>)

-- | Shallow-structural 'NFData', mirroring the @'Alg' v b@ instance in
-- "ExchangeAlgebra.Algebra.Core": it forces the two-layer @base@ / @delta@ maps to
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
    {-# SPECIALIZE instance (HatBaseClass b, Note n) => Redundant (Journal n) Double b #-}
    {-# SPECIALIZE instance (HatBaseClass b, Note n) => Redundant (Journal n) MoneyDouble b #-}
    {-# SPECIALIZE instance (HatBaseClass b, Note n) => Redundant (Journal n) MoneyDecimal b #-}
    barWithin tolerance x = map (barWithin tolerance) (gather plank x)
    (.^) = map (.^)
    (.+) = mappend
    (.*) x  = map ((.*) x)
    norm = norm . toAlg
    (.-) x = map (.-) (gather plank x)
    compress = map compress

------------------------------------------------------------------
-- | Build a t'Journal' from a list of postings. @O(N)@ via a strict left fold
-- (@L.foldl' (.+) mempty@).
--
-- == Why a strict left fold
--
-- This was previously a lazy right fold (@foldr (.+) mempty@), kept that way to
-- freeze the same-base accumulation order for 'Double' (whose addition is
-- non-associative, so reordering affected the former sequential 'norm'). The strict left
-- fold is far cheaper at scale — the lazy right fold builds a deep right-nested
-- thunk that is expensive to force (≈40x at N=20000, ~15x at N=10000 in the core
-- benchmark). The fold direction was switched to the fast version once the
-- audited/exact path moved to t'ExchangeAlgebra.Algebra.Value.MoneyDecimal'; see
-- plans/in-progress/LAZY_EVAL_AUDIT.md and SELECTABLE_VALUE_TYPE_PLAN.md.
--
-- == Behavior contract
--
-- 'fromList' preserves the /multiset/ of postings exactly (no posting is added,
-- dropped, or merged across bases that would otherwise be kept apart). When two
-- postings collide on the /same note key and same base/ (and therefore land in
-- one t'ExchangeAlgebra.Algebra.Core.Alg' sequence) the strict left fold orders that
-- @Seq@ opposite to the old lazy right fold. That order is observable through
-- 'Eq' \/ 'Show' \/ @Binary@, and for 'Double' through the sequential
-- sums in 'barWithin' (IEEE-754 addition is non-associative). Postings that differ in
-- note or base land in separate map entries and are unaffected.
--
-- For finite postings, 'norm', 'bar', and balance are independent of
-- construction order for every value type, including Double and MoneyDouble.
--
-- >>> type Test = Journal T.Text Double (HatBase CountUnit)
-- >>> x = [(1.00.@Hat:<Yen .| z) | z <- ["Loan Payment","Purchace Apple"]] :: [Test]
-- >>> toMap (fromList x) == toMap ((1.00.@Hat:<Yen .| "Purchace Apple") .+ (1.00.@Hat:<Yen .| "Loan Payment") :: Test)
-- True
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
sigma xs f = (`mkJournalNonZero` Map.empty) $ L.foldl' step Map.empty xs
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
    (`mkJournalNonZero` Map.empty) $ L.foldl' outer Map.empty xs
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
-- @norm (decTo keyOf x) + norm residual == norm x@ before rounding, or for
-- value types with exact sums. Rounded Double values need not satisfy this
-- decomposition law: with T = 2^53, put Not T and Not 1 on Cash and Not 1
-- on Sales, and classify Cash only. The left side is T, but norm x is T+2.
--
-- Entries classified to @Nothing@ or to 'plank' are dropped as residual
-- ('plank' is the blank note and cannot carry a class).
--
-- Complexity: O(m) single pass (via 'EA.decBy') + O(k) journal construction.
--
-- >>> type TJ = Journal T.Text Double (HatBase CountUnit)
-- >>> let alg = 100 .@ Not:<Yen .+ 30 .@ Hat:<Yen .+ 50 .@ Not:<Dollar :: Alg Double (HatBase CountUnit)
-- >>> let j = decTo (\(_ :< a) -> Just (if a == Yen then "cash" else "other")) alg :: TJ
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
    (`mkJournalNonZero` Map.empty) $ Map.fromList
        [ (n, alg)
        | (n, alg) <- M.toList (EA.decBy kf x)
        , not (isPlank n) ]

-- | Summation in a monadic context. Applies a monadic function to each element and mconcats the results.
--
-- Complexity: O(|xs| * cost(f))
--
-- NOTE: kept as @mconcat <$> forM xs f@ deliberately. A strict @foldM@ left fold
-- was tried (plan WI-3) but it changes the '<>' association order, which for
-- 'Alg'/t'Journal' reorders the audit-trail sequence. The order is observable
-- through Eq, Show, Binary, and the sequential sums in barWithin. For finite
-- postings, norm, bar, and balance are order-independent for every value type.
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
-- A note for which @f@ returns zero is dropped (the normal form on
-- t'Journal'), in the same traversal.
--
-- Complexity: O(j * cost(f)) where j is the number of Notes
--
-- >>> case map (const EA.Zero) (1 .@ Not:<Yen .| "entry" :: Journal T.Text Double (HatBase CountUnit)) of { Zero -> True; _ -> False }
-- True
map :: (HatVal v, HatBaseClass b, Note n)
    => (Alg v b -> Alg v b) -> Journal n v b -> Journal n v b
map f js = mkJournalNonZero result Map.empty
  where
    result = Map.mapMaybe (\a -> case f a of
        projected | EA.isZero projected -> Nothing
                  | otherwise           -> Just projected) (toMap js)

-- NB. The unused 'parallelMap'\/'parMap' helpers (a 'Control.Parallel.Strategies'
-- based variant of 'map') were removed as dead code: neither was exported nor
-- called. Reintroduce from history if a parallel journal map is needed.

-- | Left-biased replacement of whole Notes. If a Note in the left Journal also
-- exists in the right Journal, the complete left value replaces the right
-- value. This is not redundant addition; contrast with '(.+)'.
--
-- Complexity: O(n + m) where n, m are the number of Notes in each Journal
--
-- >>> type Test = Journal T.Text Double (HatBase CountUnit)
-- >>> x = 10.00.@Not:<Yen .| "A" :: Test
-- >>> y = 20.00.@Not:<Yen .| "B" :: Test
-- >>> z = 30.00.@Hat:<Yen .| "A" :: Test
-- >>> toMap (replaceNotes z (x .+ y)) == toMap (y .+ z)
-- True
replaceNotes :: (HatVal v, HatBaseClass b, Note n)
             => Journal n v b -> Journal n v b -> Journal n v b
replaceNotes x y = mkJournalNonZero (Map.union (toMap x) (toMap y)) Map.empty

------------------------------------------------------------------
-- | projWithNote
-- Projecting with Note.
--
-- A query list containing 'plank' acts as a __wildcard__: the journal is
-- returned unchanged (the blank note matches every note).
--
-- >>> type Test = Journal T.Text Double (HatBase CountUnit)
-- >>> x = 1.00.@Hat:<Yen .+ 1.00.@Not:<Amount .| "cat"  :: Test
-- >>> y = 2.00.@Hat:<Yen .+ 2.00.@Not:<Amount .| "dog"  :: Test
-- >>> z = 3.00.@Hat:<Yen .+ 3.00.@Not:<Amount .| "fish" :: Test
-- >>> toMap (projWithNote ["dog","cat"] (x .+ y .+ z)) == toMap ((1.00.@Not:<Amount .+ 1.00.@Hat:<Yen .| "cat") .+ (2.00.@Not:<Amount .+ 2.00.@Hat:<Yen .| "dog") :: Test)
-- True
projWithNote :: (HatVal v, HatBaseClass b, Note n)
             => [n] -> Journal n v b -> Journal n v b
projWithNote ns js
    | any isPlank ns = js
projWithNote [n] js = (`mkJournalNonZero` Map.empty) $ case lookupNote n js of
    Nothing -> Map.empty
    Just a  -> Map.singleton n a
projWithNote ns js =
    (`mkJournalNonZero` Map.empty) $
      S.foldl'
        (\acc n -> case lookupNote n js of
            Nothing -> acc
            Just a  -> Map.insert n a acc)
        Map.empty
        (S.fromList ns)

------------------------------------------------------------------
-- | projWithBase
-- Projecting with Base. A note whose projection is zero is dropped (the
-- normal form on t'Journal').
--
-- >>> type Test = Journal T.Text Double (HatBase CountUnit)
-- >>> x = 1.00.@Hat:<Yen .+ 1.00.@Not:<Amount .| "cat"  :: Test
-- >>> y = 2.00.@Not:<Yen .+ 2.00.@Hat:<Amount .| "dog"  :: Test
-- >>> z = 3.00.@Hat:<Yen .+ 3.00.@Not:<Amount .| "fish" :: Test
-- >>> L.sort [(n, EA.toASCList a) | (n, a) <- Map.toList (toMap (projWithBase [Not:<Amount] (x .+ y .+ z)))]
-- [("cat",[1.00:@Not:<Amount]),("fish",[3.00:@Not:<Amount])]
projWithBase :: (HatVal v, HatBaseClass b, Note n)
             => [b] -> Journal n v b -> Journal n v b
{-# INLINE projWithBase #-}
projWithBase [] _ = mempty
projWithBase bs js = map (EA.proj bs) js

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
-- Each note is netted exactly, and the unrounded residuals of the notes are
-- added and rounded once; the equation holds before rounding, or when sums
-- are represented exactly. Residuals T+1 and 1 in separate notes (T = 2^53)
-- yield T+2 here, but T after rounded note-local bar outputs. Sides cancel
-- only when exactly equal.
--
-- Complexity: O(j * proj cost) where j is the number of Notes
{-# SPECIALIZE projWithBaseNetNorm :: (HatBaseClass b, Note n) => [b] -> Journal n Double b -> Double #-}
{-# SPECIALIZE projWithBaseNetNorm :: (HatBaseClass b, Note n) => [b] -> Journal n MoneyDouble b -> MoneyDouble #-}
{-# SPECIALIZE projWithBaseNetNorm :: (HatBaseClass b, Note n) => [b] -> Journal n MoneyDecimal b -> MoneyDecimal #-}
projWithBaseNetNorm :: (HatVal v, HatBaseClass b, Note n)
                 => [b] -> Journal n v b -> v
projWithBaseNetNorm [] _ = 0
projWithBaseNetNorm bs js =
    roundAccum $ Map.foldl' (\total alg ->
        mergeAccum total (EA.projectedNetState bs alg)) emptyAccum (toMap js)

------------------------------------------------------------------
-- | projWithNoteBase
-- Projecting with Note and Base. A note whose projection is zero is dropped
-- (the normal form on t'Journal').
--
-- 'plank' in the note list acts as a note-wildcard: the projection widens to
-- all notes (falls back to 'projWithBase').
--
-- >>> type Test = Journal T.Text Double (HatBase CountUnit)
-- >>> x = 1.00.@Hat:<Yen .+ 1.00.@Not:<Amount .| "cat"  :: Test
-- >>> y = 2.00.@Not:<Yen .+ 2.00.@Hat:<Amount .| "dog"  :: Test
-- >>> z = 3.00.@Hat:<Yen .+ 3.00.@Not:<Amount .| "fish" :: Test
-- >>> projWithNoteBase ["dog","fish"] [Not:<Amount] (x .+ y .+ z)
-- 3.00:@Not:<Amount .| "fish"
projWithNoteBase :: (HatVal v, HatBaseClass b, Note n)
                 => [n] -> [b] -> Journal n v b -> Journal n v b
{-# INLINE projWithNoteBase #-}
projWithNoteBase _ [] _ = mempty
projWithNoteBase ns bs js
    | any isPlank ns = projWithBase bs js
projWithNoteBase [n] bs js = (`mkJournalNonZero` Map.empty) $ case lookupNote n js of
    Nothing -> Map.empty
    Just a  -> case EA.proj bs a of
        projected | EA.isZero projected -> Map.empty
                  | otherwise           -> Map.singleton n projected
projWithNoteBase [] bs js = projWithBase bs js
projWithNoteBase ns bs js =
    (`mkJournalNonZero` Map.empty) $
      S.foldl'
        (\acc n -> case lookupNote n js of
            Nothing -> acc
            Just a  -> case EA.proj bs a of
                projected | EA.isZero projected -> acc
                          | otherwise           -> Map.insert n projected acc)
        Map.empty
        (S.fromList ns)

-- | Directly compute the /bar-netted/ norm of a note-and-base projection,
-- without constructing an intermediate Journal. Like 'projWithBaseNetNorm' this
-- goes through 'EA.projNetNorm', so per note each projected base is netted
-- (positive-part normalization):
--
-- @projWithNoteBaseNetNorm ns bs js == norm (map bar (projWithNoteBase ns bs js))@
--
-- 'plank' in the note list acts as a note-wildcard (widens to all notes, like
-- 'projWithNote').
--
-- which is __not__ the same as @norm (projWithNoteBase ns bs js)@ when a query
-- selects both sides of one base (see 'projWithBaseNetNorm').
-- The equation holds before rounding, or when sums are represented exactly.
-- Residuals T+1 and 1 in separate notes (T = 2^53) yield T+2 here, but T
-- after rounded note-local bar outputs.
-- Each selected note is netted exactly, and the unrounded residuals are added
-- and rounded once. Sides cancel only when exactly equal.
--
-- Complexity: O(|ns| * proj cost)
{-# SPECIALIZE projWithNoteBaseNetNorm :: (HatBaseClass b, Note n) => [n] -> [b] -> Journal n Double b -> Double #-}
{-# SPECIALIZE projWithNoteBaseNetNorm :: (HatBaseClass b, Note n) => [n] -> [b] -> Journal n MoneyDouble b -> MoneyDouble #-}
{-# SPECIALIZE projWithNoteBaseNetNorm :: (HatBaseClass b, Note n) => [n] -> [b] -> Journal n MoneyDecimal b -> MoneyDecimal #-}
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
    roundAccum $ S.foldl'
        (\total n -> case lookupNote n js of
            Nothing -> total
            Just a  -> mergeAccum total (EA.projectedNetState bs a))
        emptyAccum
        (S.fromList ns)

-- NB. Two RULES that rewrote @norm (projWithBase bs js)@ to 'projWithBaseNetNorm'
-- (and the note-base analogue) were REMOVED here: the equation is false whenever
-- a query selects both sides of one base (e.g. a @HatNot@ wildcard) — the
-- left-hand side sums both sides, the right-hand side bar-nets them (verified:
-- 14.0 vs 6.0 on a both-sided base). A rewrite rule must be semantics-preserving;
-- callers who want the fused netted read-out call 'projWithBaseNetNorm' \/
-- 'projWithNoteBaseNetNorm' explicitly.

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
    in mkJournalNonZero base' delta'

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
-- >>> type Test = Journal (T.Text, Int) Double (HatBase CountUnit)
-- >>> x = 10.00.@Not:<Yen .| ("A", 1) :: Test
-- >>> y = 20.00.@Hat:<Yen .| ("B", 1) :: Test
-- >>> z = 30.00.@Not:<Yen .| ("A", 2) :: Test
-- >>> toMap (filterByAxis 0 (NoteAxisKey ("A" :: T.Text)) (x .+ y .+ z)) == toMap ((10.00.@Not:<Yen .| ("A",1)) .+ (30.00.@Not:<Yen .| ("A",2)) :: Test)
-- True
--
-- >>> toMap (filterByAxis 1 (NoteAxisKey (1 :: Int)) (x .+ y .+ z)) == toMap ((10.00.@Not:<Yen .| ("A",1)) .+ (20.00.@Hat:<Yen .| ("B",1)) :: Test)
-- True
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
    in mkJournalNonZero result Map.empty

------------------------------------------------------------------
-- | gather
-- Gathers all Alg into one on the given Note.
--
-- >>> type Test = Journal T.Text Double (EA.HatBase EA.CountUnit)
-- >>> x = 20.00.@Not:<Yen .+ 20.00.@Hat:<Dollar .| "Withdrawal" :: Test
-- >>> y = 10.00.@Hat:<Yen .+ 10.00.@Not:<Dollar .| "Deposits" :: Test
-- >>> gather "A" (x .+ y)
-- 10.00:@Not:<Dollar .+ 20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen .+ 10.00:@Hat:<Yen .| "A"
gather :: (HatVal v, HatBaseClass b, Note n)
       => n -> Journal n v b -> Journal n v b
gather n js = (toAlg js) .| n

-- | Gather notes, net each full base exactly, and group unrounded residuals by
-- the winning base's key. Round each callback argument once. The callback can
-- return multiple notes. Nonfinite-input bases retain their original postings
-- and notes and are not passed to the callback. Finite accumulation overflow
-- is rounded only at the callback boundary, which may receive infinity.
--
-- For finite postings, each callback amount is the once-rounded sum of the
-- selected exact base residuals, independent of posting order and with no
-- tolerance. This law applies to Double, MoneyDouble, and MoneyDecimal.
{-# SPECIALIZE postFromNetBy :: (HatBaseClass b, Note n, Ord k) => (b -> Maybe k) -> (k -> Double -> Journal n Double b) -> Journal n Double b -> Journal n Double b #-}
{-# SPECIALIZE postFromNetBy :: (HatBaseClass b, Note n, Ord k) => (b -> Maybe k) -> (k -> MoneyDouble -> Journal n MoneyDouble b) -> Journal n MoneyDouble b -> Journal n MoneyDouble b #-}
{-# SPECIALIZE postFromNetBy :: (HatBaseClass b, Note n, Ord k) => (b -> Maybe k) -> (k -> MoneyDecimal -> Journal n MoneyDecimal b) -> Journal n MoneyDecimal b -> Journal n MoneyDecimal b #-}
postFromNetBy :: (HatVal v, HatBaseClass b, Note n, Ord k)
              => (b -> Maybe k)
              -> (k -> v -> Journal n v b)
              -> Journal n v b
              -> Journal n v b
postFromNetBy keyOf post journal = retained .+ M.foldlWithKey'
    (\result key state -> result .+ post key (roundAccum state)) mempty amounts
  where
    (nonfinite, amounts) = EA.postNetStates keyOf (toAlg journal)
    retained
        | HSet.null nonfiniteBases = mempty
        | otherwise = map (EA.filter (\entry ->
            HSet.member (base (EA._hatBase entry)) nonfiniteBases)) journal
    nonfiniteBases = HSet.fromList (Prelude.map base (EA.bases nonfinite))
