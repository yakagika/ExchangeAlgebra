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
{-# LANGUAGE ScopedTypeVariables       #-}

-- | Generic journals, note indices, and redundancy-preserving operations.
-- This Journal layer owns the representation and instances for Definitions 10-12.
-- It uses generic Algebra owners; "ExchangeAlgebra.Journal.Core" exposes the
-- public operations. Read the note definitions before the journal operations.
module ExchangeAlgebra.Journal.Core.Representation ( Note(..)
                                                   , HasNotePeriod(..)
                                                   , NoteFieldKey(..)
                                                   , NoteFieldIndex
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
                                                   , filterByNoteField
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
-- * NoteFieldKey
------------------------------------------------------------------

-- | An existential type that holds one note field with its type erased.
-- Used to decompose tuple notes into keys for their field positions
-- for indexing, mirroring how @ElementKey@ works for bases in 'Alg'.
data NoteFieldKey = forall a. (Eq a, Hashable a, Typeable a) => NoteFieldKey !a

instance Eq NoteFieldKey where
    NoteFieldKey x == NoteFieldKey y = case cast y of
        Nothing -> False
        Just y' -> x == y'

instance Hashable NoteFieldKey where
    hashWithSalt salt (NoteFieldKey x) = salt `hashWithSalt` typeOf x `hashWithSalt` x

------------------------------------------------------------------
-- * NoteFieldIndex
------------------------------------------------------------------

-- | Index of notes by field position and value.
-- Each field value maps to the set of notes that contain it at that position.
-- Mirrors @AxisPosting@ in 'Alg'.
type NoteFieldIndex n = IntMap (Map.HashMap NoteFieldKey (HashSet n))

{-# INLINE emptyNoteFieldIndex #-}
emptyNoteFieldIndex :: NoteFieldIndex n
emptyNoteFieldIndex = IntMap.empty

{-# INLINE insertNoteFieldIndex #-}
-- | Complexity: O(d) where d is the number of components in the Note
insertNoteFieldIndex :: (Eq n, Hashable n) => [NoteFieldKey] -> n -> NoteFieldIndex n -> NoteFieldIndex n
insertNoteFieldIndex !keys !note !idx =
    snd $ L.foldl' step (0 :: Int, idx) keys
  where
    step (!axis, !acc) !k =
        let !axisMap = IntMap.findWithDefault Map.empty axis acc
            !notes0 = Map.lookupDefault HSet.empty k axisMap
            !notes1 = HSet.insert note notes0
            !axisMap' = Map.insert k notes1 axisMap
            !acc' = IntMap.insert axis axisMap' acc
        in (axis + 1, acc')

{-# INLINE queryNoteFieldIndex #-}
-- | Query the 'NoteFieldIndex' for notes with the given value at the specified field position.
--
-- Complexity: O(1) (two map lookups)
queryNoteFieldIndex :: Int -> NoteFieldKey -> NoteFieldIndex n -> HashSet n
queryNoteFieldIndex !axis !key !idx =
    case IntMap.lookup axis idx of
        Nothing -> HSet.empty
        Just axisMap -> Map.lookupDefault HSet.empty key axisMap

------------------------------------------------------------------
-- * Note
------------------------------------------------------------------

-- | Type class for journal annotations (notes attached to postings).
-- @plank@ represents a blank note (analogous to @mempty@ in @Monoid@).
--
-- @toNoteFieldKeys@ returns a key for each note field for 'NoteFieldIndex'
-- indexing, mirroring how @toElementKeys@ works for bases.
-- For tuple notes, the index uses the position of each field.
-- The default returns a single key containing the note itself.
--
-- == Prefer an ADT note over @String@
--
-- A note's /event/ component is best modeled as a small enumeration (an ADT)
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
    -- | Return a key for each note field for 'NoteFieldIndex' indexing.
    -- The default returns a single key containing the note itself.
    toNoteFieldKeys :: a -> [NoteFieldKey]
    toNoteFieldKeys a = [NoteFieldKey a]

-- | Extracts the simulation period from a 'Note'.
--
-- A simulation policy or a multi-period ledger uses this period to group
-- entries. The built-in tuple instances use the last field as the period.
-- Define an instance to specify the period for a custom note type.
--
-- >>> periodOf ("buy" :: T.Text, 7 :: Int)
-- 7
--
-- >>> periodOf ("buy" :: T.Text, "shopA" :: T.Text, 3 :: Int)
-- 3
class (Note n, Ord (PeriodOf n)) => HasNotePeriod n where
  -- | The type of the period extracted from the note.
  type PeriodOf n
  -- | Extract the period. The tuple instances use the last field.
  periodOf :: n -> PeriodOf n

-- | The second field of a pair note is its period.
instance (Note e, Note t) => HasNotePeriod (e, t) where
  type PeriodOf (e, t) = t
  periodOf = snd

-- | The third field of a triple note is its period.
instance (Note e1, Note e2, Note t) => HasNotePeriod (e1, e2, t) where
  type PeriodOf (e1, e2, t) = t
  periodOf (_, _, t) = t

-- | Default instance for using @Int@ to represent a simulation term.
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
    toNoteFieldKeys (a, b) = [NoteFieldKey a, NoteFieldKey b]

instance (Note a, Note b, Note c) => Note (a, b, c) where
    plank = (plank, plank, plank)
    toNoteFieldKeys (a, b, c) = [NoteFieldKey a, NoteFieldKey b, NoteFieldKey c]

instance (Note a, Note b, Note c, Note d) => Note (a, b, c, d) where
    plank = (plank, plank, plank, plank)
    toNoteFieldKeys (a, b, c, d) = [NoteFieldKey a, NoteFieldKey b, NoteFieldKey c, NoteFieldKey d]

------------------------------------------------------------------
-- * Journal
------------------------------------------------------------------

-- | Transaction data with annotations.
--   Stored in two layers, base and delta, with a note index for each layer.
--   A query by note component builds both indices on first use. Each append leaves
--   a new unevaluated delta index, so alternating queries and appends rebuild it in time
--   proportional to the delta note count, bounded by compaction at 128 notes.
--   Updates are appended only to delta and periodically compacted into base.
--
--   __Invariants.__ The constructor is not exported; build with 'mkJournal',
--   '(.|)' or 'fromList'. Its internal cache\/index fields @_jBaseAxis@ and
--   @_jDeltaAxis@ must be
--   exactly the note field indices (@buildNoteFieldIndex@) of @_jBase@ and
--   @_jDelta@ respectively. The component-filtered query path ('filterByNoteField') reads
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
               , _jBaseAxis  :: NoteFieldIndex n
               , _jDeltaAxis :: NoteFieldIndex n
                 -- ^ Built lazily: only 'filterByNoteField' reads this cache.
                 -- A strict field builds an index on every '(.|)' and append.
               } -> Journal n v b

deltaCompactThreshold :: Int
deltaCompactThreshold = 128

-- | Build the note field index from map keys.
{-# INLINE buildNoteFieldIndex #-}
buildNoteFieldIndex :: Note n => Map.HashMap n a -> NoteFieldIndex n
buildNoteFieldIndex =
    Map.foldlWithKey'
        (\acc n _ -> insertNoteFieldIndex (toNoteFieldKeys n) n acc)
        emptyNoteFieldIndex

-- | The only safe way to build a t'Journal' from a base layer and a delta
-- layer. It keeps the note component indices consistent with both layers and
-- returns the normal form; see the invariants note on t'Journal'.
--
-- A note whose algebra is zero ('EA.isZero') is dropped from the layer that
-- holds it. Both maps are read once; a map without a zero note is used as
-- given, and only a map that holds one is rebuilt without it.
--
-- Complexity: O(n) where n is the number of notes in both layers; each note's
-- algebra is evaluated to WHNF. Both note indices are built on demand.
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
-- zero. It does not check this. It builds the note component indices as
-- 'mkJournal' does: both indices are built on demand.
{-# INLINE mkJournalNonZero #-}
mkJournalNonZero :: (Note n, HatVal v, HatBaseClass b)
                 => Map.HashMap n (Alg v b) -> Map.HashMap n (Alg v b) -> Journal n v b
mkJournalNonZero bs delta = Journal bs delta baseIdx deltaIdx
  where
    ~baseIdx = buildNoteFieldIndex bs
    ~deltaIdx = buildNoteFieldIndex delta

-- | Construct a Journal from a HashMap. A note whose algebra is zero is
-- dropped (see 'mkJournal').
--
-- Complexity: O(n) where n is the number of notes; the map is rebuilt only
-- when it holds a zero note. Base component index is built lazily on first component
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
appendMap :: forall v b n. (HatVal v, HatBaseClass b, Note n)
          => Map.HashMap n (Alg v b) -> Journal n v b -> Journal n v b
appendMap rhs j@(Journal bs delta baseAxis _)
    | Map.null rhs = j
    | otherwise = compactIfNeeded $ Journal bs delta' baseAxis deltaAxis'
  where
    ~deltaAxis' = buildNoteFieldIndex delta'
    !delta' = Map.foldlWithKey' step delta rhs

    step :: Map.HashMap n (Alg v b) -> n -> Alg v b -> Map.HashMap n (Alg v b)
    step !dAcc !k !v =
        case Map.lookup k dAcc of
            Nothing
                | EA.isZero v -> dAcc
                | otherwise   -> Map.insert k v dAcc
            Just dv ->
                let !dMerged = dv .+ v
                in if EA.isZero dMerged
                    then Map.delete k dAcc
                    else Map.insert k dMerged dAcc

-- | Serialize a journal as its note count followed by each note and its
-- algebra. Notes with the same hash are encoded in 'Ord' order.
-- Decoding checks each algebra as the 'Alg' instance does and fails
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
        EA.foldrHashMapByHash
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

-- | Smart constructor that attaches a Note (annotation) to a member of the redundant algebra to build a Journal.
-- Returns an empty Journal when the member of the redundant algebra is zero, consistent with
-- the zero-value handling in @appendMap@ and 'sigmaOn'.
--
-- Fixity @infix 5@: it binds more tightly than '(.+)' and less tightly than
-- '(.@)', so each term carries its own note and a multi-term algebra needs
-- parentheses.
--
-- >>> type FixityTest = Journal T.Text Double (HatBase CountUnit)
-- >>> j = 10.00.@Not:<Yen .| "a" .+ 20.00.@Hat:<Yen .| "b" :: FixityTest
-- >>> toMap j == toMap ((10.00.@Not:<Yen .| "a") .+ (20.00.@Hat:<Yen .| "b"))
-- True
-- >>> toMap ((10.00.@Not:<Yen .+ 20.00.@Hat:<Dollar) .| "a" :: FixityTest) == toMap ((10.00.@Not:<Yen .| "a") .+ (20.00.@Hat:<Dollar .| "a"))
-- True
--
-- Complexity: O(1)
(.|) :: (HatVal v, HatBaseClass b, Note n)
      => Alg v b -> n -> Journal n v b
(.|) alg n
    | EA.isZero alg = mempty
    | otherwise    = mkJournalNonZero Map.empty (Map.singleton n alg)

infix 5 .|

------------------------------------------------------------------
-- Show
------------------------------------------------------------------
-- | Render each non-blank note as its algebra entries followed by one '(.|)'.
-- Because '(.|)' binds more tightly than '(.+)', the entries of one note are
-- parenthesized when there are two or more of them, and the groups are joined
-- with '(.+)' without parentheses. The result can be pasted back as an
-- expression with the current fixities.
--
-- The blank note remains invisible when it is the only group. In a mixed
-- journal it is rendered explicitly, so every summand is one complete group.
--
-- >>> type ShowTest = Journal T.Text Double (HatBase CountUnit)
-- >>> (mempty :: ShowTest)
-- 0
-- >>> (5.00.@Not:<Yen .| plank :: ShowTest)
-- 5.00:@Not:<Yen
-- >>> one = (20.00.@Not:<Yen .+ 20.00.@Hat:<Dollar) .| "N" :: ShowTest
-- >>> one
-- (20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen) .| "N"
-- >>> toMap ((20.00.@Hat:<Dollar .+ 20.00.@Not:<Yen) .| "N" :: ShowTest) == toMap one
-- True
-- >>> two = 10.00.@Hat:<Yen .| "M" :: ShowTest
-- >>> show (one .+ two) `elem` ["10.00:@Hat:<Yen .| \"M\" .+ (20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen) .| \"N\"", "(20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen) .| \"N\" .+ 10.00:@Hat:<Yen .| \"M\""]
-- True
-- >>> toMap ((10.00.@Hat:<Yen .| "M") .+ ((20.00.@Hat:<Dollar .+ 20.00.@Not:<Yen) .| "N") :: ShowTest) == toMap (one .+ two)
-- True
-- >>> blank = 5.00.@Not:<Yen .| plank :: ShowTest
-- >>> show (one .+ blank) `elem` ["(20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen) .| \"N\" .+ 5.00:@Not:<Yen .| \"\"", "5.00:@Not:<Yen .| \"\" .+ (20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen) .| \"N\""]
-- True
instance (HatVal v, HatBaseClass b, Note n) => Show (Journal n v b) where
    showsPrec d js =
        case groups of
            [] -> showString "0"
            [(k, a)]
                | isPlank k -> showsPrec d a
                | otherwise -> showParen (d > 5) (showsGroup k a)
            _ -> showParen (d > 3) (showsGroups groups)
      where
        m = toMap js
        groups = reverse $ EA.foldrHashMapByHash collectGroup [] m

        collectGroup k a acc
            | not (isPlank k) && Prelude.null (EA.toASCList a) = acc
            | otherwise = (k, a) : acc

        showsGroups [] = id
        showsGroups [(k, a)] = showsGroup k a
        showsGroups ((k, a):rest) =
            showsGroup k a
            . showString " .+ "
            . showsGroups rest

        showsGroup k a =
            showsGroupAlg k a
            . showString " .| "
            . showsPrec 6 k

        showsGroupAlg k a =
            case (if isPlank k then id else reverse) (EA.toASCList a) of
                []  -> showString "0"
                [x] -> showsPrec 6 x
                xs  -> showParen True (showsAlgEntries xs)

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
-- >>> x = (20.00.@Not:<Yen .+ 20.00.@Hat:<Dollar) .| "Withdrawal" :: Test
-- >>> y = (10.00.@Hat:<Yen .+ 10.00.@Not:<Dollar) .| "Deposits" :: Test
-- >>> toMap (x .+ y) == toMap (((10.00.@Not:<Dollar .+ 10.00.@Hat:<Yen) .| "Deposits") .+ ((20.00.@Hat:<Dollar .+ 20.00.@Not:<Yen) .| "Withdrawal") :: Test)
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
-- instance), but does not touch the lazily built component indices (@_jBaseAxis@ /
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
    norm = norm . EA.unionsMerge . materializeMap
    (.-) x = map (.-) (gather plank x)
    compress = map compress

------------------------------------------------------------------
-- | Build a t'Journal' from a list using a strict left fold.
-- Each note retains the input order of the terms on each base and side.
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
        | otherwise = Map.insertWith (flip (.+)) n alg m

{-# INLINE mergeJournalMapIfNonZero #-}
mergeJournalMapIfNonZero :: (HatVal v, HatBaseClass b, Note n)
                         => Map.HashMap n (Alg v b)
                         -> Journal n v b
                         -> Map.HashMap n (Alg v b)
mergeJournalMapIfNonZero !acc js
    | isZero js = acc
    | otherwise = mergeJournalMap acc js

-- | Apply a function to each list element and sum the resulting journals.
-- Terms for each note, base, and side retain their list order.
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
-- Effects run in list order. The result combines their values using 'mconcat'.
sigmaM :: (Monoid m, Monad m0) => [a] -> (a -> m0 m) -> m0 m
sigmaM xs f = mconcat <$> CM.forM xs f

------------------------------------------------------------------
-- | Combine all notes into a single algebra.
-- Merge both layers of each note first, then combine notes in the traversal
-- order of the resulting @HashMap@, visiting notes with the same hash in
-- 'Ord' order. For lawful notes, bases, and values, equal 'toMap' results
-- imply equal 'toAlg' results.
--
-- The cost includes the merges and sorting each run of equal-hash notes.
toAlg :: (HatVal v, HatBaseClass b, Note n)
      => Journal n v b -> Alg v b
toAlg = EA.mkAlgFromMap . EA.foldlHashMapByHash mergeNote Map.empty . materializeMap
  where
    mergeNote acc _ algebra = EA.mergeAlgMap acc algebra

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
-- >>> x = (1.00.@Hat:<Yen .+ 1.00.@Not:<Amount) .| "cat"  :: Test
-- >>> y = (2.00.@Hat:<Yen .+ 2.00.@Not:<Amount) .| "dog"  :: Test
-- >>> z = (3.00.@Hat:<Yen .+ 3.00.@Not:<Amount) .| "fish" :: Test
-- >>> toMap (projWithNote ["dog","cat"] (x .+ y .+ z)) == toMap (((1.00.@Not:<Amount .+ 1.00.@Hat:<Yen) .| "cat") .+ ((2.00.@Not:<Amount .+ 2.00.@Hat:<Yen) .| "dog") :: Test)
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
-- >>> x = (1.00.@Hat:<Yen .+ 1.00.@Not:<Amount) .| "cat"  :: Test
-- >>> y = (2.00.@Not:<Yen .+ 2.00.@Hat:<Amount) .| "dog"  :: Test
-- >>> z = (3.00.@Hat:<Yen .+ 3.00.@Not:<Amount) .| "fish" :: Test
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
-- >>> x = (1.00.@Hat:<Yen .+ 1.00.@Not:<Amount) .| "cat"  :: Test
-- >>> y = (2.00.@Not:<Yen .+ 2.00.@Hat:<Amount) .| "dog"  :: Test
-- >>> z = (3.00.@Hat:<Yen .+ 3.00.@Not:<Amount) .| "fish" :: Test
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
-- | Retain a note and all its terms when the predicate accepts the note and
-- its algebra. The predicate runs once per note and receives the same algebra
-- as in 'toMap'. A shared note's algebra is combined only if the predicate
-- evaluates it.
--
-- Complexity: O(n) expected for scanning both layers and looking up notes,
-- plus the costs of the predicates and any algebra additions they evaluate.
-- Here n is the number of notes across the two layers.
filterWithNote :: forall v b n. (HatVal v, HatBaseClass b, Note n)
               => (n -> Alg v b -> Bool)
               -> Journal n v b
               -> Journal n v b
filterWithNote predicate (Journal baseLayer deltaLayer _ _)
    | Map.null deltaLayer = mkJournalNonZero (Map.filterWithKey predicate baseLayer) Map.empty
    | Map.null baseLayer  = mkJournalNonZero Map.empty (Map.filterWithKey predicate deltaLayer)
    | otherwise          = mkJournalNonZero retainedBase retainedDelta
  where
    retainedBase :: Map.HashMap n (Alg v b)
    retainedBase = Map.filterWithKey keepBase baseLayer

    retainedDelta :: Map.HashMap n (Alg v b)
    retainedDelta = Map.filterWithKey keepDelta deltaLayer

    keepBase :: n -> Alg v b -> Bool
    keepBase note algebra = case Map.lookup note deltaLayer of
        Nothing    -> predicate note algebra
        Just delta -> predicate note (algebra .+ delta)

    keepDelta :: n -> Alg v b -> Bool
    keepDelta note algebra
        | Map.member note baseLayer = Map.member note retainedBase
        | otherwise                 = predicate note algebra

-- | Select journal entries whose note has the specified value at the given field position.
-- Uses the base and delta 'NoteFieldIndex' caches for O(|result|) retrieval
-- after index construction.
--
-- Field positions are zero-based. For a note of type @(EventName, Term)@:
--
--   * Position 0 holds the event name.
--   * Position 1 holds the period.
--
-- For non-tuple notes, position 0 is the only valid position.
--
-- Complexity: O(|result|) after index construction; O(n) for the first base note field query on a Journal value
--
-- >>> type Test = Journal (T.Text, Int) Double (HatBase CountUnit)
-- >>> x = 10.00.@Not:<Yen .| ("A", 1) :: Test
-- >>> y = 20.00.@Hat:<Yen .| ("B", 1) :: Test
-- >>> z = 30.00.@Not:<Yen .| ("A", 2) :: Test
-- >>> toMap (filterByNoteField 0 (NoteFieldKey ("A" :: T.Text)) (x .+ y .+ z)) == toMap ((10.00.@Not:<Yen .| ("A",1)) .+ (30.00.@Not:<Yen .| ("A",2)) :: Test)
-- True
--
-- >>> toMap (filterByNoteField 1 (NoteFieldKey (1 :: Int)) (x .+ y .+ z)) == toMap ((10.00.@Not:<Yen .| ("A",1)) .+ (20.00.@Hat:<Yen .| ("B",1)) :: Test)
-- True
{-# INLINE filterByNoteField #-}
filterByNoteField :: (HatVal v, HatBaseClass b, Note n)
             => Int -> NoteFieldKey -> Journal n v b -> Journal n v b
filterByNoteField axis key j@(Journal _ _ baseIdx deltaIdx) =
    let !matched = HSet.union
            (queryNoteFieldIndex axis key baseIdx)
            (queryNoteFieldIndex axis key deltaIdx)
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
-- >>> x = (20.00.@Not:<Yen .+ 20.00.@Hat:<Dollar) .| "Withdrawal" :: Test
-- >>> y = (10.00.@Hat:<Yen .+ 10.00.@Not:<Dollar) .| "Deposits" :: Test
-- >>> gather "A" (x .+ y)
-- (10.00:@Not:<Dollar .+ 20.00:@Hat:<Dollar .+ 20.00:@Not:<Yen .+ 10.00:@Hat:<Yen) .| "A"
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
    (nonfinite, amounts) = EA.postNetStates keyOf (EA.unionsMerge (materializeMap journal))
    retained
        | HSet.null nonfiniteBases = mempty
        | otherwise = map (EA.filter (\entry ->
            HSet.member (base (EA._hatBase entry)) nonfiniteBases)) journal
    nonfiniteBases = case nonfinite of
        EA.Zero                       -> HSet.empty
        _ :@ basis                    -> HSet.singleton (base basis)
        EA.Liner { EA._realg = pairs } -> Map.keysSet pairs
