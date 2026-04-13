{- |
    Module     : ExchangeAlgebra.Journal
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

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
    , sigmaM
    , map
    , insert
    , projWithNote
    , projWithBase
    , projWithNoteBase
    , projWithBaseNorm
    , projWithNoteNorm
    , filterWithNote
    , filterByAxis
    , gather
    ) where

import qualified    ExchangeAlgebra.Algebra as EA
import              ExchangeAlgebra.Algebra.Base
import              ExchangeAlgebra.Algebra ( HatVal(..)
                                            , HatBaseClass(..)
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
import              Control.Parallel.Strategies (using, parTraversable, rdeepseq, NFData)
import qualified    Data.Set                as S
import qualified    Data.List               as L
import qualified    Data.Map.Strict         as M
import              Data.Hashable
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
data Journal n v b where
     Journal :: (Note n, HatVal v, HatBaseClass b)
            => { _jBase      :: !(Map.HashMap n (Alg v b))
               , _jDelta     :: !(Map.HashMap n (Alg v b))
               , _jVersion   :: !Int
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
          => Map.HashMap n (Alg v b) -> Map.HashMap n (Alg v b) -> Int -> Journal n v b
mkJournal base delta ver = Journal base delta ver baseIdx deltaIdx
  where
    ~baseIdx = buildNoteAxisPosting base
    !deltaIdx = buildNoteAxisPosting delta

-- | Construct a Journal from a HashMap.
--
-- Complexity: O(1). Base axis index is built lazily on first axis query.
{-# INLINE fromMap #-}
fromMap :: (HatVal v, HatBaseClass b, Note n)
        => Map.HashMap n (Alg v b) -> Journal n v b
fromMap m = mkJournal m Map.empty 0

-- | Retrieve all entries of a Journal as a HashMap.
-- Merges the base and delta layers.
--
-- Complexity: O(n) where n is the number of Notes
{-# INLINE toMap #-}
toMap :: (HatVal v, HatBaseClass b, Note n)
      => Journal n v b -> Map.HashMap n (Alg v b)
toMap = materializeMap

{-# INLINE materializeMap #-}
materializeMap :: (HatVal v, HatBaseClass b, Note n)
               => Journal n v b -> Map.HashMap n (Alg v b)
materializeMap (Journal base delta _ _ _) =
    Map.unionWith (.+) base delta

{-# INLINE lookupNote #-}
lookupNote :: (HatVal v, HatBaseClass b, Note n)
           => n -> Journal n v b -> Maybe (Alg v b)
lookupNote n (Journal base delta _ _ _) =
    case (Map.lookup n delta, Map.lookup n base) of
        (Nothing, Nothing) -> Nothing
        (Just d, Nothing)  -> Just d
        (Nothing, Just b)  -> Just b
        (Just d, Just b)   -> Just (b .+ d)

{-# INLINE compactIfNeeded #-}
compactIfNeeded :: (HatVal v, HatBaseClass b, Note n)
                => Journal n v b -> Journal n v b
compactIfNeeded j@(Journal base delta ver _ _)
    | Map.size delta < deltaCompactThreshold = j
    | otherwise = mkJournal (Map.unionWith (.+) base delta) Map.empty (ver + 1)

{-# INLINE appendMap #-}
appendMap :: (HatVal v, HatBaseClass b, Note n)
          => Map.HashMap n (Alg v b) -> Journal n v b -> Journal n v b
appendMap rhs j@(Journal base delta ver baseAxis deltaAxis)
    | Map.null rhs = j
    | otherwise = compactIfNeeded $ Journal base delta' (ver + 1) baseAxis deltaAxis'
  where
    (delta', deltaAxis') = Map.foldlWithKey' step (delta, deltaAxis) rhs

    step (!dAcc, !idxAcc) !k !v =
        let !dMerged = case Map.lookup k dAcc of
                Nothing -> v
                Just dv -> dv .+ v
            !keys = toNoteAxisKeys k
        in if EA.isZero dMerged
            then (Map.delete k dAcc, deleteNoteAxisPosting keys k idxAcc)
            else (Map.insert k dMerged dAcc, insertNoteAxisPosting keys k idxAcc)

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
isZero (Journal base delta _ _ _) = Map.null base && Map.null delta

pattern Zero :: (HatVal v, HatBaseClass b, Note n) => Journal n v b
pattern Zero <- (isZero -> True)
    where
        Zero = mkJournal Map.empty Map.empty 0

-- | Smart constructor that attaches a Note (annotation) to an algebra element to build a Journal.
--
-- Complexity: O(1)
(.|) :: (HatVal v, HatBaseClass b, Note n)
      => Alg v b -> n -> Journal n v b
(.|) alg n = mkJournal Map.empty (Map.singleton n alg) 1

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
    mempty = mkJournal Map.empty Map.empty 0
    mappend = (<>)

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

    balance xs
        | (norm . decR) xs == (norm . decL) xs = True
        | otherwise                            = False

    diffRL xs
        | r > l     = (Credit, r - l)
        | l > r     = (Debit, l - r)
        | otherwise = (Side, 0)
      where
        r = (norm . decR) xs
        l = (norm . decL) xs

------------------------------------------------------------------
-- | fromList
--
-- >>> type Test = Journal String Double (HatBase AccountTitles)
-- >>> x = [(1.00:@Hat:<Cash .| z) | z <- ["Loan Payment","Purchace Apple"]] :: [Test]
-- >>> fromList x
-- 1.00:@Hat:<Cash.|"Loan Payment" .+ 1.00:@Hat:<Cash.|"Purchace Apple"
fromList :: (HatVal v, HatBaseClass b, Note n)
         => [Journal n v b] -> Journal n v b
fromList = foldr (.+) mempty

------------------------------------------------------------------
{-# INLINE mergeJournalMap #-}
mergeJournalMap :: (HatVal v, HatBaseClass b, Note n)
                => Map.HashMap n (Alg v b)
                -> Journal n v b
                -> Map.HashMap n (Alg v b)
mergeJournalMap !acc (Journal base delta _ _ _)
    | Map.null base && Map.null delta = acc
    | otherwise =
        let !acc1 = Map.foldlWithKey' mergeOne acc base
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

-- | Summation in a monadic context. Applies a monadic function to each element and mconcats the results.
--
-- Complexity: O(|xs| * cost(f))
sigmaM :: (Monoid m, Monad m0) => [a] -> (a -> m0 m) -> m0 m
sigmaM xs f = mconcat <$> CM.forM xs f

------------------------------------------------------------------
-- | Combine entries from all Notes in a Journal into a single Alg.
--
-- Complexity: O(total number of base keys across all Notes)
toAlg :: (HatVal v, HatBaseClass b, Note n)
      => Journal n v b -> Alg v b
toAlg (Journal base delta _ _ _) =
    EA.unionsMerge (Map.elems base ++ Map.elems delta)

------------------------------------------------------------------
-- | Apply function f to the entry of each Note in the Journal.
-- Applies to merged Note entries (base + delta), preserving semantics.
--
-- Complexity: O(j * cost(f)) where j is the number of Notes
map :: (HatVal v, HatBaseClass b, Note n)
    => (Alg v b -> Alg v b) -> Journal n v b -> Journal n v b
map f = fromMap . Map.map f . toMap

parallelMap :: (NFData b, Ord k) => (a -> b) -> Map.HashMap k a -> Map.HashMap k b
parallelMap f m = Map.map f m `using` parTraversable rdeepseq

parMap :: (HatVal v, HatBaseClass b, Note n)
    => (Alg v b -> Alg v b) -> Journal n v b -> Journal n v b
parMap f = fromMap . parallelMap f . toMap

-- | Insert x into y. If x's Note already exists in y, it is overwritten with x's value.
--
-- >>> type Test = Journal String Double (HatBase AccountTitles)
-- >>> x = 10.00:@Not:<Cash .| "A" :: Test
-- >>> y = 20.00:@Not:<Cash .| "B" :: Test
-- >>> z = 30.00:@Hat:<Cash .| "A" :: Test
-- >>> insert z (x .+ y)
-- 30.00:@Hat:<Cash.|"A" .+ 20.00:@Not:<Cash.|"B"
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
-- 2.00:@Not:<Amount.|"dog" .+ 2.00:@Hat:<Yen.|"dog" .+ 1.00:@Not:<Amount.|"cat" .+ 1.00:@Hat:<Yen.|"cat"
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
{-# INLINE [0] projWithBase #-}
projWithBase [] _ = mempty
projWithBase bs js = fromMap $ Map.map (EA.proj bs) (toMap js)

-- | Directly compute the norm after filtering by the specified bases.
-- Equivalent to @norm (projWithBase bs js)@ but without constructing an intermediate Journal.
--
-- Complexity: O(j * proj cost) where j is the number of Notes
projWithBaseNorm :: (HatVal v, HatBaseClass b, Note n)
                 => [b] -> Journal n v b -> v
projWithBaseNorm [] _ = 0
projWithBaseNorm bs js =
    Map.foldl' (\acc alg -> acc + EA.projNorm bs alg) 0 (toMap js)

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
{-# INLINE [0] projWithNoteBase #-}
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

-- | Directly compute the norm after filtering by the specified Notes and bases.
-- Equivalent to @norm (projWithNoteBase ns bs js)@ but without constructing an intermediate Journal.
--
-- Complexity: O(|ns| * proj cost)
projWithNoteNorm :: (HatVal v, HatBaseClass b, Note n)
                 => [n] -> [b] -> Journal n v b -> v
projWithNoteNorm _ [] _ = 0
projWithNoteNorm ns bs js
    | any isPlank ns = projWithBaseNorm bs js
projWithNoteNorm [n] bs js = case lookupNote n js of
    Nothing -> 0
    Just a  -> EA.projNorm bs a
projWithNoteNorm [] bs js = projWithBaseNorm bs js
projWithNoteNorm ns bs js =
    S.foldl'
        (\acc n -> case lookupNote n js of
            Nothing -> acc
            Just a  -> acc + EA.projNorm bs a)
        0
        (S.fromList ns)

{-# RULES
"EJ.projWithBaseNorm/from-norm-projWithBase"
    forall bs js. norm (projWithBase bs js) = projWithBaseNorm bs js
"EJ.projWithNoteNorm/from-norm-projWithNoteBase"
    forall ns bs js. norm (projWithNoteBase ns bs js) = projWithNoteNorm ns bs js
  #-}

------------------------------------------------------------------
-- | Filter by a predicate on Note-entry pairs.
-- Applies the filter to both the base and delta layers.
--
-- Complexity: O(n) where n is the number of Notes
filterWithNote :: (HatVal v, HatBaseClass b, Note n)
               => (n -> Alg v b -> Bool) -> Journal n v b -> Journal n v b
filterWithNote f (Journal base delta ver _ _) =
    let !base' = Map.filterWithKey f base
        !delta' = Map.filterWithKey f delta
    in mkJournal base' delta' ver

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
-- 30.00:@Not:<Cash.|("A",2) .+ 10.00:@Not:<Cash.|("A",1)
--
-- >>> filterByAxis 1 (NoteAxisKey (1 :: Int)) (x .+ y .+ z)
-- 20.00:@Hat:<Cash.|("B",1) .+ 10.00:@Not:<Cash.|("A",1)
{-# INLINE filterByAxis #-}
filterByAxis :: (HatVal v, HatBaseClass b, Note n)
             => Int -> NoteAxisKey -> Journal n v b -> Journal n v b
filterByAxis axis key j@(Journal _ _ _ baseIdx deltaIdx) =
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
