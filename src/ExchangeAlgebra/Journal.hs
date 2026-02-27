{- |
    Module     : ExchangeAlgebra.Record
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

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

module ExchangeAlgebra.Journal
    ( module ExchangeAlgebra.Algebra.Base
    , HatVal(..)
    , HatBaseClass(..)
    , Redundant(..)
    , Exchange(..)
    , pattern (:@)
    , (.@)
    , Note(..)
    , Journal(..)
    , pattern ExchangeAlgebra.Journal.Zero
    , (.|)
    , toAlg
    , toMap
    , fromMap
    , fromList
    , sigma
    , sigmaM
    , map
    , insert
    , projWithNote
    , projWithBase
    , projWithNoteBase
    , projWithBaseNorm
    , projWithNoteNorm
    , filterWithNote
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
import              Control.Parallel.Strategies (using, parTraversable, rdeepseq, NFData)
import qualified    Data.Set                as S
import              Data.Hashable
import qualified    Data.Text               as T
import qualified    Control.Monad           as CM
import qualified    Data.Binary             as Binary

-- | 摘要のクラス
class (Show a, Eq a, Ord a, Hashable a) => Note a where
    plank :: a
    isPlank :: a -> Bool
    isPlank x = x == plank

instance Note String where
    plank = ""

instance Note T.Text where
    plank = ""

instance (Note a, Note b) => Note (a, b) where
    plank = (plank, plank)

instance (Note a, Note b, Note c) => Note (a, b, c) where
    plank = (plank, plank, plank)

instance (Note a, Note b, Note c, Note d) => Note (a, b, c, d) where
    plank = (plank, plank, plank, plank)

-- | 摘要の付随した取引データ
--   base + delta の2層で保持し、更新は基本的に delta に追記する。
data Journal n v b where
     Journal :: (Note n, HatVal v, HatBaseClass b)
            => { _jBase    :: !(Map.HashMap n (Alg v b))
               , _jDelta   :: !(Map.HashMap n (Alg v b))
               , _jVersion :: !Int
               } -> Journal n v b

deltaCompactThreshold :: Int
deltaCompactThreshold = 128

{-# INLINE fromMap #-}
fromMap :: (HatVal v, HatBaseClass b, Note n)
        => Map.HashMap n (Alg v b) -> Journal n v b
fromMap m = Journal m Map.empty 0

{-# INLINE toMap #-}
toMap :: (HatVal v, HatBaseClass b, Note n)
      => Journal n v b -> Map.HashMap n (Alg v b)
toMap = materializeMap

{-# INLINE materializeMap #-}
materializeMap :: (HatVal v, HatBaseClass b, Note n)
               => Journal n v b -> Map.HashMap n (Alg v b)
materializeMap (Journal base delta _) = Map.unionWith (.+) base delta

{-# INLINE lookupNote #-}
lookupNote :: (HatVal v, HatBaseClass b, Note n)
           => n -> Journal n v b -> Maybe (Alg v b)
lookupNote n (Journal base delta _) =
    case (Map.lookup n delta, Map.lookup n base) of
        (Nothing, Nothing) -> Nothing
        (Just d, Nothing)  -> Just d
        (Nothing, Just b)  -> Just b
        (Just d, Just b)   -> Just (b .+ d)

{-# INLINE compactIfNeeded #-}
compactIfNeeded :: (HatVal v, HatBaseClass b, Note n)
                => Journal n v b -> Journal n v b
compactIfNeeded j@(Journal base delta ver)
    | Map.size delta < deltaCompactThreshold = j
    | otherwise = Journal (Map.unionWith (.+) base delta) Map.empty (ver + 1)

{-# INLINE appendMap #-}
appendMap :: (HatVal v, HatBaseClass b, Note n)
          => Map.HashMap n (Alg v b) -> Journal n v b -> Journal n v b
appendMap rhs j@(Journal base delta ver)
    | Map.null rhs = j
    | otherwise = compactIfNeeded $ Journal base' delta' (ver + 1)
  where
    (base', delta') = Map.foldlWithKey' step (base, delta) rhs

    step (!bAcc, !dAcc) !k !v =
        case Map.lookup k dAcc of
            Just dv ->
                let !d' = Map.insert k (dv .+ v) dAcc
                in (bAcc, d')
            Nothing -> case Map.lookup k bAcc of
                Just bv ->
                    let !b' = Map.delete k bAcc
                        !d' = Map.insert k (bv .+ v) dAcc
                    in (b', d')
                Nothing ->
                    let !d' = Map.insert k v dAcc
                    in (bAcc, d')

instance ( Note n
         , HatVal v
         , HatBaseClass b
         , Binary.Binary n
         , Binary.Binary (Alg v b)
         ) => Binary.Binary (Journal n v b) where
    put = Binary.put . Map.toList . toMap
    get = fromMap . Map.fromList <$> Binary.get

isZero :: (HatVal v, HatBaseClass b, Note n)
       => Journal n v b -> Bool
-- | Complexity: O(1)
isZero (Journal base delta _) = Map.null base && Map.null delta

pattern Zero :: (HatVal v, HatBaseClass b, Note n) => Journal n v b
pattern Zero <- (isZero -> True)
    where
        Zero = Journal Map.empty Map.empty 0

-- | smart constructer of :||
-- Complexity: O(1)
(.|) :: (HatVal v, HatBaseClass b, Note n)
      => Alg v b -> n -> Journal n v b
(.|) alg n = Journal Map.empty (Map.singleton n alg) 1

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

-- | (.+) for Journal
addJournal :: (HatVal v, HatBaseClass b, Note n)
           => Journal n v b -> Journal n v b -> Journal n v b
-- | Complexity:
--   amortized O(size(rhs)) updates, plus occasional compaction.
addJournal lhs rhs = appendMap (toMap rhs) lhs

instance (HatVal v, HatBaseClass b, Note n) => Monoid (Journal n v b) where
    mempty = Journal Map.empty Map.empty 0
    mappend = (<>)

instance (HatVal v, HatBaseClass b, Note n) => Redundant (Journal n) v b where
    (.^) = map (.^)
    (.+) = mappend
    (.*) x  = map ((.*) x)
    norm = norm . toAlg
    (.-) x = map (.-) (gather plank x)
    compress = map compress

instance (Note n, HatVal v, ExBaseClass b) => Exchange (Journal n) v b where
    decR js = map (EA.filter (\x -> x /= EA.Zero && (whichSide . EA._hatBase) x == Debit)) js
    decL xs = map (EA.filter (\x -> x /= EA.Zero && (whichSide . EA._hatBase) x == Credit)) xs
    decP xs = map (EA.filter (\x -> x /= EA.Zero && (isHat . EA._hatBase) x)) xs
    decM xs = map (EA.filter (\x -> x /= EA.Zero && (not . isHat . EA._hatBase) x)) xs

    balance xs
        | (norm . decR) xs == (norm . decL) xs = True
        | otherwise                            = False

    diffRL xs
        | r > l     = (Debit, r - l)
        | l > r     = (Credit, l - r)
        | otherwise = (Side, 0)
      where
        r = (norm . decR) xs
        l = (norm . decL) xs

------------------------------------------------------------------
fromList :: (HatVal v, HatBaseClass b, Note n)
         => [Journal n v b] -> Journal n v b
fromList = foldr (.+) mempty

------------------------------------------------------------------
sigma :: (HatVal v, HatBaseClass b, Note n)
      => [a] -> (a -> Journal n v b) -> Journal n v b
sigma xs f = foldr ((.+) . f) mempty xs

sigmaM :: (Monoid m, Monad m0) => [a] -> (a -> m0 m) -> m0 m
sigmaM xs f = mconcat <$> CM.forM xs f

------------------------------------------------------------------
toAlg :: (HatVal v, HatBaseClass b, Note n)
      => Journal n v b -> Alg v b
toAlg (Journal base delta _) =
    let !z = Map.foldl' (.+) EA.Zero base
    in Map.foldl' (.+) z delta

------------------------------------------------------------------
map :: (HatVal v, HatBaseClass b, Note n)
    => (Alg v b -> Alg v b) -> Journal n v b -> Journal n v b
map f = fromMap . Map.map f . toMap

parallelMap :: (NFData b, Ord k) => (a -> b) -> Map.HashMap k a -> Map.HashMap k b
parallelMap f m = Map.map f m `using` parTraversable rdeepseq

parMap :: (HatVal v, HatBaseClass b, Note n)
    => (Alg v b -> Alg v b) -> Journal n v b -> Journal n v b
parMap f = fromMap . parallelMap f . toMap

-- | insert x y : x の note が優先される
insert :: (HatVal v, HatBaseClass b, Note n)
        => Journal n v b -> Journal n v b -> Journal n v b
insert x y = fromMap (Map.union (toMap x) (toMap y))

------------------------------------------------------------------
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
projWithBase :: (HatVal v, HatBaseClass b, Note n)
             => [b] -> Journal n v b -> Journal n v b
{-# INLINE [0] projWithBase #-}
projWithBase [] _ = mempty
projWithBase bs js = fromMap $ Map.map (EA.proj bs) (toMap js)

projWithBaseNorm :: (HatVal v, HatBaseClass b, Note n)
                 => [b] -> Journal n v b -> v
projWithBaseNorm [] _ = 0
projWithBaseNorm bs js =
    Map.foldl' (\acc alg -> acc + EA.projNorm bs alg) 0 (toMap js)

------------------------------------------------------------------
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
filterWithNote :: (HatVal v, HatBaseClass b, Note n)
               => (n -> Alg v b -> Bool) -> Journal n v b -> Journal n v b
filterWithNote f = fromMap . Map.filterWithKey f . toMap

------------------------------------------------------------------
gather :: (HatVal v, HatBaseClass b, Note n)
       => n -> Journal n v b -> Journal n v b
gather n js = (toAlg js) .| n
