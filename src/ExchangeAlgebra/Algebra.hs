{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}

{- |
    Module     : ExchangeAlgebra.Algebra
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

-}


module ExchangeAlgebra.Algebra
    ( module ExchangeAlgebra.Algebra.Base
    , Nearly
    , isNearlyNum
    , Redundant(..)
    , Exchange(..)
    , HatVal(..)
    , Alg(..)
    , isZero
    , (.@)
    , (<@)
    , vals
    , bases
    , fromList
    , toList
    , sigma
    , toASCList
    , map
    , filter
    , proj
    , projCredit
    , projDebit
    , projByAccountTitle
    , projNorm
    , projCurrentAssets
    , projFixedAssets
    , projDeferredAssets
    , projCurrentLiability
    , projFixedLiability
    , projCapitalStock
    , rounding)where

import              ExchangeAlgebra.Algebra.Base

import              Debug.Trace
import qualified    Data.Text           as T
import              Data.Text           (Text)
import qualified    Data.List           as L (foldl', map, length, elem,sort,sortOn,filter, or, and,any, sum, concat)
import              Prelude             hiding (map, head, filter,tail, traverse, mapM)
import qualified    Data.Time           as Time
import              Data.Time
import qualified    Data.HashMap.Strict     as Map
import qualified    Data.IntMap.Strict      as IntMap
import qualified    Data.IntSet             as IntSet
import qualified    Data.Foldable       as Foldable (foldMap,foldl',toList)
import qualified    Data.Sequence       as Seq
import              Data.Sequence       (Seq)
import qualified    Data.Maybe          as Maybe
import qualified    Number.NonNegative  as NN  -- 非負の実数
import              Numeric.NonNegative.Class (C)
import              Data.Bifunctor
import              Data.Biapplicative
import              Algebra.Additive (C)
import qualified    Data.Scientific     as D (Scientific, fromFloatDigits, formatScientific, FPFormat(..))
import Control.DeepSeq
import Control.Parallel.Strategies (rpar, rseq, runEval, using, Strategy, rdeepseq)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Data.Hashable
import qualified Data.Binary as Binary

------------------------------------------------------------------
-- * 丸め込み判定
------------------------------------------------------------------
class (Eq a, Ord a) => Nearly a where
    isNearly     :: a -> a -> a -> Bool

instance Nearly Int where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Integer where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Float where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Double where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly NN.Double where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

{-# INLINE isNearlyNum #-}
-- | Complexity: O(1)
-- Assumes primitive numeric operations and comparisons are constant time.
isNearlyNum :: (Show a, Num a, Ord a) => a -> a -> a -> Bool
isNearlyNum x y t
    | x == y    = True
    | x >  y    = abs (x - y) <= abs t
    | x <  y    = abs (y - x) <= abs t
    | otherwise = error $ "on isNearlyNum: " ++ show x ++ ", " ++ show y

------------------------------------------------------------
-- * Algebra
------------------------------------------------------------
------------------------------------------------------------------
-- ** Definition of Reducduncy (これを継承すれば冗長代数になる)
------------------------------------------------------------------

-- | Reduncdant Class
--
--  Redundant ⊃ Exchange
--
-- hat calculation
-- >>> (.^) (10:@Not:<Cash .+ 10:@Hat:<Deposits)
-- 10.00:@Hat:<Cash .+ 10.00:@Not:<Deposits
--
-- bar calculation
-- >>> x = 10:@Not:<Cash .+ 10:@Hat:<Deposits
-- >>> y = 5:@Hat:<Cash .+ 5:@Not:<Deposits
-- >>> (.-) $ x .+ y
-- 5.00:@Not:<Cash .+ 5.00:@Hat:<Deposits
--
-- norm calculation
-- >>> norm $ 10:@Not:<Cash .+ 10:@Hat:<Deposits
-- 20.0
--
-- (.*) calculation
-- >>> (.*) 5 $ 10:@Not:<Cash .+ 10:@Hat:<Deposits
-- 50.00:@Not:<Cash .+ 50.00:@Hat:<Deposits
--
-- compress calculation
-- >>> compress $ 10:@Not:<Cash .+ 5:@Hat:<Cash .+ 3:@Not:<Cash
-- 5.00:@Hat:<Cash .+ 13.00:@Not:<Cash

class (HatVal n, HatBaseClass b, Monoid (a n b)) =>  Redundant a n b where
    -- |
    (.^) :: a n b -> a n b

    -- | bar calculation
    (.-) :: a n b -> a n b

    bar :: a n b -> a n b
    bar = (.-)

    -- | compress same Base algebras keep dividing different Hat Values
    compress :: a n b -> a n b

    -- | + calculation; alias of <> in monoid
    (.+) :: a n b -> a n b -> a n b

    -- | multiplication
    (.*) :: n -> a n b -> a n b

    -- | get value part
    norm :: a n b -> n

    {-# INLINE (<+) #-}
    (<+) :: (Applicative f) => f (a n b) -> f (a n b) -> f (a n b)
    (<+) x y = (.+) <$> x <*> y


infixr 7 .^
infixr 2 .-
infixr 3 .+
infixr 3 <+

------------------------------------------------------------
-- ** Definition of Exchange Algebra
------------------------------------------------------------
class (Redundant a n b ) => Exchange a n b where
    decR :: a n b -> a n b       -- ^ R-L decomposition
    decL :: a n b -> a n b
    decP :: a n b -> a n b       -- ^ P-M decomposition
    decM :: a n b -> a n b
    balance :: a n b -> Bool     -- ^ norm Balance
    diffRL :: a n b -> (Side, n)   -- ^ if not balanced, the R-L difference


------------------------------------------------------------------
-- * Algebra
------------------------------------------------------------------

class   ( Show n
        , Ord n
        , Eq n
        , Nearly n
        , Fractional n
        , RealFloat n
        , Num n) => HatVal n where

        zeroValue :: n

        isZeroValue :: n -> Bool
        isZeroValue x
            | zeroValue == x = True
            | otherwise      = False

        isErrorValue :: n -> Bool


instance RealFloat NN.Double where
    floatRadix      = floatRadix    . NN.toNumber
    floatDigits     = floatDigits   . NN.toNumber
    floatRange      = floatRange    . NN.toNumber
    decodeFloat     = decodeFloat   . NN.toNumber
    encodeFloat m e = NN.fromNumber (encodeFloat m e)
    exponent        = exponent      . NN.toNumber
    significand     = NN.fromNumber . significand . NN.toNumber
    scaleFloat n    = NN.fromNumber . scaleFloat n . NN.toNumber
    isNaN           = isNaN         . NN.toNumber
    isInfinite      = isInfinite    . NN.toNumber
    isDenormalized  = isDenormalized . NN.toNumber
    isNegativeZero  = isNegativeZero . NN.toNumber
    isIEEE          = isIEEE        . NN.toNumber

instance HatVal NN.Double where
    {-# INLINE zeroValue #-}
    zeroValue = 0
    {-# INLINE isErrorValue #-}
    isErrorValue x  =  isNaN        (NN.toNumber x)
                    || isInfinite   (NN.toNumber x)

instance HatVal Prelude.Double where
    {-# INLINE zeroValue #-}
    zeroValue = 0

    {-# INLINE isErrorValue #-}
    isErrorValue x  =  isNaN        x
                    || isInfinite   x
                    || x < 0

data Pair v where
 Pair :: {_hatSide :: !(Seq v)
         ,_notSide :: !(Seq v)} -> Pair v
         deriving (Eq)

instance (Binary.Binary v) => Binary.Binary (Pair v) where
    put (Pair hs ns) = do
        Binary.put (Foldable.toList hs)
        Binary.put (Foldable.toList ns)
    get = do
        hs <- Binary.get
        ns <- Binary.get
        pure (Pair (Seq.fromList hs) (Seq.fromList ns))


instance (HatVal v) => Ord (Pair v) where
    {-# INLINE compare #-}
    compare (Pair hs1 ns1) (Pair hs2 ns2) = compare ((sum hs1) - (sum ns1)) ((sum hs2) - (sum ns2))

    (<) x y | compare x y == LT = True
            | otherwise         = False

    (>) x y | compare x y == GT = True
            | otherwise         = False

    (<=) x y | compare x y == LT   = True
             | compare x y == EQ   = True
             | otherwise           = False

    (>=) x y | compare x y == GT = True
             | compare x y == EQ = True
             | otherwise         = False

    max x y | x >= y    = x
            | otherwise = y

    min x y | x <= y    = x
            | otherwise = y

{-# INLINE nullPair #-}
-- | Complexity: O(1)
nullPair :: Pair v
nullPair = Pair Seq.empty Seq.empty

{-# INLINE isNullPair #-}
-- | Complexity: O(1)
isNullPair :: Pair v -> Bool
isNullPair (Pair hs ns) = Seq.null hs && Seq.null ns

{-# INLINE pairAppend #-}
-- | Complexity: O(log(min(h1,h2)) + log(min(n1,n2)))
-- where h1/h2 and n1/n2 are the lengths of the appended 'Seq's on each side.
pairAppend :: Pair v -> Pair v -> Pair v
pairAppend (Pair x1 y1) (Pair x2 y2) =
    let !hs = x1 Seq.>< x2
        !ns = y1 Seq.>< y2
    in Pair hs ns

-- | 代数元 数値と基底のペア
data  Alg v b where
        Zero  :: Alg v b
        (:@)  :: {_val :: !v, _hatBase :: !b} -> Alg v b
        Liner :: { _realg       :: !(Map.HashMap (BasePart b) (Pair v))
                 , _axisPosting :: ~(IntMap.IntMap (Map.HashMap AxisKey IntSet.IntSet))
                 , _bpToId      :: ~(Map.HashMap (BasePart b) Int)
                 , _idToBp      :: ~(IntMap.IntMap (BasePart b))
                 , _nextBpId    :: ~Int
                 , _allBpIds    :: ~IntSet.IntSet
                 } ->  Alg v b

instance ( HatBaseClass b
         , Binary.Binary v
         , Binary.Binary b
         , Binary.Binary (BasePart b)
         ) => Binary.Binary (Alg v b) where
    put Zero = Binary.put (0 :: Int)
    put (v :@ b) = do
        Binary.put (1 :: Int)
        Binary.put v
        Binary.put b
    put (Liner m _ _ _ _ _) = do
        Binary.put (2 :: Int)
        Binary.put (Map.toList m)

    get = do
        tag <- Binary.get
        case (tag :: Int) of
            0 -> pure Zero
            1 -> (:@) <$> Binary.get <*> Binary.get
            2 -> linerFromMap . Map.fromList <$> Binary.get
            _ -> fail ("Binary decode failure for Alg: unknown tag " ++ show tag)

type AxisPosting = IntMap.IntMap (Map.HashMap AxisKey IntSet.IntSet)

{-# INLINE emptyAxisPosting #-}
-- | Complexity: O(1)
emptyAxisPosting :: AxisPosting
emptyAxisPosting = IntMap.empty

{-# INLINE insertAxisPosting #-}
-- | Complexity: O(d * (hash-insert + intset-insert))
-- In practice this is near O(d), where d is the number of axes in 'BasePart'.
insertAxisPosting :: [AxisKey] -> Int -> AxisPosting -> AxisPosting
insertAxisPosting !keys !bpId !idx =
    snd $ L.foldl' step (0 :: Int, idx) keys
  where
    step (!axis, !acc) !k =
        let !axisMap = IntMap.findWithDefault Map.empty axis acc
            !ids0 = Map.lookupDefault IntSet.empty k axisMap
            !ids1 = IntSet.insert bpId ids0
            !axisMap' = Map.insert k ids1 axisMap
            !acc' = IntMap.insert axis axisMap' acc
        in (axis + 1, acc')

{-# INLINE queryAxisPosting #-}
-- | Complexity: O(d + intersection cost)
-- d is the number of axes; intersections are performed in ascending set-size order.
queryAxisPosting :: [AxisKey] -> AxisPosting -> IntSet.IntSet -> IntSet.IntSet
queryAxisPosting !keys !idx !allIds =
    case matchedSets of
        Left ()  -> IntSet.empty
        Right [] -> allIds
        Right xs ->
            let !(x:rest) = L.sortOn IntSet.size xs
            in L.foldl' IntSet.intersection x rest
  where
    matchedSets =
        L.foldl' collect (Right []) (zip [0 :: Int ..] keys)

    collect (Left ()) _ = Left ()
    collect (Right acc) (!axis, !k)
        | axisIsWildcard k = Right acc
        | otherwise =
            case IntMap.lookup axis idx of
                Nothing -> Left ()
                Just axisMap -> case Map.lookup k axisMap of
                    Nothing -> Left ()
                    Just ids -> Right (ids : acc)

{-# INLINE linerFromMap #-}
-- | Complexity: O(n * d * (hash-insert + intset-insert))
-- n is the number of distinct base keys in the map.
linerFromMap :: (HatBaseClass b)
             => Map.HashMap (BasePart b) (Pair v)
             -> Alg v b
linerFromMap m = Liner m idx bpToId idToBp nextBpId allIds
  where
    ~(idx, bpToId, idToBp, nextBpId, allIds) =
        Map.foldlWithKey'
            (\(!idxAcc, !bpToIdAcc, !idToBpAcc, !nextId, !allIdsAcc) bp _ ->
                let !bpId = nextId
                    !idx' = insertAxisPosting (toAxisKeys bp) bpId idxAcc
                    !bpToId' = Map.insert bp bpId bpToIdAcc
                    !idToBp' = IntMap.insert bpId bp idToBpAcc
                    !allIds' = IntSet.insert bpId allIdsAcc
                in (idx', bpToId', idToBp', bpId + 1, allIds'))
            (emptyAxisPosting, Map.empty, IntMap.empty, 0, IntSet.empty)
            m

-- | Complexity: O(1)
isZero :: Alg v b -> Bool
isZero Zero = True
isZero _    = False

{-# INLINE singleton #-}
-- | Complexity: O(1)
singleton :: (HatVal v, HatBaseClass b) => v -> b -> Alg v b
singleton v b | isZeroValue v  = Zero
              | isErrorValue v = error  $ "errorValue at (.@) val: "
                               ++ show v
                               ++ show ":@"
                               ++ show b
              | otherwise      = v :@ b

{-# INLINE (.@) #-}
-- | Complexity: O(1)
(.@) :: (HatVal n, HatBaseClass b) => n -> b -> Alg n b
(.@) v b = singleton v b

-- | Complexity: O(1) plus Applicative effects.
(<@) :: (HatVal n, Applicative f, HatBaseClass b)
     => f n  -> b -> f (Alg n b)
(<@) v b = (.@) <$> v <*> (pure b)


infixr 6 :@
infixr 6 .@
infixr 6 <@

-- | Complexity: O(digits(v))
-- Formatting cost is proportional to the textual precision of the number.
showV ::  (HatVal v) => v -> String
showV v = D.formatScientific D.Generic (Just 2) (D.fromFloatDigits v)

instance (HatVal v, HatBaseClass b) =>  Eq (Alg v b) where
    (==) Zero Zero = True
    (==) Zero _    = False
    (==) _    Zero = False
    (==) (v1:@b1) (v2:@b2) = (v1 == v2) && (b1 == b2)
    (==) (Liner m1 _ _ _ _ _) (Liner m2 _ _ _ _ _) = m1 == m2
    (==) _ _ = False
    (/=) x y = not (x == y)

instance (HatVal v, HatBaseClass b) => Ord (Alg v b) where
    {-# INLINE compare #-}
    compare Zero Zero = EQ
    compare Zero _ = LT
    compare _ Zero = GT

    compare (v:@b) (Liner _ _ _ _ _ _) = LT
    compare (Liner _ _ _ _ _ _) (v:@b) = GT
    compare (v1:@b1) (v2:@b2)
        | b1 == b2 = compare v1 v2
        | b1 >  b2  = GT
        | b1 <  b2  = LT

    compare (Liner m1 _ _ _ _ _) (Liner m2 _ _ _ _ _) = compare m1 m2

    (<) x y | compare x y == LT = True
            | otherwise         = False

    (>) x y | compare x y == GT = True
            | otherwise         = False

    (<=) x y | compare x y == LT   = True
             | compare x y == EQ   = True
             | otherwise           = False

    (>=) x y | compare x y == GT = True
             | compare x y == EQ = True
             | otherwise         = False

    max x y | x >= y    = x
            | otherwise = y

    min x y | x <= y    = x
            | otherwise = y

instance (HatVal v, HatBaseClass b) => Show (Alg v b) where
    show Zero       = "0"
    show (v:@b)     = (showV v) ++ ":@" ++ show b
    show xs = let ls = toASCList xs
            in  go ls
        where
            go []     = "0"
            go [y]    = show y
            go (y:ys) = show y ++ " .+ " ++ go ys


instance NFData (Alg v b) where
    rnf Zero      = Zero `seq` ()
    rnf (v:@b)    = v `seq` b `seq` ()
    rnf (Liner m _ _ _ _ _) = Map.foldrWithKey (\k v acc -> k `seq` v `seq` acc) () m
------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------

instance  (HatVal n, HatBaseClass b) => Semigroup (Alg n b) where
    {-# INLINE (<>) #-}
    -- | Associative law ;convert to right join
    (<>)  = union



-- | union two trees
--
-- >>> type Test = Alg NN.Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Hat:<Yen .+ 2:@Not:<Amount :: Test
-- >>> union x y
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen .+ 1.00:@Not:<Amount .+ 2.00:@Not:<Amount
{-# INLINE union #-}
-- | Complexity:
--   - singleton/singleton and singleton/liner cases: O(n * d * index-build)
--   - liner/liner case: O(n + m) for map union plus O((n+m) * d * index-build)
-- where n and m are distinct key counts on each side.
union :: (HatVal n, HatBaseClass b) =>  Alg n b -> Alg n b -> Alg n b
union Zero x  = x
union x Zero  = x
-- singletons
union (v1:@b1) (v2:@b2)
    | isZeroValue v1 = case isZeroValue v2 of
                            True  -> Zero
                            False -> v2:@b1
    | isZeroValue v2 = v1:@b2
    | otherwise      = insert b2 v2 (v1:@b1)
-- If one side is a singleton
union x (v:@b) = insert b v x
union (v:@b) x = insert b v x

-- In the case of multiple elements
union (Liner m1 _ _ _ _ _) (Liner m2 _ _ _ _ _) = linerFromMap (Map.unionWith pairAppend m1 m2)


{-# INLINE insert #-}
-- | Complexity:
--   - into Zero or singleton: O(1) to O(d * index-build)
--   - into Liner: O(n * d * index-build) due to rebuilding 'linerFromMap'
-- where n is the number of distinct base keys after insertion.
insert :: (HatVal v,HatBaseClass b) => b -> v -> Alg v b ->  Alg v b
insert _ v x | isZeroValue v = x
insert !b !v Zero       = v .@ b
insert !b1 !v1 (v2:@b2) = case isHat b1 of
                            True  -> insert b2 v2
                                   $ linerFromMap
                                   $ Map.singleton (base b1)
                                   $ nullPair {_hatSide = Seq.singleton v1}
                            False -> insert b2 v2
                                   $ linerFromMap
                                   $ Map.singleton (base b1)
                                   $ nullPair {_notSide = Seq.singleton v1}
insert !b !v (Liner m _ _ _ _ _)  = case isHat b of
                        True  -> insertLiner (nullPair {_hatSide = Seq.singleton v})
                        False -> insertLiner (nullPair {_notSide = Seq.singleton v})
  where
    !bp = base b
    insertLiner !pairToInsert =
        let !m' = Map.insertWith pairAppend bp pairToInsert m
        in linerFromMap m'

------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------

instance (HatVal n, HatBaseClass b) => Monoid (Alg n b) where
    -- 単位元
    mempty = Zero
    mappend = (<>)
    mconcat = unions

{-# INLINE unions #-}
-- | Complexity: O(sum of 'union' costs over the fold)
-- For a long list this is typically the dominant construction cost.
unions :: (HatVal n, Foldable f, HatBaseClass b) => f (Alg n b) -> Alg n b
unions ts = Foldable.foldl' union Zero ts

------------------------------------------------------------------
-- Redundant
------------------------------------------------------------------

instance (HatVal n, HatBaseClass b) => Redundant Alg n b where
    (.^) Zero       = Zero
    (.^) (n:@ b)    = n :@ (revHat b)
    (.^) (Liner ms idx bpToId idToBp nextBpId allIds) = Liner
                    (Map.map (\ (Pair hs ns) -> Pair ns hs) ms)
                    idx
                    bpToId
                    idToBp
                    nextBpId
                    allIds

    (.+) = mappend

    x  .*  Zero      = Zero
    0  .*  x         = Zero
    x  .* (v:@b)     = (x * v) :@ b
    x  .* (Liner ms idx bpToId idToBp nextBpId allIds) = Liner
                     (Map.map (\ (Pair hs ns) -> Pair (fmap (x *) hs) (fmap (x *) ns)) ms)
                     idx
                     bpToId
                     idToBp
                     nextBpId
                     allIds

    norm Zero       = 0
    norm (v:@b)     = v
    norm (Liner ms _ _ _ _ _) = Map.foldl' (\ !x (Pair hs ns) -> x + Foldable.foldl' (+) 0 hs + Foldable.foldl' (+) 0 ns) 0 ms

    {-# INLINE (.-) #-}
    (.-) Zero = Zero
    (.-) (v:@b) = v:@b
    (.-) (Liner m _ _ _ _ _) = let !res = Map.mapMaybe f m
                   in case null res of
                        True -> Zero
                        False -> linerFromMap res
        where
            {-# INLINE f #-}
            f (Pair hs ns) = let (h, n) = (sum hs,sum ns)
                           in case isNearlyNum h n 1e-13 of -- 精度 13桁
                                        True -> Nothing
                                        False -> case compare h n of
                                                    GT -> Just (Pair (Seq.singleton (h - n)) Seq.empty)
                                                    LT -> Just (Pair Seq.empty (Seq.singleton (n - h)))

    {-# INLINE compress #-}
    compress Zero       = Zero
    compress (v:@b)     = v:@b
    compress (Liner m idx bpToId idToBp nextBpId allIds)  = Liner
                        (Map.map (\ (Pair hs ns) -> Pair (Seq.singleton (sum hs))
                                                          (Seq.singleton (sum ns))) m)
                        idx
                        bpToId
                        idToBp
                        nextBpId
                        allIds


instance (HatVal n, ExBaseClass b) =>  Exchange Alg n b where
    -- | filter Debit side
    decR xs = filter (\x -> x /= Zero && (whichSide . _hatBase) x == Debit) xs

    -- | filter Credit side
    decL xs = filter (\x -> x /= Zero && (whichSide . _hatBase) x == Credit) xs

    -- | filter Plus Stock
    decP xs = filter (\x -> x /= Zero && (isHat . _hatBase ) x) xs

    -- | filter Minus Stock
    decM xs = filter (\x -> x /= Zero && (not. isHat. _hatBase) x) xs

    -- | check Credit Debit balance
    balance xs  | (norm . decR) xs == (norm . decL) xs = True
                | otherwise                            = False

    -- |
    diffRL xs  | r > l = (Debit, r - l)
               | l > r = (Credit, l -r)
               | otherwise = (Side,0)
        where
        r = (norm . decR) xs
        l = (norm . decL) xs

------------------------------------------------------------------
-- * 基本の関数
------------------------------------------------------------------

-- | vals
-- get vals
-- Complexity: O(s), where s is total number of scalar entries stored in all pairs.
vals :: (HatVal v, HatBaseClass b) => Alg v b -> [v]
vals Zero = []
vals (v:@b) = [v]
vals (Liner m _ _ _ _ _) =
    reverse $
        Map.foldl'
            (\acc (Pair hs ns) ->
                Foldable.foldl' (flip (:))
                    (Foldable.foldl' (flip (:)) acc hs)
                    ns
            )
            []
            m


-- | bases
-- get bases
-- Complexity: O(s), where s is total number of scalar entries stored in all pairs.
bases :: (HatVal v, HatBaseClass b) => Alg v b -> [b]
bases Zero = []
bases (v:@b) = [b]
bases (Liner m _ _ _ _ _) = Map.foldlWithKey' f [] m
    where
        f ::  (HatVal v, HatBaseClass b) => [b] -> BasePart b -> Pair v ->  [b]
        f xs b (Pair {_hatSide = hs, _notSide = ns})
            = Foldable.foldl' (g Not b) (Foldable.foldl' (g Hat b) xs hs) hs

        g ::  (HatVal v, HatBaseClass b) => Hat -> BasePart b -> [b] -> v -> [b]
        g h b ys v = (merge h b):ys

{-# INLINE fromList #-}
-- | convert List to Alg n b
-- Complexity: O(sum of 'union' costs), because this is implemented via 'mconcat'.
--
-- >>> type Test = Alg NN.Double (HatBase AccountTitles)
-- >>> xs = [1:@Hat:<Cash,1:@Not:<Deposits, 2:@Hat:<Cash, 2:@Not:<Deposits] :: [Test]
-- >>> fromList xs
-- 1.00:@Hat:<Cash .+ 2.00:@Hat:<Cash .+ 1.00:@Not:<Deposits .+ 2.00:@Not:<Deposits
--
--  >>> type Test = Alg NN.Double (HatBase CountUnit)
--  >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
--  >>> y = 2:@Hat:<Yen .+ 2:@Not:<Amount :: Test
--  >>> fromList [x,y]
--  1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen .+ 1.00:@Not:<Amount .+ 2.00:@Not:<Amount

fromList ::(HatVal v, HatBaseClass b ) => [Alg v b] -> Alg v b
fromList = mconcat



-- | sigma 
-- Complexity: O(sum of 'union' costs over produced elements).
--
-- >>> type Test = Alg NN.Double (HatBase CountUnit)
-- >>> sigma [1,2] (\x -> x:@Hat:<Yen)
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen

sigma :: (HatVal v, HatBaseClass b) => [a] -> (a -> Alg v b) -> Alg v b
sigma xs f = L.foldl' (\acc x -> acc <> f x) Zero xs

-- | convert Alg n b to List
-- Complexity: O(s), where s is total number of scalar entries.
--
-- >>> toList (10:@Hat:<(Cash) .+ 10:@Hat:<(Deposits) .+ Zero :: Alg NN.Double (HatBase AccountTitles))
-- [10.00:@Hat:<Deposits,10.00:@Hat:<Cash]
--
-- you need define type variables to use this for Zero
-- >>> toList Zero :: [Alg NN.Double (HatBase AccountTitles)]
-- []
toList :: (HatVal v, HatBaseClass b) => Alg v b -> [Alg v b]
toList Zero       = []
toList (v:@b)     = [v:@b]
toList (Liner m _ _ _ _ _)  = Map.foldlWithKey' f [] m
    where
        f :: (HatVal v, HatBaseClass b) =>  [Alg v b] -> BasePart b -> Pair v -> [Alg v b]
        f xs b Pair {_hatSide = hs, _notSide = ns}
            = Foldable.foldl' (g Hat b) (Foldable.foldl' (g Not b) xs ns) hs

        g :: (HatVal v, HatBaseClass b) => Hat -> BasePart b -> [Alg v b] -> v -> [Alg v b]
        g h b ys v
            | isZeroValue v = ys
            | otherwise     = (v :@ (merge h b)):ys

{-# INLINE toASCList #-}
-- | Complexity: O(s log s), dominated by sorting the list representation.
toASCList :: (HatVal v, HatBaseClass b) => Alg v b -> [Alg v b]
toASCList = L.sort . toList


-- | map
-- Complexity: O(s + c), where s is traversed scalar entries and c is transformed output size.
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> map (\ (x:@hb) ->  x:@(toHat hb)) $ x .+ y
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen .+ 1.00:@Hat:<Amount .+ 2.00:@Hat:<Amount
--
-- >>> type Test = Alg Double Hat
-- >>> x = 1:@Hat .+ 1:@Not :: Test
-- >>> y = 2:@Not .+ 2:@Hat :: Test
-- >>> map (\ (x:@hb) -> (2 * x):@hb) $ x .+ y
-- 2.00:@Hat .+ 4.00:@Hat .+ 2.00:@Not .+ 4.00:@Not

map :: (HasCallStack,HatVal v, HatBaseClass b)
     => (Alg v b -> Alg v b) -> Alg v b -> Alg v b
map f Zero      = Zero
map f (v:@b)    = let  v2:@b2 = f (v:@b)
                in case isZeroValue v2 of
                    True  -> Zero
                    False -> (v2 :@ b2)
map f (Liner m _ _ _ _ _) = mkAlgFromMap $ (Map.foldrWithKey (p f) dnilMap m) Map.empty
    where
        {-# INLINE dnilMap #-}
        dnilMap = id
        {-# INLINE dappendMap #-}
        dappendMap = (.)
        {-# INLINE dsingleMap #-}
        dsingleMap (bp, p') = Map.insertWith pairAppend bp p'

        {-# INLINE p #-}
        p :: (HatVal v, HatBaseClass b)
          => (Alg v b -> Alg v b)
          -> BasePart b
          -> Pair v
          -> DMap (BasePart b) (Pair v)
          -> DMap (BasePart b) (Pair v)
        p f b Pair {_hatSide=hs, _notSide=ns} accDList =
            let (dl1, hs2) = q f Hat b hs
                (dl2, ns2) = q f Not b ns
                prefix     = dappendMap dl1 dl2
            in case (Seq.null hs2, Seq.null ns2) of
                (True,True)   -> dappendMap prefix accDList
                (True,False)  -> dappendMap prefix
                               . dappendMap (dsingleMap (b, nullPair{_notSide = ns2}))
                               $ accDList
                (False,True)  -> dappendMap prefix
                               . dappendMap (dsingleMap (b, nullPair{_hatSide = hs2}))
                               $ accDList
                (False,False) -> dappendMap prefix
                               . dappendMap (dsingleMap (b, Pair hs2 ns2))
                               $ accDList
        {-# INLINE q #-}
        q :: (HatVal v, HatBaseClass b)
          => (Alg v b -> Alg v b)
          -> Hat
          -> BasePart b
          -> Seq v
          -> (DMap (BasePart b) (Pair v), Seq v)
        q f h b vs = Foldable.foldl' (r f h b) (dnilMap, Seq.empty) vs

        {-# INLINE r #-}
        r  :: (HatVal v, HatBaseClass b)
           => (Alg v b -> Alg v b)
           -> Hat
           -> BasePart b
           -> (DMap (BasePart b) (Pair v), Seq v)
           -> v
           -> (DMap (BasePart b) (Pair v), Seq v)
        r f h b (dlAcc,vsAcc) v = case f (v:@(merge h b)) of
                            Zero   ->  (dlAcc, vsAcc)
                            ------------------------------------------------------------------
                            v2:@b2
                                | isZeroValue v2 ->  (dlAcc, vsAcc)
                                | b2 .== (merge h b) -> (dlAcc, v2 Seq.<| vsAcc)
                                | isHat (hat b2)     -> (dappendMap dlAcc (dsingleMap ( base b2
                                                                          ,nullPair{_hatSide = Seq.singleton v2}))
                                                        ,vsAcc )
                                | otherwise          -> (dappendMap dlAcc (dsingleMap ( base b2
                                                                          ,nullPair{_notSide = Seq.singleton v2} ))
                                                        ,vsAcc )

-- 差分リストを定義
type DList a = [a] -> [a]
type DMap k v = Map.HashMap k v -> Map.HashMap k v

{-# INLINE dnil #-}
-- | Complexity: O(1)
dnil :: DList a
dnil = id

{-# INLINE dappend #-}
-- | Complexity: O(1)
dappend :: DList a -> DList a -> DList a
dappend = (.)  -- 関数合成

{-# INLINE dsingle #-}
-- | Complexity: O(1)
dsingle :: a -> DList a
dsingle x = \rest -> x : rest

{-# INLINE dToList #-}
-- | Complexity: O(k), where k is the resulting list length.
dToList :: DList a -> [a]
dToList dl = dl []

{-# INLINE dFromList #-}
-- | Complexity: O(k) to capture the prefix list xs.
dFromList :: [a] -> DList a
dFromList xs = (xs ++)


{-# INLINE filter #-}
-- | filter
-- Complexity: O(s), where s is total number of scalar entries.
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> filter (isHat . _hatBase) $ x .+ y
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Amount
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> filter ((1 <). _val) $ x .+ y
-- 2.00:@Not:<Yen .+ 2.00:@Hat:<Amount


filter :: (HatVal v, HatBaseClass b) => (Alg v b -> Bool) -> Alg v b -> Alg v b
filter f Zero                 = Zero
filter f (v:@b) | f (v:@b)    = v:@b
                | otherwise   = Zero

filter f (Liner m _ _ _ _ _) =
    -- mapMaybeWithKey で新しい Map を構築
    let m' = Map.mapMaybeWithKey
               (\basePart (Pair hs ns) ->
                  -- hs, ns それぞれをフィルタ
                  let hs' = filterSide basePart Hat hs
                      ns' = filterSide basePart Not ns
                  in
                    -- 両方とも空になればエントリ削除 (Nothing)
                    if Seq.null hs' && Seq.null ns'
                       then Nothing
                       else Just (Pair hs' ns'))
             m
    in
      -- 結果の Map が空なら Zero, そうでなければ Liner m'
      if Map.null m' then Zero else linerFromMap m'
  where
    ----------------------------------------------------------------
    -- basePart と Hat/Not を元に「v:@(merge h basePart)」を作り，
    -- 与えられた述語 f を満たすか判定するためのフィルタ関数
    ----------------------------------------------------------------
    -- filterSide :: BasePart b -> Hat -> Seq v -> Seq v
    {-# INLINE filterSide #-}
    filterSide bp h = Seq.filter (\val -> f (val :@ merge h bp))

------------------------------------------------------------
-- | proj
-- Complexity:
--   - exact single-key path: expected O(1)
--   - wildcard single-key path: O(queryAxisPosting + c * verify)
--   - multi-pattern path: O(sum pattern costs + union costs)
-- where c is candidate count returned by the posting index.
-- >>> type Test = Alg NN.Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> proj [Hat:<Yen] $ x .+ y
-- 1.00:@Hat:<Yen
--
-- >>> type Test = Alg NN.Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> proj [HatNot:<Amount] $ x .+ y
-- 2.00:@Hat:<Amount .+ 1.00:@Not:<Amount
--
-- >>> type Test = Alg NN.Double (HatBase (AccountTitles, CountUnit))
-- >>> x = 1:@Hat:<(Cash,Yen) .+ 1:@Not:<(Products,Amount) :: Test
-- >>> y = 2:@Not:<(Cash,Yen) .+ 2:@Hat:<(Deposits,Yen) :: Test
-- >>> proj [Hat:<((.#),Yen)] $ x .+ y
-- 1.00:@Hat:<(Cash,Yen) .+ 2.00:@Hat:<(Deposits,Yen)
--
-- >>> type Test = HatBase CountUnit
-- >>> compareHatBase (Not:<(.#) :: Test) (Not:<Yen :: Test)
-- EQ
--
-- >>> type Test = Alg NN.Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> proj [Not:<(.#)] $ x .+ y
-- 2.00:@Not:<Yen .+ 1.00:@Not:<Amount
--
------------------------------------------------------------

proj :: (HatVal v, HatBaseClass b)  => [b] -> Alg v b -> Alg v b
proj []     _         = Zero
proj _     Zero       = Zero
proj [b] (v:@b2)
    | b .== b2  = v:@b2
    | otherwise = Zero
proj [b] (Liner m idx _ idToBp _ allIds) =
    mkAlgFromMap $ projSingleMap b m idx idToBp allIds
proj (b:bs) (v:@b2)
    |  b .== b2       = v:@b2
    | otherwise       = proj bs (v:@b2)
proj (b:bs) (Liner m idx _ idToBp _ allIds) =
    mkAlgFromMap $
        L.foldl'
            (\acc q -> Map.unionWith pairAppend acc (projSingleMap q m idx idToBp allIds))
            Map.empty
            (b:bs)

{-# INLINE choosePairByHat #-}
-- | Complexity: O(1)
choosePairByHat :: Hat -> Pair v -> Pair v
choosePairByHat h Pair {_hatSide = hs, _notSide = ns} =
    case h of
        Hat    -> nullPair {_hatSide = hs}
        Not    -> nullPair {_notSide = ns}
        HatNot -> Pair {_hatSide = hs, _notSide = ns}

{-# INLINE projSingleMap #-}
-- | Complexity:
--   - wildcard path: O(queryAxisPosting + c * verify)
--   - exact path: expected O(1)
projSingleMap
    :: (HatBaseClass b)
    => b
    -> Map.HashMap (BasePart b) (Pair v)
    -> AxisPosting
    -> IntMap.IntMap (BasePart b)
    -> IntSet.IntSet
    -> Map.HashMap (BasePart b) (Pair v)
projSingleMap b m idx idToBp allIds
    | haveWiledcard bp =
        let !ids = queryAxisPosting (toAxisKeys bp) idx allIds
        in IntSet.foldl'
            (\acc bpId -> case IntMap.lookup bpId idToBp of
                Nothing -> acc
                Just bp0 -> case Map.lookup bp0 m of
                    Nothing -> acc
                    Just p  -> if bp .== bp0
                        then Map.insert bp0 (choosePairByHat h p) acc
                        else acc)
            Map.empty
            ids
    | otherwise = case Map.lookup bp m of
        Nothing -> Map.empty
        Just p  -> Map.singleton bp (choosePairByHat h p)
  where
    !bp = base b
    !h = hat b

{-# INLINE mkAlgFromMap #-}
-- | Complexity: O(n) to inspect shape and possibly rebuild index.
mkAlgFromMap :: (HatVal v, HatBaseClass b) => Map.HashMap (BasePart b) (Pair v) -> Alg v b
mkAlgFromMap m
    | Map.null m = Zero
    | otherwise  = case Map.toList m of
        [(b, p)] -> Maybe.fromMaybe (linerFromMap $ Map.singleton b p) (singlePairToAlg b p)
        _        -> linerFromMap m

{-# INLINE singlePairToAlg #-}
-- | Complexity: O(1)
singlePairToAlg :: (HatVal v, HatBaseClass b) => BasePart b -> Pair v -> Maybe (Alg v b)
singlePairToAlg b (Pair hs ns) = case (Seq.viewl hs, Seq.viewl ns) of
    (Seq.EmptyL, n Seq.:< nsRest) | Seq.null nsRest -> Just (n :@ merge Not b)
    (h Seq.:< hsRest, Seq.EmptyL) | Seq.null hsRest -> Just (h :@ merge Hat b)
    _                                                 -> Nothing

------------------------------------------------------------------

-- | proj devit algs の代わりに Elem に Text や Int などがある場合は projCredit を使う
-- Complexity: O(s), implemented via 'filter'.
projCredit :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCredit = filter (\x -> (whichSide . _hatBase) x == Credit)

-- | proj debit algs の代わりに Elem に Text や Int などがある場合は projDebit を使う
-- Complexity: O(s), implemented via 'filter'.
projDebit :: (HatVal n, ExBaseClass b)  => Alg n b -> Alg n b
projDebit = filter (\x -> (whichSide . _hatBase) x == Debit)

-- | Complexity: O(s), implemented via 'filter'.
projByAccountTitle :: (HatVal n, ExBaseClass b) => AccountTitles -> Alg n b -> Alg n b
projByAccountTitle at alg = filter (f at) alg
    where
        f :: (HatVal n,ExBaseClass b) => AccountTitles -> Alg n b -> Bool
        f at Zero = False
        f at x    = ((getAccountTitle ._hatBase) x) .== at

-- | Complexity: O(cost(proj) + cost(bar) + cost(norm)).
projNorm :: (HatVal n, HatBaseClass b) => [b] -> Alg n b -> n
projNorm [] _ = 0
projNorm _ Zero = 0
projNorm bs (v :@ b)
    | L.any (.== b) bs = v
    | otherwise        = 0
projNorm [b] (Liner m idx _ idToBp _ allIds) =
    foldProjectedNorm (projSingleMap b m idx idToBp allIds)
projNorm bs (Liner m idx _ idToBp _ allIds) =
    foldProjectedNorm $
        L.foldl'
            (\acc q -> Map.unionWith pairAppend acc (projSingleMap q m idx idToBp allIds))
            Map.empty
            bs

{-# INLINE foldProjectedNorm #-}
-- | Complexity: O(k), where k is the number of projected base keys.
foldProjectedNorm :: (HatVal n) => Map.HashMap k (Pair n) -> n
foldProjectedNorm = Map.foldl' (\acc p -> acc + barNormPair p) 0

{-# INLINE barNormPair #-}
-- | Complexity: O(h + n), where h/n are side lengths within the pair.
barNormPair :: (HatVal n) => Pair n -> n
barNormPair (Pair hs ns) =
    let !h = Foldable.foldl' (+) 0 hs
        !n = Foldable.foldl' (+) 0 ns
    in if isNearlyNum h n 1e-13
        then 0
        else if h > n then h - n else n - h


-- | 流動資産の取得
-- Complexity: O(s), a constant-depth composition of filters.
projCurrentAssets :: ( HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCurrentAssets  = (filter (\x -> (fixedCurrent . _hatBase) x == Current))
                   . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                   . projDebit

-- | 固定資産
-- Complexity: O(s), a constant-depth composition of filters.
projFixedAssets :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projFixedAssets = (filter (\x -> (fixedCurrent . _hatBase) x == Fixed))
                . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                . projDebit

-- | 繰延資産
-- 税法固有の繰延資産は、「投資その他の資産」に長期前払費用等の適当な項目を付して表示する。
-- Complexity: O(s), a constant-depth composition of filters.
projDeferredAssets :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projDeferredAssets  = (filter (\x -> (fixedCurrent . _hatBase) x == Other))
                    . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                    . projDebit

-- | 流動負債
-- Complexity: O(s), a constant-depth composition of filters.
projCurrentLiability :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCurrentLiability  = (filter (\x -> (fixedCurrent . _hatBase) x == Current))
                      . (filter (\x -> (whatDiv . _hatBase) x      == Liability))
                      . projCredit

-- | 固定負債
-- Complexity: O(s), a constant-depth composition of filters.
projFixedLiability :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projFixedLiability  = (filter (\x -> (fixedCurrent . _hatBase) x == Fixed))
                    . (filter (\x -> (whatDiv . _hatBase) x      == Liability))
                    . projCredit

-- | 株主資本
-- Complexity: O(1) currently (undefined placeholder).
projCapitalStock :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCapitalStock = undefined


-- * バランス

{- | バランスしていない場合の処理 -}
-- Complexity: O(1) currently (undefined placeholder).
forceBalance = undefined


-- * 端数処理

{- | 端数処理
割り算と掛け算に利用
基本的には切り上げで処理する
勘定科目の乗除には全てこれを適用
-}

rounding :: NN.Double -> NN.Double
-- Complexity: O(1)
rounding = fromIntegral . ceiling
