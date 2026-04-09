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

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

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
    , foldEntries
    , sigma
    , sigma2When
    , sigmaFromMap
    , toASCList
    , map
    , filter
    , proj
    , projCredit
    , projDebit
    , projByAccountTitle
    , projNorm
    , balanceBy
    , foldEntriesToMap
    , projCurrentAssets
    , projFixedAssets
    , projDeferredAssets
    , projCurrentLiability
    , projFixedLiability
    , projCapitalStock
    , rounding
    , unionsMerge)where

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
import qualified    Data.Map.Strict         as M
import qualified    Data.Foldable       as Foldable (foldMap,foldl',foldr,toList)
import qualified    Data.Sequence       as Seq
import              Data.Sequence       (Seq)
import qualified    Data.Maybe          as Maybe
import qualified    Number.NonNegative  as NN  -- Non-negative real numbers
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
-- * Approximate equality
------------------------------------------------------------------

-- | Type class providing approximate equality for numeric values.
-- Performs equality comparison with tolerance for floating-point rounding errors.
class (Eq a, Ord a) => Nearly a where
    -- | @isNearly x y t@ : Returns True if the difference between x and y is within the tolerance t.
    -- Complexity: O(1)
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
-- ** Definition of Redundancy (subclassing this makes a redundant algebra)
------------------------------------------------------------------

-- | Type class for Redundant Algebra.
-- Provides fundamental exchange algebra operations: hat, bar, norm, scalar product, and compress.
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
    -- | Hat operation. Flips Hat/Not on all elements.
    -- Complexity: O(1) for singleton, O(n) for Liner (n is the number of base keys)
    (.^) :: a n b -> a n b

    -- | Bar operation. Cancels Hat/Not on the same base and retains only the difference.
    -- Complexity: O(n) (n is the number of base keys)
    (.-) :: a n b -> a n b

    -- | Alias for bar operation. Identical to @(.-)@.
    bar :: a n b -> a n b
    bar = (.-)

    -- | Aggregates values on the same base. Sums while preserving the Hat/Not distinction.
    -- Complexity: O(n) (n is the number of base keys)
    compress :: a n b -> a n b

    -- | Addition of algebra elements. Alias for the Monoid @<>@ operation.
    -- Complexity: O(union cost)
    (.+) :: a n b -> a n b -> a n b

    -- | Scalar product. Multiplies all element values by a scalar.
    -- Complexity: O(1) for singleton, O(n) for Liner
    (.*) :: n -> a n b -> a n b

    -- | Norm. Returns the sum of all element values.
    -- Complexity: O(n) (n is the number of base keys)
    norm :: a n b -> n

    -- | Addition in an Applicative context.
    -- Complexity: O(union cost)
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

-- | Type class for Exchange Algebra. In addition to Redundant Algebra, provides
-- debit(R)/credit(L) decomposition, stock increase(P)/decrease(M) decomposition, and balance checking.
class (Redundant a n b ) => Exchange a n b where
    -- | Extracts only the debit side elements. Complexity: O(s)
    decR :: a n b -> a n b
    -- | Extracts only the credit side elements. Complexity: O(s)
    decL :: a n b -> a n b
    -- | Extracts only the Hat (stock increase) side elements. Complexity: O(s)
    decP :: a n b -> a n b
    -- | Extracts only the Not (stock decrease) side elements. Complexity: O(s)
    decM :: a n b -> a n b
    -- | Checks whether the norms of debit and credit sides are equal. Complexity: O(s)
    balance :: a n b -> Bool
    -- | Returns the debit-credit difference as a (Side, difference) pair. Complexity: O(s)
    diffRL :: a n b -> (Side, n)


------------------------------------------------------------------
-- * Algebra
------------------------------------------------------------------

-- | Type class for algebra element values.
-- Provides zero-value and error-value predicates.
-- Instances are defined for 'Double' and 'NN.Double' (non-negative reals).
class   ( Show n
        , Ord n
        , Eq n
        , Nearly n
        , Fractional n
        , RealFloat n
        , Num n) => HatVal n where

        -- | Zero value. Complexity: O(1)
        zeroValue :: n

        -- | Tests whether the value is zero. Complexity: O(1)
        isZeroValue :: n -> Bool
        isZeroValue x
            | zeroValue == x = True
            | otherwise      = False

        -- | Tests whether the value is an error value (NaN, Infinity, etc.). Complexity: O(1)
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
    {-# INLINABLE put #-}
    {-# INLINABLE get #-}
    put (Pair hs ns) = do
        Binary.put (Seq.length hs :: Int)
        Foldable.foldr (\x k -> Binary.put x >> k) (pure ()) hs
        Binary.put (Seq.length ns :: Int)
        Foldable.foldr (\x k -> Binary.put x >> k) (pure ()) ns
    get = do
        hsLen <- Binary.get :: Binary.Get Int
        hs <- go hsLen Seq.empty
        nsLen <- Binary.get :: Binary.Get Int
        ns <- go nsLen Seq.empty
        pure (Pair hs ns)
      where
        go :: Binary.Binary a => Int -> Seq a -> Binary.Get (Seq a)
        go n !acc
            | n <= 0 = pure acc
            | otherwise = do
                x <- Binary.get
                go (n - 1) (acc Seq.|> x)


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

-- | Algebra element. An element of exchange algebra consisting of a value-base pair.
-- Zero is the zero element, @(:@)@ is a singleton, and Liner is a HashMap-based multi-element representation.
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
    {-# INLINABLE put #-}
    {-# INLINABLE get #-}
    put Zero = Binary.put (0 :: Int)
    put (v :@ b) = do
        Binary.put (1 :: Int)
        Binary.put v
        Binary.put b
    put (Liner m _ _ _ _ _) = do
        Binary.put (2 :: Int)
        Binary.put (Map.size m :: Int)
        Map.foldrWithKey
            (\bp p k -> Binary.put bp >> Binary.put p >> k)
            (pure ())
            m

    get = do
        tag <- Binary.get
        case (tag :: Int) of
            0 -> pure Zero
            1 -> (:@) <$> Binary.get <*> Binary.get
            2 -> do
                n <- Binary.get :: Binary.Get Int
                linerFromMap <$> go n Map.empty
            _ -> fail ("Binary decode failure for Alg: unknown tag " ++ show tag)
      where
        go n !acc
            | n <= 0 = pure acc
            | otherwise = do
                bp <- Binary.get
                p <- Binary.get
                go (n - 1) (Map.insert bp p acc)

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

-- | Tests whether the algebra element is zero (empty).
--
-- Complexity: O(1)
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
-- | Smart constructor that builds an algebra element from a value and a base.
-- Returns Zero for zero values, and throws an exception for error values.
--
-- Complexity: O(1)
(.@) :: (HatVal n, HatBaseClass b) => n -> b -> Alg n b
(.@) v b = singleton v b

-- | Constructs an algebra element in an Applicative context. Lifted version of @(.@)@.
--
-- Complexity: O(1) + Applicative effects
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
    -- Identity element
    mempty = Zero
    mappend = (<>)
    mconcat = unions

{-# INLINE unions #-}
-- | Complexity: O(sum of 'union' costs over the fold)
-- For a long list this is typically the dominant construction cost.
unions :: (HatVal n, Foldable f, HatBaseClass b) => f (Alg n b) -> Alg n b
unions ts = Foldable.foldl' union Zero ts

{-# INLINE mergeAlgMap #-}
mergeAlgMap :: (HatVal n, HatBaseClass b)
            => Map.HashMap (BasePart b) (Pair n)
            -> Alg n b
            -> Map.HashMap (BasePart b) (Pair n)
mergeAlgMap !acc Zero = acc
mergeAlgMap !acc (v :@ b)
    | isZeroValue v = acc
    | otherwise =
        let !p = if isHat b
                 then nullPair {_hatSide = Seq.singleton v}
                 else nullPair {_notSide = Seq.singleton v}
        in Map.insertWith pairAppend (base b) p acc
mergeAlgMap !acc (Liner m _ _ _ _ _)
    | Map.null m = acc
    | otherwise = Map.unionWith pairAppend acc m

{-# INLINE mergeAlgMapIfNonZero #-}
mergeAlgMapIfNonZero :: (HatVal n, HatBaseClass b)
                     => Map.HashMap (BasePart b) (Pair n)
                     -> Alg n b
                     -> Map.HashMap (BasePart b) (Pair n)
mergeAlgMapIfNonZero !acc Zero = acc
mergeAlgMapIfNonZero !acc alg@(v :@ _)
    | isZeroValue v = acc
    | otherwise = mergeAlgMap acc alg
mergeAlgMapIfNonZero !acc alg = mergeAlgMap acc alg

{-# INLINE unionsMerge #-}
-- | Merge multiple Algs by directly combining their internal HashMaps,
-- building the AxisPosting index only once at the end.
unionsMerge :: (HatVal n, Foldable f, HatBaseClass b) => f (Alg n b) -> Alg n b
unionsMerge ts =
    let !m = Foldable.foldl' mergeAlgMap Map.empty ts
    in mkAlgFromMap m

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
            f p@(Pair hs ns) =
                let !h = Foldable.foldl' (+) 0 hs
                    !n = Foldable.foldl' (+) 0 ns
                in case isNearlyNum h n 1e-13 of -- precision 13 digits
                    True -> Nothing
                    False -> case (Seq.length hs, Seq.length ns) of
                        -- Already in canonical form: singleton on winning side, empty on other
                        (1, 0) | h > n -> Just p
                        (0, 1) | n > h -> Just p
                        _ -> case compare h n of
                            GT -> Just (Pair (Seq.singleton (h - n)) Seq.empty)
                            LT -> Just (Pair Seq.empty (Seq.singleton (n - h)))

    {-# INLINE compress #-}
    compress Zero       = Zero
    compress (v:@b)     = v:@b
    compress (Liner m idx bpToId idToBp nextBpId allIds)  = Liner
                        (Map.map compressPair m)
                        idx
                        bpToId
                        idToBp
                        nextBpId
                        allIds
      where
        {-# INLINE compressPair #-}
        compressPair p@(Pair hs ns) = case (Seq.length hs, Seq.length ns) of
            (1, 1) -> p  -- already singleton on both sides, reuse
            (1, 0) -> p  -- already singleton + empty, reuse
            (0, 1) -> p  -- already empty + singleton, reuse
            _      -> Pair (Seq.singleton (Foldable.foldl' (+) 0 hs))
                           (Seq.singleton (Foldable.foldl' (+) 0 ns))


instance (HatVal n, ExBaseClass b) =>  Exchange Alg n b where
    -- | filter Credit side
    decR xs = filter (\x -> x /= Zero && (whichSide . _hatBase) x == Credit) xs

    -- | filter Debit side
    decL xs = filter (\x -> x /= Zero && (whichSide . _hatBase) x == Debit) xs

    -- | filter Plus Stock
    decP xs = filter (\x -> x /= Zero && (isHat . _hatBase ) x) xs

    -- | filter Minus Stock
    decM xs = filter (\x -> x /= Zero && (not. isHat. _hatBase) x) xs

    -- | check Credit Debit balance
    balance xs  | (norm . decR) xs == (norm . decL) xs = True
                | otherwise                            = False

    -- |
    diffRL xs  | r > l = (Credit, r - l)
               | l > r = (Debit, l -r)
               | otherwise = (Side,0)
        where
        r = (norm . decR) xs
        l = (norm . decL) xs

------------------------------------------------------------------
-- * Basic functions
------------------------------------------------------------------

-- | Returns all values contained in the algebra element as a list.
--
-- Complexity: O(s) (s is the total number of scalar entries)
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


-- | Returns all bases contained in the algebra element as a list.
--
-- Complexity: O(s) (s is the total number of scalar entries)
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



-- | Summation function that applies a function to each element of a list and sums the results.
-- Complexity: O(sum of 'union' costs over produced elements).
--
-- >>> type Test = Alg NN.Double (HatBase CountUnit)
-- >>> sigma [1,2] (\x -> x:@Hat:<Yen)
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen

{-# INLINE sigma #-}
sigma :: (HatVal v, HatBaseClass b) => [a] -> (a -> Alg v b) -> Alg v b
sigma xs f = mkAlgFromMap $ L.foldl' step Map.empty xs
  where
    step !acc !x = mergeAlgMapIfNonZero acc (f x)

-- | Conditional summation over a double loop. For all combinations of two lists,
-- applies the function only to pairs that satisfy the condition and sums the results.
--
-- Complexity: O(|xs| * |ys| * union cost)
{-# INLINE sigma2When #-}
sigma2When :: (HatVal v, HatBaseClass b)
           => [a]
           -> [c]
           -> (a -> c -> Bool)
           -> (a -> c -> Alg v b)
           -> Alg v b
sigma2When xs ys cond f =
    mkAlgFromMap $ L.foldl' outer Map.empty xs
  where
    outer !acc !x = L.foldl' (inner x) acc ys
    inner !x !acc !y
        | cond x y = mergeAlgMapIfNonZero acc (f x y)
        | otherwise = acc

-- | Summation using keys and values from a Map. Skips entries with zero values.
--
-- Complexity: O(|map| * union cost)
{-# INLINE sigmaFromMap #-}
sigmaFromMap :: (HatVal v, HatBaseClass b, Ord k)
             => M.Map k v
             -> (k -> v -> Alg v b)
             -> Alg v b
sigmaFromMap kvs f =
    mkAlgFromMap $ M.foldlWithKey' step Map.empty kvs
  where
    step !acc !k !v
        | isZeroValue v = acc
        | otherwise = mergeAlgMapIfNonZero acc (f k v)

-- | Converts an algebra element to a list.
-- Complexity: O(s) (s is the total number of scalar entries)
--
-- >>> toList (10:@Hat:<(Cash) .+ 10:@Hat:<(Deposits) .+ Zero :: Alg NN.Double (HatBase AccountTitles))
-- [10.00:@Hat:<Cash,10.00:@Hat:<Deposits]
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

{-# INLINE foldEntries #-}
-- | Strict left fold over scalar entries without building an intermediate list.
foldEntries :: (HatVal v, HatBaseClass b)
            => (acc -> v -> b -> acc)
            -> acc
            -> Alg v b
            -> acc
foldEntries _ !acc Zero = acc
foldEntries f !acc (v :@ b)
    | isZeroValue v = acc
    | otherwise = f acc v b
foldEntries f !acc (Liner m _ _ _ _ _) =
    Map.foldlWithKey' step acc m
  where
    step !acc0 !bp (Pair hs ns) =
        let !hatBase = merge Hat bp
            !notBase = merge Not bp
            !acc1 = Foldable.foldl' (\a v -> if isZeroValue v then a else f a v hatBase) acc0 hs
        in Foldable.foldl' (\a v -> if isZeroValue v then a else f a v notBase) acc1 ns

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

-- Difference list definition
type DList a = [a] -> [a]
type DMap k v = Map.HashMap k v -> Map.HashMap k v

{-# INLINE dnil #-}
-- | Complexity: O(1)
dnil :: DList a
dnil = id

{-# INLINE dappend #-}
-- | Complexity: O(1)
dappend :: DList a -> DList a -> DList a
dappend = (.)  -- Function composition

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
    -- Build a new Map using mapMaybeWithKey
    let m' = Map.mapMaybeWithKey
               (\basePart (Pair hs ns) ->
                  -- Filter each of hs and ns
                  let hs' = filterSide basePart Hat hs
                      ns' = filterSide basePart Not ns
                  in
                    -- Remove the entry (Nothing) if both become empty
                    if Seq.null hs' && Seq.null ns'
                       then Nothing
                       else Just (Pair hs' ns'))
             m
    in
      -- If the resulting Map is empty, return Zero; otherwise Liner m'
      if Map.null m' then Zero else linerFromMap m'
  where
    ----------------------------------------------------------------
    -- Filter function that constructs "v:@(merge h basePart)" from
    -- basePart and Hat/Not, and tests whether it satisfies predicate f
    ----------------------------------------------------------------
    -- filterSide :: BasePart b -> Hat -> Seq v -> Seq v
    {-# INLINE filterSide #-}
    filterSide bp h = Seq.filter (\val -> f (val :@ merge h bp))

------------------------------------------------------------
-- | proj
-- Complexity:
--  exact single-key path: expected O(1)
--  wildcard single-key path: O(queryAxisPosting + c * verify)
--  multi-pattern path: O(sum pattern costs + union costs)
--
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

-- | Projects only the credit side elements.
-- Use this instead of decL when the base contains non-Enum elements such as Text or Int.
--
-- Complexity: O(s) (s is the total number of scalar entries)
projCredit :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCredit = filter (\x -> (whichSide . _hatBase) x == Credit)

-- | Projects only the debit side elements.
-- Use this instead of decR when the base contains non-Enum elements such as Text or Int.
--
-- Complexity: O(s) (s is the total number of scalar entries)
projDebit :: (HatVal n, ExBaseClass b)  => Alg n b -> Alg n b
projDebit = filter (\x -> (whichSide . _hatBase) x == Debit)

-- | Projects only the elements matching the specified account title.
--
-- Complexity: O(s) (s is the total number of scalar entries)
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


-- | Compute the net balance as the difference of two projections.
-- @balanceBy plusBases minusBases alg@ computes
-- @projNorm plusBases alg - projNorm minusBases alg@.
--
-- Useful for calculating stock quantities, profits, etc.
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = 100 :@ Not:<Cash .+ 30 :@ Hat:<Cash :: T
-- >>> balanceBy [Not:<Cash] [Hat:<Cash] alg
-- 70.0
--
-- >>> balanceBy [Hat:<Cash] [Not:<Cash] alg
-- -70.0
balanceBy :: (HatVal n, HatBaseClass b) => [b] -> [b] -> Alg n b -> n
balanceBy plusBases minusBases alg =
    projNorm plusBases alg - projNorm minusBases alg

-- | Fold algebra entries into a @Map@, combining values with @(+)@.
--
-- The selector function examines each entry @(v, b)@ and optionally returns
-- a @(key, value)@ pair. Values for duplicate keys are summed.
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = 10 :@ Hat:<Cash .+ 20 :@ Hat:<Deposits .+ 5 :@ Hat:<Cash :: T
-- >>> let f v (Hat :< a) = Just (a, v); f _ _ = Nothing
-- >>> foldEntriesToMap f alg
-- fromList [(Cash,15.0),(Deposits,20.0)]
foldEntriesToMap :: (HatVal v, HatBaseClass b, Ord k)
                 => (v -> b -> Maybe (k, v))
                 -> Alg v b
                 -> M.Map k v
foldEntriesToMap f = foldEntries step M.empty
  where
    step acc v b = case f v b of
        Just (k, v') -> M.insertWith (+) k v' acc
        Nothing      -> acc

-- | Projects only current assets.
-- Extracts asset items classified as current from the debit side.
--
-- Complexity: O(s) (s is the total number of scalar entries)
projCurrentAssets :: ( HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCurrentAssets  = (filter (\x -> (fixedCurrent . _hatBase) x == Current))
                   . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                   . projDebit

-- | Projects only fixed assets.
-- Extracts asset items classified as fixed from the debit side.
--
-- Complexity: O(s) (s is the total number of scalar entries)
projFixedAssets :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projFixedAssets = (filter (\x -> (fixedCurrent . _hatBase) x == Fixed))
                . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                . projDebit

-- | Projects only deferred assets.
-- Tax-specific deferred assets are presented under "investments and other assets" with appropriate items such as long-term prepaid expenses.
--
-- Complexity: O(s) (s is the total number of scalar entries)
projDeferredAssets :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projDeferredAssets  = (filter (\x -> (fixedCurrent . _hatBase) x == Other))
                    . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                    . projDebit

-- | Projects only current liabilities.
-- Extracts liability items classified as current from the credit side.
--
-- Complexity: O(s) (s is the total number of scalar entries)
projCurrentLiability :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCurrentLiability  = (filter (\x -> (fixedCurrent . _hatBase) x == Current))
                      . (filter (\x -> (whatDiv . _hatBase) x      == Liability))
                      . projCredit

-- | Projects only fixed liabilities.
-- Extracts liability items classified as fixed from the credit side.
--
-- Complexity: O(s) (s is the total number of scalar entries)
projFixedLiability :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projFixedLiability  = (filter (\x -> (fixedCurrent . _hatBase) x == Fixed))
                    . (filter (\x -> (whatDiv . _hatBase) x      == Liability))
                    . projCredit

-- | Projects only capital stock.
--
-- __Note__: Not yet implemented. Calling this will throw an exception.
projCapitalStock :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCapitalStock = undefined


-- * Balance

{- | Handling when the balance does not hold -}
-- Complexity: O(1) currently (undefined placeholder).
forceBalance = undefined


-- * Rounding

-- | Rounding (ceiling).
-- Applied to the results of division and multiplication; uses ceiling rounding by default.
-- This should be applied to all multiplication and division of account titles.
--
-- Complexity: O(1)
rounding :: NN.Double -> NN.Double
rounding = fromIntegral . ceiling
