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

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

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
    , (<@)
    , vals
    , bases
    , fromList
    , toList
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
import qualified    Data.List           as L (foldl', map, length, elem,sort,filter, or, and,any, sum, concat)
import              Prelude             hiding (map, head, filter,tail, traverse, mapM)
import qualified    Data.Time           as Time
import              Data.Time
import qualified    Data.HashMap.Strict     as Map
import qualified    Data.Foldable       as Foldable (foldMap,foldl)
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
 Pair :: {_hatSide :: [v]
         ,_notSide :: [v]} -> Pair v
         deriving (Eq)


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
nullPair :: Pair v
nullPair = Pair [] []

{-# INLINE isNullPair #-}
isNullPair :: Pair v -> Bool
isNullPair (Pair [] []) = True
isNullPair _            = False

{-# INLINE pairAppend #-}
pairAppend :: Pair v -> Pair v -> Pair v
pairAppend (Pair x1 y1) (Pair x2 y2) = Pair (x1 ++ x2) (y1 ++ y2)

-- | 代数元 数値と基底のペア
data  Alg v b where
        Zero  :: Alg v b
        (:@)  :: {_val :: v, _hatBase :: b} -> Alg v b
        Liner :: {_realg :: Map.HashMap (BasePart b) (Pair v)} ->  Alg v b

isZero :: Alg v b -> Bool
isZero Zero = True
isZero _    = False

{-# INLINE singleton #-}
singleton :: (HatVal v, HatBaseClass b) => v -> b -> Alg v b
singleton v b | isZeroValue v  = Zero
              | isErrorValue v = error  $ "errorValue at (.@) val: "
                               ++ show v
                               ++ show ":@"
                               ++ show b
              | otherwise      = v :@ b

{-# INLINE (.@) #-}
(.@) :: (HatVal n, HatBaseClass b) => n -> b -> Alg n b
(.@) v b = singleton v b

(<@) :: (HatVal n, Applicative f, HatBaseClass b)
     => f n  -> b -> f (Alg n b)
(<@) v b = (.@) <$> v <*> (pure b)


infixr 6 :@
infixr 6 .@
infixr 6 <@

showV ::  (HatVal v) => v -> String
showV v = D.formatScientific D.Generic (Just 2) (D.fromFloatDigits v)

instance (HatVal v, HatBaseClass b) =>  Eq (Alg v b) where
    (==) Zero Zero = True
    (==) Zero _    = False
    (==) _    Zero = False
    (==) (v1:@b1) (v2:@b2) = (v1 == v2) && (b1 == b2)
    (==) (Liner m1) (Liner m2) = m1 == m2
    (==) _ _ = False
    (/=) x y = not (x == y)

instance (HatVal v, HatBaseClass b) => Ord (Alg v b) where
    {-# INLINE compare #-}
    compare Zero Zero = EQ
    compare Zero _ = LT
    compare _ Zero = GT

    compare (v:@b) (Liner m) = LT
    compare (Liner m) (v:@b) = GT
    compare (v1:@b1) (v2:@b2)
        | b1 == b2 = compare v1 v2
        | b1 >  b2  = GT
        | b1 <  b2  = LT

    compare (Liner m1) (Liner m2) = compare m1 m2

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
    rnf (Liner m) = Map.foldrWithKey (\k v acc -> k `seq` v `seq` acc) () m
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
union (Liner m1) (Liner m2) = Liner (Map.unionWith pairAppend m1 m2)


{-# INLINE insert #-}
insert :: (HatVal v,HatBaseClass b) => b -> v -> Alg v b ->  Alg v b
insert b v Zero       = v .@ b
insert b1 v1 (v2:@b2) = case isHat b1 of
                            True  -> insert b2 v2
                                   $ Liner
                                   $ Map.singleton (base b1)
                                   $ nullPair {_hatSide = [v1]}
                            False -> insert b2 v2
                                   $ Liner
                                   $ Map.singleton (base b1)
                                   $ nullPair {_notSide = [v1]}
insert b v (Liner m)  = case isHat b of
                        True  -> Liner
                               $ Map.insertWith pairAppend
                                                (base b)
                                                nullPair {_hatSide = [v]}
                                                m
                        False -> Liner
                               $ Map.insertWith pairAppend
                                                (base b)
                                                nullPair {_notSide =[v]}
                                                m

------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------

instance (HatVal n, HatBaseClass b) => Monoid (Alg n b) where
    -- 単位元
    mempty = Zero
    mappend = (<>)
    mconcat = unions

{-# INLINE unions #-}
unions :: (HatVal n, Foldable f, HatBaseClass b) => f (Alg n b) -> Alg n b
unions ts = Foldable.foldl union Zero ts

------------------------------------------------------------------
-- Redundant
------------------------------------------------------------------

instance (HatVal n, HatBaseClass b) => Redundant Alg n b where
    (.^) Zero       = Zero
    (.^) (n:@ b)    = n :@ (revHat b)
    (.^) (Liner ms) = Liner
                    $ Map.map (\ (Pair hs ns) -> Pair ns hs) ms

    (.+) = mappend

    x  .*  Zero      = Zero
    0  .*  x         = Zero
    x  .* (v:@b)     = (x * v) :@ b
    x  .* (Liner ms) = Liner
                     $ Map.map (\ (Pair hs ns) -> Pair (L.map (x *) hs) (L.map (x *) ns)) ms

    norm Zero       = 0
    norm (v:@b)     = v
    norm (Liner ms) = Map.foldl (\ x (Pair hs ns) -> x + sum hs + sum ns) 0 ms

    {-# INLINE (.-) #-}
    (.-) Zero = Zero
    (.-) (v:@b) = v:@b
    (.-) (Liner m) = let !res = Map.mapMaybe f m
                   in case null res of
                        True -> Zero
                        False -> Liner res
        where
            {-# INLINE f #-}
            f (Pair hs ns) = let (h, n) = (sum hs,sum ns)
                           in case isNearlyNum h n 1e-13 of -- 精度 13桁
                                        True -> Nothing
                                        False -> case compare h n of
                                                    GT -> Just (Pair [h - n] [])
                                                    LT -> Just (Pair [] [n - h])

    {-# INLINE compress #-}
    compress Zero       = Zero
    compress (v:@b)     = v:@b
    compress (Liner m)  = Liner
                        $ Map.map (\ (Pair hs ns) -> Pair [sum hs] [sum ns]) m


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
vals :: (HatVal v, HatBaseClass b) => Alg v b -> [v]
vals Zero = []
vals (v:@b) = [v]
vals (Liner m) = Map.foldl (\ xs (Pair hs ns) -> xs ++ hs ++ ns) [] m


-- | bases
-- get bases
bases :: (HatVal v, HatBaseClass b) => Alg v b -> [b]
bases Zero = []
bases (v:@b) = [b]
bases (Liner m) = Map.foldlWithKey f [] m
    where
        f ::  (HatVal v, HatBaseClass b) => [b] -> BasePart b -> Pair v ->  [b]
        f xs b (Pair {_hatSide = hs, _notSide = ns})
            = L.foldl' (g Not b) (L.foldl' (g Hat b) xs hs) hs

        g ::  (HatVal v, HatBaseClass b) => Hat -> BasePart b -> [b] -> v -> [b]
        g h b ys v = (merge h b):ys

{-# INLINE fromList #-}
-- | convert List to Alg n b
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

-- | convert Alg n b to List
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
toList (Liner m)  = Map.foldlWithKey f [] m
    where
        f :: (HatVal v, HatBaseClass b) =>  [Alg v b] -> BasePart b -> Pair v -> [Alg v b]
        f xs b Pair {_hatSide = hs, _notSide = ns}
            = L.foldl' (g Hat b) (L.foldl' (g Not b) xs ns) hs

        g :: (HatVal v, HatBaseClass b) => Hat -> BasePart b -> [Alg v b] -> v -> [Alg v b]
        g h b ys v
            | isZeroValue v = ys
            | otherwise     = (v :@ (merge h b)):ys

{-# INLINE toASCList #-}
toASCList :: (HatVal v, HatBaseClass b) => Alg v b -> [Alg v b]
toASCList = L.sort . toList


-- | map
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
map f (Liner m) = case dToList (Map.foldrWithKey (p f) dnil m)of
                    []                -> Zero
                    [(b,Pair [] [n])] -> n:@(merge Not b)
                    [(b,Pair [h] [])] -> h:@(merge Hat b)
                    [(b,Pair ns hs)]  -> Liner $ Map.singleton b (Pair ns hs)
                    xs                -> Liner $ Map.fromListWith pairAppend xs
    where
        {-# INLINE p #-}
        p :: (HatVal v, HatBaseClass b)
          => (Alg v b -> Alg v b)
          -> BasePart b
          -> Pair v
          -> DList (BasePart b,Pair v)
          -> DList (BasePart b,Pair v)
        p f b Pair {_hatSide=hs, _notSide=ns} accDList =
            let (dl1, hs2) = q f Hat b hs
                (dl2, ns2) = q f Not b ns
                prefix     = dappend dl1 dl2
            in case (null hs2, null ns2) of
                (True,True)   -> dappend prefix accDList
                (True,False)  -> dappend prefix
                               . dappend (dsingle (b, nullPair{_notSide = ns2}))
                               $ accDList
                (False,True)  -> dappend prefix
                               . dappend (dsingle (b, nullPair{_hatSide = hs2}))
                               $ accDList
                (False,False) -> dappend prefix
                               . dappend (dsingle (b, Pair hs2 ns2))
                               $ accDList
        {-# INLINE q #-}
        q :: (HatVal v, HatBaseClass b)
          => (Alg v b -> Alg v b)
          -> Hat
          -> BasePart b
          -> [v]
          -> (DList (BasePart b,Pair v),[v])
        q f h b vs = L.foldl' (r f h b) (dnil,[]) vs

        {-# INLINE r #-}
        r  :: (HatVal v, HatBaseClass b)
           => (Alg v b -> Alg v b)
           -> Hat
           -> BasePart b
           -> (DList (BasePart b,Pair v),[v])
           -> v
           -> (DList (BasePart b,Pair v),[v])
        r f h b (dlAcc,vsAcc) v = case f (v:@(merge h b)) of
                            Zero   ->  (dlAcc, vsAcc)
                            ------------------------------------------------------------------
                            v2:@b2
                                | isZeroValue v2 ->  (dlAcc, vsAcc)
                                | b2 .== (merge h b) -> (dlAcc, v2 : vsAcc)
                                | isHat (hat b2)     -> (dappend dlAcc (dsingle ( base b2
                                                                       ,nullPair{_hatSide=[v2]}))
                                                        ,vsAcc )
                                | otherwise          -> (dappend dlAcc (dsingle ( base b2
                                                                       ,nullPair{_notSide=[v2]} ))
                                                        ,vsAcc )

-- 差分リストを定義
type DList a = [a] -> [a]

{-# INLINE dnil #-}
dnil :: DList a
dnil = id

{-# INLINE dappend #-}
dappend :: DList a -> DList a -> DList a
dappend = (.)  -- 関数合成

{-# INLINE dsingle #-}
dsingle :: a -> DList a
dsingle x = \rest -> x : rest

{-# INLINE dToList #-}
dToList :: DList a -> [a]
dToList dl = dl []

{-# INLINE dFromList #-}
dFromList :: [a] -> DList a
dFromList xs = (xs ++)


{-# INLINE filter #-}
-- | filter
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

filter f (Liner m) =
    -- mapMaybeWithKey で新しい Map を構築
    let m' = Map.mapMaybeWithKey
               (\basePart (Pair hs ns) ->
                  -- hs, ns それぞれをフィルタ
                  let hs' = filterSide basePart Hat hs
                      ns' = filterSide basePart Not ns
                  in
                    -- 両方とも空になればエントリ削除 (Nothing)
                    if null hs' && null ns'
                       then Nothing
                       else Just (Pair hs' ns'))
             m
    in
      -- 結果の Map が空なら Zero, そうでなければ Liner m'
      if Map.null m' then Zero else Liner m'
  where
    ----------------------------------------------------------------
    -- basePart と Hat/Not を元に「v:@(merge h basePart)」を作り，
    -- 与えられた述語 f を満たすか判定するためのフィルタ関数
    ----------------------------------------------------------------
    -- filterSide :: BasePart b -> Hat -> [v] -> [v]
    {-# INLINE filterSide #-}
    filterSide bp h = dfilter (\val -> f (val :@ merge h bp))

    -- | 差分リストを用いた手実装filter
    {-# INLINE dfilter #-}
    dfilter :: (a -> Bool) -> [a] -> [a]
    dfilter p xs = go xs id
      where
        go []     dl = dl []
        go (y:ys) dl
          | p y       = go ys (dl . (y:))
          | otherwise = go ys dl

------------------------------------------------------------
-- | proj
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
proj (b:bs) (v:@b2)
    |  b .== b2       = v:@b2
    | otherwise       = proj bs (v:@b2)
proj (b:bs) (Liner m) = case dToList (go (b:bs) m) of
                            []                -> Zero
                            [(b,Pair [] [n])] -> n:@(merge Not b)
                            [(b,Pair [h] [])] -> h:@(merge Hat b)
                            [(b,Pair ns hs)]  -> Liner $ Map.singleton b (Pair ns hs)
                            xs                -> Liner $ Map.fromListWith pairAppend xs
    where
    {-# INLINE go #-}
    go :: (HatVal v, HatBaseClass b)
       => [b]
       -> Map.HashMap (BasePart b) (Pair v)
       -> DList (BasePart b, Pair v)
    go [] _ = dnil
    go (b:bs)  m
        | null m                 = dnil
        | haveWiledcard (base b) = dappend (Map.foldrWithKey (f b) dnil m)
                                 $ go bs m

        | otherwise = case Map.lookup (base b) m of
                            Nothing -> go bs m
                            Just Pair {_hatSide = hs
                                      ,_notSide = ns} -> let res = case hat b of
                                                                    Hat    -> ((base b), nullPair {_hatSide = hs})
                                                                    Not    -> ((base b), nullPair {_notSide = ns})
                                                                    HatNot -> ((base b), Pair {_hatSide = hs
                                                                                              ,_notSide = ns})
                                                       in dappend (dsingle res) (go bs m)
    {-# INLINE f #-}
    f ::  (HatVal v, HatBaseClass b)
       => b
       -> BasePart b
       -> Pair v
       -> DList (BasePart b,Pair v)
       -> DList (BasePart b,Pair v)
    f b bp Pair {_hatSide=hs, _notSide=ns} accDList
        | (base b) .== bp = let res = case hat b of
                                        Hat    -> (bp, nullPair {_hatSide = hs})
                                        Not    -> (bp, nullPair {_notSide = ns})
                                        HatNot -> (bp, Pair {_hatSide = hs
                                                            ,_notSide = ns})
                          in dappend (dsingle res) accDList
        | otherwise       = accDList

------------------------------------------------------------------

-- | proj devit algs の代わりに Elem に Text や Int などがある場合は projCredit を使う
projCredit :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCredit = filter (\x -> (whichSide . _hatBase) x == Credit)

-- | proj debit algs の代わりに Elem に Text や Int などがある場合は projDebit を使う
projDebit :: (HatVal n, ExBaseClass b)  => Alg n b -> Alg n b
projDebit = filter (\x -> (whichSide . _hatBase) x == Credit)

projByAccountTitle :: (HatVal n, ExBaseClass b) => AccountTitles -> Alg n b -> Alg n b
projByAccountTitle at alg = filter (f at) alg
    where
        f :: (HatVal n,ExBaseClass b) => AccountTitles -> Alg n b -> Bool
        f at Zero = False
        f at x    = ((getAccountTitle ._hatBase) x) .== at

projNorm :: (HatVal n, HatBaseClass b) => [b] -> Alg n b -> n
projNorm bs alg  = norm $ (.-) $ proj bs alg


-- | 流動資産の取得
projCurrentAssets :: ( HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCurrentAssets  = (filter (\x -> (fixedCurrent . _hatBase) x == Current))
                   . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                   . projDebit

-- | 固定資産
projFixedAssets :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projFixedAssets = (filter (\x -> (fixedCurrent . _hatBase) x == Fixed))
                . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                . projDebit

-- | 繰延資産
-- 税法固有の繰延資産は、「投資その他の資産」に長期前払費用等の適当な項目を付して表示する。
projDeferredAssets :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projDeferredAssets  = (filter (\x -> (fixedCurrent . _hatBase) x == Other))
                    . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                    . projDebit

-- | 流動負債
projCurrentLiability :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCurrentLiability  = (filter (\x -> (fixedCurrent . _hatBase) x == Current))
                      . (filter (\x -> (whatDiv . _hatBase) x      == Liability))
                      . projCredit

-- | 固定負債
projFixedLiability :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projFixedLiability  = (filter (\x -> (fixedCurrent . _hatBase) x == Fixed))
                    . (filter (\x -> (whatDiv . _hatBase) x      == Liability))
                    . projCredit

-- | 株主資本
projCapitalStock :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCapitalStock = undefined


-- * バランス

{- | バランスしていない場合の処理 -}
forceBalance = undefined


-- * 端数処理

{- | 端数処理
割り算と掛け算に利用
基本的には切り上げで処理する
勘定科目の乗除には全てこれを適用
-}

rounding :: NN.Double -> NN.Double
rounding = fromIntegral . ceiling
