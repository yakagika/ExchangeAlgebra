{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}


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
    ( module ExchangeAlgebra.Algebra
    , module ExchangeAlgebra.Algebra.Base )where

import              ExchangeAlgebra.Algebra.Base

import              Debug.Trace
import qualified    Data.Text           as T
import              Data.Text           (Text)
import qualified    Data.List           as L (foldr1, map, length, elem,sort,foldl1,filter, or, and, sum)
import              Prelude             hiding (map, head, filter,tail, traverse, mapM)
import qualified    Data.Time           as Time
import              Data.Time
import qualified    Data.Map.Strict     as Map
import qualified    Data.Map.Strict     as Map
import qualified    Data.Maybe          as Maybe
import qualified    Number.NonNegative  as NN  -- 非負の実数
import              Numeric.NonNegative.Class (C)
import              Data.Bifunctor
import              Data.Biapplicative
import              Algebra.Additive (C)

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

class (HatVal n, HatBaseClass b, Monoid (a n b)) =>  Redundant a n b where
    -- | hat calculation
    (.^) :: a n b -> a n b

    -- | bar calculation
    (.-) :: a n b -> a n b

    -- | compress same Base algebras keep dividing different Hat Values
    compress :: a n b -> a n b

    -- | + calculation; alias of <> in monoid
    (.+) :: a n b -> a n b -> a n b

    -- | multiplication
    (.*) :: n -> a n b -> a n b

    -- | get value part
    norm :: a n b -> n

    {-# INLINE (.|) #-}
    -- | alias of nolm
    (.|) :: a n b -> n
    (.|) = norm

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
    balance :: a n b -> Bool -- ^ norm Balance


------------------------------------------------------------------
-- * Algebra
------------------------------------------------------------------

class   ( Show n
        , Ord n
        , Eq n
        , Nearly n
        , Fractional n
        , Num n
        , Numeric.NonNegative.Class.C n) => HatVal n where

        zeroValue :: n

        isZeroValue :: n -> Bool
        isZeroValue x
            | zeroValue == x = True
            | otherwise      = False

        isErrorValue :: n -> Bool

instance HatVal NN.Double where
    zeroValue = 0
    isErrorValue x  =  isNaN        (NN.toNumber x)
                    || isInfinite   (NN.toNumber x)

-- | 代数元 数値と基底のペア
--
-- Use (.+) instead of (:+) except for pattern match.

data  Alg n b where
    Zero :: Alg n b
    (:@) :: {_val :: n, _hatBase :: b}  -> Alg n b
    (:+) :: (Alg n b) -> (Alg n b) -> Alg n b

(.@) :: (HatVal n, HatBaseClass b) => n -> b -> Alg n b
(.@) v b = case isErrorValue v of
                False -> v :@ b
                True  -> error  $ "errorValue at (:@) val: "
                                ++ show v
                                ++ show ":@"
                                ++ show b

(<@) :: (HatVal n, Applicative f, HatBaseClass b)
     => f n  -> b -> f (Alg n b)
(<@) v b = (.@) <$> v <*> (pure b)


infixr 6 :@
infixr 6 .@
infixr 6 <@
infixr 5 :+

instance (Show n, Show b) => Show (Alg n b) where
    show Zero               = "0"
    show (v :@ b)           = (show v)          ++ ":@"    ++ show b
    show (x:@b :+ y:@c)     = (show (x:@b))     ++ " .+ "  ++ show (y:@c)
    -- 左結合になっている場合のみ表記変更
    show ((w :+ z) :+ x:@b) = (show (w :+ z))   ++ " |.+ " ++ show (x:@b)
    show (x :+ y)           = (show x)          ++ " .+ "  ++ show y

instance (HatVal n, HatBaseClass b) =>  Eq (Alg n b) where
    (==) Zero Zero = True
    (==) Zero _    = False
    (==) _    Zero = False

    (==) (v :@ b) (v' :@ b')
        | v == v' && b .== b'    = True
        | otherwise              = False

    -- 交換法則
    (==) (x :+ y) (w :+ z)       = (x == w && y == z)  || (x == z && y == w)
    (/=) x y                     = not (x == y)

instance (HatVal n, HatBaseClass b) =>  Ord (Alg n b) where
    compare Zero Zero = EQ
    compare Zero _    = LT
    compare _    Zero = GT
    compare (v :@ b) (v' :@ b')
        | b .== b'   = compare v v'
        | otherwise  = compare b b'
    -- :+ に関しては定義しない

    (<) x y | compare x y == LT = True
            | otherwise         = False

    (>) x y | compare x y == GT = True
            | otherwise         = False


    (<=) x y | compare x y == LT || compare x y == EQ   = True
             | otherwise                                = False

    (>=) x y | compare x y == GT || compare x y == EQ   = True
             | otherwise                                = False

    max x y | x >= y    = x
            | otherwise = y

    min x y | x <= y    = x
            | otherwise = y


instance  (HatVal n, HatBaseClass b) => Semigroup (Alg n b) where
    {-# INLINE (<>) #-}

    -- | Associative law ;convert to right join
    x <> y  = foldr (<<>>) Zero
            $ (flip L.filter)
            (toList x ++ toList y)
            $ \z -> z /= Zero && (_val z) /= 0
            where
            {-# INLINE (<<>>) #-}
            Zero <<>> Zero = Zero
            Zero <<>> x    = x
            x    <<>> Zero = x
            x    <<>> y    = x :+ y

    {- 格好いいけど遅いので使わない
    (v:@b) <> (w:@c) = case (v == 0 , w == 0) of
                            (True, True)   -> Zero
                            (True, False)  -> (w:@c)
                            (False, True)  -> (v:@b)
                            (False, False) -> (v:@b) :+ (w:@c)

    -- | 単位元の演算
    Zero   <>  Zero   = Zero
    Zero   <> (v:@b)  = case v == 0 of
                            True  -> Zero
                            False -> (v:@b)

    (v:@b) <> Zero    = case v == 0 of
                            True  -> Zero
                            False -> (v:@b)

    (x:+y) <> Zero    = x <> y
    Zero   <> (z:+w)  = z <> w

    -- | 結合法則(というか右結合に変換)
    (v:@b) <> (z:+w) = case v == 0 of
                        True  -> (z <> w)
                        False -> (v:@b) :+ (z <> w)

    (x:+y) <> (w:@c) = case w == 0 of
                        True  -> (x <> y)
                        False -> (w:@c) :+ (x <> y)

    (x:+y) <> (z:+w) = x <> (y <> (z <> w))
    -}

instance (HatVal n, HatBaseClass b) => Monoid (Alg n b) where
    -- 単位元
    mempty = Zero
    mappend = (<>)
    mconcat = foldr mappend mempty

instance Bifunctor Alg where
    bimap _ _ Zero     = Zero
    bimap f g (v:@b)   = (f v) :@ (g b)
    bimap f g (x:+xs)  = (bimap f g x) :+ (bimap f g xs)

instance (HatVal n, HatBaseClass b) => Redundant Alg n b where
    (.^) Zero               = Zero
    (.^) (v :@ b)           = v :@ (revHat b)
    (.^) (x :+ y)           = (.^) x .+ (.^) y

    (.+) = mappend

    x  .*  Zero    = Zero
    0  .*  x       = Zero
    x  .*  (v:@b)  = case v == 0 of
                        True  -> Zero
                        False -> (x * v):@b

    a  .*  (x:+y)  = (a.* x) .+ (a .* y)

    norm Zero       = 0
    norm (v :@ b)   = v
    norm xs         = L.sum $ vals xs

    {-# INLINE (.-) #-}
    (.-) Zero       = Zero
    (.-) (v :@ b)   | v == 0    = Zero
                    | otherwise = v :@ b

    (.-) xs         = f Zero $ L.sort $ toList xs
        where
        {-# INLINE f #-}
        f :: (HatVal n, HatBaseClass b) => Alg n b -> [Alg n b] -> Alg n b
        f x []           = x
        f x [y]          = x >< y
        f Zero   (y:ys)  = f y ys
        f (v:@b) (y:ys)  = case ((v:@b) >< y) of
                                        Zero             -> f Zero   ys
                                        (w:@c)           -> f (w:@c) ys
                                        ((w:@c):+(x:@d)) -> (w:@c) .+ (f (x:@d) ys)

        {-# INLINE (><) #-}
        (><) ::  (HatVal n, HatBaseClass b) => Alg n b -> Alg n b -> Alg n b
        Zero     >< Zero   = Zero
        Zero     >< (v:@b) = case v == 0 of True  -> Zero
                                            False -> (v:@b)
        (v:@b)   >< Zero   = case v == 0 of True  -> Zero
                                            False -> (v:@b)

        (v:@b) >< (w:@c)    | (b /= c)  &&  ((revHat b) /= c) = case (v == 0 , w == 0) of
                                            (True,  True)   -> Zero
                                            (True,  False)  -> (w:@c)
                                            (False, True)   -> (v:@b)
                                            (False, False)  -> (v:@b) :+ (w:@c)
                            | otherwise =  let h = hat b
                                        in let n = hat c
                                        in case (h, n) of
                                            (Hat, Hat) -> (v + w) :@b
                                            (Not, Not) -> (v + w) :@b
                                            (Not, Hat)  | v == w             -> Zero
                                                        | isNearly v w 1e-10 -> Zero -- 丸め込み
                                                        | v >  w             -> (v - w):@b
                                                        | v <  w             -> (w - v):@c
                                            (Hat, Not)  | v == w             -> Zero
                                                        | isNearly v w 1e-10 -> Zero -- 丸め込み
                                                        | v >  w             -> (v - w):@b
                                                        | v <  w             -> (w - v):@c

    {-# INLINE compress #-}
    compress Zero       = Zero
    compress (v :@ b)   | v == 0    = Zero
                        | otherwise = v :@ b

    compress xs         = g Zero $ L.sort $ toList xs
        where
        {-# INLINE g #-}
        g :: (HatVal n, HatBaseClass b) => Alg n b -> [Alg n b] -> Alg n b
        g x []           = x
        g x [y]          = x >><< y
        g Zero   (y:ys)  = g y ys
        g (v:@b) (y:ys)  = case ((v:@b) >><< y) of
                                        Zero             -> g Zero   ys
                                        (w:@c)           -> g (w:@c) ys
                                        ((w:@c):+(x:@d)) -> (w:@c) .+ (g (x:@d) ys)

        {-# INLINE (>><<) #-}
        (>><<) ::  (HatVal n, HatBaseClass b) => Alg n b -> Alg n b -> Alg n b
        Zero    >><< Zero   = Zero
        Zero    >><< (v:@b) = case v == 0 of True  -> Zero
                                             False -> (v:@b)
        (v:@b)  >><< Zero   = case v == 0 of True  -> Zero
                                             False -> (v:@b)

        (v:@b)  >><< (w:@c) | (b /= c)  &&  ((revHat b) /= c) = case (v == 0 , w == 0) of
                                            (True,  True)   -> Zero
                                            (True,  False)  -> (w:@c)
                                            (False, True)   -> (v:@b)
                                            (False, False)  -> (v:@b) :+ (w:@c)
                            | otherwise =  let h = hat b
                                        in let n = hat c
                                        in case (h, n) of
                                            (Hat, Hat) -> (v + w) :@b
                                            (Not, Not) -> (v + w) :@b
                                            (Not, Hat) -> (w:@c) :+ (v:@b)
                                            (Hat, Not) -> (v:@b) :+ (w:@c)

instance (HatVal n, ExBaseClass a) =>  Exchange Alg n a where
    decR xs = filter (\x -> x /= Zero && (whichSide . _hatBase) x == Debit) xs
    decL xs = filter (\x -> x /= Zero && (whichSide . _hatBase) x == Credit) xs
    decP xs = filter (\x -> x /= Zero && (isHat . _hatBase ) x) xs
    decM xs = filter (\x -> x /= Zero && (not. isHat. _hatBase) x) xs
    balance xs  | (norm . decR) xs == (norm . decL) xs = True
                | otherwise                            = False




------------------------------------------------------------------
-- * 基本の関数
------------------------------------------------------------------

-- | 全てHatかどうかを判定する
--
-- >>> allHat $ 10:@Hat:<Cash .+ 12:@Hat:<Deposits
-- True
--
-- >>> allHat $ 10:@Hat:<Cash .+ 12:@Not:<Deposits
-- False
--
-- Note: in case Zero, this returns True
-- >>> allNot (Zero :: Alg (HatBase AccountTitles))
-- True
--
-- use (.+) instead of (:+)

allHat :: (HatVal n, HatBaseClass b) =>  Alg n b -> Bool
allHat xs = L.and $ L.map (isHat . _hatBase) $ toList xs

-- | 全てNotかどうかを判定する
--
--
-- >>> allNot $ 10:@Hat:<Cash .+ 12:@Hat:<Deposits
-- False
--
-- >>> allNot $ 10:@Not:<Cash .+ 12:@Not:<Deposits
-- True

allNot ::(HatVal n, HatBaseClass b) =>   Alg n b -> Bool
allNot xs = L.and $ L.map (isNot . _hatBase) $ toList xs

vals :: (HatVal n, HatBaseClass b) =>  Alg n b -> [n]
vals Zero     = [0]
vals (x :@ y) = [x]
vals xs = L.map _val $ toList xs

bases ::(HatVal n, HatBaseClass b) =>  Alg n b -> [Maybe b]
bases Zero = [Nothing]
bases (v :@ b) = [Just b]
bases (x :+ y) = bases x ++ bases y

length :: (HatVal n, HatBaseClass b) => Alg n b -> Int
length = L.length . toList

isZero :: (HatVal n, HatBaseClass b) => Alg n b -> Bool
isZero Zero = True
isZero _    = False

isSingle :: (HatVal n, HatBaseClass b) => Alg n b -> Bool
isSIngle (_ :@ _) = True
isSingle _        = False

isFormula :: (HatVal n, HatBaseClass b) => Alg n b -> Bool
isFormula (x :+ y) = True
isFormula _        = False

{-# INLINE fromList #-}
fromList ::(HatVal n, HatBaseClass b ) => [Alg n b] -> Alg n b
fromList = mconcat

-- | convert Alg n b to List
--
-- >>> toList $ 10:@Hat:<(Cash) :+ 10:@Hat:<(Deposits) :+ Zero
-- [10.0:@Hat:<Cash,10.0:@Hat:<Deposits]
--
-- you need define type variables to use this for Zero
-- >>> toList Zero :: [Alg (HatBase AccountTitles)]
-- []
{-# INLINE toList #-}
toList :: Alg n b -> [Alg n b]
toList Zero     = []
toList (v :@ b) = [(v :@ b)]
toList (x :+ y) = toList x ++ toList y

head :: Alg n b -> Alg n b
head Zero = Zero
head (v :@ b) = (v :@ b)
head (x :+ y) = head x

-- |
tail :: (HatVal n, HatBaseClass b) => Alg n b -> Alg n b
tail Zero            = Zero
tail (v:@b)          = Zero
tail (Zero :+ y)     = y
tail ((v:@ b) :+ y)  = y
tail (x :+ y)        = (tail x) .+ y

{-# INLINE map #-}
-- | map
map :: (HatVal n, HatBaseClass b) => (Alg n a -> Alg n b) -> Alg n a -> Alg n b
map f  Zero    = f Zero
map f (v :@ b) = f (v :@ b)
map f (x :+ y) = (map f x) .+ map f y

traverse :: (HatVal n, HatBaseClass b, Applicative f)
         => (Alg n a -> f (Alg n b)) -> Alg n a -> f (Alg n b)
traverse f xs = fromList <$>  (sequenceA . fmap f)  (toList xs)

{-# INLINE mapM #-}
mapM :: (HatVal n,HatBaseClass b, Applicative f)
     => (Alg n a -> f (Alg n b)) -> Alg n a -> f (Alg n b)
mapM = traverse

{-# INLINE forM #-}
forM :: (HatVal n, HatBaseClass b, Applicative f)
     => Alg n a -> (Alg n a -> f (Alg n b)) -> f (Alg n b)
forM = flip mapM


{-# INLINE filter #-}
-- | filter
filter :: (HatVal n, HatBaseClass b) => (Alg n b -> Bool) -> Alg n b -> Alg n b
filter f Zero                 = Zero
filter f (v:@b) | f (v:@b)    = v:@b
                | otherwise   = Zero
filter f (x:+y) | f x         = case (filter f y) of
                                    Zero -> x
                                    ys   -> x .+ ys
                | otherwise  = filter f y

{- | projection
[\
Let x = \sum_{e_i \in \Gamma}{a_i \times e_i} , then Project[e_k](x) = a_k e_k is defined as projection operatirs.\\
\forall A \subset \Gannma Project[A](x) is defined as Projecton[A](x) = \sum_{e \in A}{project[e](x)}
\]
-}
{-# INLINE proj #-}
proj :: (HatVal n, HatBaseClass b)  => [b] -> Alg n b -> Alg n b
proj bs  alg = filter (f bs) alg
    where
    f ::(HatVal n, HatBaseClass b)  => [b] -> Alg n b  -> Bool
    f _   Zero       = False
    f []  _          = False
    f [b] (v:@eb)    = b .== eb
    f bs  (v:@eb)    = L.or $ L.map (\x -> eb .== x) bs
    f [b] xs         = error $ "error at proj : you might use (:+) instead of (.+)."
    f bs  xs         = error $ "error at proj : you might use (:+) instead of (.+)."

-- | proj devit algs の代わりに Elem に Text や Int などがある場合は projCredit を使う
projCredit :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCredit = filter (\x -> (whichSide . _hatBase) x == Credit)

-- | proj debit algs の代わりに Elem に Text や Int などがある場合は projDebit を使う
projDebit :: (HatVal n, ExBaseClass b)  => Alg n b -> Alg n b
projDebit = filter (\x -> (whichSide . _hatBase) x == Credit)

projByAccountTitle :: (HatVal n, ExBaseClass b) => AccountTitles -> Alg n b -> Alg n b
projByAccountTitle at alg = filter (f at) alg
    where
        f :: (ExBaseClass b) => AccountTitles -> Alg n b -> Bool
        f at Zero = False
        f at x    = ((getAccountTitle ._hatBase) x) .== at

{-# INLINE projNorm #-}
projNorm :: (HatVal n, HatBaseClass b) => [b] -> Alg n b -> n
projNorm bs alg  = norm $ (.-) $ proj bs alg

-- | Baseの大小（==Algの大小）でソート

sort :: (HatVal n, HatBaseClass b) => Alg n b -> Alg n b
sort Zero      = Zero
sort (v :@ b)  = (v :@ b)
sort (x :+ y)  = foldl1 (.+) $ L.sort $ toList (x .+ y)


-- | normの大小でソート
normSort :: Alg n b -> Alg n b
normSort = undefined


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
