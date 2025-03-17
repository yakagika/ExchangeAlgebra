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


module ExchangeAlgebraMap.Algebra
    ( module ExchangeAlgebraMap.Algebra.Base
    , Nearly
    , isNearlyNum
    , Redundant(..)
    , Exchange(..)
    , HatVal(..)
    , Alg(Zero,_val)
    , Identifier
    , _hatBase
    , isZero
    , (<@)
    , pattern (:@)
    , pattern Liner
    , isLiner
    , vals
    , bases
    , keys
    , allHat
    , allNot
    , fromList
    , toList
    , map
    , traverse
    , mapM
    , forM
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

import              ExchangeAlgebraMap.Algebra.Base

import              Debug.Trace
import qualified    Data.Text           as T
import              Data.Text           (Text)
import qualified    Data.List           as L (foldr1, map, length, elem,sort,foldl1,filter, or, and,any, sum)
import              Prelude             hiding (map, head, filter,tail, traverse, mapM)
import qualified    Data.Time           as Time
import              Data.Time
import qualified    Data.Map.Strict     as Map
import qualified    Data.Map.Strict     as Map
import qualified    Data.Foldable       as Foldable (foldMap,foldl)
import qualified    Data.Maybe          as Maybe
import qualified    Number.NonNegative  as NN  -- 非負の実数
import              Numeric.NonNegative.Class (C)
import              Data.Bifunctor
import              Data.Biapplicative
import              Algebra.Additive (C)
import qualified    Data.Scientific     as D (Scientific, fromFloatDigits, formatScientific, FPFormat(..))
import Control.DeepSeq
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
    -- >>> (.^) (10:@Not:<Cash .+ 10:@Hat:<Deposits)
    -- 10.00:@Hat:<Cash .+ 10.00:@Not:<Deposits
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
    floatRadix    = floatRadix    . NN.toNumber
    floatDigits   = floatDigits   . NN.toNumber
    floatRange    = floatRange    . NN.toNumber
    decodeFloat   = decodeFloat   . NN.toNumber
    encodeFloat m e = NN.fromNumber (encodeFloat m e)
    exponent      = exponent      . NN.toNumber
    significand   = NN.fromNumber . significand . NN.toNumber
    scaleFloat n  = NN.fromNumber . scaleFloat n . NN.toNumber
    isNaN         = isNaN         . NN.toNumber
    isInfinite    = isInfinite    . NN.toNumber
    isDenormalized = isDenormalized . NN.toNumber
    isNegativeZero = isNegativeZero . NN.toNumber
    isIEEE        = isIEEE        . NN.toNumber

instance HatVal NN.Double where
    zeroValue = 0
    isErrorValue x  =  isNaN        (NN.toNumber x)
                    || isInfinite   (NN.toNumber x)

instance HatVal Prelude.Double where
    zeroValue = 0
    isErrorValue x  =  isNaN        x
                    || isInfinite   x
                    || x < 0

-- データ代数 Hat が セミグループになる.
-- 単位元もない(未実装)
-- 交換代数をデータ代数の要素にする

-- | 代数元 数値と基底のペア
--
-- Use (.+) instead of (:+) except for pattern match.

{-
-- 線形結合の要素
data ExElm n b where
    (:@) :: {_val :: n, _hatBase :: b}  -> ExElm n b

-- 線形結合
data Assoc n b
    Zero :: Alg n b
    (:+) :: (ExElm n b) -> (Assoc n b) -> Assoc n b

のように分離したほうが良い.
-}


data  Alg n b where
    Zero :: Alg n b
    Node :: { _size    :: !Size
            , _key :: !(b,Identifier)
            , _val     :: n
            , _left    :: !(Alg n b)
            , _right   :: !(Alg n b)} -> Alg n b

instance NFData (Alg n b) where
    rnf Zero = Zero `seq` ()
    rnf (Node s k v l r) = s `seq` k `seq` v `seq` l `seq` r `seq` ()

_hatBase = fst . _key

isZero Zero = True
isZero _    = False

type Size = Prelude.Int
type Identifier = Prelude.Int

sizeOf :: Alg n b -> Int
sizeOf Zero = 0
sizeOf (Node s _ _ _ _) = s


singleton :: (HatVal n, HatBaseClass b) => b -> n ->  Alg n b
singleton b n | n == 0 = Zero
              | otherwise = case isErrorValue n of
                    False -> Node 1 (b,0) n Zero Zero
                    True  -> error  $ "errorValue at (.@) val: "
                                    ++ show n
                                    ++ show ":@"
                                    ++ show b

(.@) :: (HatVal n, HatBaseClass b) => n -> b -> Alg n b
(.@) n b = singleton b n

(<@) :: (HatVal n, Applicative f, HatBaseClass b)
     => f n  -> b -> f (Alg n b)
(<@) v b = (.@) <$> v <*> (pure b)

-- | パターンマッチで (:@) が利用できるようにする
pattern (:@) :: (HatVal n, HatBaseClass b) => n -> b -> Alg n b
pattern (:@) n b <- Node 1 (b,0) n Zero Zero
    where
        (:@) = (.@)

-- | 元(Zero,Singleton)と線型結合(Liner)を判別するための関数
isLiner x | sizeOf x > 2 = True
          | otherwise    = False

-- | 線形結合のパターンマッチ
pattern Liner <- (isLiner -> True)

infixr 6 :@
infixr 6 .@
infixr 6 <@

instance (HatVal n, HatBaseClass b) => Show (Alg n b) where
    show Zero                 = "0"
    show (Node s (b,i) v l r) =  let vStr = D.formatScientific D.Generic (Just 2) (D.fromFloatDigits v)
                              in case (isZero l, isZero r) of
                                    (True, True)    -> vStr ++ ":@" ++ show b
                                    (True, False)   -> vStr ++ ":@" ++ show b
                                                    ++ " .+ " ++ show r
                                    (False, True)   -> show l
                                                    ++ " .+ " ++ vStr ++ ":@" ++ show b
                                    (False, False)  -> show l
                                                    ++ " .+ " ++ vStr ++ ":@" ++ show b
                                                    ++ " .+ " ++ show r

instance (HatVal n, HatBaseClass b) =>  Eq (Alg n b) where
    (==) Zero Zero = True
    (==) Zero _    = False
    (==) _    Zero = False
    (==) (Node s1 (b1,i1) v1 l1 r1) (Node s2 (b2,i2) v2 l2 r2)
        = case (v1:@b1) == (v2:@b2) of
            False -> False
            True  -> case (l1 == l2) of
                    False -> False
                    True  -> r1 == r2
    (/=) x y = not (x == y)

instance (HatVal n, HatBaseClass b) =>  Ord (Alg n b) where
    compare Zero Zero = EQ
    compare Zero _    = LT
    compare _    Zero = GT
    compare (Node _ (b1,_) v1 Zero Zero) (Node _ (b2,_) v2 Zero Zero)
        | b1 == b2  = compare v1 v2
        | otherwise = compare b1 b2
    -- Node に関しては定義しない

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
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen .+ 2.00:@Not:<Amount .+ 1.00:@Not:<Amount

union :: (HatVal n, HatBaseClass b) =>  Alg n b -> Alg n b -> Alg n b
union Zero t2  = t2
union t1 Zero  = t1
-- If one side is a singleton
union t1 (Node _ (b,_) n Zero Zero) = insert b n t1
union (Node _ (b,_) n Zero Zero) t2 = insert b n t2
-- In the case of multiple elements
union t1@(Node _ b1 n1 l1 r1) t2 = link b1 n1 l1l2 r1r2
            where (l2, r2) = split b1 t2
                  !l1l2 = union l1 l2
                  !r1r2 = union r1 r2

{-# INLINE insert #-}
insert :: (HatVal n,HatBaseClass b) => b -> n -> Alg n b ->  Alg n b
insert b n Zero = Node 1 (b,0) n Zero Zero
insert b n t@(Node s (b2,i2) n2 l r) = b `seq` go b n 0 t
    where
        go :: (HatVal n,HatBaseClass b)
           => b -> n -> Identifier -> Alg n b ->  Alg n b
        go b n i Zero = n.@ b
        go b n i t@(Node sz (by,iy) ny l r) =
            case compare b by of
                -- 小さい場合は左側に入れる
                LT -> balanceL (by,iy) ny (go b n i l) r
                -- 大きい場合は右に入れる
                GT -> balanceR (by,iy) ny l (go b n i r)
                -- 等しい場合はIdentifierで分岐
                EQ -> case compare (b,i) (by, iy) of
                    -- identifier が既存のものより大きくなるまで追加
                    LT -> go b n (i + 1) t
                    EQ -> go b n (i + 1) t
                    -- 既存のものを超えたら右側に追加
                    GT -> balanceR (by,iy) ny l (go b n i r)

delta = 3
ratio = 2

{-# INLINE balanceL #-}
balanceL :: (HatVal n, HatBaseClass b)
         => (b,Identifier) -> n -> Alg n b -> Alg n b -> Alg n b
balanceL b n l r = case r of
  Zero -> case l of
           Zero                   -> Node 1 b n Zero Zero
           (Node _ _ _ Zero Zero) -> Node 2 b n l Zero

           (Node _ lb ln Zero (Node _ lrb lrn _ _))
                -> Node 3 lrb lrn (Node 1 lb ln Zero Zero) (Node 1 b n Zero Zero)

           (Node _ lb ln ll@(Node _ _ _ _ _) Zero)
                -> Node 3 lb ln ll (Node 1 b n Zero Zero)

           (Node ls lb ln ll@(Node lls _ _ _ _) lr@(Node lrs lrb lrn lrl lrr))
             | lrs < ratio*lls  -> Node (1+ls) lb ln ll (Node (1+lrs) b n lr Zero)
             | otherwise        -> Node (1+ls) lrb lrn (Node (1+lls+sizeOf lrl) lb ln ll lrl) (Node (1+sizeOf lrr) b n lrr Zero)

  (Node rs _ _ _ _) -> case l of
           Zero -> Node (1+rs) b n Zero r

           (Node ls lb ln ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Node lls _ _ _ _, Node lrs lrb lrn lrl lrr)
                     | lrs < ratio*lls -> Node (1+ls+rs) lb ln ll (Node (1+rs+lrs) b n lr r)
                     | otherwise -> Node (1+ls+rs) lrb lrn (Node (1+lls+sizeOf lrl) lb ln ll lrl) (Node (1+rs+sizeOf lrr) b n lrr r)
                   (_, _) -> error "Failure in balanceL"
              | otherwise -> Node (1+ls+rs) b n l r

{-# INLINE balanceR #-}
balanceR :: (HatVal n, HatBaseClass b)
         =>  (b,Identifier) -> n -> Alg n b -> Alg n b -> Alg n b
balanceR b n l r = case l of
  Zero -> case r of
           Zero -> Node 1 b n Zero Zero -- 終端,始端はNull

           (Node _ _ _ Zero Zero)
                    -> Node 2 b n Zero r

           (Node _ rb rn Zero rr@(Node _ _ _ _ _))
                    -> Node 3 rb rn (Node 1 b n Zero Zero) rr

           (Node _ rb rn (Node _ rlb rln _ _) Zero)
                    -> Node 3 rlb rln (Node 1 b n Zero Zero) (Node 1 rb rn Zero Zero)

           (Node rs rb rn rl@(Node rls rlb rln rll rlr) rr@(Node rrs _ _ _ _))
             | rls < ratio*rrs  -> Node (1+rs) rb rn (Node (1+rls) b n Zero rl) rr
             | otherwise        -> Node (1+rs) rlb rln (Node (1+sizeOf rll) b n Zero rll) (Node (1+rrs+sizeOf rlr) rb rn rlr rr)

  (Node ls _ _ _ _) -> case r of
           Zero -> Node (1+ls) b n l Zero

           (Node rs rb rn rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Node rls rlb rln rll rlr, Node rrs _ _ _ _)
                     | rls < ratio*rrs -> Node (1+ls+rs) rb rn (Node (1+ls+rls) b n l rl) rr
                     | otherwise -> Node (1+ls+rs) rlb rln (Node (1+ls+sizeOf rll) b n l rll) (Node (1+rrs+sizeOf rlr) rb rn rlr rr)
                   (_, _) -> error "Failure in balanceR"
              | otherwise -> Node (1+ls+rs) b n l r


{-# INLINE link #-}
link :: (HatVal n, HatBaseClass b) => (b, Identifier) -> n -> Alg n b -> Alg n b -> Alg n b
link kx x Zero r    = insertMin kx x r
link kx x l    Zero = insertMax kx x l
link kx x l@(Node sizeL ky y ly ry) r@(Node sizeR kz z lz rz)
  | delta*sizeL < sizeR  = balanceL kz z (link kx x l lz) rz
  | delta*sizeR < sizeL  = balanceR ky y ly (link kx x ry r)
  | otherwise            = bin kx x l r

{-# INLINE bin #-}
bin :: (HatVal n, HatBaseClass b) => (b, Identifier) -> n -> Alg n b -> Alg n b -> Alg n b
bin k x l r = Node (sizeOf l + sizeOf r + 1) k x l r

{-# INLINE split #-}
split :: (HatVal n, HatBaseClass b) => (b,Identifier) -> Alg n b -> (Alg n b, Alg n b)
split !k0 t0 = go k0 t0
  where
    go k t =
      case t of
        Zero            -> (Zero, Zero)
        Node _ kx@(b,n) x l r -> case compare k kx of
          LT -> let (lt,gt) = go k l in (lt,link kx x gt r)
          GT -> let (lt, gt) = go k r in (link kx x l lt, gt)
          EQ -> (l, insert b x r)



{-# INLINE insertMax #-}
insertMax,insertMin :: (HatVal n, HatBaseClass b) => (b,Identifier) -> n -> Alg n b -> Alg n b
insertMax kx@(b,i) x t
  = case t of
      Zero -> singleton b x
      Node _ ky y l r
          -> balanceR ky y l (insertMax kx x r)

{-# INLINE insertMin #-}
insertMin kx@(b,i) x t
  = case t of
      Zero -> singleton b x
      Node _ ky y l r
          -> balanceL ky y (insertMin kx x l) r

vals :: (HatVal n, HatBaseClass b) => Alg n b -> [n]
vals m
  = [x | (_,x) <- assocs m]


bases :: (HatVal n, HatBaseClass b) => Alg n b -> [b]
bases m
    = [b | ((b,_),_) <- assocs m]

keys  :: (HatVal n, HatBaseClass b) => Alg n b -> [(b,Identifier)]
keys m
  = [k | (k,_) <- assocs m]

assocs :: (HatVal n, HatBaseClass b) => Alg n b -> [((b,Identifier),n)]
assocs m
  = foldrWithKey (\b n xs -> (b,n):xs) [] m

foldrWithKey :: (HatVal n, HatBaseClass b)
             => ((b,Identifier) -> n -> c -> c) -> c -> Alg n b -> c
foldrWithKey f = go
  where
    go z Zero              = z
    go z (Node _ b n l r) = go (f b n (go z r)) l

-- | b -> n のリストを「(b, 0)」をキーにしてツリーに挿入
--   合算後は Identifier=0 など固定で良いと仮定
fromListB :: (HatVal n, HatBaseClass b) => [(b, n)] -> Alg n b
fromListB = foldr (\(b, sumN) acc -> insert b sumN acc) Zero

-- netHat
netHat :: (HatVal n, HatBaseClass b)
       => Map.Map b n
       -> Map.Map b n
netHat m0 = go (Map.keys m0) m0
  where
    go []     m = m
    go (b:bs) m =
      case Map.lookup b m of
        Nothing -> go bs m
        Just val ->
          let bHat = revHat b
          in case Map.lookup bHat m of
               Nothing -> go bs m
               Just valHat -> case compare val valHat of
                         EQ -> go bs (Map.delete b    (Map.delete bHat m))
                         GT -> go bs (Map.insert b    (val - valHat) (Map.delete bHat m))
                         LT -> go bs (Map.insert bHat (valHat - val ) (Map.delete b m))

-- | fromAscList => O(n)で平衡BST化
fromAscList :: (HatVal n, HatBaseClass b) => [(b,n)] -> Alg n b
fromAscList [] = Zero
fromAscList kvs = buildBalanced kvs (Prelude.length kvs)

buildBalanced :: (HatVal n, HatBaseClass b)
              => [(b,n)]
              -> Int
              -> Alg n b
buildBalanced _ 0 = Zero
buildBalanced xs len =
  let mid = len `div` 2
      (ls, (bMid,valMid):rs) = splitAt mid xs
      leftT  = buildBalanced ls mid
      rightT = buildBalanced rs (len - mid - 1)
      sz     = sizeOf leftT + sizeOf rightT + 1
  in Node sz (bMid,0) valMid leftT rightT

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

{-
pattern (:+) :: (HatVal n, HatBaseClass b) => Alg n b -> Alg n b -> Alg n b
pattern (:+) x y <- ((\x y -> (sizeOf x + sizeOf , sizeOf y > 1 )) -> (False, x, y))
    where
        x :+ y = x union y
-}



------------------------------------------------------------------
-- Redundant
------------------------------------------------------------------

instance (HatVal n, HatBaseClass b) => Redundant Alg n b where
    (.^) Zero    = Zero
    (.^) (n:@ b)  = n :@ (revHat b)
    (.^) xs   = map (.^) xs

    (.+) = mappend

    x  .*  Zero    = Zero
    0  .*  x       = Zero
    x  .*  (Node s (b,i) n Zero Zero)  = case n == 0 of
                        True  -> Zero
                        False -> Node s (b,0) (x * n) Zero Zero

    a  .*  (Node s (b,i) n l r)  = (Node s (b,i) (a*n) (a.*l) (a.*r))

    norm Zero       = 0
    norm (Node s (b,i) n Zero Zero)   = n
    norm xs         = L.sum $ vals xs

    {-# INLINE (.-) #-}
    (.-) Zero = Zero
    (.-) tree = let
            -- 1) 木をリスト化 => [((b,Identifier), n)]
            kvs = assocs tree

            -- 2) b ごとに合算 => Map b n
            map0 =  foldr
                    (\((b,_),val) acc -> Map.insertWith (+) b val acc)
                    Map.empty
                    kvs
            -- 3) b と revHat b を相殺
            map1 = netHat map0
            -- 4) 昇順 [(b,n)]
            bnList = Map.toAscList map1
            -- 5) 平衡BSTに再構築
            result = fromAscList bnList
          in
            result


    {-# INLINE compress #-}
    compress Zero       = Zero
    compress tree = let
            -- 1) 木をリスト化 => [((b,Identifier), n)]
            kvs = assocs tree

            -- 2) b ごとに合算 => Map b n
            map0 =  foldr
                    (\((b,_),val) acc -> Map.insertWith (+) b val acc)
                    Map.empty
                    kvs
            -- 4) 昇順 [(b,n)]
            bnList = Map.toAscList map0
            -- 5) 平衡BSTに再構築
            result = fromAscList bnList
          in
            result

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

-- | 全てHatかどうかを判定する
--
-- >>> allHat (10:@Hat:<Cash .+ 12:@Hat:<Deposits :: Alg NN.Double (HatBase AccountTitles))
-- True
--
-- >>> allHat (10:@Hat:<Cash .+ 12:@Not:<Deposits :: Alg NN.Double (HatBase AccountTitles))
-- False
--
-- Note: in case Zero, this returns True
-- >>> allNot (Zero :: Alg NN.Double (HatBase AccountTitles))
-- True
--
-- use (.+) instead of (:+)

allHat :: (HatVal n, HatBaseClass b) =>  Alg n b -> Bool
allHat xs = L.and $ L.map (isHat . _hatBase) $ toList xs

-- | 全てNotかどうかを判定する
--
--
-- >>> allNot (10:@Hat:<Cash .+ 12:@Hat:<Deposits :: Alg NN.Double (HatBase AccountTitles))
-- False
--
-- >>> allNot ( 10:@Not:<Cash .+ 12:@Not:<Deposits :: Alg NN.Double (HatBase AccountTitles))
-- True

allNot ::(HatVal n, HatBaseClass b) =>   Alg n b -> Bool
allNot xs = L.and $ L.map (isNot . _hatBase) $ toList xs

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
--  1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen .+ 2.00:@Not:<Amount .+ 1.00:@Not:<Amount

fromList ::(HatVal n, HatBaseClass b ) => [Alg n b] -> Alg n b
fromList = mconcat

-- | convert Alg n b to List
--
-- >>> toList (10:@Hat:<(Cash) .+ 10:@Hat:<(Deposits) .+ Zero :: Alg NN.Double (HatBase AccountTitles))
-- [10.00:@Hat:<Cash,10.00:@Hat:<Deposits]
--
-- you need define type variables to use this for Zero
-- >>> toList Zero :: [Alg NN.Double (HatBase AccountTitles)]
-- []
{-# INLINE toList #-}
toList :: (HatVal n, HatBaseClass b) => Alg n b -> [Alg n b]
toList Zero     = []
toList xs       = foldrWithKey (\(b,i) n xs -> if n /= 0 then (n:@b):xs else xs) [] xs

{-# INLINE map #-}
-- | map
map :: (HatVal n, HatBaseClass b, HatBaseClass a)
     => (Alg n a -> Alg n b) -> Alg n a -> Alg n b
map f Zero     = f Zero
map f (n :@ b) = f (n :@ b)
map f x        = fromList
               $ L.map f
               $ toList x

traverse :: (HatVal n, HatBaseClass b, HatBaseClass a, Applicative f)
         => (Alg n a -> f (Alg n b)) -> Alg n a -> f (Alg n b)
traverse f xs = fromList <$>  (sequenceA . fmap f)  (toList xs)

{-# INLINE mapM #-}
mapM :: (HatVal n,HatBaseClass b, HatBaseClass a, Applicative f)
     => (Alg n a -> f (Alg n b)) -> Alg n a -> f (Alg n b)
mapM = traverse

{-# INLINE forM #-}
forM :: (HatVal n, HatBaseClass b,HatBaseClass a, Applicative f)
     => Alg n a -> (Alg n a -> f (Alg n b)) -> f (Alg n b)
forM = flip mapM


{-# INLINE filter #-}
-- | filter
filter :: (HatVal n, HatBaseClass b) => (Alg n b -> Bool) -> Alg n b -> Alg n b
filter f Zero                 = Zero
filter f (v:@b) | f (v:@b)    = v:@b
                | otherwise   = Zero
filter f (Node _ (b,i) n l r)
    | f (n:@ b) = join (b,i) n (filter f l) (filter f r)
    | otherwise = merge (filter f l) (filter f r)

join :: (HatVal n, HatBaseClass b)
      => (b,Identifier) -> n -> Alg n b -> Alg n b -> Alg n b
join kx x Zero r  = insertMin kx x r
join kx x l Zero  = insertMax kx x l
join kx x l@(Node sizeL ky y ly ry) r@(Node sizeR kz z lz rz)
  | delta*sizeL <= sizeR  = balanceL kz z (join kx x l lz) rz
  | delta*sizeR <= sizeL  = balanceR ky y ly (join kx x ry r)
  | otherwise             = bin kx x l r

{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}


merge :: (HatVal n, HatBaseClass b)
      => Alg n b -> Alg n b -> Alg n b
merge Zero r   = r
merge l Zero   = l
merge l@(Node sizeL kx x lx rx) r@(Node sizeR ky y ly ry)
  | delta*sizeL <= sizeR = balanceL ky y (merge l ly) ry
  | delta*sizeR <= sizeL = balanceR kx x lx (merge rx r)
  | otherwise            = glue l r

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}


glue :: (HatVal n, HatBaseClass b)
      => Alg n b -> Alg n b -> Alg n b
glue Zero r = r
glue l Zero = l
glue l r
  | sizeOf l > sizeOf r = let ((km,m),l') = deleteFindMax l
                        in balanceL km m l' r
  | otherwise           = let ((km,m),r') = deleteFindMin r
                        in balanceR km m l r'


-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map

deleteFindMin :: (HatVal n, HatBaseClass b)
              => Alg n b -> (((b,Identifier),n),Alg n b)
deleteFindMin t
  = case t of
      Node _ k x Zero r -> ((k,x),r)
      Node _ k x l r   -> let (km,l') = deleteFindMin l in (km,balanceL k x l' r)
      Zero             -> (error "deleteFindMin: can not return the minimal element of an empty map", Zero)

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
-- > deleteFindMax empty                                      Error: can not return the maximal element of an empty map

deleteFindMax :: (HatVal n, HatBaseClass b)
               => Alg n b -> (((b,Identifier),n),Alg n b)
deleteFindMax t
  = case t of
      Node _ k x l Zero -> ((k,x),l)
      Node _ k x l r   -> let (km,r') = deleteFindMax r in (km,balanceL k x l r')
      Zero             -> (error "deleteFindMax: can not return the maximal element of an empty map", Zero)

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
{-# INLINE proj #-}
proj :: (HatVal n, HatBaseClass b)  => [b] -> Alg n b -> Alg n b
proj bs  alg = filter (f bs) alg
    where
    {-# INLINE f #-}
    f ::(HatVal n, HatBaseClass b)  => [b] -> Alg n b  -> Bool
    f _   Zero       = False
    f []  _          = False
    f [b] (v:@eb)    = b .== eb
    f bs  (v:@eb)    = L.any (\x -> eb .== x) bs

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

{-# INLINE projNorm #-}
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
