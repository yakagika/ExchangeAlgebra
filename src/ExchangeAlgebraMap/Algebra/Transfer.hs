{- |
    Module     : ExchangeAlgebra.Transfer
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


{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternGuards      #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE PostfixOperators   #-}



module ExchangeAlgebraMap.Algebra.Transfer
    ( TransTable (..)
    , isNullTable
    , insert
    , updateFunction
    , transfer
    , transferKeepWiledcard
    , table
    , TransTableParts
    , (.->)
    , (|%)
    , createTransfer
    , incomeSummaryAccount
    , netIncomeTransfer
    , grossProfitTransferKeepWiledcard
    , ordinaryProfitTransferKeepWiledcard
    , retainedEarningTransferKeepWiledcard
    , finalStockTransferKeepWiledcard
    ) where

import qualified    ExchangeAlgebraMap.Algebra as EA
import              ExchangeAlgebraMap.Algebra


import qualified    Number.NonNegative  as NN       ( Double
                                                    , fromNumber
                                                    , toNumber,T) -- 非負の実数
import qualified    Data.Maybe          as Maybe
import              Text.Show.Unicode               ( ushow)
import              GHC.Exts                        ( reallyUnsafePtrEquality#
                                                    , isTrue#
                                                    , build
                                                    , lazy)
import              Data.Semigroup                  ( Semigroup(stimes)
                                                    , stimesIdempotentMonoid)
import              Data.Monoid                     ( Monoid(..))
import qualified    Data.Foldable       as Foldable
import              Data.Foldable                   ( Foldable())
import              Data.Bits                       ( shiftL
                                                    , shiftR)
import              Utils.Containers.Internal.StrictPair
import              Debug.Trace

------------------------------------------------------------------
-- * 基本計算処理
------------------------------------------------------------------
-- ** 振替
type Size = Int

-- | 振替変換テーブル
data TransTable n b where
     NullTable   :: (HatVal n, HatBaseClass b) => TransTable n b
     TransTable  :: (HatVal n, HatBaseClass b)
                 => { _size       :: Size
                    , _before     :: b                  {- ^ 変換前の基底 -}
                    , _transFunc  :: (n -> n)           {- ^ 値の変換用の関数 -}
                    , _after      :: b                  {- ^ 変換後の基底 -}
                    , _left       :: TransTable n b
                    , _right      :: TransTable n b }
                    -> TransTable n b

isNullTable NullTable = True
isNullTable _         = False

instance (HatBaseClass b) => Show (TransTable n b) where
    show NullTable                = "[]"
    show (TransTable s b f a l r)                   = "[(" ++ ushow b
                                                    ++ ","
                                                    ++ ushow a
                                                    ++ ",<function>)"
                                                    ++ (if isNullTable l then "" else "," ++ (Prelude.tail. Prelude.init .ushow) l)
                                                    ++ (if isNullTable r then "" else "," ++ (Prelude.tail. Prelude.init .ushow) r)
                                                    ++ "]"

instance (HatVal n,HatBaseClass b) => Semigroup (TransTable n b) where
    (<>)   = union
    stimes = stimesIdempotentMonoid

instance (HatVal n, HatBaseClass b) => Monoid (TransTable n b) where
    mempty  = NullTable
    mconcat = unions
    mappend = (<>)

{-# INLINE union #-}
union ::(HatBaseClass b) => TransTable n b -> TransTable n b -> TransTable n b
union t1 NullTable  = t1
union NullTable t2 = t2
union t1 (TransTable _ b f a NullTable NullTable) = insertR b f a t1
union (TransTable _ b f a NullTable NullTable) t2 = insert b f a t2
union t1@(TransTable _ b1 f1 a1 l1 r1) t2 = case split b1 t2 of
  (l2, r2) | l1l2 `ptrEq` l1 && r1r2 `ptrEq` r1 -> t1
           | otherwise -> link b1 f1 a1 l1l2 r1r2
           where !l1l2 = union l1 l2
                 !r1r2 = union r1 r2

{-# INLINE link #-}
link :: (HatVal n, HatBaseClass b) => b -> (n -> n) -> b -> TransTable n b -> TransTable n b -> TransTable n b
link kx fx x NullTable r  = insertMin kx fx x r
link kx fx x l NullTable  = insertMax kx fx x l
link kx fx x l@(TransTable sizeL ky fy y ly ry) r@(TransTable sizeR kz fz z lz rz)
  | delta*sizeL < sizeR  = balanceL kz fz z (link kx fx x l lz) rz
  | delta*sizeR < sizeL  = balanceR ky fy y ly (link kx fx x ry r)
  | otherwise            = bin kx fx x l r

{-# INLINE bin #-}
bin :: (HatVal n, HatBaseClass b) => b -> (n -> n) -> b -> TransTable n b -> TransTable n b -> TransTable n b
bin k f x l r
  = TransTable (size l + size r + 1) k f x l r

{-# INLINE split #-}
split :: (HatBaseClass b) => b  -> TransTable n b -> (TransTable n b, TransTable n b)
split !k0 t0 = toPair $ go k0 t0
  where
    go k t =
      case t of
        NullTable            -> NullTable :*: NullTable
        TransTable _ kx fx x l r -> case compareElement k kx of
          LT -> let (lt :*: gt) = go k l in lt :*: link kx fx x gt r
          GT -> let (lt :*: gt) = go k r in link kx fx x l lt :*: gt
          EQ -> (l :*: r)

{-# INLINE insertMax #-}
insertMax,insertMin :: (HatVal n, HatBaseClass b) => b -> (n -> n) -> b -> TransTable n b -> TransTable n b
insertMax kx fx x t
  = case t of
      NullTable -> singleton kx fx x
      TransTable _ ky fy y l r
          -> balanceR ky fy y l (insertMax kx fx x r)

{-# INLINE insertMin #-}
insertMin kx fx x t
  = case t of
      NullTable -> singleton kx fx x
      TransTable _ ky fy y l r
          -> balanceL ky fy y (insertMin kx fx x l) r

{-# INLINE unions #-}
unions :: (HatVal n, Foldable f, HatBaseClass b) => f (TransTable n b) -> TransTable n b
unions ts = Foldable.foldl' union NullTable ts

{-# INLINE null #-}
null :: (HatBaseClass b) => TransTable n b -> Bool
null NullTable = True
null (TransTable _ _ _ _ _ _) = False

{-# INLINE size #-}
size :: (HatBaseClass b) => TransTable n b -> Size
size NullTable = 0
size (TransTable s _ _ _ _ _) = s

{-| 振替変換
振替変換は、交換代数元の要素の基底を別の基底に振り替える変換となります。
振替変換では、変換 対象の値は変わりません。例えば、次のような交換代数元 a があり、
a = 6^ < e1 > +2 < e2 > +2 < e3 > +4 < e4 > +5^ < e5 > 変換定義 t を次のように定義した場合、
( from) < e1 > -> (to) < eA >
( from) < e2 > -> (to) < eA >
( from) < e3 > -> (to) < eA >
変換結果 r は、次のようになります。
r = 6^ < e1 > +2 < e2 > +2 < e3 > +4 < e4 > +5^ < e5 >
   +  6 < e 1 > + 6 ^ < e A >
   + 2 ^ < e 2 > + 2 < e A >
   + 2 ^ < e 3 > + 2 < e A >
 = 6 ^ < e 1 > + 2 < e 2 > + 2 < e 3 > + 4 < e 4 > + 5 ^ < e 5 >
 = 6 ^ < e 1 > + 2 < e 2 > + 2 < e 3 > + 4 < e 4 > + 5 ^ < e 5 >
   + 6 < e 1 > + 6 ^ < e A > + 2 ^ < e 2 > + 4 < e A > + 2 ^ < e 3 >
-}
{-# INLINE transfer #-}
transfer :: (HatVal n, HatBaseClass b) => Alg n b -> TransTable n b -> Alg n b
transfer alg NullTable                              = alg
transfer Zero (TransTable _ b f a l r)              = Zero
transfer (v:@ hb1) (TransTable _ hb2 f a l r)       | hb1 ./= hb2 = case compareElement hb1 hb2 of
                                                            LT -> transfer (v :@ hb1) l
                                                            GT -> transfer (v :@ hb1) r
                                                    | hb1 .== hb2 = (f v) :@ a

{- I forgot why this is needed....
                                                    | hb1 .== hb2 = case compare v (f v) of
                                                            LT -> ((f v) - v) :@ hb1 -- 変換後に増えた分足す
                                                               .+ (f v)       :@ a
                                                            EQ -> (f v)       :@ a
                                                            GT -> (v - (f v)) :@ hb1 -- あまり
                                                               .+ (f v)       :@ a
-}
transfer xs tt = EA.map (\x -> transfer x tt) xs

-- | タプルの内, ワイルドカードは変換しない
{-# INLINE transferKeepWiledcard #-}
transferKeepWiledcard :: (HatVal n, HatBaseClass b) => Alg n b -> TransTable n b -> Alg n b
transferKeepWiledcard alg NullTable                              = alg
transferKeepWiledcard Zero (TransTable _ b f a l r)              = Zero
transferKeepWiledcard (v:@ hb1) (TransTable _ hb2 f a l r)
    | hb1 ./= hb2 = case compareElement hb1 hb2 of
            LT -> transferKeepWiledcard (v :@ hb1) l
            GT -> transferKeepWiledcard (v :@ hb1) r
    | hb1 .== hb2 = (f v) :@ keepWiledcard hb1 a

{- I forgot why this is needed....
    | hb1 .== hb2 = case compare v (f v) of
            LT -> ((f v) - v) :@ hb1 -- 変換後に増えた分足す
               .+ (f v)       :@ keepWiledcard hb1 a
            EQ -> (f v)       :@ keepWiledcard hb1 a
            GT -> (v - (f v)) :@ hb1 -- あまり
               .+ (f v)       :@ keepWiledcard hb1 a
-}

transferKeepWiledcard xs tt = EA.map (\x -> transferKeepWiledcard x tt) xs



{-# INLINE singleton #-}
singleton :: (HatVal n,HatBaseClass b) => b ->(n -> n) -> b -> TransTable n b
singleton before f after = TransTable 1 before f after NullTable NullTable

{-# INLINE insert #-}
insert :: (HatVal n,HatBaseClass b) => b -> (n -> n) -> b -> TransTable n b ->  TransTable n b
insert b = go b b
    where
    go :: (HatVal n,HatBaseClass b) =>  b -> b -> (n -> n) -> b -> TransTable n b -> TransTable n b
    go orig !_  f  x NullTable = singleton (lazy orig) f x
    go orig !bx fx x t@(TransTable sy by fy y l r) =
        case compareElement bx by of
            LT | l' `ptrEq` l -> t
               | otherwise -> balanceL by fy y l' r
               where !l' = go orig bx fx x l
            GT | r' `ptrEq` r -> t
               | otherwise -> balanceR by fy y l r'
               where !r' = go orig bx fx x r
            EQ | x `ptrEq` y && (lazy orig `seq` (orig `ptrEq` by)) -> t
               | otherwise -> TransTable sy (lazy orig) fx x l r

{-# INLINE insertR #-}
insertR ::  (HatVal n,HatBaseClass b) => b ->  (n -> n) -> b ->  TransTable n b -> TransTable n b
insertR kx0 = go kx0 kx0
  where
    go :: (HatVal n,HatBaseClass b) => b -> b ->  (n -> n) -> b -> TransTable n b -> TransTable n b
    go orig !_  fx ax NullTable = singleton (lazy orig) fx ax
    go orig !bx fx ax t@(TransTable _ by fy ay l r) =
        case compareElement bx by of
            LT | l' `ptrEq` l -> t
               | otherwise -> balanceL by fy ay l' r
               where !l' = go orig bx fx ax l
            GT | r' `ptrEq` r -> t
               | otherwise -> balanceR by fy ay l r'
               where !r' = go orig bx fx ax r
            EQ -> t

-- | 価格テーブルを変更する
updateFunction:: (HatVal n,HatBaseClass b) => b -> (n -> n) -> b -> TransTable n b ->  TransTable n b
updateFunction b = go b b
    where
    go :: (HatVal n,HatBaseClass b) =>  b -> b -> (n -> n) -> b -> TransTable n b -> TransTable n b
    go orig !_  f  x NullTable = singleton (lazy orig) f x
    go orig !kx fx x t@(TransTable sz ky fy y l r) =
        case compareElement kx ky of
            LT | l' `ptrEq` l -> t
               | otherwise -> balanceL ky fy y l' r
               where !l' = go orig kx fx x l
            GT | r' `ptrEq` r -> t
               | otherwise -> balanceR ky fy y l r'
               where !r' = go orig kx fx x r
            EQ | x `ptrEq` y && (lazy orig `seq` (orig `ptrEq` ky)) -> t
               | otherwise -> TransTable sz (lazy orig) (fx . fy) x l r


{-# INLINE ptrEq #-}
ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)

delta = 3
ratio = 2

balanceL :: (HatVal n, HatBaseClass b) => b -> (n -> n) -> b -> TransTable n b -> TransTable n b -> TransTable n b
balanceL b f a l r = case r of
  NullTable -> case l of
           NullTable -> TransTable 1 b f a NullTable NullTable
           (TransTable _ _ _ _ NullTable NullTable)
                -> TransTable 2 b f a l NullTable

           (TransTable _ lb lf la NullTable (TransTable _ lrb lrf lra _ _))
                -> TransTable 3 lrb lrf lra (TransTable 1 lb lf la NullTable NullTable) (TransTable 1 b f a NullTable NullTable)

           (TransTable _ lb lf la ll@(TransTable _ _ _ _ _ _) NullTable)
                -> TransTable 3 lb lf la ll (TransTable 1 b f a NullTable NullTable)

           (TransTable ls lb lf la ll@(TransTable lls _ _ _ _ _) lr@(TransTable lrs lrb lrf lra lrl lrr))
             | lrs < ratio*lls  -> TransTable (1+ls) lb lf la ll (TransTable (1+lrs) b f a lr NullTable)
             | otherwise        -> TransTable (1+ls) lrb lrf lra (TransTable (1+lls+size lrl) lb lf la ll lrl) (TransTable (1+size lrr) b f a lrr NullTable)

  (TransTable rs _ _ _ _ _) -> case l of
           NullTable -> TransTable (1+rs) b f a NullTable r

           (TransTable ls lb lf la ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (TransTable lls _ _ _ _ _, TransTable lrs lrb lrf lra lrl lrr)
                     | lrs < ratio*lls -> TransTable (1+ls+rs) lb lf la ll (TransTable (1+rs+lrs) b f a lr r)
                     | otherwise -> TransTable (1+ls+rs) lrb lrf lra (TransTable (1+lls+size lrl) lb lf la ll lrl) (TransTable (1+rs+size lrr) b f a lrr r)
                   (_, _) -> error "Failure in Data.Map.balanceL"
              | otherwise -> TransTable (1+ls+rs) b f a l r

balanceR :: (HatVal n, HatBaseClass b) =>  b -> (n -> n) -> b -> TransTable n b -> TransTable n b -> TransTable n b
balanceR b f a l r = case l of
  NullTable -> case r of
           NullTable
                    -> TransTable 1 b f a NullTable NullTable -- 終端,始端はNull

           (TransTable _ _ _ _ NullTable NullTable)
                    -> TransTable 2 b f a NullTable r

           (TransTable _ rb rf ra NullTable rr@(TransTable _ _ _ _ _ _))
                    -> TransTable 3 rb rf ra (TransTable 1 b f a NullTable NullTable) rr

           (TransTable _ rb rf ra (TransTable _ rlb rlf rla _ _) NullTable)
                    -> TransTable 3 rlb rlf rla (TransTable 1 b f a NullTable NullTable) (TransTable 1 rb rf ra NullTable NullTable)

           (TransTable rs rb rf ra rl@(TransTable rls rlb rlf rla rll rlr) rr@(TransTable rrs _ _ _ _ _))
             | rls < ratio*rrs  -> TransTable (1+rs) rb rf ra    (TransTable (1+rls) b f a NullTable rl) rr
             | otherwise        -> TransTable (1+rs) rlb rlf rla (TransTable (1+size rll) b f a NullTable rll) (TransTable (1+rrs+size rlr) rb rf ra rlr rr)

  (TransTable ls _ _ _ _ _) -> case r of
           NullTable -> TransTable (1+ls) b f a l NullTable

           (TransTable rs rb rf ra rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (TransTable rls rlb rlf rla rll rlr, TransTable rrs _ _ _ _ _)
                     | rls < ratio*rrs -> TransTable (1+ls+rs) rb rf ra (TransTable (1+ls+rls) b f a l rl) rr
                     | otherwise -> TransTable (1+ls+rs) rlb rlf rla (TransTable (1+ls+size rll) b f a l rll) (TransTable (1+rrs+size rlr) rb rf ra rlr rr)
                   (_, _) -> error "Failure in Data.Map.balanceR"
              | otherwise -> TransTable (1+ls+rs) b f a l r

lookup :: (HatVal n, HatBaseClass b) => b -> TransTable n b -> Maybe (TransTable n b)
lookup k = k `seq` go
  where
    go NullTable = Nothing
    go (TransTable s b f a l r) =
        case compare k b of
            LT -> go l
            GT -> go r
            EQ -> Just (TransTable s b f a l r)



-- | make TransTable from list
--
-- >>> ExchangeAlgebraMap.Algebra.Transfer.fromList [(Hat:<(Cash),Hat:<(Building),(id :: NN.Double -> NN.Double) ),(Not:<(Building),Not:<(Cash),id)]
-- [(Hat:<Cash,Hat:<Building,<function>),(Not:<Building,Not:<Cash,<function>)]

fromList :: (HatVal n, HatBaseClass b) => [(b,b,(n -> n))] -> TransTable n b
fromList [] = NullTable
fromList [(b1,a1, f1)] = a1 `seq` TransTable 1 b1 f1 a1 NullTable NullTable
fromList ((b1,a1, f1)  : xs0)   | not_ordered b1 xs0 = a1 `seq` fromList' (TransTable 1 b1 f1 a1 NullTable NullTable) xs0
                                | otherwise = a1 `seq` go (1::Int) (TransTable 1 b1 f1 a1 NullTable NullTable) xs0
  where
    not_ordered _ [] = False
    not_ordered kx ((ky, _, _) : _) = kx >= ky
    {-# INLINE not_ordered #-}

    fromList' t0 xs = Foldable.foldl' ins t0 xs
      where ins t (k,x,f) = insert k f x t

    go !_ t [] = t
    go _ t [(kx, x, fx)] = x `seq` insertMax kx fx x t
    go s l xs@((kx, x, fx) : xss) | not_ordered kx xss = fromList' l xs
                                  | otherwise = case create s xss of
                                    (r, ys, []) -> x `seq` go (s `shiftL` 1) (link kx fx x l r) ys
                                    (r, _,  ys) -> x `seq` fromList' (link kx fx x l r) ys

    create !_ [] = (NullTable, [], [])
    create s xs@(xp : xss)
      | s == 1 = case xp of (kx, x, fx)  | not_ordered kx xss -> x `seq` (TransTable 1 kx fx x NullTable NullTable, [], xss)
                                         | otherwise -> x `seq` (TransTable 1 kx fx x NullTable NullTable, xss, [])
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_, [], _) -> res
                      (l, [(ky, y, fy)], zs) -> y `seq` (insertMax ky fy y l, [], zs)
                      (l, ys@((ky, y, fy):yss), _) | not_ordered ky yss -> (l, [], ys)
                                                   | otherwise -> case create (s `shiftR` 1) yss of
                                                      (r, zs, ws) -> y `seq` (link ky fy y l r, zs, ws)


-- | make TransTable from list
-- same as fromList
-- >>> table $ Hat:<(Cash) :-> Hat:<(Building) |% (id :: NN.Double -> NN.Double) ++ Hat:<(Building) :-> Hat:<(Cash) |% id
-- [(Hat:<Cash,Hat:<Building,<function>),(Hat:<Building,Hat:<Cash,<function>)]

table ::  (HatVal n, HatBaseClass b) => [(b,b,(n -> n))] -> TransTable n b
table = ExchangeAlgebraMap.Algebra.Transfer.fromList

data TransTableParts b where
  (:->)   :: (HatBaseClass b) => b -> b -> TransTableParts b

(.->) :: (HatBaseClass b) => b -> b -> TransTableParts b
(.->) b1 b2  = b1 :-> b2

instance (HatBaseClass b) => Show (TransTableParts b) where
  show (b1 :-> b2) = show b1 ++ " :-> " ++ show b2

-- | Syntax to make list for makeList
--
-- >>> Hat:<(Yen,Cash):-> Hat:<(Yen,Building) |% (id :: NN.Double -> NN.Double) ++ Not:<(Yen,Building)  :-> Not:<(Yen, Cash)  |% id
-- [(Hat:<(Yen,Cash),Hat:<(Yen,Building),<function>),(Not:<(Yen,Building),Not:<(Yen,Cash),<function>)]

(|%) :: (HatVal n, HatBaseClass b) => TransTableParts b -> (n -> n) -> [(b,b,(n -> n))]
(|%) (b1 :-> b2) f = [(b1,b2,f)]

infixr 8 .->
infixr 8 :->
infixr 7 |%

instance (HatVal n) => Show (n -> n) where
    show f = "<function>"

{-
instance Show (TransTable n b) where
    show (TransTable n b)         = "TransTable "         ++ show b
    show (PartialTransTable b)  = "PartialTransTable "  ++ show b
    show (FunctionTransTable b) = "FunctionTransTable " ++
                                (L.foldl1 (++)
                                (L.map (\(before, (f, after)) -> "(" ++ show before
                                                              ++ ", ( <function:: NN.Double -> NN.Double>, "
                                                              ++ show after ++ ")")
                                (Map.toList b)))

-}



createTransfer :: (HatVal n, ExBaseClass b) => [(b,b,(n -> n))] -> (Alg n b -> Alg n b)
createTransfer tt = \ts -> transferKeepWiledcard ts $ table tt

-- * 決算振替仕訳

-- | Income Summary Account 当期純利益の算定
incomeSummaryAccount :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
incomeSummaryAccount alg =  let (dc,diff) = diffRL alg
                         in let x = case dc of
                                        Debit  -> diff :@ (toNot wiledcard) .~ NetIncome
                                        Credit -> diff :@ (toNot wiledcard) .~ NetLoss
                         in alg .+  x

-- | 当期純利益の振替
netIncomeTransfer :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
netIncomeTransfer = createTransfer
    $  (toNot wiledcard) .~ NetIncome :-> (toNot wiledcard) .~ RetainedEarnings |% id
    ++ (toNot wiledcard) .~ NetLoss   :-> (toNot wiledcard) .~ RetainedEarnings |% id


-- **  仕分け

-- | Gross Profit Transfer
grossProfitTransferKeepWiledcard :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
grossProfitTransferKeepWiledcard
    =  createTransfer
    $  (toNot wiledcard) .~ WageExpenditure :-> (toHat wiledcard) .~ GrossProfit |% id
    ++ (toHat wiledcard) .~ WageExpenditure :-> (toNot wiledcard) .~ GrossProfit |% id
    ------------------------------------------------------------------
    ++ (toNot wiledcard) .~ Depreciation    :-> (toHat wiledcard) .~ GrossProfit |% id
    ++ (toHat wiledcard) .~ Depreciation    :-> (toNot wiledcard) .~ GrossProfit |% id
    ------------------------------------------------------------------
    ++ (toNot wiledcard) .~ ValueAdded      :-> (toNot wiledcard) .~ GrossProfit |% id
    ++ (toHat wiledcard) .~ ValueAdded      :-> (toHat wiledcard) .~ GrossProfit |% id
    ------------------------------------------------------------------
    ++ (toNot wiledcard) .~ Sales           :-> (toNot wiledcard) .~ GrossProfit |% id
    ++ (toHat wiledcard) .~ Sales           :-> (toHat wiledcard) .~ GrossProfit |% id
    ------------------------------------------------------------------
    ++ (toNot wiledcard) .~ Purchases       :-> (toHat wiledcard) .~ GrossProfit |% id
    ++ (toHat wiledcard) .~ Purchases       :-> (toNot wiledcard) .~ GrossProfit |% id

-- | Ordinary Profit Transfer
--
-- >>>  import qualified Number.NonNegative as NN
-- >>>  type Test = Alg NN.Double (HatBase (CountUnit, AccountTitles))
-- >>>  x = 2279.0:@Not:<(Yen,Depreciation) .+ 500475.0:@Not:<(Yen,InterestEarned) :: Test
-- >>>  ordinaryProfitTransferKeepWiledcard x
-- 2279.00:@Hat:<(Yen,OrdinaryProfit) .+ 500475.00:@Not:<(Yen,OrdinaryProfit)

ordinaryProfitTransferKeepWiledcard :: (HatVal n, ExBaseClass b) =>  Alg n b -> Alg n b
ordinaryProfitTransferKeepWiledcard
  = createTransfer
  $  (toNot wiledcard) .~ GrossProfit               :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ GrossProfit               :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ------------------------------------------------------------------
  ++ (toNot wiledcard) .~ InterestEarned            :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ InterestEarned            :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ------------------------------------------------------------------
  ++ (toNot wiledcard) .~ InterestExpense           :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ InterestExpense           :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  ------------------------------------------------------------------
  ++ (toNot wiledcard) .~ SubsidyIncome             :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ SubsidyIncome             :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ------------------------------------------------------------------
  ++ (toNot wiledcard) .~ TaxesExpense              :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ TaxesExpense              :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  -- Government
  ++ (toNot wiledcard) .~ TaxesRevenue              :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ TaxesRevenue              :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ------------------------------------------------------------------
  ++ (toNot wiledcard) .~ CentralBankPaymentIncome  :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ CentralBankPaymentIncome  :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ------------------------------------------------------------------
  ++ (toNot wiledcard) .~ Depreciation              :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ Depreciation              :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  ------------------------------------------------------------------
  ++ (toNot wiledcard) .~ WageExpenditure           :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ WageExpenditure           :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  ------------------------------------------------------------------
  ++ (toNot wiledcard) .~ SubsidyExpense            :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ SubsidyExpense            :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  ------------------------------------------------------------------
  -- Household
  ++ (toNot wiledcard) .~ WageEarned                :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ WageEarned                :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ------------------------------------------------------------------
  ++ (toNot wiledcard) .~ ConsumptionExpenditure    :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ ConsumptionExpenditure    :-> (toNot wiledcard) .~ OrdinaryProfit |% id
  -- CentralBank
  ++ (toNot wiledcard) .~ CentralBankPaymentExpense :-> (toHat wiledcard) .~ OrdinaryProfit |% id
  ++ (toHat wiledcard) .~ CentralBankPaymentExpense :-> (toNot wiledcard) .~ OrdinaryProfit |% id


-- | Retained Earning Transfer
retainedEarningTransferKeepWiledcard :: (HatVal n, ExBaseClass b) =>  Alg n b -> Alg n b
retainedEarningTransferKeepWiledcard
  = createTransfer
  $  (toNot wiledcard) .~ OrdinaryProfit            :-> (toNot wiledcard) .~ RetainedEarnings |% id
  ++ (toHat wiledcard) .~ OrdinaryProfit            :-> (toHat wiledcard) .~ RetainedEarnings |% id

-- | Final Stock Transfer (損益勘定)
finalStockTransferKeepWiledcard ::(HatVal n, ExBaseClass b) =>  Alg n b -> Alg n b
finalStockTransferKeepWiledcard  = (.-)
                    . retainedEarningTransferKeepWiledcard
                    . ordinaryProfitTransferKeepWiledcard
                    . grossProfitTransferKeepWiledcard


