{- |
    Module     : ExchangeAlgebra.Simulate
    Copyright  : (c) Kaya Akagi. 2024
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}

{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}

module ExchangeAlgebra.Simulate
    (StateTime
    ,initTerm
    ,lastTerm
    ,nextTerm
    ,prevTerm
    ,UpdatePattern (..)
    ,Updatable
    ,initialize
    ,updatePattern
    ,copy
    ,modify
    ,update
    ,initAll
    ,updateAll
    ,StateSpace
    ,normal
    ,normal'
    ,updateGen) where

import              Control.Monad
import              Control.Monad.ST
import              GHC.Generics
import              System.Random


class (Eq t, Show t, Ord t) => StateTime t where
    initTerm :: t
    lastTerm :: t
    nextTerm :: t -> t
    prevTerm :: t -> t


-- | 値の次の期の情報をどうするのかのパラメーター
data UpdatePattern = Copy         -- 前期の情報をそのままコピー
                   | Modify       -- 何らかの方法でupdate (単体でできる場合のみ)
                   | DoNothing    -- 放置
                   deriving (Show, Eq)

-- | 環境変数の系列
class (Monad (m s),StateTime t) => Updatable t m s a  where
    initialize      :: StdGen  -> t -> m s a

    updatePattern   :: t -> a -> m s UpdatePattern

    {-# INLINE copy #-}
    copy            :: t -> a -> m s ()
    copy t x = undefined

    {-# INLINE modify #-}
    modify          :: t -> a -> m s ()
    modify t x = undefined

    {-# INLINE update #-}
    update          :: t -> a -> m s ()
    update t x =  updatePattern t x >>= \k
                    -> case k of
                            DoNothing -> return ()
                            Copy      -> copy   t x
                            Modify    -> modify t x


class (Monad (m s),StateTime t) => StateSpace t m s a where
    {-# INLINE initAll #-}
    initAll ::  StdGen -> t -> m s a

    -- DefaultSignatures 拡張
    default initAll :: (Generic a, GUpdatable t m s (Rep a)) =>  StdGen  -> t -> m s a
    initAll g t = GHC.Generics.to <$> gInitialize g t

    -- DefaultSignatures 拡張
    {-# INLINE updateAll #-}
    updateAll :: t -> a -> m s ()
    default updateAll :: (Generic a, GUpdatable t m s (Rep a)) => t -> a -> m s ()
    updateAll t a = gUpdate t (GHC.Generics.from a)

    -- 開始期を指定する
    initT :: a -> m s t
    initT _ = return initTerm

    -- 終了期を指定する
    lastT :: a -> m s t
    lastT _ = return lastTerm

    -- 乱数シードを返す
    randomSeeds :: t -> a -> m s [Int]
    randomSeeds _ _  = return $ replicate 5 1

-- Genericを用いた自動導出のための補助型クラス
class  (Monad (m s),StateTime t)
        => GUpdatable t m s f where
    gInitialize :: StdGen  -> t -> m s (f p)

    gUpdate :: t -> f p -> m s ()

-- コンストラクタに対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t)
        => GUpdatable t m s U1 where
    gInitialize _ _ = return U1
    gUpdate _ _ = return ()

-- | プリミティブ型に対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, Updatable t m s a)
        => GUpdatable t m s (K1 i a) where
    gInitialize g t = K1 <$> initialize g t
    gUpdate t (K1 x) = update t x

-- |直和型に対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, GUpdatable t m s f, GUpdatable t m s g)
        => GUpdatable t m s (f :+: g) where
    gInitialize g t  = gInitialize g t
    gUpdate t (L1 f) = gUpdate t f
    gUpdate t (R1 g) = gUpdate t g

-- | レコードフィールドに対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, GUpdatable t m s g, GUpdatable t m s f)
        => GUpdatable t m s (g :*: f) where
    gInitialize g t = (:*:) <$> gInitialize g t <*> gInitialize g t
    gUpdate t (x :*: y) = gUpdate t y >> gUpdate t x

-- メタデータに対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, GUpdatable t m s f)
        => GUpdatable t m s (M1 p l f) where -- メタデータは無視する
    gInitialize g t = M1 <$> gInitialize g t
    gUpdate t (M1 f) = gUpdate t f


------------------------------------------------------------------
-- * 乱数関連
------------------------------------------------------------------

-- ** 正規分布
-- cf. https://hackage.haskell.org/package/normaldistribution-1.1.0.3/docs/src/Data-Random-Normal.html

-- | box muller methodによる正規分布の近似計算
boxMuller :: Floating a => a -> a -> (a,a)
boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
                                               t = 2 * pi * u2

normal :: (RandomGen g, Random a, Floating a) => g -> (a,g)
normal g0 = (fst $ boxMuller u1 u2, g2)
  where
     (u1,g1) = randomR (0,1) g0
     (u2,g2) = randomR (0,1) g1

normal' :: (RandomGen g, Random a, Floating a) => (a,a) -> g -> (a,g)
normal' (mean, sigma) g = (x * sigma + mean, g') where (x, g') = normal g

-- | 乱数発生器を指定された回数更新する
-- 実装上一つの乱数生成器を複数の変数で使い回すことになる
-- 特に初期化において,同じ乱数が発生するので,
-- 適当な回数更新を繰り返す必要がある
updateGen :: (RandomGen g) => g -> Prelude.Int -> g
updateGen g n
    | n <= 1     = g'
    | otherwise  = updateGen g' (n-1)
    where
    (_,g') = (genWord32 g)
