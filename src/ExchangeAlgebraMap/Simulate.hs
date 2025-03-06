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
{-# LANGUAGE Rank2Types         #-}

module ExchangeAlgebraMap.Simulate
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
    ,updateGen
    ,EnvironmentVariables) where

import              Control.Monad
import              Control.Monad.ST
import              GHC.Generics
import              System.Random
import              Data.Ix

class (Eq t, Show t, Ord t) => StateTime t where
    initTerm :: t
    lastTerm :: t
    nextTerm :: t -> t
    prevTerm :: t -> t

class (Eq e, Show e) => EnvironmentVariables e where


-- | A parameter that indicates the pattern for updating the value to the next period
data UpdatePattern = Copy         -- | Copy the previous state
                   | Modify       -- | Modyfy the previous state
                   | DoNothing    -- | Do nothing (if the initialisation has generated the state for all periods)
                   deriving (Show, Eq)

-- | Elements of StateSpace
class (Monad (m s),StateTime t,EnvironmentVariables e)
      => Updatable t e m s a  where
    -- StdGenは現状は全てのインスタンスの初期化で同じ値を返す.
    -- (乱数生成器を更新していない)
    -- 現状異なる乱数をインスタンスごとに利用したい場合は,updateGenなどを利用
    -- 同一の乱数生成器を更新して使えるようにする予定
    initialize      :: StdGen  -> t -> e -> m s a

    updatePattern   :: t -> e -> a -> m s UpdatePattern

    {-# INLINE copy #-}
    copy            :: t -> e -> a -> m s ()
    copy t e x = undefined

    {-# INLINE modify #-}
    modify          :: t -> e -> a -> m s ()
    modify t e x = undefined

    {-# INLINE update #-}
    update          :: t -> e -> a -> m s ()
    update t e x =  updatePattern t e x >>= \k
                    -> case k of
                            DoNothing -> return ()
                            Copy      -> copy   t e x
                            Modify    -> modify t e x

class (Monad (m s),StateTime t,EnvironmentVariables e)
      => StateSpace t e m s a where
    {-# INLINE initAll #-}
    initAll ::  StdGen -> t -> e -> m s a

    -- DefaultSignatures 拡張
    default initAll :: (Generic a, GUpdatable t e m s (Rep a)) =>  StdGen  -> t -> e ->  m s a
    initAll g t e = GHC.Generics.to <$> gInitialize g t e

    -- DefaultSignatures 拡張
    {-# INLINE updateAll #-}
    updateAll :: t -> e -> a -> m s ()
    default updateAll :: (Generic a, GUpdatable t e m s (Rep a)) => t -> e -> a -> m s ()
    updateAll t e a = gUpdate t e (GHC.Generics.from a)

    -- 開始期を指定する
    initT :: e -> a -> m s t
    initT _ _ = return initTerm

    -- 終了期を指定する
    lastT :: e -> a -> m s t
    lastT _ _ = return lastTerm

    -- | 乱数シードを定義する(デフォルト42)
    -- 乱数シードを明示的に定義することも可能
    -- 現状使い道なし
    randomSeeds :: t -> e -> a -> m s Int
    randomSeeds _ _ _ = return 42

-- Genericを用いた自動導出のための補助型クラス
class  (Monad (m s),StateTime t,EnvironmentVariables e)
        => GUpdatable t e m s f where
    gInitialize :: StdGen  -> t -> e -> m s (f p)

    gUpdate :: t -> e -> f p -> m s ()

-- コンストラクタに対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t,EnvironmentVariables e)
        => GUpdatable t e m s U1 where
    gInitialize _ _ _ = return U1
    gUpdate _ _ _ = return ()

-- | プリミティブ型に対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, EnvironmentVariables e, Updatable t e m s a)
        => GUpdatable t e m s (K1 i a) where
    gInitialize g t e = K1 <$> initialize g t e
    gUpdate t e (K1 x) = update t e x

-- |直和型に対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, EnvironmentVariables e, GUpdatable t e m s f, GUpdatable t e m s g)
        => GUpdatable t e m s (f :+: g) where
    gInitialize g t e = gInitialize g t e
    gUpdate t e (L1 f) = gUpdate t e f
    gUpdate t e (R1 g) = gUpdate t e g

-- | レコードフィールドに対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, EnvironmentVariables e, GUpdatable t e m s g, GUpdatable t e m s f)
        => GUpdatable t e m s (g :*: f) where
    gInitialize g t e = (:*:) <$> gInitialize g t e <*> gInitialize g t e
    gUpdate t e (x :*: y) = gUpdate t e y >> gUpdate t e x

-- メタデータに対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, EnvironmentVariables e, GUpdatable t e m s f)
        => GUpdatable t e m s (M1 p l f) where -- メタデータは無視する
    gInitialize g t e = M1 <$> gInitialize g t e
    gUpdate t e (M1 f) = gUpdate t e f


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
