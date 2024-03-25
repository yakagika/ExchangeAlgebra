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
    ,StateSpace) where

import              Control.Monad
import              Control.Monad.ST
import              GHC.Generics


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
    initialize      :: t -> m s a

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
    initAll ::   t -> m s a

    -- DefaultSignatures 拡張
    default initAll :: (Generic a, GUpdatable t m s (Rep a)) =>  t -> m s a
    initAll t = GHC.Generics.to <$> gInitialize t

    -- DefaultSignatures 拡張
    {-# INLINE updateAll #-}
    updateAll :: t -> a -> m s ()
    default updateAll :: (Generic a, GUpdatable t m s (Rep a)) => t -> a -> m s ()
    updateAll t a = gUpdate t (GHC.Generics.from a)

-- Genericを用いた自動導出のための補助型クラス
class  (Monad (m s),StateTime t)
        => GUpdatable t m s f where
    gInitialize :: t -> m s (f p)

    gUpdate :: t -> f p -> m s ()

-- コンストラクタに対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t)
        => GUpdatable t m s U1 where
    gInitialize _ = return U1
    gUpdate _ _ = return ()

-- | プリミティブ型に対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, Updatable t m s a)
        => GUpdatable t m s (K1 i a) where
    gInitialize t = K1 <$> initialize t
    gUpdate t (K1 x) = update t x

-- |直和型に対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, GUpdatable t m s f, GUpdatable t m s g)
        => GUpdatable t m s (f :+: g) where
    gInitialize t  = gInitialize t
    gUpdate t (L1 f) = gUpdate t f
    gUpdate t (R1 g) = gUpdate t g

-- | レコードフィールドに対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, GUpdatable t m s g, GUpdatable t m s f)
        => GUpdatable t m s (g :*: f) where
    gInitialize t = (:*:) <$> gInitialize t <*> gInitialize t
    gUpdate t (x :*: y) = gUpdate t y >> gUpdate t x

-- メタデータに対するGUpdatableのインスタンス
instance (Monad (m s),StateTime t, GUpdatable t m s f)
        => GUpdatable t m s (M1 p l f) where -- メタデータは無視する
    gInitialize t = M1 <$> gInitialize t
    gUpdate t (M1 f) = gUpdate t f

