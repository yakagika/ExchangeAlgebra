{- |
    Module     : ExchangeAlgebra.Simulate
    Copyright  : (c) Kaya Akagi. 2024
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DerivingVia            #-}
module ExchangeAlgebraMap.Simulate
    (StateTime
    ,initTerm
    ,lastTerm
    ,nextTerm
    ,prevTerm
    ,UpdatePattern(..)
    ,Updatable(unwrap, Inner)
    ,initialize
    ,updatePattern
    ,copy
    ,modify
    ,update
    ,initAll
    ,updateAll
    ,StateSpace(event,randomSeeds)
    ,normal
    ,normal'
    ,updateGen
    ,InitVariables
    ,UpdatableSTRef(..)
    ,UpdatableSTArray(..)
    ,modifyArray
    ,Event(..)
    ,eventAll
    ,runSimulation
    ,leontiefInverse
    ,rippleEffect) where

import              Control.Monad
import              GHC.Generics
import              System.Random
import              Data.Ix
import              Data.Kind
import              Control.Monad.ST
import              Data.Array.ST
import              Data.Array.IO
import              Data.STRef
import qualified    Control.Monad                   as CM
import              Data.Array
import              System.IO
import Data.List (intersperse)

------------------------------------------------------------------
class (Eq t, Show t, Ord t, Enum t, Ix t) => StateTime t where
    initTerm :: t
    lastTerm :: t
    nextTerm :: t -> t
    prevTerm :: t -> t

-- | Parameters for initialisation
-- Referenced during initialisation
-- Values referred to during simulation should be in Updatable
class (Eq e, Show e) => InitVariables e where

------------------------------------------------------------------
class (Ord e, Show e, Enum e, Eq e, Bounded e) => Event e where
    fstEvent :: e
    fstEvent = minBound
    lastEvent :: e
    lastEvent = maxBound

------------------------------------------------------------------
-- | A parameter that indicates the pattern for updating the value to the next period
data UpdatePattern = Copy         -- | Copy the previous state
                   | Modify       -- | Modyfy the previous state
                   | DoNothing    -- | Do nothing (if the initialisation has generated the state for all periods)
                   deriving (Show, Eq)

------------------------------------------------------------------
-- | Elements of StateSpace
class (StateTime t,InitVariables v)
      => Updatable t v (a :: Type -> Type) s | a s -> t v where
    type Inner a s
    unwrap :: a s -> Inner a s

    -- StdGenは現状は全てのインスタンスの初期化で同じ値を返す.
    -- (乱数生成器を更新していない)
    -- 現状異なる乱数をインスタンスごとに利用したい場合は,updateGenなどを利用
    -- 同一の乱数生成器を更新して使えるようにする予定
    initialize    :: StdGen -> t -> v -> ST s (a s)

    updatePattern :: (a s) -> ST s UpdatePattern

    {-# INLINE copy #-}
    copy :: StdGen -> t -> v -> (a s) -> ST s ()
    copy g t v x    = undefined

    {-# INLINE modify #-}
    modify :: StdGen -> t -> v -> (a s) -> ST s ()
    modify g t v x  = undefined

    {-# INLINE update #-}
    update :: StdGen -> t -> v -> (a s) -> ST s ()
    update g t v x  =  updatePattern x >>= \p
                    -> case p of
                            DoNothing -> return ()
                            Copy      -> copy   g t v x
                            Modify    -> modify g t v x


-- | for newtype A s = A (STRef s B)
-- example:
-- newtype URef s = URef (STRef s Int)
-- instance UpdatableRef URef s Int where
--     _unwrapU (URef x) = x
--     _wrapU x = (URef x)

class UpdatableSTRef wrapper s b | wrapper s -> b where
  _unwrapURef :: wrapper s -> STRef s b

  _wrapURef :: STRef s b -> wrapper s

  newURef    :: b -> ST s (wrapper s)
  newURef b = _wrapURef <$> newSTRef b

  readURef   :: wrapper s -> ST s b
  readURef = readSTRef . _unwrapURef

  writeURef  :: wrapper s -> b -> ST s ()
  writeURef = writeSTRef . _unwrapURef

  modifyURef :: wrapper s -> (b -> b) -> ST s ()
  modifyURef x f = modifySTRef (_unwrapURef x) f


-- | modifyArray
-- example:
-- newtype UArray s = UArray (STArray s (Int,Int) Double)
-- instance UpdatableSTArray UArray s (Int,Int) Double where
--   _unwrapUArray (UArray arr) = arr
--   _wrapUArray arr = UArray arr

{-# INLINE modifyArray #-}
modifyArray ::(MArray a t m, Ix i) => a i t -> i -> (t -> t) -> m ()
modifyArray ar e f = readArray ar e >>= \ x -> writeArray ar e (f x)

class (Ix b) => UpdatableSTArray wrapper s b c | wrapper s -> b c where
  _unwrapUArray :: wrapper s -> STArray s b c

  _wrapUArray :: STArray s b c -> wrapper s

  getUBounds :: wrapper s -> ST s (b,b)
  getUBounds = getBounds . _unwrapUArray

  newUArray    :: (b,b) -> c -> ST s (wrapper s)
  newUArray b c = _wrapUArray <$> newArray b c

  readUArray   :: wrapper s -> b -> ST s c
  readUArray arr = readArray (_unwrapUArray arr)

  writeUArray  :: wrapper s -> b -> c -> ST s ()
  writeUArray arr = writeArray (_unwrapUArray arr)

  modifyUArray :: wrapper s -> b -> (c -> c) -> ST s ()
  modifyUArray x f = modifyArray (_unwrapUArray x) f

------------------------------------------------------------------
class (StateTime t,InitVariables v, Event e)
      => StateSpace t v e (a :: Type -> Type) s | a s -> t v e where
    {-# INLINE initAll #-}
    initAll ::  StdGen -> t -> v -> ST s (a s)

    -- DefaultSignatures 拡張
    default initAll :: (Generic (a s), GUpdatable t v (Rep (a s)) s)
                    =>  StdGen -> t -> v ->  ST s (a s)
    initAll g t v = GHC.Generics.to <$> gInitialize g t v

    -- DefaultSignatures 拡張
    {-# INLINE updateAll #-}
    updateAll :: StdGen -> t -> v -> a s -> ST s ()
    default updateAll :: (Generic (a s), GUpdatable t v (Rep (a s)) s)
                      => StdGen -> t -> v -> a s -> ST s ()
    updateAll g t v a = gUpdate g t v (GHC.Generics.from a)

    -- イベント処理
    event :: a s -> t -> e -> ST s ()

    -- 開始期を指定する
    initT :: v ->  a s -> ST s t
    initT _ _ = return initTerm

    -- 終了期を指定する
    lastT :: v -> a s -> ST s t
    lastT _ _ = return lastTerm

    -- | 乱数シードを定義する(デフォルト42)
    -- 乱数シードを明示的に定義することも可能
    -- 現状使い道なし
    randomSeeds :: a s -> ST s Int
    randomSeeds _ = return 42


-- | Event全体を結合
{-# INLINE eventAll #-}
eventAll :: forall t v e a s. (StateSpace t v e a s) => a s -> t ->  ST s ()
eventAll wld t =  CM.forM_ [fstEvent .. lastEvent]
                $ \e -> event wld t e

-- | Simulation
{-# INLINE simulate #-}
simulate :: (StateSpace t v e a s)
         => StdGen -> a s -> v -> ST s ()
simulate g wld v = loop g wld initTerm v
  where
  {-# INLINE loop #-}
  loop :: (StateSpace t v e a s)
       => StdGen -> a s -> t -> v -> ST s ()
  loop g wld t v
    | t == lastTerm =  updateAll g t v wld >>= \_
                    -> eventAll wld t
    | otherwise     =  updateAll g t v wld >>= \_
                    -> eventAll wld t
                    >> loop g wld (nextTerm t) v

-- | 初期化 → 指定されたイベントの実行までをこなす
runSimulation :: (StateSpace t v e a RealWorld)
              => StdGen -> v ->  IO (a RealWorld)
runSimulation gen v = stToIO $ initAll gen initTerm v >>= \wld
                    -> simulate gen wld v
                    >> return wld

-- Genericを用いた自動導出のための補助型クラス
class  (StateTime t,InitVariables v)
        => GUpdatable t v a s where
    gInitialize :: StdGen  -> t -> v -> ST s (a s)

    gUpdate :: StdGen -> t -> v -> a s -> ST s ()

-- コンストラクタに対するGUpdatableのインスタンス
instance (StateTime t,InitVariables v)
        => GUpdatable t v U1 s where
    gInitialize _ _ _ = return U1
    gUpdate _ _ _ _ = return ()

-- | プリミティブ型に対するGUpdatableのインスタンス
instance (StateTime t, InitVariables v, Updatable t v a s)
        => GUpdatable t v (K1 i (a s)) s where
    gInitialize g t v = K1 <$> initialize g t v
    gUpdate g t v (K1 x) = update g t v x

-- |直和型に対するGUpdatableのインスタンス
instance (StateTime t, InitVariables v, GUpdatable t v p s, GUpdatable t v q s)
        => GUpdatable t v (p :+: q) s where
    gInitialize g t v = gInitialize g t v
    gUpdate g t v (L1 p) = gUpdate g t v p
    gUpdate g t v (R1 q) = gUpdate g t v q

-- | レコードフィールドに対するGUpdatableのインスタンス
instance (StateTime t, InitVariables v, GUpdatable t v p s, GUpdatable t v q s)
        => GUpdatable t v (p :*: q) s where
    gInitialize g t v = (:*:) <$> gInitialize g t v <*> gInitialize g t v
    gUpdate g t v (x :*: y) = gUpdate g t v y >> gUpdate g t v x

-- メタデータに対するGUpdatableのインスタンス
instance (StateTime t, InitVariables v, GUpdatable t v f s)
        => GUpdatable t v (M1 p l f) s where -- メタデータは無視する
    gInitialize g t v = M1 <$> gInitialize g t v
    gUpdate g t v (M1 f) = gUpdate g t v f

------------------------------------------------------------------
-- * Rippele Effect Analysis
------------------------------------------------------------------

-- | Generate Identity Matrix
identity :: Int -> IO (IOArray (Int, Int) Double)
identity n = newArray ((1, 1), (n, n)) 0 >>= \arr -> do
    forM_ [1..n] $ \i -> writeArray arr (i, i) 1
    return arr

-- | Calculate inverse matrix with Gauss-Jordan Method
inverse :: IOArray (Int, Int) Double -> IO (IOArray (Int, Int) Double)
inverse mat = do
    bnds <- getBounds mat
    let ((1,1),(n,_)) = bnds
    inv <- identity n

    forM_ [1..n] $ \i -> do
        pivot <- readArray mat (i,i)
        forM_ [1..n] $ \j -> do
            modifyArray mat (i,j) (/pivot)
            modifyArray inv (i,j) (/pivot)
        forM_ [1..n] $ \k -> when (k /= i) $ do
            factor <- readArray mat (k,i)
            forM_ [1..n] $ \j -> do
                mVal <- readArray mat (i,j)
                iVal <- readArray inv (i,j)
                modifyArray mat (k,j) (\x -> x - factor * mVal)
                modifyArray inv (k,j) (\x -> x - factor * iVal)

    return inv

  where
    modifyArray arr ix f = readArray arr ix >>= writeArray arr ix . f

{- | Calculate Leontief's Inverse Matrix
ex.
main :: IO ()
main = do
    mat <- newListArray ((1,1),(2,2)) [0.2, 0.3, 0.4, 0.1]
    result <- leontiefInverse mat
    putStrLn "Leontief Inverse (波及効果行列):"
    writeLeontiefInverse "output.csv" result
-}

leontiefInverse :: IOArray (Int, Int) Double -> IO (IOArray (Int, Int) Double)
leontiefInverse a = do
    bnds <- getBounds a
    temp <- newArray bnds 0
    forM_ (range bnds) $ \(i,j) -> do
        val <- readArray a (i,j)
        writeArray temp (i,j) (if i == j then 1 - val else -val)
    inverse temp

-- | 逆行列を利用して実際の需要増加量を計算する
-- 産業に1単位の需要増加があった場合の波及効果を計算する
rippleEffect :: Int -> IOArray (Int, Int) Double -> IO (IOArray (Int, Int) Double)
rippleEffect industry inverseArr = do
    ((r1,c1),(r2,c2)) <- getBounds inverseArr
    result <- newArray ((r1,c1),(r2,c2)) 0
    forM_ [r1..r2] $ \i -> do
        val <- readArray inverseArr (i, industry)
        writeArray result (i, industry) val
    return result


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
