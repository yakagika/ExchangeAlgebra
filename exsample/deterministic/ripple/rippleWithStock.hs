
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

{-
Simulation sample of ripple effects
Each agent has inventory as an input constraint, and production is carried out within the inventory.
Ripple effects are measured within a network consisting of 10 agents.
The model consists of stages, with final demand generation -> sales -> production -> ordering carried out at each stage.
Final demand increases at a specified time, and the impact is measured.
-}

-- Original
import              ExchangeAlgebraMapJournal
import qualified    ExchangeAlgebraMap.Journal  as EJ
import qualified    ExchangeAlgebraMap.Journal.Transfer as EJT
import              ExchangeAlgebraMap.Journal.Transfer ((.->)
                                                        ,(|%))

import qualified    ExchangeAlgebraMap.Simulate as ES
import qualified    ExchangeAlgebraMap.Simulate
import qualified    ExchangeAlgebraMap.Simulate.Visualize as ESV


-- Other
import qualified    Data.Map.Strict         as M
import qualified    Data.Text               as T

import qualified    Control.Monad                   as CM
import              Control.Monad
import              Control.Monad.State
import              Control.Monad.ST
import              Data.Array.ST
import              Data.Array.IO
import              Data.Array (Array)
import              Data.STRef
import qualified    Data.List                       as L
import qualified    Data.Map.Strict                 as Map
import GHC.Generics
import System.Random
import Data.Coerce
import Control.Concurrent.Async (mapConcurrently,forConcurrently_)
import Statistics.Distribution hiding (mean, stdDev)
import Statistics.Distribution.Normal
import GHC.Generics (Generic)
import Data.Hashable

-- Debug
import Debug.Trace



------------------------------------------------------------------
-- * directories

fig_dir = "exsample/deterministic/ripple/result/fig/withStock/"


------------------------------------------------------------------
-- *  状態系の導入
------------------------------------------------------------------

instance StateTime Term where
    initTerm = 1
    lastTerm = 100
    nextTerm = \x -> x + 1
    prevTerm = \x -> x - 1

-- | 追加需要が発生する期
addedTerm :: Term
addedTerm = 50

------------------------------------------------------------------
-- ** 初期化用のパラメータ定義
------------------------------------------------------------------

data InitVar = InitVar {_initInv          :: Double -- 初期在庫保有量
                       ,_stockOutRate     :: Double -- 製品在庫欠品許容率
                       ,_materialOutRate  :: Double -- 原材料在庫欠品許容率
                       ,_addedDemand      :: Double -- 20期めの追加需要
                       ,_finalDemand      :: Double -- 各産業の最終需要
                       ,_inhouseRatio     :: Double -- 内製部門比率
                       ,_steadyProduction :: Double -- 定常的な生産
                       } deriving (Eq, Show)


instance InitVariables InitVar where

------------------------------------------------------------------
-- ** 簿記の状態空間の定義
------------------------------------------------------------------
-- ExBase Elementをインスタンス宣言する
-- wiledcardのみ指定すればOK

-- 取引主体ID
-- 一つが最終需要部門
type Entity = Int

instance Element Entity where
    wiledcard = -1
instance BaseClass Entity where

-- 最小
fstEnt  = 1
-- 最大 (最終需要部門)
lastEnt = 10

industries = [fstEnt .. lastEnt -1]
finalDemandSector = lastEnt

-- 期
type Term = Prelude.Int

-- ExBaseをインスタンス宣言する
-- 会計勘定科目の位置のみ指定すればOK

type VEHatBase = HatBase ( EJ.AccountTitles
                         , Entity
                         , Entity
                         , EJ.CountUnit)

instance ExBaseClass VEHatBase where
    getAccountTitle (h :< (a,c,e,u))   = a
    setAccountTitle (h :< (a,c,e,u)) b = h :< (b,c,e,u)


-- | Event の種類
-- 上から順番にイベントが実行される
data EventName
    = ToAmount                          -- ^ 価格から物量評価へ変換
    | SalesPurchase                     -- ^ 販売購入
    | Production                        -- ^ 保有する中間投入財を使用して生産
    | Order                             -- ^ 発注量の決定
    | Consumption                       -- ^ 最終需要部門の消費
    | ToPrice                           -- ^ 物量から価格評価へ変換
    | Plank                             -- ^ Plank
    deriving (Ord,Show, Enum, Eq, Bounded, Generic)

-- | ElementのインスタンスはHashableである必要がある
-- {-# LANGUAGE DeriveGeneric #-}
--  import GHC.Generics (Generic)
--  import Data.Hashable
-- を記載したうえで, deriving (Generic)
-- instance Hashable a where
-- で大抵は自動導出される

instance Hashable EventName where

instance Note EventName where
    plank = Plank

instance Event EventName where

instance Note Term where
    plank = -1

-- | 取引
type Transaction = EJ.Journal (EventName,Term) Double VEHatBase

termJournal :: Term -> Transaction -> Transaction
termJournal t = EJ.filterWithNote (\(e,t') _ -> t' == t )

-- | 元帳
newtype Ledger s = Ledger (STRef s Transaction)

instance UpdatableSTRef Ledger s Transaction where
   _unwrapURef (Ledger x) = x
   _wrapURef x = (Ledger x)

-- 取引の初期状態
-- 在庫だけ一定量保有
initTransaction :: Entity -> Double -> Transaction
initTransaction e d
    | e == finalDemandSector = Zero -- 最終需要部門
    | otherwise              = (.+) (d :@ Not :<(Products,e,e,Amount) .| (plank,initTerm))
                             $ EJ.fromList [ (d/10) :@ Not :<(Products,e2,e,Amount) .| (plank,initTerm)
                                           | e2 <- industries]

initLedger :: Double -> ST s (Ledger s)
initLedger d = newURef (EJ.fromList [ initTransaction c d
                                     | c <- [fstEnt..lastEnt]])

-- 一般化の適用
instance Updatable Term InitVar Ledger s where
    type Inner Ledger s = STRef s Transaction
    unwrap (Ledger a) = a

    initialize _ _ e = initLedger (_initInv e)

    updatePattern _  = return Modify

    -- 過去のTermを次の期のTermに変更して追加する
    modify g t e x  =  readURef x >>= \le
                    -> let added = f1 t (termJournal (t-1) le)
                    in modifyURef x (\z -> z .+ added)
        where
        f1 t x = EJ.gather (plank, t) $ f2 x
        f2   = EJT.finalStockTransfer
            . inventoryCount
            . (.-)

-- | 棚卸し仕訳
-- 物量で商品が売上と同額貸方にあるので,
-- 同額の売上に変換することで分記法から,三分割法の売上に変換される
-- ここでは,Cashが発生する機会が販売購入以外にないため,この実装で良いが
-- 他に同名の勘定科目が生じるイベントがある場合には,摘要情報を利用する必要がある.
inventoryCount ::  Transaction -> Transaction
inventoryCount tr = EJT.transfer tr
                  $ EJT.table
                  $ (toNot wiledcard) .~ Cash .-> (toNot wiledcard) .~ Sales     |% id
                  ++(toHat wiledcard) .~ Cash .-> (toNot wiledcard) .~ Purchases |% id

------------------------------------------------------------------
-- ** 価格の状態空間の定義
------------------------------------------------------------------

-- | 販売価格のテーブル
type Price = Double
type PriceTable = M.Map (Term,Entity) Price
newtype Prices s = Prices (STRef s PriceTable)

instance UpdatableSTRef Prices s PriceTable where
   _unwrapURef (Prices x) = x
   _wrapURef x = (Prices x)

-- 販売価格の初期状態
-- 今回は全て価格同一(1)

initPrice :: Term -> Entity -> Price
initPrice t _ = 1

-- 価格履歴の初期状態
initPrices :: ST s (Prices s)
initPrices  = newURef $ M.fromList [((t,c),initPrice t c)
                                    | t <- [initTerm .. lastTerm]
                                    , c <- [fstEnt..lastEnt]]

-- ** STArray s (ID, Term) Double
-- 価格は固定
instance Updatable Term InitVar Prices s where
    type Inner Prices s = STRef s PriceTable
    unwrap (Prices a) = a
    initialize _ _ _ = initPrices
    updatePattern _ = return DoNothing

type VETransTable = EJT.TransTable Double VEHatBase

-- | 価格テーブルから物量→価格評価への変換テーブルを作成
toCashTable :: PriceTable -> VETransTable
toCashTable pt = EJT.table
                $ concatMap (\((_, c), p) -> f c p) (M.toList pt)
    where
        {-# INLINE f #-}
        f :: Entity -> Double -> [(VEHatBase,VEHatBase, Double -> Double)]
        f c p =  (HatNot:<(Products,c,(.#),Amount)) .-> (HatNot:<(Products,c,(.#),Yen)) |% (*p)


-- | 価格テーブルから価格→物量評価への変換テーブルを作成
toAmountTable :: PriceTable -> VETransTable
toAmountTable pt = EJT.table
                $ concatMap (\((_, c), p) -> f c p) (M.toList pt)
    where
        {-# INLINE f #-}
        f :: Entity -> Double -> [(VEHatBase,VEHatBase, Double -> Double)]
        f c p =  (HatNot:<(Products,c,(.#),Yen)).-> (HatNot:<(Products,c,(.#),Amount)) |% (/p)

-- | 価格の取得
getPrice :: World s -> Term -> Entity -> ST s Price
getPrice wld t c =  readURef (_prices wld) >>= \pt
                 -> case M.lookup (t,c) pt of
                    Nothing -> return 0
                    Just x  -> return x

-- | 製造原価を計算する
getCost = undefined


------------------------------------------------------------------
-- ** 製品在庫
-- 安全在庫の計算
-- 安全在庫＝安全係数×使用量の標準偏差×√（発注リードタイム＋発注間隔）
-- 発注リードタイム + 発注間隔 = 1として
-- 安全在庫＝安全係数×使用量の標準偏差 で計算
-- 使用量(製品在庫) = 販売量
------------------------------------------------------------------
-- | 安全製品在庫
type SafetyStock = Double

-- | 欠品許容率(製品在庫,原材料在庫)
type OutRatio = Double

-- | 製品在庫における安全係数
-- 正規分布における欠品許容率のZ値
type SafetyStockFactor = Double

-- | エージェントごとの製品在庫の安全係数
-- 今回は全エージェント一定だがエージェントごとに変更可能
type SSFTable = M.Map Entity SafetyStockFactor

-- 欠品許容率から安全係数(z値)を計算するプログラム
-- 欠品許容率(例: 0.05は欠品率5%)を与えて安全係数を計算
calculateSafetyFactor :: OutRatio -> SafetyStockFactor
calculateSafetyFactor stockoutRate
  | stockoutRate <= 0 || stockoutRate >= 1 = error "The acceptable stock-out rate must be greater than 0 and less than 1."
  | otherwise = quantile standard (1 - stockoutRate)
  where
    standard = normalDistr 0 1

-- | 製品在庫安全係数の状態系
newtype SSFs s = SSFs (STRef s SSFTable)

instance UpdatableSTRef SSFs s SSFTable where
   _unwrapURef (SSFs x) = x
   _wrapURef x = (SSFs x)

-- 在庫比率の初期状態
-- 今回は全て同一比率
-- この比率次第でサプライチェーンがスタックする?

-- 価格履歴の初期状態
initSSFs :: InitVar -> ST s (SSFs s)
initSSFs v   = newURef
            $ M.fromList    [(e, calculateSafetyFactor (_stockOutRate v))
                            | e <- [fstEnt .. lastEnt]]

-- ** STArray s (ID, Term) Double
-- 価格は固定
instance Updatable Term InitVar SSFs s where
    type Inner SSFs s = STRef s SSFTable
    unwrap (SSFs a) = a
    initialize _ _ v = initSSFs v
    updatePattern _ = return DoNothing


------------------------------------------------------------------
-- ** 原材料在庫
------------------------------------------------------------------

-- | 安全原材料在庫
type SafetyMaterial = Double

-- | 原材料在庫における安全係数
-- 正規分布における欠品許容率のZ値
type SafetyMaterialFactor = Double

-- | 原材料在庫比率
-- 今回は全エージェント一定だがエージェントごとに変更可能
type SMFTable = M.Map Entity SafetyMaterialFactor

-- | 原材料在庫安全係数の状態系
newtype SMFs s = SMFs (STRef s SMFTable)

instance UpdatableSTRef SMFs s SMFTable where
   _unwrapURef (SMFs x) = x
   _wrapURef x = (SMFs x)

-- 原材料在庫比率の初期状態
initSMFs :: InitVar -> ST s (SMFs s)
initSMFs v = newURef
           $ M.fromList  [(e,calculateSafetyFactor (_materialOutRate v))
                         | e <- [fstEnt .. lastEnt]]

-- ** STArray s (ID, Term) Double
-- 価格は固定
instance Updatable Term InitVar SMFs s where
    type Inner SMFs s = STRef s SMFTable
    unwrap (SMFs a) = a
    initialize _ _ v = initSMFs v
    updatePattern _  = return DoNothing


------------------------------------------------------------------
-- ** 定常的な生産
------------------------------------------------------------------
type StedyProd = Double
newtype SP s = SP (STRef s StedyProd)

instance UpdatableSTRef SP s StedyProd where
   _unwrapURef (SP x) = x
   _wrapURef x = (SP x)

-- ** STArray s (ID, Term) Double
-- 価格は固定
instance Updatable Term InitVar SP s where
    type Inner SP s = STRef s StedyProd
    unwrap (SP a) = a
    initialize _ _ e = newURef (_steadyProduction e)
    updatePattern _ = return DoNothing

------------------------------------------------------------------
-- ** 生産関数の定義
------------------------------------------------------------------
-- 一単位生産する場合の簿記を定義しておいて,それに (.*) 演算すること
-- で生産量を示す
-- 財別に作る必要がある? (取り敢えず今回は1企業1財 (産業概念で行う))
-- 祖付加価値は一律2割とする(今回扱っているのは投入財の部分のみ= 合計0.8となる)

-- 生産関数(投入係数行列)
-- 物量表記
type InputCoefficient = Double

type Col = Entity
type Row = Entity
-- |  -- (Term, Commodity1, Commodity2) の配列
newtype ICTable s = ICTable (STArray s (Term, Row, Col) InputCoefficient)

instance UpdatableSTArray ICTable s (Term, Row, Col) InputCoefficient where
  _unwrapUArray (ICTable arr) = arr
  _wrapUArray arr = ICTable arr

-- ** 1つの Term に対する投入係数を生成 (乱数を使用, 列和 = 1)
initTermCoefficients :: StdGen -> Double -> M.Map Entity [InputCoefficient]
initTermCoefficients g inhouseRatio =
    let generateRow g =
            let (vals, g') = generateRandomList g lastEnt
                total = sum vals
                normalized = L.map (\x -> (x / total)*inhouseRatio) vals -- 祖付加価値分0.2差し引き
            in (normalized, g')
        (rows, _) = foldl (\(m, g0) c2 -> let (row, g1) = generateRow g0 in (M.insert c2 row m, g1)) (M.empty, g) [fstEnt..lastEnt]
    in rows

-- ** 乱数リストを生成 (0 ~ 1.0 の範囲)
-- 最終需要部門は0
generateRandomList :: StdGen -> Prelude.Int -> ([Double], StdGen)
generateRandomList g n = let (xs, g') = runState (replicateM (n-1) (state $ randomR (0, 1.0)))
                                                 (updateGen g 1000)
                       in let ys = L.map (\x -> if x < 0.1 then 0 else x) xs
                       in (ys ++ [0], g')

-- ** 生産関数の初期状態 (STArray を使用, Term ごとに固定)
initICTables :: StdGen -> Double -> ST s (ICTable s)
initICTables g inhouseRatio = do
    arr <- newUArray ((initTerm, fstEnt, fstEnt), (lastTerm, lastEnt, lastEnt)) 0  -- 初期値は0
    let termCoefficients = M.fromList [(t, initTermCoefficients g inhouseRatio) | t <- [initTerm .. lastTerm]]
    forM_ [(t, c2) | t <- [initTerm .. lastTerm], c2 <- [fstEnt .. lastEnt]] $ \(t, c2) -> do
        let row = termCoefficients M.! t M.! c2  -- Term ごとに固定
        forM_ (zip [fstEnt..lastEnt] row) $ \(c1, coef) ->
            writeUArray arr (t, c1, c2) coef
    return arr


-- | 生産関数の更新
-- 前の期の簿記から計算する
-- ただし,今回は価格固定なので変化なし
instance Updatable Term InitVar ICTable s where
    type Inner ICTable s = STArray s (Term, Row, Col) InputCoefficient
    unwrap (ICTable a) = a
    initialize g _ e = initICTables g (_inhouseRatio e)
    updatePattern _ = return DoNothing


-- | 投入係数の取得
-- 1単位の e1 の生産に必要な e2
getInputCoefficient :: World s -> Term -> Entity -> Entity -> ST s InputCoefficient
getInputCoefficient wld t e1 e2 =  do
                let ics = (_ics wld)
                readUArray ics (t,e2,e1)


------------------------------------------------------------------
-- ** 発注書の状態空間
------------------------------------------------------------------
-- 発注された中間投入財の量 = 生産が必要な量
-- 基本的には発注分を除けば前年度と同じ量を発注
-- 財ごとのInt
type OrderAmount = Double

data OrderRelation = Relation {_supplier :: Entity
                              ,_customer :: Entity}
                              deriving (Eq, Ord, Ix)

newtype OrderTable s = OrderTable (STArray s (Term,OrderRelation) OrderAmount)

instance UpdatableSTArray OrderTable s (Term,OrderRelation) OrderAmount where
  _unwrapUArray (OrderTable arr) = arr
  _wrapUArray arr = OrderTable arr


-- 発注量の初期状態
initOrders :: ICTable s -> Double -> Double -> ST s (OrderTable s)
initOrders icTable finalDemand inhouseRatio = do
    ((tMin, c1Min, c2Min), (tMax, c1Max, c2Max)) <- getUBounds icTable
    ordersArr <- newUArray ((tMin, Relation {_supplier = c1Min, _customer = c2Min})
                            ,(tMax,Relation {_supplier = c1Max, _customer = c2Max})) 0
    forM_ [(t, c1, c2) | t <- [tMin .. tMax]
                       , c1 <- [c1Min .. c1Max]
                       , c2 <- [c2Min .. c2Max]] $ \(t, c1, c2) -> do
        when (c2 == finalDemandSector) $ do
            c <- readUArray icTable (t, c1,c2)
            writeUArray ordersArr
                        (t, Relation {_supplier = c1, _customer = c2})
                        ((c / inhouseRatio) * finalDemand)
    return ordersArr

-- ** STArray s (ID, Term) NN.Double
instance Updatable Term InitVar OrderTable s where
    type Inner OrderTable s = STArray s (Term,OrderRelation) OrderAmount
    unwrap (OrderTable a) = a
    initialize g _ e = do
        icTable <- initICTables g (_inhouseRatio e)  -- ICTable を初期化
        initOrders icTable (_finalDemand e) (_inhouseRatio e)  -- それを基に OrdersTable を作成

    updatePattern _  = return Modify

    -- 前期の残りの分を追加
    modify g t e x  =  case (t == initTerm, t == addedTerm)of
                    (True, _)   -> return ()
                    (_, True)   -> forM_ [fstEnt..lastEnt] $ \e1
                                -> forM_ [fstEnt..lastEnt] $ \e2
                                -> case (e1 == 10, e2==9) of -- 9の最終需要の増加
                                    (True,True) -> readUArray x (t-1, Relation {_supplier = e2
                                                                               ,_customer = e1}) >>= \y
                                                -> modifyUArray x (t,Relation {_supplier = e2
                                                                              ,_customer = e1})
                                                                  (\x -> x + y + (_addedDemand e))
                                    ------------------------------------------------------------------
                                    _           -> readUArray x (t-1, Relation {_supplier = e2
                                                                              ,_customer = e1}) >>= \y
                                                -> modifyUArray x (t,Relation {_supplier = e2
                                                                              ,_customer = e1}) (\x -> x + y)
                    ------------------------------------------------------------------
                    _           -> forM_ [fstEnt..lastEnt] $ \e1
                                -> forM_ [fstEnt..lastEnt] $ \e2
                                -> readUArray x (t-1, Relation {_supplier = e2
                                                               ,_customer = e1}) >>= \y
                                -> modifyUArray x (t,Relation {_supplier = e2
                                                              ,_customer = e1})
                                                  (\x -> x + y)

-- | 個別の発注量の取得
getOrder :: World s -> Term -> OrderRelation -> ST s OrderAmount
getOrder wld t r =  let arr = (_orders wld)
                 in readUArray arr (t, r)

-- | 総受注量の取得
getOrderTotal :: World s -> Term -> Entity -> ST s OrderAmount
getOrderTotal wld t e1
    | t < 1 = return 0
    | otherwise = do
        let arr = (_orders wld)
        values <- CM.mapM (\e2 -> readUArray arr (t, Relation {_supplier = e1
                                                              ,_customer = e2}))
                          [fstEnt .. lastEnt]
        return $ sum values

-- | 発注
order :: World s -> Term -> OrderRelation -> OrderAmount -> ST s ()
order wld t r x
    | t > lastTerm = return ()
    | otherwise    = modifyUArray (_orders wld) (t, r) (\y -> y + x)

------------------------------------------------------------------
-- * World
------------------------------------------------------------------
-- | 状態空間の定義
data World s = World { _ledger  :: Ledger s -- ^ 元帳
                     , _prices  :: Prices s -- ^ 価格テーブル
                     , _ics     :: ICTable s  -- ^ 投入係数
                     , _orders  :: OrderTable s -- ^ 発注量
                     , _smf     :: SMFs s  -- ^ 原材料在庫の安全係数
                     , _ssf     :: SSFs s -- ^ 製品在庫の安全係数
                     , _sp      :: SP s -- ^ 定常的な生産量
                     }
                     deriving (Generic)

-- deriving Generic をしていれば
-- 空のインスタンス宣言で自動でinitSS,updateSSが使えるようになる
instance StateSpace Term InitVar EventName World s where
    event = event'
------------------------------------------------------------------
-- * 汎用関数
------------------------------------------------------------------
{-# INLINE termAmount #-}
-- | 価格による物量評価
-- 最終期以外は記録がAmountでなされている
-- 価格が固定なので,毎回作る必要はなくボトルネックとなっている
-- 今後の拡張性のために現行の形式を採用
termAmount t le pt =  let !termedle = termJournal t le
                   in let !amountTable = toAmountTable pt
                   in EJT.transfer termedle amountTable

{-# INLINE mean #-}
-- | 平均を計算
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

{-# INLINE stdDev #-}
-- | 標本標準偏差を計算
stdDev :: [Double] -> Double
stdDev xs
  | length xs < 2 = error "You need at least two pieces of data."
  | otherwise     = sqrt variance
  where
    avg = mean xs
    variance = sum [(x - avg)^2 | x <- xs] / fromIntegral (length xs)


-- | 適正在庫
-- 安全在庫 + 10期の需要の平均
appropriateStock :: World s -> Term -> Entity -> ST s Double
appropriateStock wld t e
    =  getSafetyStock wld t e >>= \ss
    -> mean <$> CM.mapM (\ t' -> getTermDemand wld t' e) [max 1 (t-10) .. t] >>= \ avo
    -> return $ avo + ss


-- | 安全製品在庫の計算
-- 安全在庫＝安全係数×使用量の標準偏差×√（発注リードタイム＋発注間隔）
-- 発注リードタイム + 発注間隔 = 1として
-- 安全在庫＝安全係数×需要量の標準偏差 で計算
-- 使用量 = 販売量
-- 今期を含めた10期分の販売量の標準偏差を利用
getSafetyStock :: World s -> Term -> Entity -> ST s SafetyStock
getSafetyStock wld t e = readURef (_ssf wld) >>= \ssf -- 安全係数
                       -> demandStdDev wld t e >>= \ stdDev -- 標準偏差
                       -> case M.lookup e ssf of
                                    Nothing -> error "ssf is empty"
                                    Just x  -> return (x * stdDev)
    where
    -- | 今期を含めた5期分の販売量の標準偏差
    {-# INLINE demandStdDev #-}
    demandStdDev :: World s -> Term -> Entity -> ST s Double
    demandStdDev wld t e
        | t < 2     = return 1 -- 1期分しかない場合は標準正規分布を仮定
        | otherwise = stdDev
                    <$> CM.mapM (\ t' -> getTermDemand wld t' e)
                                [max 1 (t-10) .. t]


-- | 適正原材料在庫
-- 安全在庫 + 10期の使用量の平均
appropriateMaterial :: World s -> Term -> Entity -> Entity -> ST s Double
appropriateMaterial wld t e1 e2 = do
    sm <- getSafetyMaterial wld t e1 e2
    c <- getInputCoefficient wld t e1 e2
    tm <- mean <$> (CM.forM [max 1 (t-10) .. t] $ \ t' -> (*) c <$> getTermDemand wld t' e1)
    return $ sm + tm

-- | 安全原材料在庫の計算
-- 安全在庫＝安全係数×使用量の標準偏差×√（発注リードタイム＋発注間隔）
-- 発注リードタイム + 発注間隔 = 1として
-- 安全在庫＝安全係数×使用量の標準偏差 で計算
-- 今期を含めた5期分の使用量の標準偏差を利用
getSafetyMaterial :: World s -> Term -> Entity -> Entity -> ST s SafetyMaterial
getSafetyMaterial wld t e1 e2
    =  readURef (_smf wld) >>= \ smf
    -> inputStdDev wld t e1 e2 >>= \ stdDev -- 標準偏差
    -> case M.lookup e1 smf of
        Nothing -> error "ssf is empty"
        Just x  -> return (x * stdDev)
    where
    -- | 今期を含めた10期分のe1によるe2の中間投入量の標準偏差
    inputStdDev :: World s -> Term -> Entity -> Entity -> ST s Double
    inputStdDev wld t e1 e2
        | t < 2     = return 1 -- 1期分しかない場合は標準正規分布を仮定
        | otherwise =  getInputCoefficient wld t e1 e2 >>= \c
                    -> stdDev <$> (CM.forM [max 1 (t-10) .. t] $ \ t'
                    -> (*) c <$> getTermDemand wld t' e1)

-- | 一期の使用量を取得する
getTermInput :: World s -> Term -> Entity -> Entity -> ST s Double
getTermInput wld t e1 e2 = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let !result = norm
                $ EJ.projWithNoteBase [(Production,t)] [Hat:<(Products, e2,e1,Amount)]
                $ termAmount t le pt
    return result


-- | 一期の製品在庫保有量を取得する
getTermStock :: World s -> Term -> Entity -> ST s Double
getTermStock wld t e = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let result = norm
                $ EJ.projWithBase [Not:<(Products,e,e,Amount)]
                $ (.-)
                $ termAmount t le pt
    return result

-- | 一期の原材料在庫保有量を取得する
getTermMaterial :: World s -> Term -> Entity -> Entity -> ST s Double
getTermMaterial wld t e1 e2 = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let !result = norm
                $ projWithBase [Not :<(Products, e2, e1, Amount)]
                $ (.-)
                $ termAmount t le pt
    return result

-- | 一期の算出を取得する
getTermProduction :: World s -> Term -> Entity -> ST s Double
getTermProduction wld t e = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let !result = norm
               $ EJ.projWithNoteBase [(Production,t)] [Not:<(Products, e,e,Amount)]
               $ termAmount t le pt
    return result

-- | 一期の利益を取得する
getTermProfit :: World s -> Term -> Entity -> ST s Double
getTermProfit wld t e = do
    le <- readURef (_ledger wld)
    let !termTr = termJournal t le
        !tr    = EJT.grossProfitTransfer termTr
        !plus  = norm $ EJ.projWithBase [Not:<(Cash,(.#),e,Yen)] tr
        !minus = norm $ EJ.projWithBase [Hat:<(Cash,(.#),e,Yen)] tr
    return (plus - minus)

-- | 一期の販売量を取得する
getTermSales :: World s -> Term -> Entity -> ST s Double
getTermSales wld t e = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let !result = norm
                $ EJ.projWithNoteBase [(SalesPurchase,t)] [Hat:<(Products, e,e,Amount)]
                $ termAmount t le pt
    return result


-- | 一期の総需要を取得する
-- 発注の残りと販売量の和
getTermDemand :: World s -> Term -> Entity -> ST s Double
getTermDemand wld t e = do
    sales <- getTermSales wld t e
    order <- getOrderTotal wld t e
    return (sales + order)

-- | 一単位の財の簿記を取得する
getOneProduction :: World s -> Term -> Entity -> ST s Transaction
getOneProduction wld t c = do
    let arr =  (_ics wld)  -- ICTable を取得
    inputs <- CM.forM industries $ \c2 -> do
        coef <- readUArray arr (t, c2, c)  -- c を生産するために必要な c2 の投入係数
        return $ coef :@ Hat :<(Products, c2, c, Amount) .| (Production,t) -- c2 の消費を記録
    let !totalInput = foldl (.+) Zero inputs  -- すべての中間投入を結合
    return $! (1 :@ Not :<(Products, c, c, Amount) .| (Production,t)) .+ totalInput   -- 生産と投入の合計


-- | 生産可能量の計算
-- 現在の原材料在庫に基づいて生産可能量を計算する
-- 在庫量 / 投入係数の最小値が生産可能量
getPossibleVolume :: World s -> Term -> Entity -> ST s Double
getPossibleVolume wld t e1 = do
    le  <- readURef (_ledger wld)
    let ics = (_ics wld)
        termedle = termJournal t le

    -- 各 e2 (原材料) に対して、生産可能量の計算
    mbps <- CM.forM industries $ \e2 -> do
        -- 原材料在庫
        let n = norm
              $ projWithBase [Not :< (Products, e2, e1, Amount)]
              $ (.-) termedle

        -- 投入係数
        c <- readUArray ics (t, e2, e1)
        -- 0除算防止
        case (c == 0) of
            False -> return (Just (n / c))
            True  -> return Nothing

    let possibleVolumes = L.map (\(Just x) -> x)
                        $ L.filter (\x -> if x == Nothing then False else True) mbps
    return $ minimum possibleVolumes  -- 可能な生産量の最小値を返す

-- 記帳
journal :: World s ->  Transaction -> ST s ()
journal wld Zero = return ()
journal wld js   = modifyURef (_ledger wld) (\x -> x .+ js)

------------------------------------------------------------------
-- * 状態の更新
------------------------------------------------------------------
-- ** イベントの設定
-- 同じ取引を繰り返すだけ


event' :: World s -> Term -> EventName -> ST s ()

-- 通常簿記を物量簿記に変換する
-- 初期から物量なので今回は何もしない
event' wld t ToAmount = return ()

-- 物量簿記を通常簿記に変換する
-- 最終期のみ実施
event' wld t ToPrice
    | t == lastTerm =  readURef (_prices wld) >>= \pt
                    -> modifyURef (_ledger wld) $ \le
                    -> EJT.transfer le (toCashTable pt)
    | otherwise = return ()

------------------------------------------------------------------
-- 受注分販売・購入する
-- 在庫を受注の割合で販売する
-- 売れた分受注を減らす
event' wld t SalesPurchase = do
    le <- readURef (_ledger wld)  -- 簿記の状態を取得
    let os = _orders wld  -- OrdersTable を取得

    forM_ industries $ \e1 -> do
        -- 初期で総需要が在庫を上回った場合の対策
        when (t == initTerm) $ do
            finalDemand <- getOrder wld t Relation {_supplier = e1, _customer = finalDemandSector}
            c <- getInputCoefficient wld t e1 e1
            order wld t Relation {_supplier = e1, _customer = e1} (c * finalDemand)

        -- 受注総量を取得
        totalOrder <- getOrderTotal wld t e1


        -- 在庫保有量
        stock <- getTermStock wld t e1

        -- 各 e2 (注文元) に対して処理
        forM_ [fstEnt..lastEnt] $ \e2 -> do
            -- e2 から e1 への受注量
            orderAmount <- readUArray os (t, Relation {_supplier = e1
                                                      ,_customer = e2})

            -- 受注がゼロでない場合に販売処理を実施
            when (orderAmount > 0) $ do
                -- 在庫に応じた販売量
                let sellAmount = min orderAmount (stock  * (orderAmount / totalOrder))
                when (sellAmount > 0 ) $ do
                    p <- getPrice wld t e1
                    let toAdd =  sellAmount      :@ Hat :<(Products, e1, e1, Amount) -- 受注側 販売財
                             .+ (sellAmount * p) :@ Not :<(Cash,(.#),e1,Yen)          -- 受注側 販売益
                             .+  sellAmount      :@ Not :<(Products, e1, e2, Amount) -- 発注側 購入財
                             .+ (sellAmount * p) :@ Hat :<(Cash,(.#),e2,Yen)          -- 発注側 購入額
                    journal wld (toAdd .| (SalesPurchase,t))
                    -- 受注を減らす
                    writeUArray os (t, Relation {_supplier = e1
                                                ,_customer = e2})
                                   (orderAmount - sellAmount)


------------------------------------------------------------------
-- | 不足分及び,在庫不足分に対して現在保有している原材料在庫で生産する
-- 安全在庫に補充点方式で補充する
event' wld t Production = do
    le <- readURef (_ledger wld)
    -- 定常的な生産量
    sp <- readURef (_sp wld)
    forM_ industries $ \e1 -> do
        -- 安全在庫
        as <- appropriateStock wld t e1
        -- 販売後のストックの不足分
        short <- getOrderTotal wld t e1
        -- 在庫保有量
        stock <- getTermStock wld t e1
        -- 不足分と安全在庫確保分生産する
        let plan = case compare (as + short) stock of
                        -- 在庫が少ない場合 定常的な生産量以上生産する
                        GT -> case compare ((as + short) - stock) sp of
                                GT -> (as + short) - stock
                                _  -> sp
                        -- 等しいか多い場合 生産する必要なし
                        -- ただし最低限の生産を行う
                        _ -> sp

        pv <- getPossibleVolume wld t e1  -- 生産可能量を取得
        -- 生産が必要な量の内 生産できる量
        let prod = min plan pv

        {-
        when (e1 == 4) $ do
            trace ("--------------------------") return ()
            trace (show t ++ "-as: " ++ show as) return ()
            trace (show t ++ "-pv: " ++ show pv) return ()
            trace (show t ++ "-pl: " ++ show plan) return ()
            trace (show t ++ "-pd: " ++ show prod) return ()
        -}
        when (prod > 0 ) $ do
            op <- getOneProduction wld t e1  -- 1単位の生産簿記を取得
            journal wld (prod .* op)  -- 生産処理を記帳


------------------------------------------------------------------
-- 発注量を計算
-- 安全在庫に補充点方式で補充する
-- 安全在庫に対する不足分を補充

event' wld t Order = do
    le <- readURef (_ledger wld)
    -- 定常的な生産量
    sp <- readURef (_sp wld)

    forM_ industries $ \e1 -> do
        -- 安全在庫
        as <- appropriateStock wld t e1
        -- 販売後のストックの不足分
        short <- getOrderTotal wld t e1
        -- 在庫保有量
        stock <- getTermStock wld t e1
        -- 今期の生産量
        prod <- getTermProduction wld t e1

        -- 生産しても足りなかった分
        let plan = case compare (as + short) stock of
                        -- 在庫が少ない場合 定常的な生産量以上生産する
                        GT -> case compare ((as + short) - stock) sp of
                                GT -> (as + short) - stock
                                _  -> case compare sp prod of
                                        GT -> sp - prod
                                        _  -> 0
                        -- 等しいか多い場合 生産する必要なし
                        -- ただし最低限の生産を行う
                        _ -> case compare sp prod of
                                GT -> sp - prod
                                _  -> 0

        -- e2に対する発注量を計算する
        forM_ industries $ \e2 -> do
                -- 現状の原材料在庫保有量
                ms <- getTermMaterial wld t e1 e2
                -- 投入係数
                c <- getInputCoefficient wld t e1 e2
                -- 安全原材料在庫
                am <- appropriateMaterial wld t e1 e2

                -- 不足分の生産に必要な原材料在庫
                let short_material = plan * c
                -- 不足分と安全在庫の確保のために必要な原材料在庫
                -- 自家消費分はストックと無関係に確保
                let total
                        | e1 == e2  = short_material + am
                        | otherwise = case compare (short_material + am) ms of
                                        GT -> (short_material + am) - ms
                                        _  -> 0

                {-
                when (e1 == 4) $ do
                    trace (show t ++ "-ms(" ++ show e2 ++ "): " ++ show ms) return ()
                    -- trace (show t ++ "-am(" ++ show e2 ++ "): " ++ show am) return ()
                    trace (show t ++ "-tl(" ++ show e2 ++ "): " ++ show total) return ()
                -}
                -- 次の期に発注する
                order wld (t+1) Relation {_supplier = e2, _customer = e1} total

------------------------------------------------------------------
-- 最終需要部門が購入したものを消費する
event' wld t Consumption = do
    -- 保有している消費財
    le <- readURef (_ledger wld)
    let total_consume = projWithNoteBase [(SalesPurchase,t)]
                                         [Not:<(Products,(.#),finalDemandSector,(.#))]
                                         le
    journal wld $ gather (Consumption,t)
                $ (.^) total_consume
------------------------------------------------------------------
event' wld t Plank = return ()

------------------------------------------------------------------
-- * 実行
------------------------------------------------------------------
main :: IO ()
main = do
    let seed = 42
    let gen = mkStdGen seed
        defaultEnv = InitVar {_initInv             = 100
                             ,_stockOutRate        = 0.1
                             ,_materialOutRate     = 0.1
                             ,_addedDemand         = 0
                             ,_finalDemand         = 300
                             ,_inhouseRatio        = 0.4
                             ,_steadyProduction    = 0}

        smallStockEnv   = defaultEnv {_stockOutRate = 0.1}
        largeStockEnv   = defaultEnv {_stockOutRate = 0.05}
        defaultAddedEnv = defaultEnv    {_addedDemand = 20}
        smallAddedEnv   = smallStockEnv {_addedDemand = 20}
        largeAddedEnv   = largeStockEnv {_addedDemand = 20}

{- For Test
        envs =  [defaultEnv]

        envNames = ["default"]
-}

        envs =  [defaultEnv
                ,smallStockEnv
                ,largeStockEnv
                ,defaultAddedEnv
                ,smallAddedEnv
                ,largeAddedEnv]

        envNames = ["default"
                   ,"smallstock"
                   ,"largestock"
                   ,"default-added"
                   ,"smallstock-added"
                   ,"largestock-added"]
    ------------------------------------------------------------------
    print "start simulation"
    results <- mapConcurrently (runSimulation gen) envs


    let resMap = Map.fromList
               $ zip envNames results

    ------------------------------------------------------------------
    print "printing..."
    forConcurrently_ envNames $ \n -> do
        let fs = [getTermProduction
                 ,getTermStock
                 ,getTermProfit
                 ,getTermSales
                 ,getTermDemand]
            fnames = ["Production"
                     ,"Stock"
                     ,"Profit"
                     ,"Sales"
                     ,"Demand"]

        forConcurrently_ (zip fs fnames) $ \ (f, fn) -> do
            ESV.plotLineVector f ((fstEnt,initTerm),(lastEnt -1,lastTerm))
                               (resMap Map.! n) (fig_dir ++ n ++ "/") fn

        if n == "default-added"
            then do
                ESV.plotWldsDiffLine getTermProduction
                         (fstEnt,lastEnt -1)
                         ((resMap Map.! n),(resMap Map.! "default"))
                         (fig_dir ++ n ++ "/")
                         "Difference in production volume"

                ESV.plotMultiLines ["added","normal"]
                       [getTermProduction,getTermProduction]
                       (fstEnt,lastEnt -1)
                       [(resMap Map.! n),(resMap Map.! "default")]
                       (fig_dir ++ n ++ "/")
                       "Comparison of production volume"
        else if n == "largestock-added"
            then do
                ESV.plotWldsDiffLine getTermProduction
                         (fstEnt,lastEnt -1)
                         ((resMap Map.! n),(resMap Map.! "largestock"))
                         (fig_dir ++ n ++ "/")
                         "Difference in production volume"

                ESV.plotMultiLines ["added","normal"]
                       [getTermProduction,getTermProduction]
                       (fstEnt,lastEnt -1)
                       [(resMap Map.! n),(resMap Map.! "largestock")]
                       (fig_dir ++ n ++ "/")
                       "Comparison of production volume"
        else if n == "smallstock-added"
            then do
                ESV.plotWldsDiffLine getTermProduction
                         (fstEnt,lastEnt -1)
                         ((resMap Map.! n),(resMap Map.! "smallstock"))
                         (fig_dir ++ n ++ "/")
                         "Difference in production volume"

                ESV.plotMultiLines ["added","normal"]
                       [getTermProduction,getTermProduction]
                       (fstEnt,lastEnt -1)
                       [(resMap Map.! n),(resMap Map.! "smallstock")]
                       (fig_dir ++ n ++ "/")
                       "Comparison of production volume"
        else return ()