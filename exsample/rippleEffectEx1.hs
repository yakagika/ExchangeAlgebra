
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

{-
波及効果のシュミレーションサンプル
50エージェントから構成されるネットワーク内で波及効果を計測する
最終需要の発生→生産計画→発注を各ステージで行う.
全エージェントが同時に行動を行う.
発注書 → 生産計画 → 生産可能な分を生産 → 足りない分を発注 → Loop
(在庫確保分少し余剰に生産する?)
生産可能分だけだと,在庫がなくなったら終わり.
在庫比率を一定にする.
最終需要は発注に一部追加される.
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
import System.Random -- 乱数
import Data.Coerce
import Control.Concurrent.Async (mapConcurrently,forConcurrently_)

-- Debug
import Debug.Trace

------------------------------------------------------------------
-- *  状態系の導入
------------------------------------------------------------------

instance StateTime Term where
    initTerm = 1
    lastTerm = 50
    nextTerm = \x -> x + 1
    prevTerm = \x -> x - 1

{-
------------------------------------------------------------------
-- ** 乱数生成器の状態空間の定義(使っていない)
------------------------------------------------------------------

seed = 42

type Gen s = STRef s StdGen

-- ** STArray s (ID, Term) NN.Double
-- 価格は固定
instance Updatable Term InitVar ST s Gen where
    initialize g t e = newURef (mkStdGen seed)
    updatePattern _ _ _ = return DoNothing
-}
------------------------------------------------------------------
-- ** 初期化用のパラメータ定義
------------------------------------------------------------------

data InitVar = InitVar {_initInv          :: Double -- 初期在庫保有量
                       ,_initIR           :: Double -- 在庫比率
                       ,_initMS           :: Double -- 原材料在庫比率
                       ,_addedDemend      :: Double -- 20期めの追加需要(波及効果)
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
    deriving (Ord,Show, Enum, Eq, Bounded)

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
        f2   = EJT.finalStockTransferKeepWiledcard
            . inventoryCount
            . (.-)

-- | 棚卸し仕訳
-- 物量で商品が売上と同額貸方にあるので,
-- 同額の売上に変換することで分記法から,三分割法の売上に変換される
-- ここでは,Cashが発生する機会が販売購入以外にないため,この実装で良いが
-- 他に同名の勘定科目が生じるイベントがある場合には,摘要情報を利用する必要がある.
inventoryCount ::  Transaction -> Transaction
inventoryCount tr = EJT.transferKeepWiledcard tr
                  $ EJT.table
                  $ (toNot wiledcard) .~ Cash .-> (toNot wiledcard) .~ Sales     |% id
                  ++(toHat wiledcard) .~ Cash .-> (toNot wiledcard) .~ Purchases |% id

-- | t期の在庫保有量を計算する
culcStock :: Term -> Entity -> Transaction -> Double
culcStock t e le = norm
                 $ projWithBase [Not :<(Products, e, e, Amount)]
                 $ (.-)
                 $ termJournal t le

-- |
getTermStock :: World s -> Term -> Entity -> ST s Double
getTermStock wld t e = do
    le <- readURef (_ledger wld)
    let currentTotal = norm
                     $ EJ.projWithBase [Not:<(Products,e,e,Yen)]
                     $ (.-)
                     $ (termJournal t le)
    return currentTotal
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
                $ L.foldl (++) [] [f c p | ((t,c),p) <- M.toList pt]
    where
        f :: Entity -> Double
          -> [(VEHatBase,VEHatBase,(Double -> Double))]
        f c p =  (Hat:<(Products,c,(.#),Amount)) .-> (Hat:<(Products,c,(.#),Yen)) |% (*p)
              ++ (Not:<(Products,c,(.#),Amount)) .-> (Not:<(Products,c,(.#),Yen)) |% (*p)



-- | 価格テーブルから価格→物量評価への変換テーブルを作成
toAmountTable :: PriceTable -> VETransTable
toAmountTable pt = EJT.table
                $ L.foldl (++) [] [f c p | ((t,c),p) <- M.toList pt]
    where
        f ::  Entity -> Double
          -> [(VEHatBase,VEHatBase,(Double -> Double))]
        f c p =  (Hat:<(Products,c,(.#),Yen)).-> (Hat:<(Products,c,(.#),Amount)) |% (/p)
              ++ (Not:<(Products,c,(.#),Yen)).-> (Not:<(Products,c,(.#),Amount)) |% (/p)

-- | 価格の取得
getPrice :: World s -> Term -> Entity -> ST s Price
getPrice wld t c =  readURef (_prices wld) >>= \pt
                 -> case M.lookup (t,c) pt of
                    Nothing -> return 0
                    Just x  -> return x

-- | 製造原価を計算する
getCost = undefined


------------------------------------------------------------------
-- ** 在庫比率
------------------------------------------------------------------
-- | 在庫比率のテーブル
-- 出荷量に対して一定量を在庫に回す
-- 生産計画は受注量 × 在庫比率 となる
type InventoryRatio = Double
type IRTable = M.Map Entity InventoryRatio
newtype IRs s = IRs (STRef s IRTable)

instance UpdatableSTRef IRs s IRTable where
   _unwrapURef (IRs x) = x
   _wrapURef x = (IRs x)

-- 在庫比率の初期状態
-- 今回は全て同一比率
-- この比率次第でサプライチェーンがスタックする?

initInventoryRatio :: Entity -> Double -> InventoryRatio
initInventoryRatio _ d = d

-- 価格履歴の初期状態
initIRs :: Double -> ST s (IRs s)
initIRs d   = newURef
            $ M.fromList    [(e,initInventoryRatio e d)
                            | e <- [fstEnt .. lastEnt]]

-- ** STArray s (ID, Term) Double
-- 価格は固定
instance Updatable Term InitVar IRs s where
    type Inner IRs s = STRef s IRTable
    unwrap (IRs a) = a
    initialize _ _ e = initIRs (_initIR e)
    updatePattern _ = return DoNothing

-- | 在庫比率の取得
getInventoryRatio :: World s -> Entity -> ST s InventoryRatio
getInventoryRatio wld e     =  readURef (_irs wld) >>= \ir
                            -> case M.lookup e ir of
                                    Nothing -> return 0
                                    Just x  -> return x

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
-- ** 原材料在庫比率
------------------------------------------------------------------
-- | 原材料在庫比率
type MaterialStock = Double
type MSTable = M.Map Entity MaterialStock
newtype MSs s = MSs (STRef s MSTable)

instance UpdatableSTRef MSs s MSTable where
   _unwrapURef (MSs x) = x
   _wrapURef x = (MSs x)

-- 原材料在庫比率の初期状態
initMSs :: Double -> ST s (MSs s)
initMSs d = newURef
          $ M.fromList  [(e,d)
                        | e <- [fstEnt .. lastEnt]]

-- ** STArray s (ID, Term) Double
-- 価格は固定
instance Updatable Term InitVar MSs s where
    type Inner MSs s = STRef s MSTable
    unwrap (MSs a) = a
    initialize _ _ e = initMSs (_initMS e)
    updatePattern _  = return DoNothing

-- | 在庫比率の取得
getMaterialStock :: World s -> Entity -> ST s MaterialStock
getMaterialStock wld e     =  readURef (_mss wld) >>= \ms
                            -> case M.lookup e ms of
                                    Nothing -> return 0
                                    Just x  -> return x

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
getInputCoefficient :: World s -> Term -> (Entity, Entity) -> ST s InputCoefficient
getInputCoefficient wld t (c1,c2) =  do
                let ics = (_ics wld)
                readUArray ics (t,c1,c2)


-- | 一単位の財の簿記を取得する
getOneProduction :: World s -> Term -> Entity -> ST s Transaction
getOneProduction wld t c = do
    let arr =  (_ics wld)  -- ICTable を取得
    inputs <- CM.forM industries $ \c2 -> do
        coef <- readUArray arr (t, c2, c)  -- c を生産するために必要な c2 の投入係数
        return $ coef :@ Hat :<(Products, c2, c, Amount) .| (Production,t) -- c2 の消費を記録
    let totalInput = foldl (.+) Zero inputs  -- すべての中間投入を結合
    return $ (1 :@ Not :<(Products, c, c, Amount) .| (Production,t)) .+ totalInput   -- 生産と投入の合計

-- | 一期の算出を取得する
getTermProduction :: World s -> Term -> Entity -> ST s Double
getTermProduction wld t e = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let currentTotal = norm
                     $ EJ.projWithNoteBase [(Production,t)]
                                           [Not:<(Products, e,e,Yen)]
                                           le
    return currentTotal

-- | 一期の利益を取得する
getTermProfit :: World s -> Term -> Entity -> ST s Double
getTermProfit wld t e = do
    le <- readURef (_ledger wld)
    let tr    = EJT.grossProfitTransferKeepWiledcard le
    let plus  = norm $ EJ.projWithBase [Not:<(Cash,(.#),e,Yen)]
                     $ termJournal t tr
    let minus = norm $ EJ.projWithBase [Hat:<(Cash,(.#),e,Yen)]
                     $ termJournal t tr
    return (plus - minus)

------------------------------------------------------------------
-- ** 発注書の状態空間
------------------------------------------------------------------
-- 発注された中間投入財の量 = 生産が必要な量
-- 基本的には発注分を除けば前年度と同じ量を発注
-- 財ごとのInt
type Order = Double
newtype OrderTable s = OrderTable (STArray s (Term,Entity,Entity) Order)

instance UpdatableSTArray OrderTable s (Term,Entity,Entity) Order where
  _unwrapUArray (OrderTable arr) = arr
  _wrapUArray arr = OrderTable arr

-- 発注量の初期状態
initOrders :: ICTable s -> Double -> Double -> ST s (OrderTable s)
initOrders icTable finalDemand inhouseRatio = do
    ((tMin, c1Min, c2Min), (tMax, c1Max, c2Max)) <- getUBounds icTable
    ordersArr <- newUArray ((tMin, c1Min, c2Min), (tMax, c1Max, c2Max)) 0
    forM_ [(t, c1, c2) | t <- [tMin .. tMax]
                       , c1 <- [c1Min .. c1Max]
                       , c2 <- [c2Min .. c2Max]] $ \(t, c1, c2) -> do
        when (c2 == finalDemandSector) $ do
            c <- readUArray icTable (t, c1,c2)
            writeUArray ordersArr (t, c1, c2) ((c / inhouseRatio) * finalDemand)
    return ordersArr

-- ** STArray s (ID, Term) NN.Double
instance Updatable Term InitVar OrderTable s where
    type Inner OrderTable s = STArray s (Term,Entity,Entity) Order
    unwrap (OrderTable a) = a
    initialize g _ e = do
        icTable <- initICTables g (_inhouseRatio e)  -- ICTable を初期化
        initOrders icTable (_finalDemand e) (_inhouseRatio e)  -- それを基に OrdersTable を作成

    updatePattern _  = return Modify

    -- 前期の残りの分を追加
    modify g t e x  =  case (t == initTerm, t == 20)of
                    (True, _)   -> return ()
                    (_, True)   -> forM_ [fstEnt..lastEnt] $ \e1
                                -> forM_ [fstEnt..lastEnt] $ \e2
                                -> case (e1 == 10, e2==9) of -- 9の最終需要の増加
                                    (True,True) -> readUArray x (t-1, e2,e1) >>= \y
                                                -> modifyUArray x (t,e2,e1) (\x -> x + y + (_addedDemend e))
                                    ------------------------------------------------------------------
                                    _           -> readUArray x (t-1, e2,e1) >>= \y
                                                -> modifyUArray x (t,e2,e1) (\x -> x + y)
                    ------------------------------------------------------------------
                    _           -> forM_ [fstEnt..lastEnt] $ \e1
                                -> forM_ [fstEnt..lastEnt] $ \e2
                                -> readUArray x (t-1, e2,e1) >>= \y
                                -> modifyUArray x (t,e2,e1) (\x -> x + y)

-- | 個別の発注量の取得
getOrder :: World s -> Term -> Entity -> Entity -> ST s Order
getOrder wld t e1 e2 =  let arr = (_orders wld)
                     in readUArray arr (t,e1,e2)

-- | 総受注量の取得
getOrderTotal :: World s -> Term -> Entity -> ST s Order
getOrderTotal wld t e1 = case t < 1 of
    True -> return 0
    False -> do
        let arr = (_orders wld)
        values <- CM.mapM (\e2 -> readUArray arr (t, e1, e2)) [fstEnt .. lastEnt]
        return $ sum values

------------------------------------------------------------------
-- * World
------------------------------------------------------------------
-- | 状態空間の定義
data World s = World { _ledger  :: Ledger s
                     , _prices  :: Prices s
                     , _ics     :: ICTable s
                     , _orders  :: OrderTable s
                     , _mss     :: MSs s
                     , _irs     :: IRs s
                     , _sp      :: SP s}
                     deriving (Generic)

-- deriving Generic をしていれば
-- 空のインスタンス宣言で自動でinitSS,updateSSが使えるようになる
instance StateSpace Term InitVar EventName World s where
    event = event'
------------------------------------------------------------------
-- * 汎用関数
------------------------------------------------------------------

-- | 生産可能量の計算
-- 現在の原材料在庫に基づいて生産可能量を計算する
-- 在庫量 / 投入係数の最小値が生産可能量
getPossibleVolume :: World s -> Term -> Entity -> ST s Double
getPossibleVolume wld t e1 = do
    let ics = (_ics wld)
    le  <- readURef (_ledger wld)

    -- 各 e2 (原材料) に対して、生産可能量の計算
    mbps <- CM.forM industries $ \e2 -> do
        -- 原材料在庫
        let n = norm
              $ projWithBase [Not :< (Products, e2, e1, Amount)]
              $ (.-)
              $ termJournal t le

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
journal :: World s -> Term -> Transaction -> ST s ()
journal wld t Zero = return ()
journal wld t js   = modifyURef (_ledger wld) (\x -> x .+ js)

------------------------------------------------------------------
-- * 状態の更新
------------------------------------------------------------------
-- ** イベントの設定
-- 同じ取引を繰り返すだけ


event' :: World s -> Term -> EventName -> ST s ()

--  通常簿記を物量簿記に変換する
event' wld t ToAmount = readURef (_prices wld) >>= \pt
              -> modifyURef (_ledger wld) $ \le
              -> EJT.transferKeepWiledcard le (toAmountTable pt)

--  物量簿記を通常簿記に変換する
event' wld t ToPrice = readURef (_prices wld) >>= \pt
              -> modifyURef (_ledger wld) $ \bk
              -> EJT.transferKeepWiledcard bk (toCashTable pt)


------------------------------------------------------------------
-- 受注分販売・購入する
-- 在庫を受注の割合で販売する
-- 売れた分受注を減らす
event' wld t SalesPurchase = do
    le <- readURef (_ledger wld)  -- 簿記の状態を取得
    let os = _orders wld  -- OrdersTable を取得

    forM_ industries $ \e1 -> do
        -- 受注総量を取得
        totalOrder <- getOrderTotal wld t e1
        -- 在庫保有量
        let stock = culcStock t e1 le
        -- 各 e2 (注文元) に対して処理
        forM_ [fstEnt..lastEnt] $ \e2 -> do
            orderAmount <- readUArray os (t, e1, e2)  -- e2 から e1 への受注量

            -- 受注がゼロでない場合に販売処理を実施
            when (orderAmount > 0) $ do
                let sellAmount = min orderAmount (stock  * (orderAmount / totalOrder))  -- 在庫に応じた販売量
                when (sellAmount > 0 ) $ do
                    p <- getPrice wld t e1
                    let toAdd =  sellAmount      :@ Hat :<(Products, e1, e1, Amount) -- 受注側 販売財
                             .+ (sellAmount * p) :@ Not :<(Cash,(.#),e1,Yen)          -- 受注側 販売益
                             .+  sellAmount      :@ Not :<(Products, e1, e2, Amount) -- 発注側 購入財
                             .+ (sellAmount * p) :@ Hat :<(Cash,(.#),e2,Yen)          -- 発注側 購入額
                    journal wld t (toAdd .| (SalesPurchase,t))
                    -- 受注を減らす
                    writeUArray os (t, e1, e2) (orderAmount - sellAmount)

------------------------------------------------------------------
-- | (在庫比率 * 受注量 - 在庫の量) のうち,現在保有している原材料在庫で生産する
event' wld t Production = do
    le <- readURef (_ledger wld)  -- Book を取得
    let os = (_orders wld)  -- OrdersTable を取得
    forM_ industries $ \e1 -> do
        r <- getInventoryRatio wld e1
        -- 前期の残った需要
        prevOrder <- getOrderTotal wld (t-1) e1
        -- 販売後のストックの不足分
        short <- getOrderTotal wld t e1
        -- 販売量
        let sales = (norm . (.-))
                  $ projWithNoteBase [(SalesPurchase, t)] [Hat:<(Products,e1,e1,Amount)] le
        -- 今期の新規需要
        let newOrder = (short + sales) - prevOrder
        -- 在庫保有量
        let stock = culcStock t e1 le
        -- 定常的な生産量
        sp <- readURef (_sp wld)
        -- 今期の発注量に対するr倍以上のストックを保つだけの生産
        let plan = case compare ((r * newOrder) + short) stock of
                        -- 在庫が受注と次期の在庫より少ない場合
                        GT -> case compare (((r * newOrder) + short) - stock) sp of
                                GT -> (((r * newOrder) + short) - stock)
                                _  -> sp
                        -- 等しいか多い場合 生産する必要なし
                        -- ただし最低限の生産を行う
                        _ -> sp

        pv <- getPossibleVolume wld t e1  -- 生産可能量を取得
        -- 生産が必要な量の内 生産できる量
        let prod = min plan pv

        -- when (e1==6) $ do
        --     trace (show t ++ ":stock:" ++ show stock) return ()
        --     trace (show t ++ ":order:" ++ show n) return ()
        --     trace (show t ++ ":sales:" ++ show sales) return ()
        --     trace (show t ++ ":plan:" ++ show plan) return ()
        --     trace (show t ++ ":pv:" ++ show pv) return ()

        when (prod > 0 ) $ do
            op <- getOneProduction wld t e1  -- 1単位の生産簿記を取得
            journal wld t (prod .* op)  -- 生産処理を記帳
------------------------------------------------------------------
-- 発注量を計算
-- 在庫の販売及び不足分のうち原材料在庫で生産できた分で賄えなかった
-- 分の生産に必要な原材料在庫を発注する
event' wld t Order = do
    let os = (_orders wld)  -- OrdersTable を取得
    le <- readURef (_ledger wld)  -- Book を取得
    -- 定常的な生産量
    sp <- readURef (_sp wld)


    forM_ industries $ \e1 -> do
        r           <- getInventoryRatio wld e1
        prevOrder   <- getOrderTotal wld (t-1) e1
        short       <- getOrderTotal wld t e1
        m           <- getMaterialStock wld e1
        let sales = (norm . (.-))
                  $ projWithNoteBase [(SalesPurchase, t)] [Hat:<(Products,e1,e1,Amount)] le
        -- 今期の新規需要
        let newOrder = (short + sales) - prevOrder
        -- 今期の生産量
        let prod = norm
                 $ projWithNoteBase [(Production, t)] [Not:<(Products,e1,e1,Amount)] le
        -- 在庫保有量
        let stock = culcStock t e1 le

        -- 生産しても足りなかった分
        let plan = case compare ((r * newOrder) + short) stock of
                        -- 在庫が受注と次期の在庫より少ない場合
                        GT -> case compare (((r * newOrder) + short) - stock) sp of
                                GT -> (((r * newOrder) + short) - stock)
                                _  -> sp - prod
                        -- 等しいか多い場合 生産する必要なし
                        -- ただし最低限の生産を行う
                        _ -> sp - prod

        -- e2に対する発注量を計算する
        forM_ industries $ \e2 -> do
            -- 現状の原材料在庫保有量
            let materialStock = norm
                              $ projWithBase [Not :<(Products, e2, e1, Amount)]
                              $ (.-)
                              $ termJournal t le

            -- 投入係数
            c <- getInputCoefficient wld t (e2,e1)

            -- 不足分の生産に必要な原材料在庫
            let short_material = plan * c

            -- 今回の新規需要量を次期生産できるだけの原材料在庫
            let next_material = m * newOrder * c

            let total = case compare (short_material + next_material) materialStock of
                                    GT -> (short_material + next_material) - materialStock
                                    _  -> 0
            -- when (e1==9) $ do
            --     trace (show t ++ ":total:" ++ show total) return ()
            --     trace (show t ++ ":short_material:" ++ show short_material) return ()
            --     trace (show t ++ ":next_material:" ++ show next_material) return ()
            --     trace (show t ++ ":stock:" ++ show materialStock) return ()

            -- 今期の生産分のm倍
            modifyUArray os (t, e2, e1) (\x -> x + total)

------------------------------------------------------------------
-- 最終需要部門が購入したものを消費する
event' wld t Consumption = do
    -- 保有している消費財
    le <- readURef (_ledger wld)
    let total_consume = projWithNoteBase [(SalesPurchase,t)]
                                         [Not:<(Products,(.#),finalDemandSector,(.#))]
                                         le
    journal wld t $ gather (Consumption,t)
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
        env1 = InitVar {_initInv            = 100
                      ,_initIR              = 3.0
                      ,_initMS              = 2.1
                      ,_addedDemend         = 0
                      ,_finalDemand         = 200
                      ,_inhouseRatio        = 0.4
                      ,_steadyProduction    = 30}

        env2 = env1 {_initIR      = 2.5}
        env3 = env1 {_initIR      = 3.5}
        env4 = env2 {_addedDemend = 10}
        env5 = env3 {_addedDemend = 10}

        envName = ["default","smallstock","largestock","smallstock-added","largestock-added"]
    print "start simulation"
    results <- mapConcurrently (runSimulation gen) [env1,env2,env3,env4,env5]
    let resMap = Map.fromList
               $ zip envName results

    print "printing..."
    forConcurrently_ envName $ \n -> do
        let fig_dir = "exsample/result/fig/ripple1/"
        let fs = [getTermProduction
                 ,getTermStock
                 ,getTermProfit]
            fnames = ["Production"
                     ,"Stock"
                     ,"Profit"]

        forConcurrently_ (zip fs fnames) $ \ (f, fn) -> do
            ESV.plotLineVector f ((fstEnt,initTerm),(lastEnt,lastTerm))
                               (resMap Map.! n) (fig_dir ++ n ++ "/") fn

        if n == "largestock-added"
            then do
                ESV.plotWldsDiffLine getTermProduction
                         (fstEnt,lastEnt)
                         ((resMap Map.! n),(resMap Map.! "largestock"))
                         (fig_dir ++ n ++ "/")
                         "Production-Diff"

                ESV.plotMultiLines ["added","normal"]
                       [getTermProduction,getTermProduction]
                       (fstEnt,lastEnt)
                       [(resMap Map.! n),(resMap Map.! "largestock")]
                       (fig_dir ++ n ++ "/")
                       "Production-Compare"
        else if n == "smallstock-added"
            then do
                ESV.plotWldsDiffLine getTermProduction
                         (fstEnt,lastEnt)
                         ((resMap Map.! n),(resMap Map.! "smallstock"))
                         (fig_dir ++ n ++ "/")
                         "Production-Diff"

                ESV.plotMultiLines ["added","normal"]
                       [getTermProduction,getTermProduction]
                       (fstEnt,lastEnt)
                       [(resMap Map.! n),(resMap Map.! "smallstock")]
                       (fig_dir ++ n ++ "/")
                       "Production-Compare"
        else return ()