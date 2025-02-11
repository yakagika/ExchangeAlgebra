
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
import qualified    ExchangeAlgebra         as EA
import              ExchangeAlgebra
import qualified    ExchangeAlgebra.Transfer as ET
import qualified    ExchangeAlgebra.Simulate as ES
import              ExchangeAlgebra.Simulate


-- Other
import qualified    Number.NonNegative      as NN
import qualified    Numeric                 as N
import              Number.NonNegative

import qualified    Data.Map.Strict         as M
import qualified    Data.Text               as T

import qualified    Control.Monad                   as CM
import              Control.Monad
import              Control.Monad.State
import              Control.Monad.ST
import              Data.Array.ST
import              Data.STRef
import qualified    Data.List                       as L
import GHC.Generics
import System.Random -- 乱数

-- Debug
import Debug.Trace



------------------------------------------------------------------
-- *  状態系の導入
------------------------------------------------------------------

instance StateTime Term where
    initTerm = 1
    lastTerm = 5
    nextTerm = \x -> x + 1
    prevTerm = \x -> x - 1

------------------------------------------------------------------
-- ** 乱数生成器の状態空間の定義
------------------------------------------------------------------

seed = 42

type Gen s = STRef s StdGen

-- ** STArray s (ID, Term) NN.Double
-- 価格は固定
instance Updatable Term ST s (Gen s) where
    initialize g t = newSTRef (mkStdGen seed)
    updatePattern _ _ = return DoNothing

------------------------------------------------------------------
-- ** 簿記の状態空間の定義
------------------------------------------------------------------
-- ExBase Elementをインスタンス宣言する
-- wiledcardのみ指定すればOK
type ID = Prelude.Int

instance EA.Element ID where
    wiledcard = -1
-- 取引主体ID
type Entity = Prelude.Int

-- 最小
fstEnt  = 1
-- 最大
lastEnt = 50
-- 期
type Term = Prelude.Int

-- ExBaseをインスタンス宣言する
-- 会計勘定科目の位置のみ指定すればOK

type VEHatBase = EA.HatBase ( EA.AccountTitles
                            , Entity
                            , Entity
                            , Term
                            , EA.CountUnit)

instance ExBaseClass VEHatBase where
    getAccountTitle (h :< (a,c,e,t,u))   = a
    setAccountTitle (h :< (a,c,e,t,u)) b = h :< (b,c,e,t,u)


-- | 取引情報
type Transaction = EA.Alg NN.Double VEHatBase
-- | 簿記
type Book s = STRef s Transaction

-- 取引の初期状態
-- 在庫だけ一定量(50)保有
initTransaction :: Entity -> Entity -> Transaction
initTransaction c e = 50 .@ Not :<(Products,c,e,initTerm,Amount)

initBook :: ST s (Book s)
initBook = newSTRef $ EA.fromList [ initTransaction c c
                                  | c <- [fstEnt..lastEnt]]

-- 一般化の適用
instance Updatable Term ST s (Book s) where
    initialize _ _  = initBook

    updatePattern _ _ = return Modify

    -- 過去のTermを次の期のTermに変更して追加する
    modify t x  =  readSTRef x >>= \bk
                -> let added = g t bk
                in modifySTRef x (\z -> z .+ added)
        where
        g t x = (.-)
              $ plusTerm t
              $ EA.finalStockTransferKeepWiledcard
              $ inventoryCount
              $ proj [HatNot :<((.#),(.#),(.#),prevTerm t,(.#))] x

-- | 1期繰り上げるTransfer
plusTerm :: Term -> Transaction -> Transaction
plusTerm  t tr = ET.transferKeepWiledcard tr
                 $ EA.table
                 $  (Not:<((.#),(.#),(.#),prevTerm t,(.#))) .-> (Not:<((.#),(.#),(.#),t,(.#))) |% id
                 ++ (Hat:<((.#),(.#),(.#),prevTerm t,(.#))) .-> (Hat:<((.#),(.#),(.#),t,(.#))) |% id

-- | 棚卸し仕訳
-- 物量で商品が売上と同額貸方にあるので,
-- 同額の売上に変換することで分記法から,三分割法の売上に変換される
-- ここでは,Cashが発生する機会が販売購入以外にないため,この実装で良いが
-- 他に同名の勘定科目が生じるイベントがある場合には,摘要情報を利用する必要がある.
inventoryCount ::  Transaction -> Transaction
inventoryCount tr = ET.transferKeepWiledcard tr
                  $ EA.table
                  $ (toNot wiledcard) .~ Cash .-> (toNot wiledcard) .~ Sales     |% id
                  ++(toHat wiledcard) .~ Cash .-> (toNot wiledcard) .~ Purchases |% id

------------------------------------------------------------------
-- ** 価格の状態空間の定義
------------------------------------------------------------------

-- | 販売価格のテーブル
type Price = NN.Double
type PriceTable = M.Map (Term,Entity) Price
type Prices s = STRef s PriceTable

-- 販売価格の初期状態
-- 今回は全て価格同一(1)

initPrice :: Term -> Entity -> Price
initPrice t _ = 1

-- 価格履歴の初期状態
initPrices :: ST s (Prices s)
initPrices  = newSTRef $ M.fromList [((t,c),initPrice t c)
                                    | t <- [initTerm .. lastTerm]
                                    , c <- [1..50]]

-- ** STArray s (ID, Term) NN.Double
-- 価格は固定
instance Updatable Term ST s (Prices s) where
    initialize _ _ = initPrices
    updatePattern _ _ = return DoNothing

type VETransTable = EA.TransTable NN.Double VEHatBase

-- | 価格テーブルから物量→価格評価への変換テーブルを作成
toCashTable :: PriceTable -> VETransTable
toCashTable pt = EA.table
                $ L.foldl (++) [] [f t c p | ((t,c),p) <- M.toList pt]
    where
        f :: Term -> Entity -> NN.Double
          -> [(VEHatBase,VEHatBase,(NN.Double -> NN.Double))]
        f t c p =   (Hat:<(Products,c,(.#),t,Amount))
                .-> (Hat:<(Products,c,(.#),t,Yen))
                |% (*p)


-- | 価格テーブルから価格→物量評価への変換テーブルを作成
toAmountTable :: PriceTable -> VETransTable
toAmountTable pt = EA.table
                $ L.foldl (++) [] [f t c p | ((t,c),p) <- M.toList pt]
    where
        f :: Term -> Entity -> NN.Double
          -> [(VEHatBase,VEHatBase,(NN.Double -> NN.Double))]
        f t c p =   (Hat:<(Products,c,(.#),t,Amount))
                .-> (Hat:<(Products,c,(.#),t,Yen))
                |% (/p)



-- | 価格の取得
getPrice :: World s -> Term -> Entity -> ST s Price
getPrice wld t c =  readSTRef (_prices wld) >>= \pt
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
type InventoryRatio = NN.Double
newtype IRTable = IRTable {_irtable :: M.Map Entity InventoryRatio}
type IRs s = STRef s IRTable

-- 在庫比率の初期状態
-- 今回は全て同一比率
-- この比率次第でサプライチェーンがスタックする?

initInventoryRatio :: Entity -> InventoryRatio
initInventoryRatio _ = 1.2 -- 20%在庫分追加

-- 価格履歴の初期状態
initIRs :: ST s (IRs s)
initIRs     = newSTRef
            $ IRTable
            $ M.fromList    [(e,initInventoryRatio e)
                            | e <- [1 .. 50]]

-- ** STArray s (ID, Term) NN.Double
-- 価格は固定
instance Updatable Term ST s (IRs s) where
    initialize _ _ = initIRs
    updatePattern _ _ = return DoNothing

-- | 在庫比率の取得
getInventoryRatio :: World s -> Entity -> ST s InventoryRatio
getInventoryRatio wld e     =  readSTRef (_irs wld) >>= \ir
                            -> case M.lookup e (_irtable ir) of
                                    Nothing -> return 0
                                    Just x  -> return x

------------------------------------------------------------------
-- ** 原材料在庫比率
------------------------------------------------------------------
-- | 原材料在庫
type MaterialStock = NN.Double
newtype MSTable = MSTable {_mstable :: M.Map Entity MaterialStock}
type MSs s = STRef s MSTable

-- 原材料在庫
-- 今季受注のX倍
initMaterialStock :: Entity -> MaterialStock
initMaterialStock _ = 1.2 -- 20%在庫分追加

-- 価格履歴の初期状態
initMSs :: ST s (MSs s)
initMSs = newSTRef
        $ MSTable
        $ M.fromList    [(e,initMaterialStock e)
                        | e <- [1 .. 50]]

-- ** STArray s (ID, Term) NN.Double
-- 価格は固定
instance Updatable Term ST s (MSs s) where
    initialize _ _ = initMSs
    updatePattern _ _ = return DoNothing

-- | 在庫比率の取得
getMaterialStock :: World s -> Entity -> ST s MaterialStock
getMaterialStock wld e     =  readSTRef (_mss wld) >>= \ms
                            -> case M.lookup e (_mstable ms) of
                                    Nothing -> return 0
                                    Just x  -> return x

------------------------------------------------------------------
-- ** 生産関数の定義
------------------------------------------------------------------
-- 一単位生産する場合の簿記を定義しておいて,それに (.*) 演算すること
-- で生産量を示す
-- 財別に作る必要がある? (取り敢えず今回は1企業1財 (産業概念で行う))

-- 生産関数(投入係数行列)
-- 物量表記
type InputCoefficient = NN.Double
-- |  -- (Term, Commodity1, Commodity2) の配列
newtype ICTable s = ICTable {_ictable :: STArray s (Term, Entity, Entity) InputCoefficient}


-- ** 1つの Term に対する投入係数を生成 (乱数を使用, 列和 = 1)
initTermCoefficients :: StdGen -> M.Map Entity [InputCoefficient]
initTermCoefficients g =
    let generateRow g =
            let (vals, g') = generateRandomList g 50
                total = sum vals
                normalized = L.map (\x -> NN.fromNumber (x / total)) vals
            in (normalized, g')
        (rows, _) = foldl (\(m, g0) c2 -> let (row, g1) = generateRow g0 in (M.insert c2 row m, g1)) (M.empty, g) [fstEnt..lastEnt]
    in rows

-- ** 乱数リストを生成 (0.1 ~ 1.0 の範囲)
generateRandomList :: StdGen -> Prelude.Int -> ([Prelude.Double], StdGen)
generateRandomList g n = runState (replicateM n (state $ randomR (0.1, 1.0)))
                                  (updateGen g 1000)

-- ** 生産関数の初期状態 (STArray を使用, Term ごとに固定)
initICTables :: StdGen -> ST s (ICTable s)
initICTables g = do
    arr <- newArray ((initTerm, 1, 1), (lastTerm, 50, 50)) (NN.fromNumber 0)  -- 初期値は0
    let termCoefficients = M.fromList [(t, initTermCoefficients g) | t <- [initTerm .. lastTerm]]
    forM_ [(t, c2) | t <- [initTerm .. lastTerm], c2 <- [fstEnt .. lastEnt]] $ \(t, c2) -> do
        let row = termCoefficients M.! t M.! c2  -- Term ごとに固定
        forM_ (zip [fstEnt..lastEnt] row) $ \(c1, coef) ->
            writeArray arr (t, c1, c2) coef
    return $ ICTable arr


-- | 生産関数の更新
-- 前の期の簿記から計算する
-- ただし,今回は価格固定なので変化なし
instance Updatable Term ST s (ICTable s) where
    initialize g _ = initICTables g
    updatePattern _ _ = return DoNothing


-- | 投入係数の取得
getInputCoefficient :: World s -> Term -> (Entity, Entity) -> ST s InputCoefficient
getInputCoefficient wld t (c1,c2) =  do
                let ics = _ictable (_ics wld)
                readArray ics (t,c1,c2)


-- | 一単位の財の簿記を取得する
getOneProduction :: World s -> Term -> Entity -> ST s Transaction
getOneProduction wld t c = do
    let arr =  _ictable (_ics wld)  -- ICTable を取得
    inputs <- CM.forM [fstEnt..lastEnt] $ \c2 -> do
        coef <- readArray arr (t, c2, c)  -- c を生産するために必要な c2 の投入係数
        return $ coef .@ Hat :<(Products, c2, c, t, Amount)  -- c2 の消費を記録
    let totalInput = foldl (.+) Zero inputs  -- すべての中間投入を結合
    return $ 1 .@ Not :<(Products, c, c, t, Amount) .+ totalInput  -- 生産と投入の合計


------------------------------------------------------------------
-- ** 発注書の状態空間
------------------------------------------------------------------
-- 発注された中間投入財の量 = 生産が必要な量
-- 基本的には発注分を除けば前年度と同じ量を発注
-- 財ごとのInt
type Order = NN.Double
type OrderTable s = STArray s (Term,Entity,Entity) Order


-- 価格履歴の初期状態
initOrders :: ICTable s -> ST s (OrderTable s)
initOrders icTable = do
    ((tMin, c1Min, c2Min), (tMax, c1Max, c2Max)) <- getBounds (_ictable icTable)
    ordersArr <- newArray ((tMin, c1Min, c2Min), (tMax, c1Max, c2Max)) (NN.fromNumber 0)
    forM_ [(t, c1, c2) | t <- [tMin .. tMax], c1 <- [c1Min .. c1Max], c2 <- [c2Min .. c2Max]] $ \(t, c1, c2) -> do
        coef <- readArray (_ictable icTable) (t, c1, c2)
        writeArray ordersArr (t, c1, c2) (coef * NN.fromNumber 10)  -- 10倍して格納
    return ordersArr

-- ** STArray s (ID, Term) NN.Double
-- 価格は固定
instance Updatable Term ST s (OrderTable s) where
    initialize g _ = do
        icTable <- initICTables g  -- ICTable を初期化
        initOrders icTable    -- それを基に OrdersTable を作成

    updatePattern _ _ = return Modify

    -- 前期の残りの分を追加
    modify t x  =  forM_ [fstEnt..lastEnt] $ \e1
                -> forM_ [fstEnt..lastEnt] $ \e2
                -> readArray x (t-1, e1,e2) >>= \y
                -> readArray x (t-1, e1,e2) >>= \z
                -> writeArray x (t,e1,e2) (z + y)

-- | 個別の発注量の取得
getOrder :: World s -> Term -> Entity -> Entity -> ST s Order
getOrder wld t e1 e2 =  let arr = (_orders wld)
                     in readArray arr (t,e1,e2)

-- | 総受注量の取得
getOrderTotal :: World s -> Term -> Entity -> ST s Order
getOrderTotal wld t e1 = do
    let arr = (_orders wld)
    values <- CM.mapM (\e2 -> readArray arr (t, e1, e2)) [fstEnt .. lastEnt]
    return $ sum values
------------------------------------------------------------------
-- * World
------------------------------------------------------------------
-- | 状態空間の定義
data World s = World { _book    :: Book s
                     , _prices  :: Prices s
                     , _ics     :: ICTable s
                     , _orders  :: OrderTable s
                     , _mss     :: MSs s
                     , _irs     :: IRs s
                     , _gen     :: Gen s }
                     deriving (Generic)

-- deriving Generic をしていれば
-- 空のインスタンス宣言で自動でinitSS,updateSSが使えるようになる
instance StateSpace Term ST s (World s)


------------------------------------------------------------------
-- * 汎用関数
------------------------------------------------------------------

-- | 生産可能量の計算
-- 現在の原材料在庫に基づいて生産可能量を計算する
-- 在庫量 / 投入係数の最小値が生産可能量
getPossibleVolume :: World s -> Term -> Entity -> ST s NN.Double
getPossibleVolume wld t e1 = do
    let ics = _ictable (_ics wld)
    bk  <- readSTRef (_book wld)

    -- 各 e2 (原材料) に対して、生産可能量の計算
    possibleVolumes <- CM.forM [fstEnt .. lastEnt] $ \e2 -> do
        let n = projNorm  [Not :< (Products, e2, e1, t, Amount)] bk  -- 在庫量
        c <- readArray ics (t, e1, e2)  -- 投入係数
        return $ if c == NN.fromNumber 0 then NN.fromNumber (1 / 0) else n / c  -- 0除算防止

    return $ minimum possibleVolumes  -- 可能な生産量の最小値を返す



------------------------------------------------------------------
-- * 状態の更新
------------------------------------------------------------------
-- ** イベントの設定
-- 同じ取引を繰り返すだけ

{-

class (Eq e, Enum e, Ord e, Bounded e, StateSoace t m s a)
    => Event e t m s a
    event :: World s -> Term -> EventType -> m s ()
    eventAll :: World s -> Term -> m s ()
-}

-- | Transaction の種類
data EventName
    = ToAmount                          -- ^ 価格から物量評価へ変換
    | Order                             -- ^ 発注量の決定
    | Production                        -- ^ 保有する中間投入財を使用して生産
    | BuySell                           -- ^ 購入販売
    | ToPrice                           -- ^ 物量から価格評価へ変換
    deriving (Show, Enum, Eq, Bounded)

class (Show e, Eq e, Bounded e) => Event e where
    isJournal :: e -> Bool
    isJournal _ = False

instance Event EventName where
    isJournal Production = True
    isJournal BuySell    = True

-- 記帳
journal :: World s -> Term -> Transaction -> ST s ()
journal wld t b = case b of
        Zero -> return ()
        _    -> modifySTRef (_book wld) (\x -> x .+ b)

event :: World s -> Term -> EventName -> ST s ()

--  通常簿記を物量簿記に変換する
event wld t ToAmount = readSTRef (_prices wld) >>= \pt
              -> modifySTRef (_book wld) $ \bk
              -> let target = proj [HatNot:<(Products,(.#),(.#),t,Yen)] bk
              in bk
              .+ ((.^) target)
              .+ EA.transferKeepWiledcard target (toAmountTable pt)

--  物量簿記を通常簿記に変換する
event wld t ToPrice = readSTRef (_prices wld) >>= \pt
              -> modifySTRef (_book wld) $ \bk
              -> let target = proj [HatNot:<(Products,(.#),(.#),t,Amount)] bk
              in bk
              .+ ((.^) target)
              .+ EA.transferKeepWiledcard target (toCashTable pt)

------------------------------------------------------------------

-- 発注量を計算
-- 自身に対する発注量*(1+在庫量)に対して,必要となる投入財を発注する
-- 今回の自身への発注量に対して,必要となる投入財の在庫分も発注する
event wld t Order = do
    let os = (_orders wld)  -- OrdersTable を取得

    currentOrders <- (flip  CM.mapM) [fstEnt..lastEnt] $ \e
                  -> getOrderTotal wld t e >>= \ot
                  -> return (e,ot)

    forM_ [fstEnt..lastEnt] $ \e1 -> do
        r <- getInventoryRatio wld e1
        m <- getMaterialStock wld e1
        let co = case M.lookup e1 (M.fromList currentOrders) of
                    Nothing -> 0
                    Just x  -> x

        forM_ [fstEnt..lastEnt] $ \e2 -> do
            o <- readArray os (t, e2, e1) -- 自分からの現状の発注
            c <- getInputCoefficient wld t (e2,e1) -- 投入係数
            writeArray os (t, e2, e1) (o + m * r * c * co)


-- (在庫比率 * 受注量 - 在庫の量) のうち,現在保有している原材料在庫で生産する
-- | (在庫比率 * 受注量 - 在庫の量) のうち,現在保有している原材料在庫で生産する
event wld t Production = do
    bk <- readSTRef (_book wld)  -- Book を取得
    let os = (_orders wld)  -- OrdersTable を取得
    forM_ [fstEnt..lastEnt] $ \e1 -> do
        r <- getInventoryRatio wld e1
        n <- getOrderTotal wld t e1  -- `getOrderTotal` から総受注量を取得
        let inv = projNorm [Not :<(Products, e1, e1, t, Amount)] bk  -- 在庫保有量
        let plan = case (r * n) > inv of  -- 生産計画
                    True  -> (r * n) - inv
                    False -> NN.fromNumber 0
        pv <- getPossibleVolume wld t e1  -- 生産可能量を取得
        let prod = min plan pv  -- 生産可能量との比較
        op <- getOneProduction wld t e1  -- 1単位の生産簿記を取得
        journal wld t (prod .* op)  -- 生産処理を記帳

------------------------------------------------------------------
-- 受注分販売・購入する
-- 在庫を受注の割合で販売する
-- 売れた分受注を減らす
event wld t BuySell = do
    bk <- readSTRef (_book wld)  -- 簿記の状態を取得
    let os = _orders wld  -- OrdersTable を取得

    forM_ [fstEnt..lastEnt] $ \e1 -> do
        -- 受注総量を取得
        totalOrder <- getOrderTotal wld t e1
        -- 在庫量を取得
        let stock = projNorm [Not :< (Products, e1, e1, t, Amount)] bk

        -- 各 e2 (注文元) に対して処理
        forM_ [fstEnt..lastEnt] $ \e2 -> do
            orderAmount <- readArray os (t, e2, e1)  -- e2 から e1 への受注量

            -- 受注がゼロでない場合に販売処理を実施
            when (orderAmount > NN.fromNumber 0) $ do
                let sellAmount = min orderAmount (stock * (orderAmount / totalOrder))  -- 在庫に応じた販売量
                p <- getPrice wld t e1
                journal wld t $  sellAmount      .@ Hat :<(Products, e1, e1, t, Amount)
                             .+ (sellAmount * p) .@ Not :<(Cash,(.#),e1,t,Yen)
                             .+  sellAmount      .@ Not :<(Products, e1, e2, t, Amount)
                             .+ (sellAmount * p) .@ Hat :<(Cash,(.#),e1,t,Yen)
                writeArray os (t, e2, e1) (orderAmount - sellAmount)  -- 受注を減らす

------------------------------------------------------------------
-- | Transaction全体を結合
eventAll :: forall s.  World s -> Term ->  ST s ()
eventAll wld t =  CM.forM_ ([minBound .. maxBound] :: [EventName])
                $ \e -> event wld t e

------------------------------------------------------------------
-- * Simulation
------------------------------------------------------------------
simulate ::  World s -> ST s ()
simulate wld = loop wld initTerm
  where
  {-# INLINE loop #-}
  loop ::  World s -> Term -> ST s ()
  loop wld t
    | t == lastTerm =  eventAll wld t
    | otherwise     =  eventAll wld t
                    >> updateAll (nextTerm t) wld >>= \_
                    -> loop wld (nextTerm t)

-- | 初期化 → 指定されたイベントの実行までをこなす
runSimulation :: Term -> StdGen -> ST s (World s)
runSimulation t gen = initAll gen t >>= \wld'
                    -> simulate wld'
                    >> return wld'

------------------------------------------------------------------
-- * 実行
------------------------------------------------------------------


main :: IO ()
main = do
    gen <- getStdGen
    wld <- stToIO $ runSimulation (initTerm :: Term) gen
    bk <- stToIO $ readSTRef (_book wld)
    print $ (.-) $ proj [HatNot :<(Cash,(.#),1,(.#),(.#))] bk
    writeBS "exsample/result/csv/ripple1.csv" $ EA.grossProfitTransferKeepWiledcard bk



