
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE TypeFamilies           #-}

{-
波及効果のシュミレーションサンプル
10エージェント,投入制約なし
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
import Statistics.Distribution hiding (mean, stdDev)
import Statistics.Distribution.Normal
import GHC.Generics (Generic)
import Data.Hashable

-- Debug
import Debug.Trace



------------------------------------------------------------------
-- * directories

fig_dir = "exsample/deterministic/ripple/result/fig/withoutStock/"
csv_dir = "exsample/deterministic/ripple/result/csv/withoutStock/"

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

-- | 投入係数の取得
-- 1単位の e1 の生産に必要な e2
getInputCoefficient :: World s -> Term -> Entity -> Entity -> ST s InputCoefficient
getInputCoefficient wld t e1 e2 =  do
                let ics = (_ics wld)
                readUArray ics (t,e2,e1)

-- | 初期の投入係数行列の取得
-- 1期の投入係数行列を取得する
-- 最終需要を抜かした9*9
getInputCoefficients :: World RealWorld -> IO (IOArray (Entity,Entity) Double)
getInputCoefficients wld = do
    let arr = (_ics wld)
    result <- newArray ((fstEnt, fstEnt), (lastEnt -1, lastEnt -1)) 0
    forM_ [fstEnt .. lastEnt -1] $ \e1 ->
        forM_ [fstEnt .. lastEnt -1] $ \e2 -> do
            c <- stToIO $ readUArray arr (1,e1,e2)
            writeArray result (e1,e2) c
    return result


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
        -- c を生産するために必要な c2 の投入係数
        coef <- readUArray arr (t, c2, c)
        -- c2 の消費を記録
        return $ coef :@ Hat :<(Products, c2, c, Amount) .| (Production,t)
        -- すべての中間投入を結合
    let !totalInput = EJ.fromList inputs
        -- 生産と投入の合計
        !result = (1 :@ Not :<(Products, c, c, Amount) .| (Production,t)) .+ totalInput
    return result


-- | 中間投入量の差額(ripple Effectの把握)
-- 期間全体の総額を比較する
culcRippleEffect :: World RealWorld
                 -> World RealWorld
                 -> InitVar
                 -> IO (IOArray (Entity,Entity) Double)
culcRippleEffect notAdded added iv = do
    result <- newArray ((fstEnt, fstEnt), (lastEnt -1, lastEnt -1)) 0
    forM_ [fstEnt .. lastEnt -1] $ \e1 ->
        forM_ [fstEnt .. lastEnt -1] $ \e2 -> do
            forM_ [initTerm .. lastTerm] $ \t -> do
                inputNotAdded <- stToIO $ getTermInput notAdded t e1 e2
                inputAdded    <- stToIO $ getTermInput added t e1 e2
                when (inputNotAdded /= inputAdded) $ do
                    x <- readArray result (e2,e1)
                    writeArray result (e2,e1) (x + (inputAdded - inputNotAdded) / (_addedDemand iv))
    return result


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
        -- 各 e2 (注文元) に対して処理
        forM_ [fstEnt..lastEnt] $ \e2 -> do
            when (e1 /= e2) $ do
                -- e2 から e1 への受注量
                orderAmount <- readUArray os (t, Relation {_supplier = e1
                                                          ,_customer = e2})

                -- 受注がゼロでない場合に販売処理を実施
                -- 在庫の有無に関わらず販売
                when (orderAmount > 0) $ do
                    p <- getPrice wld t e1
                    let toAdd =  orderAmount      :@ Hat :<(Products, e1, e1, Amount) -- 受注側 販売財
                             .+ (orderAmount * p) :@ Not :<(Cash,(.#),e1,Yen)          -- 受注側 販売益
                             .+  orderAmount      :@ Not :<(Products, e1, e2, Amount) -- 発注側 購入財
                             .+ (orderAmount * p) :@ Hat :<(Cash,(.#),e2,Yen)          -- 発注側 購入額
                    journal wld (toAdd .| (SalesPurchase,t))
                    -- 受注を減らす
                    writeUArray os (t, Relation {_supplier = e1,_customer = e2}) 0

------------------------------------------------------------------
-- | 不足分を生産する
-- 投入制約なし
event' wld t Production = do
    le <- readURef (_ledger wld)
    -- 定常的な生産量
    sp <- readURef (_sp wld)
    forM_ industries $ \e1 -> do
        -- 自社製品の不足分(Hat分)を生産する
        let short = norm
                  $ EJ.projWithBase [Hat:<(Products,e1,e1,Amount)]
                  $ (.-) $ termJournal t le

        -- 不足分生産する
        let plan = max short sp
        {-
        when (e1 == 1) $ do
            trace ("--------------------------") return ()
            trace (show t ++ "-pl: " ++ show plan) return ()
        -}
        when (plan > 0 ) $ do
            op <- getOneProduction wld t e1  -- 1単位の生産簿記を取得
            journal wld (plan .* op)  -- 生産処理を記帳


------------------------------------------------------------------
-- 発注量を計算
-- 不足した原材料を発注

event' wld t Order = do
    le <- readURef (_ledger wld)
    -- 定常的な生産量
    sp <- readURef (_sp wld)

    forM_ industries $ \e1 -> do
        -- e2に対する発注量を計算する
        forM_ industries $ \e2 -> do
                when (e1 /= e2) $ do
                    -- 原材料在庫の不足分
                    let short_material = norm
                                       $ EJ.projWithBase [Hat:<(Products,e2,e1,Amount)]
                                       $ (.-) $ termJournal t le
                    {-
                    when (e1 == 4) $ do
                        trace (show t ++ "-sm(" ++ show e2 ++ "): " ++ show short_material) return ()
                    -}
                    -- 次の期に発注する
                    order wld (t+1) Relation {_supplier = e2, _customer = e1} short_material

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
        defaultEnv = InitVar {_initInv             = 0
                             ,_addedDemand         = 0
                             ,_finalDemand         = 300
                             ,_inhouseRatio        = 0.4
                             ,_steadyProduction    = 0}

        defaultAddedEnv = defaultEnv {_addedDemand = 10}
        envs =  [defaultEnv,defaultAddedEnv]

        envNames = ["default","default-added"]

    ------------------------------------------------------------------
    print "start simulation"
    results <- mapConcurrently (runSimulation gen) envs


    let resMap = Map.fromList
               $ zip envNames results


    ------------------------------------------------------------------
    print "printing tables ..."
    -- coefficient Table
    mat <- getInputCoefficients (resMap Map.! "default")
    writeIOMatrix (csv_dir ++ "io.csv") mat

    -- Basic Ripple Effect
    li  <- leontiefInverse mat
    writeIOMatrix (csv_dir ++ "leontiefInverse.csv") li
    re  <- rippleEffect 9 li
    writeIOMatrix (csv_dir ++ "rippleEffect.csv") re

    -- ABM Ripple Effect
    reABM <- culcRippleEffect (resMap Map.! "default")
                              (resMap Map.! "default-added")
                              defaultAddedEnv
    writeIOMatrix  (csv_dir ++ "rippleEffectABM.csv") reABM
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
        else return ()