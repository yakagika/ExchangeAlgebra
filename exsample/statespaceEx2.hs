
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
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
状態空間による会計シミュレーションサンプル(確率モデル)
4エージェントがこの取引を毎期繰り返すのみの尤も単純な形式
労働者等も存在しない.
価格が価格改定イベントを通じて正規分布に応じて変化する
価格と初期保有量が乱数で決定する
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
import              Control.Monad.ST
import              Data.Array.ST
import              Data.STRef
import qualified    Data.List                       as L
import GHC.Generics
import System.Random -- 乱数
------------------------------------------------------------------
-- *  状態系の導入
------------------------------------------------------------------

instance StateTime Term where
    initTerm = 1
    lastTerm = 3
    nextTerm = \x -> x + 1
    prevTerm = \x -> x - 1

------------------------------------------------------------------
-- ** 簿記の状態空間の定義
------------------------------------------------------------------
-- ExBase Elementをインスタンス宣言する
-- wiledcardのみ指定すればOK
type ID = Prelude.Int

instance EA.Element ID where
    wiledcard = -1
-- 取引主体ID
type Entity = ID
-- 最小
fstEnt  = 1
-- 最大
lastEnt = 4
-- 期
type Term = ID

-- 商品名
type CommodityName = T.Text

-- ExBaseをインスタンス宣言する
-- 会計勘定科目の位置のみ指定すればOK

type VEHatBase = EA.HatBase ( EA.AccountTitles
                            , CommodityName
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
initBook :: ST s (Book s)
initBook = newSTRef Zero

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
type PriceTable = M.Map (Term,CommodityName) Price
type Prices s = STRef s PriceTable

-- 販売価格の初期状態
-- 今回は期間別の1財1価 ,生産者別の価格,取引別の価格は取引idなどを基底に持たせる必要がある.
initPrice :: Term -> CommodityName -> NN.Double
initPrice t "a" = 2
initPrice t "b" = 3
initPrice t "c" = 4
initPrice t "d" = 5

-- 価格履歴の初期状態
initPrices :: ST s (Prices s)
initPrices  = newSTRef $ M.fromList [((t,c),initPrice t c)
                                    | t <- [initTerm .. lastTerm]
                                    , c <- ["a","b","c","d"]]

-- ** STArray s (ID, Term) NN.Double
-- 価格は固定
instance Updatable Term ST s (Prices s) where
    initialize _ _ = initPrices
    updatePattern _ _ = return DoNothing

type VETransTable = EA.TransTable NN.Double VEHatBase
-- | 価格テーブルから物量→価格評価への変換テーブルを作成
toTransTable :: PriceTable -> VETransTable
toTransTable pt = EA.table
                $ L.foldl (++) [] [f t c p | ((t,c),p) <- M.toList pt]
    where
        f :: Term -> CommodityName -> NN.Double
          -> [(VEHatBase,VEHatBase,(NN.Double -> NN.Double))]
        f t c p =   (Hat:<(Products,c,(.#),t,Amount))
                .-> (Hat:<(Products,c,(.#),t,Yen))
                |% (*p)

-- | 価格の取得
getPrice :: World s -> Term -> CommodityName -> ST s Price
getPrice wld t c =  readSTRef (_prices wld) >>= \pt
                 -> case M.lookup (t,c) pt of
                    Nothing -> return 0
                    Just x  -> return x

-- | 製造原価を計算する
getCost = undefined

------------------------------------------------------------------
-- ** 乱数生成器の状態空間の定義
------------------------------------------------------------------

type Gen s = STRef s StdGen

-- ** STArray s (ID, Term) NN.Double
-- 価格は固定
instance Updatable Term ST s (Gen s) where
    initialize g t = newSTRef g' where (x,g') = (genWord32 g)
    updatePattern _ _ = return DoNothing

------------------------------------------------------------------
-- 状態空間の定義
-- 会計と価格のみの世界
data World s = World { _book    :: Book s
                     , _prices  :: Prices s
                     , _gen     :: Gen s }
                     deriving (Generic)

-- deriving Generic をしていれば
-- 空のインスタンス宣言で自動でinitSS,updateSSが使えるようになる
instance StateSpace Term ST s (World s)


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
    = Production                        -- ^ 保有する中間投入財を使用して生産
    | DecideNewPrice                    -- ^ 価格の決定
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
writeJournal :: World s -> Term -> Transaction -> ST s ()
writeJournal wld t b = case b of
        Zero -> return ()
        _    -> modifySTRef (_book wld) (\x -> x .+ b)


event :: World s -> Term -> EventName -> ST s ()

-- 何も購入していない場合は何も中間消費がない
-- 記録において購入したものを中間消費して販売量と同量生産する
-- 在庫概念,資本制約も労働制約もない
event wld t Production = writeJournal wld t
                   $ 1 .@ Not :<(Products,"a",1,t,Amount)
                  .+ 1 .@ Hat :<(Products,"a",2,t,Amount)
                  .+ 1 .@ Not :<(Products,"b",1,t,Amount)
                  .+ 1 .@ Hat :<(Products,"b",2,t,Amount)
                  .+ 1 .@ Not :<(Products,"a",1,t,Amount)
                  .+ 1 .@ Hat :<(Products,"a",3,t,Amount)
                  .+ 1 .@ Not :<(Products,"c",3,t,Amount)
                  .+ 1 .@ Hat :<(Products,"c",2,t,Amount)
                  .+ 1 .@ Not :<(Products,"d",3,t,Amount)
                  .+ 1 .@ Hat :<(Products,"d",2,t,Amount)
                  .+ 1 .@ Not :<(Products,"d",3,t,Amount)
                  .+ 1 .@ Hat :<(Products,"d",4,t,Amount)
------------------------------------------------------------------
-- 中間消費分購入(販売)する
-- その時の価格を反映させる
event wld t BuySell = writeJournal wld t
                =<< (pure 1) <@ Hat :<(Products,"a",1,t,Amount) <+ (getPrice wld t "a") <@ Not :<(Cash,(.#),1,t,Yen)
                <+  (pure 1) <@ Not :<(Products,"a",2,t,Amount) <+ (getPrice wld t "a") <@ Hat :<(Cash,(.#),2,t,Yen)
                <+  (pure 1) <@ Hat :<(Products,"b",1,t,Amount) <+ (getPrice wld t "b") <@ Not :<(Cash,(.#),1,t,Yen)
                <+  (pure 1) <@ Not :<(Products,"b",2,t,Amount) <+ (getPrice wld t "b") <@ Hat :<(Cash,(.#),2,t,Yen)
                <+  (pure 1) <@ Hat :<(Products,"a",1,t,Amount) <+ (getPrice wld t "a") <@ Not :<(Cash,(.#),1,t,Yen)
                <+  (pure 1) <@ Not :<(Products,"a",3,t,Amount) <+ (getPrice wld t "a") <@ Hat :<(Cash,(.#),3,t,Yen)
                <+  (pure 1) <@ Hat :<(Products,"c",3,t,Amount) <+ (getPrice wld t "c") <@ Not :<(Cash,(.#),3,t,Yen)
                <+  (pure 1) <@ Not :<(Products,"c",2,t,Amount) <+ (getPrice wld t "c") <@ Hat :<(Cash,(.#),2,t,Yen)
                <+  (pure 1) <@ Hat :<(Products,"d",3,t,Amount) <+ (getPrice wld t "d") <@ Not :<(Cash,(.#),3,t,Yen)
                <+  (pure 1) <@ Not :<(Products,"d",2,t,Amount) <+ (getPrice wld t "d") <@ Hat :<(Cash,(.#),2,t,Yen)
                <+  (pure 1) <@ Hat :<(Products,"d",3,t,Amount) <+ (getPrice wld t "d") <@ Not :<(Cash,(.#),3,t,Yen)
                <+  (pure 1) <@ Not :<(Products,"d",4,t,Amount) <+ (getPrice wld t "d") <@ Hat :<(Cash,(.#),4,t,Yen)
------------------------------------------------------------------


event wld t DecideNewPrice =  readSTRef (_gen wld)      >>= \g
                           -> readSTRef (_prices wld)   >>= \p
                           -> let (p',g') = decidePrice t ["a","b","c","d"] g p
                           in writeSTRef (_prices wld) p'
                           >> writeSTRef (_gen wld) g'
    where
        decidePrice :: Term -> [CommodityName] -> StdGen -> PriceTable -> (PriceTable,StdGen)
        decidePrice t cs g p
            =  let (s,g') = normal' (0.0::Prelude.Double,1.0::Prelude.Double) g
            in case cs of
                []   -> (p,g')
                [c]  -> let current = M.lookup (t,c) p
                     in (M.insert (t,c) (f s current) p , g')
                c:xs -> let current = M.lookup (t,c) p
                     in decidePrice t xs g' (M.insert (t,c) (f s current) p)

        f :: Prelude.Double -> Maybe Price -> NN.Double
        f change current = case current of
                Nothing -> case change > 0 of
                            True  -> NN.fromNumber change
                            False -> (0 :: NN.Double)
                Just x  ->  let current' = NN.toNumber x
                        in case (change +  current') > 0 of
                            True    -> NN.fromNumber (change + current')
                            False   -> NN.fromNumber current'

------------------------------------------------------------------
--  物量簿記を通常簿記に変換する
event wld t ToPrice = readSTRef (_prices wld) >>= \pt
              -> modifySTRef (_book wld) $ \x
              -> EA.bar $ EA.transferKeepWiledcard (EA.bar x)
                        $ toTransTable pt


-- | Transaction全体を結合
eventAll :: forall s.  World s -> Term ->  ST s ()
eventAll wld t
    = CM.forM_ ([minBound .. maxBound] :: [EventName])
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
    | t >= lastTerm =  eventAll wld t
    | otherwise     =  eventAll wld t
                    >> loop wld (nextTerm t)

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
    print $ (.-) $ proj [HatNot :<((.#),(.#),1,(.#),(.#))] bk
    writeBS "exsample/result/csv/ssex1.csv" $ EA.grossProfitTransferKeepWiledcard bk



