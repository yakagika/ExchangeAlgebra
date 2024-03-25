
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns         #-} -- for initSTWorld
{-# LANGUAGE RecordWildCards        #-} -- for initSTWorld
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}

{-
状態空間による会計シミュレーションサンプル
4エージェントがこの取引を毎期繰り返すのみの尤も単純な形式
労働者等も存在しない.

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
    initialize _ = initBook

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

type VETransTable = EA.TransTable NN.Double VEHatBase
type Prices s = STArray s Term VETransTable

-- 価格の初期状態
-- 今回は期間別の1財1価 ,生産者別の価格,取引別の価格は取引idなどを基底に持たせる必要がある.
initPrice :: Term -> VETransTable
initPrice t = EA.table
            $  (Hat:<(Products,"a",(.#),t,Amount)) .-> (Hat:<(Products,"a",(.#),t,Yen)) |% (*2) -- 値段2円(個数×2)
            ++ (Not:<(Products,"a",(.#),t,Amount)) .-> (Not:<(Products,"a",(.#),t,Yen)) |% (*2)
            ------------------------------------------------------------------
            ++ (Hat:<(Products,"b",(.#),t,Amount)) .-> (Hat:<(Products,"b",(.#),t,Yen)) |% (*3) -- 3円
            ++ (Not:<(Products,"b",(.#),t,Amount)) .-> (Not:<(Products,"b",(.#),t,Yen)) |% (*3)
            ------------------------------------------------------------------
            ++ (Hat:<(Products,"c",(.#),t,Amount)) .-> (Hat:<(Products,"c",(.#),t,Yen)) |% (*4)
            ++ (Not:<(Products,"c",(.#),t,Amount)) .-> (Not:<(Products,"c",(.#),t,Yen)) |% (*4)
            ------------------------------------------------------------------
            ++ (Hat:<(Products,"d",(.#),t,Amount)) .-> (Hat:<(Products,"d",(.#),t,Yen)) |% (*5)
            ++ (Not:<(Products,"d",(.#),t,Amount)) .-> (Not:<(Products,"d",(.#),t,Yen)) |% (*5)

-- 価格履歴の初期状態
initPrices :: ST s (Prices s)
initPrices  = newListArray (initTerm, lastTerm)
              [initPrice t | t <- [initTerm .. lastTerm]]

-- ** STArray s (ID, Term) NN.Double
-- 価格は固定
instance Updatable Term ST s (Prices s) where
    initialize _ = initPrices
    updatePattern _ _ = return DoNothing

------------------------------------------------------------------
-- 状態空間の定義
-- 会計と価格のみの世界
data World s = World { _book    :: Book s
                     , _prices  :: Prices s }
                     deriving (Generic)

-- deriving Generic をしていれば
-- 空のインスタンス宣言で自動でinitSS,updateSSが使えるようになる
instance StateSpace Term ST s (World s)


------------------------------------------------------------------
-- * 状態の更新
------------------------------------------------------------------
-- ** イベントの設定
-- 同じ取引を繰り返すだけ

-- | Transaction の種類
data EventType
    = Production                        -- ^ 保有する中間投入財を使用して生産
    | BuySell                           -- ^ 購入販売
    deriving (Show, Eq, Enum, Ord, Ix, Bounded)

-- eventの一般化
event :: World s -> EventType -> Term -> ST s (Book s)
------------------------------------------------------------------
-- 何も購入していない場合は何も中間消費がない
-- 記録において購入したものを中間消費して販売量と同量生産する
-- 在庫概念,資本制約も労働制約もない
event wld Production t = newSTRef
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
event wld BuySell t = newSTRef
                $ 1 .@ Hat :<(Products,"a",1,t,Amount) .+ 2 .@ Not :<(Cash,(.#),1,t,Yen)
               .+ 1 .@ Not :<(Products,"a",2,t,Amount) .+ 2 .@ Hat :<(Cash,(.#),2,t,Yen)
               .+ 1 .@ Hat :<(Products,"b",1,t,Amount) .+ 3 .@ Not :<(Cash,(.#),1,t,Yen)
               .+ 1 .@ Not :<(Products,"b",2,t,Amount) .+ 3 .@ Hat :<(Cash,(.#),2,t,Yen)
               .+ 1 .@ Hat :<(Products,"a",1,t,Amount) .+ 2 .@ Not :<(Cash,(.#),1,t,Yen)
               .+ 1 .@ Not :<(Products,"a",3,t,Amount) .+ 2 .@ Hat :<(Cash,(.#),3,t,Yen)
               .+ 1 .@ Hat :<(Products,"c",3,t,Amount) .+ 4 .@ Not :<(Cash,(.#),3,t,Yen)
               .+ 1 .@ Not :<(Products,"c",2,t,Amount) .+ 4 .@ Hat :<(Cash,(.#),2,t,Yen)
               .+ 1 .@ Hat :<(Products,"d",3,t,Amount) .+ 5 .@ Not :<(Cash,(.#),3,t,Yen)
               .+ 1 .@ Not :<(Products,"d",2,t,Amount) .+ 5 .@ Hat :<(Cash,(.#),2,t,Yen)
               .+ 1 .@ Hat :<(Products,"d",3,t,Amount) .+ 5 .@ Not :<(Cash,(.#),3,t,Yen)
               .+ 1 .@ Not :<(Products,"d",4,t,Amount) .+ 5 .@ Hat :<(Cash,(.#),4,t,Yen)
------------------------------------------------------------------
-- 仕訳の一般化 (未実装)
-- journal  World s -> TransactionType -> Term -> ST s ()



-- イベントごとに仕訳をしないと正しく記録されない
input :: EventType -> Term -> World s -> ST s ()
input tt t wld =  event wld tt t >>= \stbk
                    -> readSTRef stbk >>= \bk
                    -> case bk of
                        Zero -> return ()
                        _    -> modifySTRef (_book wld) (\x -> x .+ bk)

-- | Transaction全体を結合
inputEveryEvent :: forall s. Term -> World s ->  ST s ()
inputEveryEvent t wld
    = CM.forM_ xs $ \tt -> input tt t wld
    where
    xs =    [ Production
            , BuySell]

--  物量簿記を通常簿記に変換する
toPrice :: Term -> World s -> ST s ()
toPrice t wld =  readArray (_prices wld) t >>= \pt
              -> modifySTRef (_book wld) $ \x
              -> EA.bar $ EA.transferKeepWiledcard (EA.bar x) pt


------------------------------------------------------------------
-- * Simulation
------------------------------------------------------------------
-- 順番にイベントをこなす
culcSingleTerm :: World s -> Term -> ST s ()
culcSingleTerm wld t =  updateAll        t wld
                     >> inputEveryEvent  t wld
                     >> toPrice          t wld

culcTotalTerm :: World s -> ST s ()
culcTotalTerm wld = loop wld initTerm
  where
  {-# INLINE loop #-}
  loop ::  World s -> Term -> ST s ()
  loop wld t
    | t >= lastTerm =  culcSingleTerm wld t
    | otherwise     =  culcSingleTerm wld t
                    >> loop wld (nextTerm t)


------------------------------------------------------------------
-- * 実行
------------------------------------------------------------------
main :: IO ()
main = do
    wld <- stToIO $ initAll (1 :: Term) >>= \wld' -> do
                  culcTotalTerm wld'
                  return wld'
    bk <- stToIO $ readSTRef (_book wld)
    print $ (.-) $ proj [HatNot :<((.#),(.#),1,(.#),(.#))] bk
    writeBS "exsample/result/csv/ssex1.csv" $ EA.grossProfitTransferKeepWiledcard bk



