
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
import              RippleEffect
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
import System.Random -- 乱数
import Control.Concurrent.Async (mapConcurrently,forConcurrently_)



-- Debug
import Debug.Trace



------------------------------------------------------------------
-- * directories

fig_dir = "exsample/deterministic/ripple/result/fig/withoutStock/"
csv_dir = "exsample/deterministic/ripple/result/csv/withoutStock/"

-- 状態系の定義
-- deriving Generic をしていれば
-- 空のインスタンス宣言で自動でinitSS,updateSSが使えるようになる
instance StateSpace Term InitVar EventName World s where
    event = event'

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
                             ,_stockOutRate        = 0.1
                             ,_materialOutRate     = 0.1
                             ,_addedDemand         = 0
                             ,_addedDemandTerm     = 20
                             ,_addedTo             = 9
                             ,_finalDemand         = 300
                             ,_finalDemandSector   = 10
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
        let fs = [getTermProduction Amount
                 ,getTermStock Amount
                 ,getTermProfit
                 ,getTermSales Amount
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
                ESV.plotWldsDiffLine (getTermProduction Amount)
                         (fstEnt,lastEnt -1)
                         ((resMap Map.! n),(resMap Map.! "default"))
                         (fig_dir ++ n ++ "/")
                         "Difference in production volume"

                ESV.plotMultiLines ["added","normal"]
                       [getTermProduction Amount,getTermProduction Amount]
                       (fstEnt,lastEnt -1)
                       [(resMap Map.! n),(resMap Map.! "default")]
                       (fig_dir ++ n ++ "/")
                       "Comparison of production volume"
        else return ()