
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
import System.Random
import Control.Concurrent.Async (mapConcurrently,forConcurrently_)

-- Debug
import Debug.Trace



------------------------------------------------------------------
-- * directories

fig_dir = "exsample/deterministic/ripple/result/fig/withStock/"

------------------------------------------------------------------

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
        -- 初期で総需要が在庫を上回った場合の対策
        when (t == initTerm) $ do
            finalDemand <- getOrder wld t Relation {_supplier = e1, _customer = finalDemandSector}
            c <- getInputCoefficient wld t e1 e1
            order wld t Relation {_supplier = e1, _customer = e1} (c * finalDemand)

        -- 受注総量を取得
        totalOrder <- getOrderTotal wld t e1


        -- 在庫保有量
        stock <- getTermStock Amount wld t e1

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
        as <- getAppropriateStock wld t e1
        -- 販売後のストックの不足分
        short <- getOrderTotal wld t e1
        -- 在庫保有量
        stock <- getTermStock Amount wld t e1
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
        as <- getAppropriateStock wld t e1
        -- 販売後のストックの不足分
        short <- getOrderTotal wld t e1
        -- 在庫保有量
        stock <- getTermStock Amount wld t e1
        -- 今期の生産量
        prod <- getTermProduction Amount wld t e1

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
                ms <- getTermMaterial Amount wld t e1 e2
                -- 投入係数
                c <- getInputCoefficient wld t e1 e2
                -- 安全原材料在庫
                am <- getAppropriateMaterial wld t e1 e2

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
                             ,_addedDemandTerm     = 20
                             ,_addedTo             = 9
                             ,_finalDemand         = 300
                             ,_finalDemandSector   = 10
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
        else if n == "largestock-added"
            then do
                ESV.plotWldsDiffLine (getTermProduction Amount)
                         (fstEnt,lastEnt -1)
                         ((resMap Map.! n),(resMap Map.! "largestock"))
                         (fig_dir ++ n ++ "/")
                         "Difference in production volume"

                ESV.plotMultiLines ["added","normal"]
                       [getTermProduction Amount,getTermProduction Amount]
                       (fstEnt,lastEnt -1)
                       [(resMap Map.! n),(resMap Map.! "largestock")]
                       (fig_dir ++ n ++ "/")
                       "Comparison of production volume"
        else if n == "smallstock-added"
            then do
                ESV.plotWldsDiffLine (getTermProduction Amount)
                         (fstEnt,lastEnt -1)
                         ((resMap Map.! n),(resMap Map.! "smallstock"))
                         (fig_dir ++ n ++ "/")
                         "Difference in production volume"

                ESV.plotMultiLines ["added","normal"]
                       [getTermProduction Amount,getTermProduction Amount]
                       (fstEnt,lastEnt -1)
                       [(resMap Map.! n),(resMap Map.! "smallstock")]
                       (fig_dir ++ n ++ "/")
                       "Comparison of production volume"
        else return ()