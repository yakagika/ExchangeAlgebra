
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE OverloadedStrings #-}


{-
状態空間による会計シミュレーションサンプル
-}

import qualified    ExchangeAlgebra         as EA
import              ExchangeAlgebra

import qualified    Number.NonNegative      as NN
import qualified    Numeric                 as N
import              Number.NonNegative

import qualified    Data.Map.Strict         as M
import qualified    Data.Text               as T

import qualified    Control.Monad                   as CM
import              Control.Monad
import              Control.Monad.ST
import              Data.Array.ST


------------------------------------------------------------------
-- * Exchange Algebra
------------------------------------------------------------------

------------------------------------------------------------------
-- ** ExBase Elements
------------------------------------------------------------------
-- ExBase Elementをインスタンス宣言する
-- wiledcardのみ指定すればOK
type ID = Prelude.Int

instance EA.Element ID where
    wiledcard = -1

-- A set of transaction entity index
type Entity = ID

--
type CommodityName = T.Text

type Period = ID

------------------------------------------------------------------
-- ** ExBase
------------------------------------------------------------------
-- ExBaseをインスタンス宣言する
-- 会計勘定科目の位置のみ指定すればOK

type VEHatBase = EA.HatBase (EA.AccountTitles, CommodityName, Entity, Period, EA.CountUnit)

instance ExBaseClass VEHatBase where
    getAccountTitle (h :< (accountTitle, commodityName,entity,period,countUnit))
      = accountTitle

    setAccountTitle (h :< (accountTitle, commodityName,entity,period,countUnit)) b
      = h :< (b, commodityName,entity,period,countUnit)


------------------------------------------------------------------
-- *  状態系の導入
------------------------------------------------------------------
-- 期間の設定

-- | 開始
initTerm = 1
-- | 終了
lastTerm = 10

-- 簿記の状態空間の定義
-- | 取引情報 一対の取引
type Flow = EA.Alg NN.Double VEHatBase

initFlow :: Flow
initFlow    =  1 .@ Hat :<(Products,"a",1,1,Amount) .+ 1 .@ Not :<(Cash,(.#),1,1,Yen)
            .+ 1 .@ Not :<(Products,"a",2,1,Amount) .+ 1 .@ Hat :<(Cash,(.#),2,1,Yen)
            .+ 1 .@ Hat :<(Products,"b",1,1,Amount) .+ 1 .@ Not :<(Cash,(.#),1,1,Yen)
            .+ 1 .@ Not :<(Products,"b",2,1,Amount) .+ 1 .@ Hat :<(Cash,(.#),2,1,Yen)
            .+ 1 .@ Hat :<(Products,"a",1,1,Amount) .+ 1 .@ Not :<(Cash,(.#),1,1,Yen)
            .+ 1 .@ Not :<(Products,"a",3,1,Amount) .+ 1 .@ Hat :<(Cash,(.#),3,1,Yen)
            .+ 1 .@ Hat :<(Products,"c",3,1,Amount) .+ 1 .@ Not :<(Cash,(.#),3,1,Yen)
            .+ 1 .@ Not :<(Products,"c",2,1,Amount) .+ 1 .@ Hat :<(Cash,(.#),2,1,Yen)
            .+ 1 .@ Hat :<(Products,"d",3,1,Amount) .+ 1 .@ Not :<(Cash,(.#),3,1,Yen)
            .+ 1 .@ Not :<(Products,"d",2,1,Amount) .+ 1 .@ Hat :<(Cash,(.#),2,1,Yen)
            .+ 1 .@ Hat :<(Products,"d",3,1,Amount) .+ 1 .@ Not :<(Cash,(.#),3,1,Yen)
            .+ 1 .@ Not :<(Products,"d",4,1,Amount) .+ 1 .@ Hat :<(Cash,(.#),4,1,Yen)

-- 環境変数の定義(価格のみ)
type VETransTable = EA.TransTable NN.Double VEHatBase
type Price s = STArray s Period VETransTable
initPrice :: VETransTable
initPrice   = EA.table
            $  (Hat:<(Products,"a",(.#),1,Amount)) .-> (Hat:<(Cash,(.#),(.#),1,Yen)) |% (*2) -- 値段2円(個数×2)
            ++ (Not:<(Products,"a",(.#),1,Amount)) .-> (Not:<(Cash,(.#),(.#),1,Yen)) |% (*2)
            ------------------------------------------------------------------
            ++ (Hat:<(Products,"b",(.#),1,Amount)) .-> (Hat:<(Cash,(.#),(.#),1,Yen)) |% (*3) -- 3円
            ++ (Not:<(Products,"b",(.#),1,Amount)) .-> (Not:<(Cash,(.#),(.#),1,Yen)) |% (*3)
            ------------------------------------------------------------------
            ++ (Hat:<(Products,"c",(.#),1,Amount)) .-> (Hat:<(Cash,(.#),(.#),1,Yen)) |% (*4)
            ++ (Not:<(Products,"c",(.#),1,Amount)) .-> (Not:<(Cash,(.#),(.#),1,Yen)) |% (*4)
            ------------------------------------------------------------------
            ++ (Hat:<(Products,"d",(.#),1,Amount)) .-> (Hat:<(Cash,(.#),(.#),1,Yen)) |% (*5)
            ++ (Not:<(Products,"d",(.#),1,Amount)) .-> (Not:<(Cash,(.#),(.#),1,Yen)) |% (*5)

-- 空間の定義
data World s = World { _transaction :: Flow
                     , _price       :: Price s } 

type STWorld s = ST s (World s)

main :: IO ()
main = do
    print "Hi"

