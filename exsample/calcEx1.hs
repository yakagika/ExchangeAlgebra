{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE OverloadedStrings #-}

{-
基底を定義して会計計算を行うサンプル
-}




import qualified    ExchangeAlgebra         as EA
import              ExchangeAlgebra

import qualified    Number.NonNegative      as NN
import qualified    Numeric                 as N
import              Number.NonNegative

import qualified    Data.Map.Strict         as M
import qualified    Data.Text               as T

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

-- 商品名
type CommodityName = T.Text

type Term = ID

------------------------------------------------------------------
-- ** ExBase
------------------------------------------------------------------
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

-- | 取引情報 一対の取引
type Transaction = EA.Alg NN.Double VEHatBase

------------------------------------------------------------------
-- * Sample Transaction
------------------------------------------------------------------
{-
\begin{align*}
flow[q] & =  x~\hat{} \langle a,1,q,Qua \rangle  + x'         \langle Cash,1,q,Yen \rangle  \\
        & + x         \langle a,2,q,Qua \rangle  + x'~\hat{}  \langle Cash,2,q,Yen \rangle  \\
        & + y~\hat{}  \langle b,1,q,Qua \rangle  + y'         \langle Cash,1,q,Yen \rangle  \\
        & + y         \langle b,2,q,Qua \rangle  + y'~\hat{}  \langle Cash,2,q,Yen \rangle  \\
        & + z~\hat{}  \langle a,1,q,Qua \rangle  + z'         \langle Cash,1,q,Yen \rangle  \\
        & + z         \langle a,3,q,Qua \rangle  + z'~\hat{}  \langle Cash,3,q,Yen \rangle  \\
        & + w~\hat{}  \langle c,3,q,Qua \rangle  + w'         \langle Cash,3,q,Yen \rangle  \\
        & + w         \langle c,2,q,Qua \rangle  + w'~\hat{}  \langle Cash,2,q,Yen \rangle  \\
        & + s~\hat{}  \langle d,3,q,Qua \rangle  + s'         \langle Cash,3,q,Yen \rangle  \\
        & + s         \langle d,2,q,Qua \rangle  + s'~\hat{}  \langle Cash,2,q,Yen \rangle  \\
        & + t~\hat{}  \langle d,3,q,Qua \rangle  + t'         \langle Cash,3,q,Yen \rangle  \\
        & + t         \langle d,4,q,Qua \rangle  + t'~\hat{}  \langle Cash,4,q,Yen \rangle
\end{align*}
-}

flow  :: Transaction
flow  =  1 .@ Hat :<(Products,"a",1,1,Amount) .+ 1 .@ Not :<(Cash,(.#),1,1,Yen)
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


-- toPriceの実装
-- MapやArrayでもいいが
-- 交換代数のTransfer (振替)として実装する

toPrice :: Transaction -> Transaction
toPrice ts
    =  EA.transferKeepWiledcard ts
    $  EA.table
    $  (Hat:<(Products,"a",(.#),1,Amount)) .-> (Not:<(Sales,(.#),(.#),1,Yen))     |% (*2) -- 値段2円(個数×2)
    ++ (Not:<(Products,"a",(.#),1,Amount)) .-> (Not:<(Purchases,(.#),(.#),1,Yen)) |% (*2)
    ------------------------------------------------------------------
    ++ (Hat:<(Products,"b",(.#),1,Amount)) .-> (Not:<(Sales,(.#),(.#),1,Yen))     |% (*3) -- 3円
    ++ (Not:<(Products,"b",(.#),1,Amount)) .-> (Not:<(Purchases,(.#),(.#),1,Yen)) |% (*3)
    ------------------------------------------------------------------
    ++ (Hat:<(Products,"c",(.#),1,Amount)) .-> (Not:<(Sales,(.#),(.#),1,Yen))     |% (*4)
    ++ (Not:<(Products,"c",(.#),1,Amount)) .-> (Not:<(Purchases,(.#),(.#),1,Yen)) |% (*4)
    ------------------------------------------------------------------
    ++ (Hat:<(Products,"d",(.#),1,Amount)) .-> (Not:<(Sales,(.#),(.#),1,Yen))     |% (*5)
    ++ (Not:<(Products,"d",(.#),1,Amount)) .-> (Not:<(Purchases,(.#),(.#),1,Yen)) |% (*5)

purchace :: Transaction -> VEHatBase -> Transaction
purchace ex b = bar $ toPrice $ proj [b] ex

main :: IO ()
main = do
    print $ proj [Not:<(Products,(.#),2,(.#),(.#))] flow
    print $ toPrice $ proj [HatNot:<(Products,(.#),2,(.#),(.#))] flow
    print $ norm $ purchace flow (Not:<(Products,"a",(.#),1,Amount))

