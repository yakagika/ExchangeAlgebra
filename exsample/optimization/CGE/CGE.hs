
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
Module for Ripple Effect Analysis
-}

module RippleEffect where

-- Original
import              ExchangeAlgebraJournal
import qualified    ExchangeAlgebra.Journal  as EJ
import qualified    ExchangeAlgebra.Journal.Transfer as EJT
import              ExchangeAlgebra.Journal.Transfer ((.->)
                                                        ,(|%))

import qualified    ExchangeAlgebra.Simulate as ES
import qualified    ExchangeAlgebra.Simulate
import qualified    ExchangeAlgebra.Simulate.Visualize as ESV


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

data InitVar = InitVar {_initStock                             :: Double                             -- ^ 初期在庫保有量 (今回は0)
                       ,_initDirectTaxRate                     :: Double                             -- ^ 直接税率
                       ,_initIndirectTaxRate                   :: Double                             -- ^ 生産税率
                       ,_initImportTariffRate                  :: Double                             -- ^ 輸入関税率
                       ,_initForeignSaving                     :: Double                             -- ^ 外国貯蓄額
                       ,_initExchangeRate                      :: Double                             -- ^ 為替レート
                       ,_initHouseholdSavingShare              :: Double                             -- ^ 家計の平均貯蓄性向
                       ,_initGovernmentSavingShare             :: Double                             -- ^ 政府の平均貯蓄性向
                       ,_initPrice                             :: Map.Map (Term, Product) Double     -- ^ 価格
                       ,_initIOCoefficients                    :: Map.Map (Product, Product) Double  -- ^ 投入係数
                       ,_initGovernmentConsumptionShare        :: Map.Map Product Double             -- ^ 政府消費総額に占める第i財の割合
                       ,_initInvestmentShare                   :: Map.Map Product Double             -- ^ 投資総額に占める第i財の割合
                       ,_initImportTariffRateByProduct         :: Map.Map Product Double             -- ^ 第 i 輸入財に対する輸入関税率
                       ,_initScaleCoefficient                  :: Map.Map Product Double             -- ^ 第 i 合成財生産関数の規模係数
                       ,_initInputShareCoefficientM            :: Map.Map Product Double             -- ^ 第 i 合成財生産関数の投入割合係数（輸入財）
                       ,_initInputShareCoefficientD            :: Map.Map Product Double             -- ^ 第 i 合成財生産関数の投入割合係数（国内財）
                       ,_initSubstitutionElasticityCoefficient :: Map.Map Product Double             -- ^ 代替の弾力性に関する係数
                       ,_initSubstitutionElasticity            :: Map.Map Product Double             -- ^ 第 i 合成財生産関数の代替の弾力性
                       ,_initTransformationScaleCoefficient    :: Map.Map Product Double             -- ^ θ_i : 第 i 変形関数の規模係数
                       ,_initOutputShareCoefficientE           :: Map.Map Product Double             -- ^ 第 i 変形関数の産出割合係数（輸出）
                       ,_initOutputShareCoefficientD           :: Map.Map Product Double             -- ^ 第 i 変形関数の産出割合係数（国内財）
                       } deriving (Eq, Show)

instance InitVariables InitVar where

------------------------------------------------------------------
-- ** キャリブレーション
-- 社会会計行列からパラメータを推定する
------------------------------------------------------------------


------------------------------------------------------------------
-- ** 簿記の状態空間の定義
------------------------------------------------------------------
-- ExBase Elementをインスタンス宣言する
-- wiledcardのみ指定すればOK

-- 取引主体ID
-- 一つが最終需要部門
data Entity = BRDIND    -- ^ BREAD INDUSTRIES
            | MLKIND    -- ^ MILK INDUSTRIES
            | HOH       -- ^ HOUSEHOLD
            | GOV       -- ^ GOVERNMENT
            | INV       -- ^ INVESTMENT
            | EXT       -- ^ EXPORT
            | Entity    -- ^ WILD CARD
    deriving (Eq, Show, Ord, Enum, Bounded, Ix, Generic)

isConsumer :: Entity -> Bool
isConsumer e = case e of
    GOV -> True
    INV -> True
    EXT -> True
    _   -> False

instance Hashable Entity where

instance Element Entity where
    wiledcard = Entity

instance BaseClass Entity where

-- 最小
fstEnt  = BRDIND
-- 最大 (最終需要部門)
lastEnt = EXT

entities = [fstEnt .. lastEnt]

-- | Product
data Product = BRDD     -- ^ BREAD DOMESTIC
             | MLKD     -- ^ MILK DOMESTIC
             | BRDF     -- ^ BREAD FOREIGN
             | MLKF     -- ^ MILK FOREIGN
             | BRDC     -- ^ BREAD COMPOSITE
             | MLKC     -- ^ MILK COMPOSITE
             | CAP      -- ^ CAPITAL
             | LAB      -- ^ LABOR
             | Product  -- ^ WILD CARD
            deriving (Eq, Show, Ord, Enum, Bounded, Ix, Generic)

instance Hashable Product where

instance Element Product where
    wiledcard = Product

instance BaseClass Product where

fstProd = BRDD
lastProd = LAB

products :: [Product]
products = [fstProd .. lastProd]

produceWhat :: Entity -> [Product] 
produceWhat e  = case e of
    BRDIND -> [BRDD]
    MLKIND -> [MLKD]
    EXT    -> [BRDF, MLKF]
    HOH    -> [CAP, LAB]
    _      -> []

producedBy :: Product -> Entity
producedBy p = case p of
    BRDD -> BRDIND
    MLKD -> MLKIND
    BRDF -> EXT
    MLKF -> EXT
    CAP  -> HOH
    LAB  -> HOH
    _    -> Entity

-- 期
type Term = Prelude.Int

-- ExBaseをインスタンス宣言する
-- 会計勘定科目の位置のみ指定すればOK

type VEHatBase = HatBase ( EJ.AccountTitles
                         , Product
                         , Entity
                         , EJ.CountUnit)

instance ExBaseClass VEHatBase where
    getAccountTitle (h :< (a,c,e,u))   = a
    setAccountTitle (h :< (a,c,e,u)) b = h :< (b,c,e,u)


-- | Event の種類
-- 上から順番にイベントが実行される
data EventName
    = ToAmount                          -- ^ 価格から物量評価へ変換
    | Production                        -- ^ 生産
    | PriceDetermination                -- ^ 財価格決定
    | SavingAndInvestment               -- ^ 貯蓄及び投資
    | SalesPurchase                     -- ^ 販売購入
    | ElementPriceDetermination         -- ^ 要素価格決定
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
    | isConsumer e = Zero -- 最終需要部門
    | otherwise    = stock .+ rowMaterial
    where 
        stock = EJ.fromList [ d :@ Not :<(Products,c,e,Amount) .| (plank,initTerm)
                            | c <- produceWhat e]
        rowMaterial = EJ.fromList [ (d/10) :@ Not :<(Products,c,e,Amount) .| (plank,initTerm)
                                  | c <- products]

initLedger :: Double -> ST s (Ledger s)
initLedger d = newURef (EJ.fromList [ initTransaction c d
                                     | c <- [fstEnt..lastEnt]])

-- 一般化の適用
instance Updatable Term InitVar Ledger s where
    type Inner Ledger s = STRef s Transaction
    unwrap (Ledger a) = a

    initialize _ _ e = initLedger (_initStock e)

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
-- ** 直接税率
------------------------------------------------------------------

-- | 直接税率
type DirectTaxRate = Double

-- | 直接税率の状態系
newtype DirectTaxRates s = DirectTaxRates (STRef s DirectTaxRate)


instance UpdatableSTRef DirectTaxRates s DirectTaxRate where
   _unwrapURef (DirectTaxRates x) = x
   _wrapURef x = (DirectTaxRates x)

-- 直接税率は固定
instance Updatable Term InitVar DirectTaxRates s where
    type Inner DirectTaxRates s = STRef s DirectTaxRate
    unwrap (DirectTaxRates a) = a
    initialize _ _ v = newURef (_initDirectTaxRate v)
    updatePattern _ = return DoNothing

-- | 直接税率の取得
getDirectTaxRate :: World s -> Term -> ST s DirectTaxRate
getDirectTaxRate wld t = readURef (_directTaxRates wld) >>= \dtr
                         -> return dtr


------------------------------------------------------------------
-- ** 生産税率
------------------------------------------------------------------

-- | 生産税率
type IndirectTaxRate = Double
newtype IndirectTaxRates s = IndirectTaxRates (STRef s IndirectTaxRate)

instance UpdatableSTRef IndirectTaxRates s IndirectTaxRate where
   _unwrapURef (IndirectTaxRates x) = x
   _wrapURef x = (IndirectTaxRates x)

-- 生産税率は固定
instance Updatable Term InitVar IndirectTaxRates s where
    type Inner IndirectTaxRates s = STRef s IndirectTaxRate
    unwrap (IndirectTaxRates a) = a
    initialize _ _ v = newURef (_initIndirectTaxRate v)
    updatePattern _ = return DoNothing

-- | 生産税率の取得
getIndirectTaxRate :: World s -> Term -> ST s IndirectTaxRate
getIndirectTaxRate wld t = readURef (_indirectTaxRates wld) >>= \ptr
                         -> return ptr

------------------------------------------------------------------
-- ** 輸入関税率
------------------------------------------------------------------

-- | 輸入関税率
type ImportTariffRate = Double
newtype ImportTariffRates s = ImportTariffRates (STRef s ImportTariffRate)


instance UpdatableSTRef ImportTariffRates s ImportTariffRate where
   _unwrapURef (ImportTariffRates x) = x
   _wrapURef x = (ImportTariffRates x)

-- 輸入関税率は固定
instance Updatable Term InitVar ImportTariffRates s where
    type Inner ImportTariffRates s = STRef s ImportTariffRate
    unwrap (ImportTariffRates a) = a
    initialize _ _ v = newURef (_initImportTariffRate v)
    updatePattern _ = return DoNothing

-- | 輸入関税率の取得
getImportTariffRate :: World s -> Term -> ST s ImportTariffRate
getImportTariffRate wld t = readURef (_importTariffRates wld) >>= \itr
                         -> return itr

------------------------------------------------------------------
-- ** 外国貯蓄額
------------------------------------------------------------------

-- | 外国貯蓄額
type ForeignSaving = Double
newtype ForeignSavings s = ForeignSavings (STRef s ForeignSaving)

instance UpdatableSTRef ForeignSavings s ForeignSaving where
   _unwrapURef (ForeignSavings x) = x
   _wrapURef x = (ForeignSavings x)

-- 外国貯蓄額は固定
instance Updatable Term InitVar ForeignSavings s where
    type Inner ForeignSavings s = STRef s ForeignSaving
    unwrap (ForeignSavings a) = a
    initialize _ _ v = newURef (_initForeignSaving v)
    updatePattern _ = return DoNothing

-- | 外国貯蓄額の取得
getForeignSaving :: World s -> Term -> ST s ForeignSaving
getForeignSaving wld t = readURef (_foreignSavings wld) >>= \fs
                         -> return fs

------------------------------------------------------------------
-- ** 為替レート
------------------------------------------------------------------

-- | 為替レート
type ExchangeRate = Double
newtype ExchangeRates s = ExchangeRates (STRef s ExchangeRate)


instance UpdatableSTRef ExchangeRates s ExchangeRate where
   _unwrapURef (ExchangeRates x) = x
   _wrapURef x = (ExchangeRates x)

-- 為替レートは固定
instance Updatable Term InitVar ExchangeRates s where
    type Inner ExchangeRates s = STRef s ExchangeRate
    unwrap (ExchangeRates a) = a
    initialize _ _ v = newURef (_initExchangeRate v)
    updatePattern _ = return DoNothing

-- | 為替レートの取得
getExchangeRate :: World s -> Term -> ST s ExchangeRate
getExchangeRate wld t = readURef (_exchangeRates wld) >>= \er
                         -> return er

------------------------------------------------------------------
-- ** 家計の平均貯蓄性向
------------------------------------------------------------------

-- | 家計の平均貯蓄性向
type HouseholdSavingShare = Double
newtype HouseholdSavings s = HouseholdSavings (STRef s HouseholdSavingShare)    

instance UpdatableSTRef HouseholdSavings s HouseholdSavingShare where
   _unwrapURef (HouseholdSavings x) = x
   _wrapURef x = (HouseholdSavings x)

-- 家計の平均貯蓄性向は固定
instance Updatable Term InitVar HouseholdSavings s where
    type Inner HouseholdSavings s = STRef s HouseholdSavingShare
    unwrap (HouseholdSavings a) = a
    initialize _ _ v = newURef (_initHouseholdSavingShare v)
    updatePattern _ = return DoNothing

-- | 家計の平均貯蓄性向の取得
getHouseholdSavingShare :: World s -> Term -> ST s HouseholdSavingShare
getHouseholdSavingShare wld t = readURef (_householdSavings wld) >>= \hss
                         -> return hss

------------------------------------------------------------------
-- ** 政府の平均貯蓄性向
------------------------------------------------------------------
-- | 政府の平均貯蓄性向
type GovernmentSavingShare = Double
newtype GovernmentSavings s = GovernmentSavings (STRef s GovernmentSavingShare)   

instance UpdatableSTRef GovernmentSavings s GovernmentSavingShare where
   _unwrapURef (GovernmentSavings x) = x
   _wrapURef x = (GovernmentSavings x)

-- 政府の平均貯蓄性向は固定
instance Updatable Term InitVar GovernmentSavings s where
    type Inner GovernmentSavings s = STRef s GovernmentSavingShare
    unwrap (GovernmentSavings a) = a
    initialize _ _ v = newURef (_initGovernmentSavingShare v)
    updatePattern _ = return DoNothing  

-- | 政府の平均貯蓄性向の取得
getGovernmentSavingShare :: World s -> Term -> ST s GovernmentSavingShare
getGovernmentSavingShare wld t = readURef (_governmentSavings wld) >>= \gss
                         -> return gss


------------------------------------------------------------------
-- ** 価格
------------------------------------------------------------------

-- | 販売価格のテーブル
type Price = Double
type PriceTable = M.Map (Term,Product) Price
newtype Prices s = Prices (STRef s PriceTable)

instance UpdatableSTRef Prices s PriceTable where
   _unwrapURef (Prices x) = x
   _wrapURef x = (Prices x)

-- 価格履歴の初期状態
initPrices :: InitVar -> ST s (Prices s)
initPrices v = do
    let priceMap = _initPrice v
    newURef $ M.fromList [((t, c), Map.findWithDefault 1 (t, c) priceMap)
                         | t <- [initTerm .. lastTerm]
                         , c <- [fstProd .. lastProd]]

-- ** STArray s (ID, Term) Double
-- 価格は固定
instance Updatable Term InitVar Prices s where
    type Inner Prices s = STRef s PriceTable
    unwrap (Prices a) = a
    initialize _ _ v = initPrices v
    updatePattern _ = return DoNothing

type VETransTable = EJT.TransTable Double VEHatBase

-- | 価格テーブルから物量→価格評価への変換テーブルを作成
toCashTable :: PriceTable -> VETransTable
toCashTable pt = EJT.table
                $ concatMap (\((_, c), p) -> f c p) (M.toList pt)
    where
        {-# INLINE f #-}
        f :: Product -> Double -> [(VEHatBase,VEHatBase, Double -> Double)]
        f c p =  (HatNot:<(Products,c,(.#),Amount)) .-> (HatNot:<(Products,c,(.#),Yen)) |% (*p)


-- | 価格テーブルから価格→物量評価への変換テーブルを作成
toAmountTable :: PriceTable -> VETransTable
toAmountTable pt = EJT.table
                $ concatMap (\((_, c), p) -> f c p) (M.toList pt)
    where
        {-# INLINE f #-}
        f :: Product -> Double -> [(VEHatBase,VEHatBase, Double -> Double)]
        f c p =  (HatNot:<(Products,c,(.#),Yen)).-> (HatNot:<(Products,c,(.#),Amount)) |% (/p)

-- | 価格の取得
getPrice :: World s -> Term -> Product -> ST s Price
getPrice wld t c =  readURef (_prices wld) >>= \pt
                 -> case M.lookup (t,c) pt of
                    Nothing -> return 0
                    Just x  -> return x

------------------------------------------------------------------
-- ** 投入係数
------------------------------------------------------------------

-- 生産関数(投入係数行列)
-- 物量表記
type InputCoefficient = Double

type Col = Product
type Row = Product
-- |  -- (Term, Commodity1, Commodity2) の配列
newtype ICTable s = ICTable (STArray s (Term, Row, Col) InputCoefficient)

instance UpdatableSTArray ICTable s (Term, Row, Col) InputCoefficient where
  _unwrapUArray (ICTable arr) = arr
  _wrapUArray arr = ICTable arr

-- ** 生産関数の初期状態 (STArray を使用, Term ごとに固定)
initICTables :: InitVar -> ST s (ICTable s)
initICTables v = do
    let ioCoefficients = _initIOCoefficients v
    arr <- newUArray ((initTerm, fstProd, fstProd), (lastTerm, lastProd, lastProd)) 0  -- 初期値は0
    forM_ [(t, c2) | t <- [initTerm .. lastTerm], c2 <- [fstProd.. lastProd]] $ \(t, c2) -> do
        forM_ [fstProd..lastProd] $ \p1 -> do
            let p2 = toEnum (fromEnum c2) :: Product
                coef = M.findWithDefault 0 (p1, p2) ioCoefficients
            writeUArray arr (t, p1, p2) coef
    return arr


-- | 生産関数の更新
-- 前の期の簿記から計算する
-- ただし,今回は価格固定なので変化なし
instance Updatable Term InitVar ICTable s where
    type Inner ICTable s = STArray s (Term, Row, Col) InputCoefficient
    unwrap (ICTable a) = a
    initialize _ _ e = initICTables e
    updatePattern _ = return DoNothing

-- | 投入係数の取得
-- 1単位の e1 の生産に必要な e2
getInputCoefficient :: World s -> Term -> Row -> Col -> ST s InputCoefficient
getInputCoefficient wld t r c =  do
                let ics = (_ics wld)
                readUArray ics (t,r,c)

-- | 初期の投入係数行列の取得
-- 1期の投入係数行列を取得する
-- 最終需要を抜かした9*9
getInputCoefficients :: World RealWorld -> (Row,Col) -> IO (IOArray (Row,Col) Double)
getInputCoefficients wld (r,c) = do
    let arr = (_ics wld)
    result <- newArray ((r, r), (c, c)) 0
    forM_ [r .. c] $ \r ->
        forM_ [r .. c] $ \c -> do
            v <- stToIO $ readUArray arr (1,r,c)
            writeArray result (r,c) v
    return result


------------------------------------------------------------------
-- ** 政府消費総額に占める第i財の割合
------------------------------------------------------------------

type GovernmentConsumptionShare = Double
newtype GovernmentConsumptionShares s = GovernmentConsumptionShares (STArray s (Term, Product) GovernmentConsumptionShare)

instance UpdatableSTArray GovernmentConsumptionShares s (Term, Product) GovernmentConsumptionShare where
  _unwrapUArray (GovernmentConsumptionShares arr) = arr
  _wrapUArray arr = GovernmentConsumptionShares arr

-- | 政府消費総額に占める第i財の割合の初期状態
initGovernmentConsumptionShares :: InitVar -> ST s (GovernmentConsumptionShares s)
initGovernmentConsumptionShares v = do
    let governmentConsumptionShares = _initGovernmentConsumptionShare v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let share = Map.findWithDefault 0 p governmentConsumptionShares
        writeUArray arr (t, p) share
    return arr

-- 政府消費総額に占める第i財の割合は固定
instance Updatable Term InitVar GovernmentConsumptionShares s where
    type Inner GovernmentConsumptionShares s = STArray s (Term, Product) GovernmentConsumptionShare
    unwrap (GovernmentConsumptionShares a) = a
    initialize _ _ v = initGovernmentConsumptionShares v
    updatePattern _ = return DoNothing

-- | 政府消費総額に占める第i財の割合の取得
getGovernmentConsumptionShare :: World s -> Term -> Product -> ST s GovernmentConsumptionShare
getGovernmentConsumptionShare wld t p = do
    let arr = (_governmentConsumptionShares wld)
    readUArray arr (t, p)

------------------------------------------------------------------
-- ** 投資総額に占める第i財の割合
------------------------------------------------------------------

type InvestmentShare = Double
newtype InvestmentShares s = InvestmentShares (STArray s (Term, Product) InvestmentShare)

instance UpdatableSTArray InvestmentShares s (Term, Product) InvestmentShare where
  _unwrapUArray (InvestmentShares arr) = arr
  _wrapUArray arr = InvestmentShares arr

-- | 投資総額に占める第i財の割合の初期状態
initInvestmentShares :: InitVar -> ST s (InvestmentShares s)
initInvestmentShares v = do
    let investmentShares = _initInvestmentShare v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let share = Map.findWithDefault 0 p investmentShares
        writeUArray arr (t, p) share
    return arr

-- 投資総額に占める第i財の割合は固定
instance Updatable Term InitVar InvestmentShares s where
    type Inner InvestmentShares s = STArray s (Term, Product) InvestmentShare
    unwrap (InvestmentShares a) = a
    initialize _ _ v = initInvestmentShares v
    updatePattern _ = return DoNothing

-- | 投資総額に占める第i財の割合の取得
getInvestmentShare :: World s -> Term -> Product -> ST s InvestmentShare
getInvestmentShare wld t p = do
    let arr = (_investmentShares wld)
    readUArray arr (t, p)  

------------------------------------------------------------------
-- ** 第 i 輸入財に対する輸入関税率
------------------------------------------------------------------

type ImportTariffRateByProduct = Double
newtype ImportTariffRateByProducts s = ImportTariffRateByProducts (STArray s (Term, Product) ImportTariffRateByProduct)

instance UpdatableSTArray ImportTariffRateByProducts s (Term, Product) ImportTariffRateByProduct where
  _unwrapUArray (ImportTariffRateByProducts arr) = arr
  _wrapUArray arr = ImportTariffRateByProducts arr

-- | 第 i 輸入財に対する輸入関税率の初期状態
initImportTariffRateByProducts :: InitVar -> ST s (ImportTariffRateByProducts s)
initImportTariffRateByProducts v = do
    let importTariffRateByProducts = _initImportTariffRateByProduct v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let rate = Map.findWithDefault 0 p importTariffRateByProducts
        writeUArray arr (t, p) rate
    return arr

-- 第 i 輸入財に対する輸入関税率は固定
instance Updatable Term InitVar ImportTariffRateByProducts s where
    type Inner ImportTariffRateByProducts s = STArray s (Term, Product) ImportTariffRateByProduct
    unwrap (ImportTariffRateByProducts a) = a
    initialize _ _ v = initImportTariffRateByProducts v
    updatePattern _ = return DoNothing

-- | 第 i 輸入財に対する輸入関税率の取得
getImportTariffRateByProduct :: World s -> Term -> Product -> ST s ImportTariffRateByProduct
getImportTariffRateByProduct wld t p = do
    let arr = (_importTariffRateByProducts wld)
    readUArray arr (t, p)

------------------------------------------------------------------
-- ** 第 i 合成財生産関数の規模係数
------------------------------------------------------------------

type ScaleCoefficient = Double
newtype ScaleCoefficients s = ScaleCoefficients (STArray s (Term, Product) ScaleCoefficient)

instance UpdatableSTArray ScaleCoefficients s (Term, Product) ScaleCoefficient where
  _unwrapUArray (ScaleCoefficients arr) = arr
  _wrapUArray arr = ScaleCoefficients arr

-- | 第 i 合成財生産関数の規模係数の初期状態
initScaleCoefficients :: InitVar -> ST s (ScaleCoefficients s)
initScaleCoefficients v = do
    let scaleCoefficients = _initScaleCoefficient v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let coef = Map.findWithDefault 0 p scaleCoefficients
        writeUArray arr (t, p) coef
    return arr

-- 第 i 合成財生産関数の規模係数は固定
instance Updatable Term InitVar ScaleCoefficients s where
    type Inner ScaleCoefficients s = STArray s (Term, Product) ScaleCoefficient
    unwrap (ScaleCoefficients a) = a
    initialize _ _ v = initScaleCoefficients v
    updatePattern _ = return DoNothing

-- | 第 i 合成財生産関数の規模係数の取得
getScaleCoefficient :: World s -> Term -> Product -> ST s ScaleCoefficient
getScaleCoefficient wld t p = do
    let arr = (_scaleCoefficients wld)
    readUArray arr (t, p)

------------------------------------------------------------------
-- ** 第 i 合成財生産関数の投入割合係数（輸入財）
------------------------------------------------------------------

type InputShareCoefficientM = Double
newtype InputShareCoefficientMs s = InputShareCoefficientMs (STArray s (Term, Product) InputShareCoefficientM)

instance UpdatableSTArray InputShareCoefficientMs s (Term, Product) InputShareCoefficientM where
  _unwrapUArray (InputShareCoefficientMs arr) = arr
  _wrapUArray arr = InputShareCoefficientMs arr

-- | 第 i 合成財生産関数の投入割合係数（輸入財）の初期状態
initInputShareCoefficientMs :: InitVar -> ST s (InputShareCoefficientMs s)
initInputShareCoefficientMs v = do
    let inputShareCoefficientMs = _initInputShareCoefficientM v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let coef = Map.findWithDefault 0 p inputShareCoefficientMs
        writeUArray arr (t, p) coef
    return arr

-- 第 i 合成財生産関数の投入割合係数（輸入財）は固定
instance Updatable Term InitVar InputShareCoefficientMs s where
    type Inner InputShareCoefficientMs s = STArray s (Term, Product) InputShareCoefficientM
    unwrap (InputShareCoefficientMs a) = a
    initialize _ _ v = initInputShareCoefficientMs v
    updatePattern _ = return DoNothing

-- | 第 i 合成財生産関数の投入割合係数（輸入財）の取得
getInputShareCoefficientM :: World s -> Term -> Product -> ST s InputShareCoefficientM
getInputShareCoefficientM wld t p = do
    let arr = (_inputShareCoefficientMs wld)
    readUArray arr (t, p)

------------------------------------------------------------------
-- ** 第 i 合成財生産関数の投入割合係数（国内財）
------------------------------------------------------------------

type InputShareCoefficientD = Double
newtype InputShareCoefficientDs s = InputShareCoefficientDs (STArray s (Term, Product) InputShareCoefficientD)

instance UpdatableSTArray InputShareCoefficientDs s (Term, Product) InputShareCoefficientD where
  _unwrapUArray (InputShareCoefficientDs arr) = arr
  _wrapUArray arr = InputShareCoefficientDs arr

-- | 第 i 合成財生産関数の投入割合係数（国内財）の初期状態
initInputShareCoefficientDs :: InitVar -> ST s (InputShareCoefficientDs s)
initInputShareCoefficientDs v = do
    let inputShareCoefficientDs = _initInputShareCoefficientD v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let coef = Map.findWithDefault 0 p inputShareCoefficientDs
        writeUArray arr (t, p) coef
    return arr

-- 第 i 合成財生産関数の投入割合係数（国内財）は固定
instance Updatable Term InitVar InputShareCoefficientDs s where
    type Inner InputShareCoefficientDs s = STArray s (Term, Product) InputShareCoefficientD
    unwrap (InputShareCoefficientDs a) = a
    initialize _ _ v = initInputShareCoefficientDs v
    updatePattern _ = return DoNothing

-- | 第 i 合成財生産関数の投入割合係数（国内財）の取得
getInputShareCoefficientD :: World s -> Term -> Product -> ST s InputShareCoefficientD
getInputShareCoefficientD wld t p = do
    let arr = (_inputShareCoefficientDs wld)
    readUArray arr (t, p)

------------------------------------------------------------------
-- ** 代替の弾力性に関する係数
------------------------------------------------------------------

type SubstitutionElasticityCoefficient = Double
newtype SubstitutionElasticityCoefficients s = SubstitutionElasticityCoefficients (STArray s (Term, Product) SubstitutionElasticityCoefficient)

instance UpdatableSTArray SubstitutionElasticityCoefficients s (Term, Product) SubstitutionElasticityCoefficient where
  _unwrapUArray (SubstitutionElasticityCoefficients arr) = arr
  _wrapUArray arr = SubstitutionElasticityCoefficients arr

-- | 代替の弾力性に関する係数の初期状態
initSubstitutionElasticityCoefficients :: InitVar -> ST s (SubstitutionElasticityCoefficients s)
initSubstitutionElasticityCoefficients v = do
    let substitutionElasticityCoefficients = _initSubstitutionElasticityCoefficient v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let coef = Map.findWithDefault 0 p substitutionElasticityCoefficients
        writeUArray arr (t, p) coef
    return arr

-- 代替の弾力性に関する係数は固定
instance Updatable Term InitVar SubstitutionElasticityCoefficients s where
    type Inner SubstitutionElasticityCoefficients s = STArray s (Term, Product) SubstitutionElasticityCoefficient
    unwrap (SubstitutionElasticityCoefficients a) = a
    initialize _ _ v = initSubstitutionElasticityCoefficients v
    updatePattern _ = return DoNothing

-- | 代替の弾力性に関する係数の取得
getSubstitutionElasticityCoefficient :: World s -> Term -> Product -> ST s SubstitutionElasticityCoefficient
getSubstitutionElasticityCoefficient wld t p = do
    let arr = (_substitutionElasticityCoefficients wld)
    readUArray arr (t, p)

------------------------------------------------------------------
-- ** 第 i 合成財生産関数の代替の弾力性
------------------------------------------------------------------

type SubstitutionElasticity = Double
newtype SubstitutionElasticities s = SubstitutionElasticities (STArray s (Term, Product) SubstitutionElasticity)

instance UpdatableSTArray SubstitutionElasticities s (Term, Product) SubstitutionElasticity where
  _unwrapUArray (SubstitutionElasticities arr) = arr
  _wrapUArray arr = SubstitutionElasticities arr

-- | 第 i 合成財生産関数の代替の弾力性の初期状態
initSubstitutionElasticities :: InitVar -> ST s (SubstitutionElasticities s)
initSubstitutionElasticities v = do
    let substitutionElasticities = _initSubstitutionElasticity v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let elasticity = Map.findWithDefault 0 p substitutionElasticities
        writeUArray arr (t, p) elasticity
    return arr

-- 第 i 合成財生産関数の代替の弾力性は固定
instance Updatable Term InitVar SubstitutionElasticities s where
    type Inner SubstitutionElasticities s = STArray s (Term, Product) SubstitutionElasticity
    unwrap (SubstitutionElasticities a) = a
    initialize _ _ v = initSubstitutionElasticities v
    updatePattern _ = return DoNothing

-- | 第 i 合成財生産関数の代替の弾力性の取得
getSubstitutionElasticity :: World s -> Term -> Product -> ST s SubstitutionElasticity
getSubstitutionElasticity wld t p = do
    let arr = (_substitutionElasticities wld)
    readUArray arr (t, p)

------------------------------------------------------------------
-- ** θ_i : 第 i 変形関数の規模係数
------------------------------------------------------------------

type TransformationScaleCoefficient = Double
newtype TransformationScaleCoefficients s = TransformationScaleCoefficients (STArray s (Term, Product) TransformationScaleCoefficient)

instance UpdatableSTArray TransformationScaleCoefficients s (Term, Product) TransformationScaleCoefficient where
  _unwrapUArray (TransformationScaleCoefficients arr) = arr
  _wrapUArray arr = TransformationScaleCoefficients arr

-- | θ_i : 第 i 変形関数の規模係数の初期状態
initTransformationScaleCoefficients :: InitVar -> ST s (TransformationScaleCoefficients s)
initTransformationScaleCoefficients v = do
    let transformationScaleCoefficients = _initTransformationScaleCoefficient v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let coef = Map.findWithDefault 0 p transformationScaleCoefficients
        writeUArray arr (t, p) coef
    return arr

-- θ_i : 第 i 変形関数の規模係数は固定
instance Updatable Term InitVar TransformationScaleCoefficients s where
    type Inner TransformationScaleCoefficients s = STArray s (Term, Product) TransformationScaleCoefficient
    unwrap (TransformationScaleCoefficients a) = a
    initialize _ _ v = initTransformationScaleCoefficients v
    updatePattern _ = return DoNothing

-- | θ_i : 第 i 変形関数の規模係数の取得
getTransformationScaleCoefficient :: World s -> Term -> Product -> ST s TransformationScaleCoefficient
getTransformationScaleCoefficient wld t p = do
    let arr = (_transformationScaleCoefficients wld)
    readUArray arr (t, p)

------------------------------------------------------------------
-- ** 第 i 変形関数の産出割合係数（輸出）
------------------------------------------------------------------

type OutputShareCoefficientE = Double
newtype OutputShareCoefficientEs s = OutputShareCoefficientEs (STArray s (Term, Product) OutputShareCoefficientE)

instance UpdatableSTArray OutputShareCoefficientEs s (Term, Product) OutputShareCoefficientE where
  _unwrapUArray (OutputShareCoefficientEs arr) = arr
  _wrapUArray arr = OutputShareCoefficientEs arr

-- | 第 i 変形関数の産出割合係数（輸出）の初期状態
initOutputShareCoefficientEs :: InitVar -> ST s (OutputShareCoefficientEs s)
initOutputShareCoefficientEs v = do
    let outputShareCoefficientEs = _initOutputShareCoefficientE v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let coef = Map.findWithDefault 0 p outputShareCoefficientEs
        writeUArray arr (t, p) coef
    return arr

-- 第 i 変形関数の産出割合係数（輸出）は固定
instance Updatable Term InitVar OutputShareCoefficientEs s where
    type Inner OutputShareCoefficientEs s = STArray s (Term, Product) OutputShareCoefficientE
    unwrap (OutputShareCoefficientEs a) = a
    initialize _ _ v = initOutputShareCoefficientEs v
    updatePattern _ = return DoNothing

-- | 第 i 変形関数の産出割合係数（輸出）の取得
getOutputShareCoefficientE :: World s -> Term -> Product -> ST s OutputShareCoefficientE
getOutputShareCoefficientE wld t p = do
    let arr = (_outputShareCoefficientEs wld)
    readUArray arr (t, p)

------------------------------------------------------------------
-- ** 第 i 変形関数の産出割合係数（国内）
------------------------------------------------------------------

type OutputShareCoefficientD = Double
newtype OutputShareCoefficientDs s = OutputShareCoefficientDs (STArray s (Term, Product) OutputShareCoefficientD)

instance UpdatableSTArray OutputShareCoefficientDs s (Term, Product) OutputShareCoefficientD where
  _unwrapUArray (OutputShareCoefficientDs arr) = arr
  _wrapUArray arr = OutputShareCoefficientDs arr

-- | 第 i 変形関数の産出割合係数（国内）の初期状態
initOutputShareCoefficientDs :: InitVar -> ST s (OutputShareCoefficientDs s)
initOutputShareCoefficientDs v = do
    let outputShareCoefficientDs = _initOutputShareCoefficientD v
    arr <- newUArray ((initTerm, fstProd), (lastTerm, lastProd)) 0  -- 初期値は0
    forM_ [(t, p) | t <- [initTerm .. lastTerm], p <- [fstProd .. lastProd]] $ \(t, p) -> do
        let coef = Map.findWithDefault 0 p outputShareCoefficientDs
        writeUArray arr (t, p) coef
    return arr

-- 第 i 変形関数の産出割合係数（国内）は固定
instance Updatable Term InitVar OutputShareCoefficientDs s where
    type Inner OutputShareCoefficientDs s = STArray s (Term, Product) OutputShareCoefficientD
    unwrap (OutputShareCoefficientDs a) = a
    initialize _ _ v = initOutputShareCoefficientDs v
    updatePattern _ = return DoNothing

-- | 第 i 変形関数の産出割合係数（国内）の取得
getOutputShareCoefficientD :: World s -> Term -> Product -> ST s OutputShareCoefficientD
getOutputShareCoefficientD wld t p = do
    let arr = (_outputShareCoefficientDs wld)
    readUArray arr (t, p)

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
initOrders :: InitVar -> ST s (OrderTable s)
initOrders initVar = do
    let ioCoefficients = _initIOCoefficients initVar
    ordersArr <- newUArray ((initTerm, Relation {_supplier = fstEnt, _customer = fstEnt})
                            ,(lastTerm, Relation {_supplier = lastEnt, _customer = lastEnt})) 0
    forM_ [(t, e1, e2) | t <- [initTerm .. lastTerm]
                       , e1 <- [fstEnt .. lastEnt]
                       , e2 <- [fstEnt .. lastEnt]] $ \(t, e1, e2) -> do
        when (isConsumer e2) $ do
            -- e1が生産する財を取得
            let products1 = produceWhat e1
            -- e1が生産する各財について、投入係数から発注量を計算
            forM_ products1 $ \p1 -> do
                -- p1を生産するために必要な各投入財p2の投入係数を取得
                let totalCoef = sum [Map.findWithDefault 0 (p2, p1) ioCoefficients | p2 <- products]
                when (totalCoef /= 0) $ do
                    -- e1からe2への発注量 
                    let orderAmount = totalCoef 
                    writeUArray ordersArr
                                (t, Relation {_supplier = e1, _customer = e2})
                                orderAmount
    return ordersArr

-- ** STArray s (ID, Term) NN.Double
instance Updatable Term InitVar OrderTable s where
    type Inner OrderTable s = STArray s (Term,OrderRelation) OrderAmount
    unwrap (OrderTable a) = a
    initialize _ _ e = do
        initOrders e  -- それを基に OrdersTable を作成（finalDemandとinhouseRatioは後で設定）

    updatePattern _  = return Modify

    -- 前期の残りの分を追加
    modify g t e x  =  case (t == initTerm)of
                    True   -> return ()
                    ------------------------------------------------------------------
                    _       -> forM_ [fstEnt..lastEnt] $ \e1
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
data World s = World { _ledger                             :: Ledger s                       -- ^ 元帳
                     , _directTaxRates                     :: DirectTaxRates s               -- ^ 直接税率
                     , _indirectTaxRates                   :: IndirectTaxRates s             -- ^ 生産税率
                     , _importTariffRates                  :: ImportTariffRates s             -- ^ 輸入関税率
                     , _foreignSavings                     :: ForeignSavings s               -- ^ 外国貯蓄額
                     , _exchangeRates                      :: ExchangeRates s                -- ^ 為替レート
                     , _householdSavings                   :: HouseholdSavings s             -- ^ 家計の平均貯蓄性向
                     , _governmentSavings                  :: GovernmentSavings s            -- ^ 政府の平均貯蓄性向
                     , _prices                             :: Prices s                       -- ^ 価格テーブル
                     , _ics                                :: ICTable s                      -- ^ 投入係数
                     , _orders                             :: OrderTable s                   -- ^ 発注量
                     , _governmentConsumptionShares        :: GovernmentConsumptionShares s  -- ^ 政府消費総額に占める第i財の割合
                     , _investmentShares                   :: InvestmentShares s             -- ^ 投資総額に占める第i財の割合
                     , _importTariffRateByProducts         :: ImportTariffRateByProducts s  -- ^ 第 i 輸入財に対する輸入関税率
                     , _scaleCoefficients                  :: ScaleCoefficients s           -- ^ 第 i 合成財生産関数の規模係数
                     , _inputShareCoefficientMs            :: InputShareCoefficientMs s     -- ^ 第 i 合成財生産関数の投入割合係数（輸入財）
                     , _inputShareCoefficientDs            :: InputShareCoefficientDs s     -- ^ 第 i 合成財生産関数の投入割合係数（国内財）
                     , _substitutionElasticityCoefficients :: SubstitutionElasticityCoefficients s -- ^ 代替の弾力性に関する係数
                     , _substitutionElasticities           :: SubstitutionElasticities s     -- ^ 第 i 合成財生産関数の代替の弾力性
                     , _transformationScaleCoefficients    :: TransformationScaleCoefficients s -- ^ θ_i : 第 i 変形関数の規模係数
                     , _outputShareCoefficientEs           :: OutputShareCoefficientEs s     -- ^ 第 i 変形関数の産出割合係数（輸出）
                     , _outputShareCoefficientDs           :: OutputShareCoefficientDs s     -- ^ 第 i 変形関数の産出割合係数（国内）
                     }
                     deriving (Generic)

------------------------------------------------------------------
-- * 汎用関数
------------------------------------------------------------------

{-# INLINE mean #-}
-- | 平均を計算
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

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

-- | 総発注量の取得 = Orderの残り + 購入量
getPlaceOrderTotal :: World s -> Term -> Entity -> ST s Double
getPlaceOrderTotal wld t e1
    | t < 1 = return 0
    | otherwise = do
        purchase <- getTermPurchase Amount wld t e1
        let arr = (_orders wld)
        values <- CM.mapM (\e2 -> readUArray arr (t, Relation {_supplier = e2
                                                              ,_customer = e1}))
                          [fstEnt .. lastEnt]
        return $ (sum values) + purchase


-- | 一期の使用量を取得する
getTermInput :: CountUnit -> World s -> Term -> Entity -> Product -> ST s Double
getTermInput u wld t e c = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let temp =  EJ.projWithNoteBase [(Production,t)] [Hat:<(Products, c,e,(.#))] le
    case u of
        Yen     -> return $ norm $ EJT.transfer temp $ toCashTable pt
        Amount  -> return $ norm $ EJT.transfer temp $ toAmountTable pt
        _       -> return $ norm $ temp


-- | 一期の製品在庫保有量を取得する
getTermStock :: CountUnit -> World s -> Term -> Entity ->  Product -> ST s Double
getTermStock u wld t e c = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let temp = EJ.projWithBase [Not:<(Products,c,e,(.#))]
             $ (.-)
             $ termJournal t le
    case u of
        Yen     -> return $ norm $ EJT.transfer temp $ toCashTable pt
        Amount  -> return $ norm $ EJT.transfer temp $ toAmountTable pt
        _       -> return $ norm $ temp


-- | 一期の原材料在庫保有量を取得する
getTermMaterial :: CountUnit -> World s -> Term -> Entity -> Product ->  ST s Double
getTermMaterial u wld t e c = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let temp = EJ.projWithBase [Not :<(Products, c, e, (.#))]
             $ (.-)
             $ termJournal t le
    case u of
        Yen     -> return $ norm $ EJT.transfer temp $ toCashTable pt
        Amount  -> return $ norm $ EJT.transfer temp $ toAmountTable pt
        _       -> return $ norm $ temp

-- | 一期の原材料在庫保有総量(自家投入を除く)を取得する
getTermMaterialTotal :: CountUnit -> World s -> Term -> Entity ->  ST s Double
getTermMaterialTotal u wld t e = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let temp = fromList 
             $ L.map (\c -> EJ.projWithBase [Not :<(Products,c, e, (.#))]
             $ (.-)
             $ termJournal t le)
             [c | c <- [fstProd .. lastProd], not $ elem c (produceWhat e)]
    case u of
        Yen     -> return $ norm $ EJT.transfer temp $ toCashTable pt
        Amount  -> return $ norm $ EJT.transfer temp $ toAmountTable pt
        _       -> return $ norm $ temp


-- | 一期の算出を取得する
getTermProduction :: CountUnit -> World s -> Term -> Entity -> ST s Double
getTermProduction u wld t e = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let temp = EJ.projWithNoteBase [(Production,t)] [Not:<(Products, c,e,(.#)) | c <- produceWhat e]
             $ termJournal t le
    case u of
        Yen     -> return $ norm $ EJT.transfer temp $ toCashTable pt
        Amount  -> return $ norm $ EJT.transfer temp $ toAmountTable pt
        _       -> return $ norm $ temp


-- | 一期の中間投入量を取得する
getTermInputTotal :: CountUnit -> World s -> Term -> Entity -> ST s Double
getTermInputTotal u wld t e = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let temp = EJ.projWithNoteBase [(Production,t)] [Hat:<(Products, (.#), e, (.#))]
             $ termJournal t le
    case u of
        Yen     -> return $ norm $ EJT.transfer temp $ toCashTable pt
        Amount  -> return $ norm $ EJT.transfer temp $ toAmountTable pt
        _       -> return $ norm $ temp


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
getTermSales :: CountUnit -> World s -> Term -> Entity -> ST s Double
getTermSales u wld t e = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let temp = EJ.projWithNoteBase [(SalesPurchase,t)] [Hat:<(Products,c,e,(.#)) | c <- produceWhat e]
             $ termJournal t le
    case u of
        Yen     -> return $ norm $ EJT.transfer temp $ toCashTable pt
        Amount  -> return $ norm $ EJT.transfer temp $ toAmountTable pt
        _       -> return $ norm $ temp

-- | 一期の購入量を取得する
getTermPurchase :: CountUnit -> World s -> Term -> Entity -> ST s Double
getTermPurchase u wld t e = do
    pt <- readURef (_prices wld)
    le <- readURef (_ledger wld)
    let temp = EJ.projWithNoteBase [(SalesPurchase,t)] [Not:<(Products, (.#),e,(.#))]
             $ termJournal t le
    case u of
        Yen     -> return $ norm $ EJT.transfer temp $ toCashTable pt
        Amount  -> return $ norm $ EJT.transfer temp $ toAmountTable pt
        _       -> return $ norm $ temp

-- | 一期の総需要を取得する
-- 発注の残りと販売量の和
getTermDemand :: World s -> Term -> Entity -> ST s Double
getTermDemand wld t e = do    
    sales <- getTermSales Amount wld t e
    order <- getOrderTotal wld t e
    return (sales + order)

-- | 一単位の財の簿記を取得する
getOneProduction :: World s -> Term -> Product -> ST s Transaction
getOneProduction wld t c = do
    let arr =  (_ics wld)  -- ICTable を取得
    inputs <- CM.forM products $ \c2 -> do
        -- c を生産するために必要な c2 の投入係数
        coef <- readUArray arr (t, c2, c)
        -- c2 の消費を記録
        return $ coef :@ Hat :<(Products, c2, producedBy c, Amount) .| (Production,t)
        -- すべての中間投入を結合
    let !totalInput = EJ.fromList inputs
        -- 生産と投入の合計
        !result = (1 :@ Not :<(Products, c, producedBy c, Amount) .| (Production,t)) .+ totalInput
    return result
    


-- 記帳
journal :: World s ->  Transaction -> ST s ()
journal wld Zero = return ()
journal wld js   = modifyURef (_ledger wld) (\x -> x .+ js)

