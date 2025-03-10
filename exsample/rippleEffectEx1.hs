
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
import              ExchangeAlgebraMapJournal
import qualified    ExchangeAlgebraMap.Journal  as EJ
import qualified    ExchangeAlgebraMap.Journal.Transfer as EJT
import              ExchangeAlgebraMap.Journal.Transfer ((.->)
                                                        ,(|%))

import qualified    ExchangeAlgebraMap.Simulate as ES

-- Other
import qualified    Data.Map.Strict         as M
import qualified    Data.Text               as T

import qualified    Control.Monad                   as CM
import              Control.Monad
import              Control.Monad.State
import              Control.Monad.ST
import              Data.Array.ST
import              Data.Array.IO
import Data.Array (Array)
import              Data.STRef
import qualified    Data.List                       as L
import GHC.Generics
import System.Random -- 乱数

-- Debug
import Debug.Trace

-- For Visutalization
import              Graphics.Rendering.Chart.Easy            hiding ( (:<),(.~))
import              Graphics.Rendering.Chart.Backend.Cairo
import              Graphics.Rendering.Chart.Axis
import              Graphics.Rendering.Chart.Axis.Int
import              Graphics.Rendering.Chart.Grid


------------------------------------------------------------------
-- * 可視化用の関数定義
-- 本論には無関係(後ほど一般化する)
------------------------------------------------------------------
type Title          = String
type FileName       = String
type Label          = String
type TimeSeries     = (Label, [[(Term, Prelude.Double)]])
type TimeSerieses   = [TimeSeries]
type GridColumns    = [TimeSerieses]
type GridMatrix     = [GridColumns]

-- | 折れ線グラフの分割表示
{-# INLINE plotGridLine #-}
plotGridLine :: (World RealWorld -> IO GridMatrix)
             -> World RealWorld -> FileName -> Title -> IO ()
plotGridLine f wld file name
    =  f wld >>= \mtx
    -> let  ys = aboveN $ (flip L.map) mtx
                $ \ (cols :: GridColumns)  -> besideN
                $ (flip L.map) cols -- columns
                $ \ (serieses :: TimeSerieses) -> layoutToGrid
                $ execEC
                $ CM.forM_ serieses
                $ \((t,xs) :: TimeSeries) -> plot $ liftEC $ do
                    plot_lines_values .= [concat xs]
                    plot_lines_title .= t
                    plot_lines_style . line_color .= opaque blue
    in void $ renderableToFile def (file ++ "/" ++ name ++ ".png")
            $ fillBackground def
            $ gridToRenderable
            $ title `wideAbove`  ys
  where
    title = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre name
    ls = def { _font_size   = 15 , _font_weight = FontWeightBold }

-- | 好みの色パレット
colorPalette :: [AlphaColour Double]
colorPalette =
  [ opaque red
  , opaque blue
  , opaque green
  , opaque orange
  , opaque magenta
  , opaque cyan
  , opaque black
  , opaque grey
  , opaque brown
  , opaque violet
  ]

{-# INLINE plotGridLineMultiColor #-}
plotGridLineMultiColor
    :: (World RealWorld -> IO GridMatrix)  -- ^ 改良版 (複数系列に対応) GridMatrix を返す関数
    -> World RealWorld
    -> FilePath     -- ^ 出力先ディレクトリ
    -> String       -- ^ グラフタイトル
    -> IO ()
plotGridLineMultiColor f wld file name = do
  mtx <- f wld
  let
    -- グリッド行方向: aboveN, 列方向: besideN
    ys = aboveN $ L.map
          (\ (cols :: GridColumns) ->
             besideN $
             L.map (\ (serieses :: TimeSerieses) ->
                   layoutToGrid $
                     execEC $
                       do CM.forM_ (zip [0..] serieses) $ \(i, (lbl, valList)) -> plot $ liftEC $ do
                            plot_lines_values .= [concat valList]
                            plot_lines_title  .= lbl
                            -- 系列ごとに色を変える
                            plot_lines_style . line_color .= (colorPalette !! (i `mod` length colorPalette))
                 ) cols
          )
          mtx

    titleRenderable = setPickFn nullPickFn $
      label ls HTA_Centre VTA_Centre name
    ls = def { _font_size = 15, _font_weight = FontWeightBold }

  void $ renderableToFile def (file ++ "/" ++ name ++ ".png") $
          fillBackground def $
          gridToRenderable $
          titleRenderable `wideAbove` ys

-- | データをグリッドに分割する
gridPathSingleLine  :: (Ord a, Show a, Ix a)
                    => [a]
                    -> STArray s (a,Term) Prelude.Double
                    -> ST s GridMatrix
gridPathSingleLine xs arr = do
        grid    <- newSTRef [] :: ST s (STRef s GridMatrix)
        col     <- newSTRef [] :: ST s (STRef s GridColumns)
        count   <- newSTRef 1  :: ST s (STRef s Prelude.Int)

        CM.forM_ xs ( \e -> do
            count' <- readSTRef count
            case count' >= 3 of
                True    ->  do
                            xs  <- CM.forM [initTerm .. lastTerm] ( \t
                                -> readArray arr (e,t) >>= \v
                                -> return (t, v))
                            col' <- readSTRef col
                            modifySTRef grid (\x -> x ++ [col' ++ [[(show e, [xs])]]])
                            writeSTRef  count 1
                            writeSTRef  col []
                ------------------------------------------------------------------
                False   ->  case e == L.last xs of
                                True  ->    do
                                            xs  <- CM.forM [initTerm .. lastTerm] ( \t
                                                -> readArray arr (e,t) >>= \v
                                                -> return (t, v))
                                            col' <- readSTRef col
                                            modifySTRef grid (\x -> x ++ [col' ++ [[(show e, [xs])]]])
                                            writeSTRef  count 1
                                            writeSTRef  col []
                                False ->    do
                                            xs  <- CM.forM [initTerm .. lastTerm] ( \t
                                                -> readArray arr (e,t) >>= \v
                                                -> return (t, v))
                                            modifySTRef col (\x -> x ++ [[(show e, [xs])]])
                                            modifySTRef count (+ 1))

        readSTRef grid >>= return

-- | gridPathSingleLine を拡張して,
--   「同じセル内に2つの系列をまとめて入れる」例
gridPathTwoLine
  :: (Ord a, Show a, Ix a)
  => [a]                           -- ^ イテレート対象 (例: [fstEnt..lastEnt])
  -> STArray s (a, Term) Double    -- ^ シリーズA用
  -> STArray s (a, Term) Double    -- ^ シリーズB用
  -> ST s GridMatrix
gridPathTwoLine xs arrA arrB = do

    -- GridMatrix全体，列(3つ毎に1列にまとめたい等)，カウンタを用意
    gridRef  <- newSTRef []        :: ST s (STRef s GridMatrix)
    colRef   <- newSTRef []        :: ST s (STRef s GridColumns)
    countRef <- newSTRef (1 :: Int)

    forM_ xs $ \ e -> do
      count' <- readSTRef countRef

      -- シリーズAのタイム系列
      timeValsA <- CM.forM [initTerm .. lastTerm] $ \ t -> do
        v <- readArray arrA (e,t)
        return (t, v)

      -- シリーズBのタイム系列
      timeValsB <- CM.forM [initTerm .. lastTerm] $ \ t -> do
        v <- readArray arrB (e,t)
        return (t, v)

      -- 同じセルに2系列をまとめる
      -- (String, [[(Term, Double)]]) がTimeSeriesの型
      -- シリーズが複数あるので TimeSerieses = [TimeSeries]
      let twoSeriesInCell =
            [ (show e ++ "_A", [timeValsA])  -- 1系列目
            , (show e ++ "_B", [timeValsB])  -- 2系列目
            ]

      -------------------------------------------------------------------------
      -- 以下は gridPathSingleLine と同じく, 3つで1カラムにまとめるロジック
      -------------------------------------------------------------------------
      if count' >= 3
        then do
          col' <- readSTRef colRef
          -- いままで溜めたcol'に今回のセルを追加して，
          -- それらをひとまとめ(1列分)としてgridRefに積む
          modifySTRef gridRef (\acc -> acc ++ [col' ++ [twoSeriesInCell]])
          -- カラムとカウンタをリセット
          writeSTRef colRef []
          writeSTRef countRef 1

        else if e == last xs
          then do
            -- 最後の要素の場合は，ここまで溜まったものをまとめて追加
            col' <- readSTRef colRef
            modifySTRef gridRef (\acc -> acc ++ [col' ++ [twoSeriesInCell]])
            writeSTRef colRef []
            writeSTRef countRef 1

          else do
            -- まだ3列に達していない場合は，colRefに追加のみ
            modifySTRef colRef (\acc -> acc ++ [twoSeriesInCell])
            modifySTRef countRef (+1)

    -- すべて終わったらGridMatrixを返す
    readSTRef gridRef

-- | 産業の粗利益の可視化
grossProfitPath :: World RealWorld -> IO GridMatrix
grossProfitPath wld =  stToIO $ do
    arr <- newArray ((fstEnt, initTerm), (lastEnt, lastTerm)) 0
        :: ST s (STArray s (Entity, Term) Double)
    CM.forM_ [initTerm .. lastTerm ] $ \t
        -> CM.forM_ [fstEnt .. lastEnt] $ \i
            -> readSTRef (_ledger wld) >>= \bk
            -> let tr    = EJT.grossProfitTransferKeepWiledcard bk
            in let plus  = norm $ EJ.projWithBase [Not:<(Cash,(.#),i,Yen)]
                                $ termJournal t tr
            in let minus = norm $ EJ.projWithBase [Hat:<(Cash,(.#),i,Yen)]
                                $ termJournal t tr
            in modifyArray arr (i, t) (\x -> x  + plus - minus)
    gridPathSingleLine [fstEnt .. lastEnt] arr

-- | 生産量の可視化
productionPath :: EnvVar -> World RealWorld -> IO GridMatrix
productionPath env wld = stToIO $ do
    arr <- newArray ((fstEnt, initTerm), (lastEnt, lastTerm)) 0
        :: ST s (STArray s (Entity, Term) Double)
    CM.forM_ [initTerm .. lastTerm ] $ \t
        -> CM.forM_ [fstEnt .. lastEnt] $ \i
            -> getTermProduction wld t env i >>= \v
            -> modifyArray arr (i, t) (\x -> x + v)

    gridPathSingleLine [fstEnt .. lastEnt] arr

-- | 在庫量の可視化
stockPath :: EnvVar -> World RealWorld -> IO GridMatrix
stockPath env wld = stToIO $ do
    arr <- newArray ((fstEnt, initTerm), (lastEnt, lastTerm)) 0
        :: ST s (STArray s (Entity, Term) Double)
    CM.forM_ [initTerm .. lastTerm ] $ \t
        -> CM.forM_ [fstEnt .. lastEnt] $ \i
            -> getTermStock wld t env i >>= \v
            -> modifyArray arr (i, t) (\x -> x + v)

    gridPathSingleLine [fstEnt .. lastEnt] arr



-- | 2つの設定の生産量の差を可視化する
productionDiffPath :: EnvVar -> EnvVar -> World RealWorld -> World RealWorld -> IO GridMatrix
productionDiffPath env1 env2 wld1 wld2 = stToIO $ do
    arr <- newArray ((fstEnt, initTerm), (lastEnt, lastTerm)) 0
        :: ST s (STArray s (Entity, Term) Double)
    CM.forM_ [initTerm .. lastTerm ] $ \t
        -> CM.forM_ [fstEnt .. lastEnt] $ \i
            -> getTermProduction wld1 t env1 i >>= \v1
            -> getTermProduction wld2 t env2 i >>= \v2
            -> modifyArray arr (i, t) (\x -> x + (v1 - v2))
    gridPathSingleLine [fstEnt .. lastEnt] arr


-- | 2つのEnvVarを受け取り, 同じスケール(例えば(Ent,Term))で2系列それぞれの値を集計し，
--   最終的にGridMatrixとして返す.
productionTwoSeriesPath
  :: EnvVar                -- ^ シリーズAに対応する EnvVar
  -> EnvVar                -- ^ シリーズBに対応する EnvVar
  -> World RealWorld
  -> World RealWorld
  -> IO GridMatrix
productionTwoSeriesPath envA envB wldA wldB = stToIO $ do
    -- 1. 2つの配列(arrA, arrB)を用意し, それぞれに値を書き込む
    arrA <- newArray ((fstEnt, initTerm), (lastEnt, lastTerm)) 0
                :: ST s (STArray s (Entity, Term) Double)
    arrB <- newArray ((fstEnt, initTerm), (lastEnt, lastTerm)) 0
                :: ST s (STArray s (Entity, Term) Double)

    -- 2. arrA, arrB に対し, EnvVarごとの値を代入 (例: getTermProduction)
    forM_ [initTerm .. lastTerm] $ \t ->
      forM_ [fstEnt .. lastEnt] $ \i -> do
        valA <- getTermProduction wldA t envA i
        valB <- getTermProduction wldB t envB i
        modifyArray arrA (i, t) (+ valA)
        modifyArray arrB (i, t) (+ valB)

    -- 3. 2系列を同じセルに詰め込むための新たなgrid化関数を呼び出す
    gridPathTwoLine [fstEnt .. lastEnt] arrA arrB

-- | modifyArray
{-# INLINE modifyArray #-}
modifyArray ::(MArray a t m, Ix i) => a i t -> i -> (t -> t) -> m ()
modifyArray ar e f = readArray ar e >>= \ x -> writeArray ar e (f x)

------------------------------------------------------------------
-- *  状態系の導入
------------------------------------------------------------------

instance StateTime Term where
    initTerm = 1
    lastTerm = 50
    nextTerm = \x -> x + 1
    prevTerm = \x -> x - 1

------------------------------------------------------------------
-- ** 乱数生成器の状態空間の定義
------------------------------------------------------------------

seed = 42

type Gen s = STRef s StdGen

-- ** STArray s (ID, Term) NN.Double
-- 価格は固定
instance Updatable Term EnvVar ST s (Gen s) where
    initialize g t e = newSTRef (mkStdGen seed)
    updatePattern _ _ _ = return DoNothing

------------------------------------------------------------------
-- ** 環境変数の定義
------------------------------------------------------------------

data EnvVar = EnvVar {_initInv :: Double -- 初期在庫保有量
                     ,_initIR  :: Double -- 在庫比率
                     ,_initMS  :: Double -- 原材料在庫比率
                     ,_addedDemend :: Double -- 20期めの追加需要(波及効果)
                     ,_finalDemand   :: Double -- 各産業の最終需要
                     ,_inhouseRatio :: Double -- 内製部門比率
                     } deriving (Eq, Show)


instance EnvironmentVariables EnvVar where


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

instance Note Term where
    plank = -1

-- | 取引
type Transaction = EJ.Journal (EventName,Term) Double VEHatBase

termJournal :: Term -> Transaction -> Transaction
termJournal t = EJ.filterWithNote (\(e,t') _ -> t' == t )

-- | 元帳
type Ledger s = STRef s Transaction

-- 取引の初期状態
-- 在庫だけ一定量保有
initTransaction :: Entity -> Double -> Transaction
initTransaction e d
    | e == finalDemandSector = Zero -- 最終需要部門
    | otherwise              = (.+) (d :@ Not :<(Products,e,e,Amount) .| (plank,initTerm))
                             $ EJ.fromList [ (d/10) :@ Not :<(Products,e2,e,Amount) .| (plank,initTerm)
                                           | e2 <- industries]

initLedger :: Double -> ST s (Ledger s)
initLedger d = newSTRef $ (EJ.fromList [ initTransaction c d
                                       | c <- [fstEnt..lastEnt]])

-- 一般化の適用
instance Updatable Term EnvVar ST s (Ledger s) where
    initialize _ _ e = initLedger (_initInv e)

    updatePattern _ _ _ = return Modify

    -- 過去のTermを次の期のTermに変更して追加する
    modify t e x  =  readSTRef x >>= \le
                  -> let added = g t (termJournal (t-1) le)
                  in modifySTRef x (\z -> z .+ added)
        where
        g t x = EJ.gather (plank, t) $ f x
        f   = EJT.finalStockTransferKeepWiledcard
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
getTermStock :: World s -> Term -> EnvVar -> Entity -> ST s Double
getTermStock wld t env e = do
    le <- readSTRef (_ledger wld)
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
type Prices s = STRef s PriceTable

-- 販売価格の初期状態
-- 今回は全て価格同一(1)

initPrice :: Term -> Entity -> Price
initPrice t _ = 1

-- 価格履歴の初期状態
initPrices :: ST s (Prices s)
initPrices  = newSTRef $ M.fromList [((t,c),initPrice t c)
                                    | t <- [initTerm .. lastTerm]
                                    , c <- [fstEnt..lastEnt]]

-- ** STArray s (ID, Term) Double
-- 価格は固定
instance Updatable Term EnvVar ST s (Prices s) where
    initialize _ _ _ = initPrices
    updatePattern _ _ _ = return DoNothing

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
type InventoryRatio = Double
newtype IRTable = IRTable {_irtable :: M.Map Entity InventoryRatio}
type IRs s = STRef s IRTable

-- 在庫比率の初期状態
-- 今回は全て同一比率
-- この比率次第でサプライチェーンがスタックする?

initInventoryRatio :: Entity -> Double -> InventoryRatio
initInventoryRatio _ d = d

-- 価格履歴の初期状態
initIRs :: Double -> ST s (IRs s)
initIRs d   = newSTRef
            $ IRTable
            $ M.fromList    [(e,initInventoryRatio e d)
                            | e <- [fstEnt .. lastEnt]]

-- ** STArray s (ID, Term) Double
-- 価格は固定
instance Updatable Term EnvVar ST s (IRs s) where
    initialize _ _ e = initIRs (_initIR e)
    updatePattern _ _ _ = return DoNothing

-- | 在庫比率の取得
getInventoryRatio :: World s -> Entity -> ST s InventoryRatio
getInventoryRatio wld e     =  readSTRef (_irs wld) >>= \ir
                            -> case M.lookup e (_irtable ir) of
                                    Nothing -> return 0
                                    Just x  -> return x

------------------------------------------------------------------
-- ** 原材料在庫比率
------------------------------------------------------------------
-- | 原材料在庫比率
type MaterialStock = Double
newtype MSTable = MSTable {_mstable :: M.Map Entity MaterialStock}
type MSs s = STRef s MSTable


-- 原材料在庫比率の初期状態
initMSs :: Double -> ST s (MSs s)
initMSs d = newSTRef
          $ MSTable
          $ M.fromList  [(e,d)
                        | e <- [fstEnt .. lastEnt]]

-- ** STArray s (ID, Term) Double
-- 価格は固定
instance Updatable Term EnvVar ST s (MSs s) where
    initialize _ _ e = initMSs (_initMS e)
    updatePattern _ _ _  = return DoNothing

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
-- 祖付加価値は一律2割とする(今回扱っているのは投入財の部分のみ= 合計0.8となる)

-- 生産関数(投入係数行列)
-- 物量表記
type InputCoefficient = Double

type Col = Entity
type Row = Entity
-- |  -- (Term, Commodity1, Commodity2) の配列
newtype ICTable s = ICTable {_ictable :: STArray s (Term, Row, Col) InputCoefficient}


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
    arr <- newArray ((initTerm, fstEnt, fstEnt), (lastTerm, lastEnt, lastEnt)) 0  -- 初期値は0
    let termCoefficients = M.fromList [(t, initTermCoefficients g inhouseRatio) | t <- [initTerm .. lastTerm]]
    forM_ [(t, c2) | t <- [initTerm .. lastTerm], c2 <- [fstEnt .. lastEnt]] $ \(t, c2) -> do
        let row = termCoefficients M.! t M.! c2  -- Term ごとに固定
        forM_ (zip [fstEnt..lastEnt] row) $ \(c1, coef) ->
            writeArray arr (t, c1, c2) coef
    return $ ICTable arr


-- | 生産関数の更新
-- 前の期の簿記から計算する
-- ただし,今回は価格固定なので変化なし
instance Updatable Term EnvVar ST s (ICTable s) where
    initialize g _ e = initICTables g (_inhouseRatio e)
    updatePattern _ _ _ = return DoNothing


-- | 投入係数の取得
getInputCoefficient :: World s -> Term -> (Entity, Entity) -> ST s InputCoefficient
getInputCoefficient wld t (c1,c2) =  do
                let ics = _ictable (_ics wld)
                readArray ics (t,c1,c2)


-- | 一単位の財の簿記を取得する
getOneProduction :: World s -> Term -> Entity -> ST s Transaction
getOneProduction wld t c = do
    let arr =  _ictable (_ics wld)  -- ICTable を取得
    inputs <- CM.forM industries $ \c2 -> do
        coef <- readArray arr (t, c2, c)  -- c を生産するために必要な c2 の投入係数
        return $ coef :@ Hat :<(Products, c2, c, Amount) .| (Production,t) -- c2 の消費を記録
    let totalInput = foldl (.+) Zero inputs  -- すべての中間投入を結合
    return $ (1 :@ Not :<(Products, c, c, Amount) .| (Production,t)) .+ totalInput   -- 生産と投入の合計

-- | 一期の算出を取得する
getTermProduction :: World s -> Term -> EnvVar -> Entity -> ST s Double
getTermProduction wld t env e = do
    pt <- readSTRef (_prices wld)
    le <- readSTRef (_ledger wld)
    let currentTotal = norm
                     $ EJ.projWithNoteBase [(Production,t)]
                                           [Not:<(Products, e,e,Yen)]
                                           le
    return currentTotal

------------------------------------------------------------------
-- ** 発注書の状態空間
------------------------------------------------------------------
-- 発注された中間投入財の量 = 生産が必要な量
-- 基本的には発注分を除けば前年度と同じ量を発注
-- 財ごとのInt
type Order = Double
type OrderTable s = STArray s (Term,Entity,Entity) Order


-- 発注量の初期状態
initOrders :: ICTable s -> Double -> Double -> ST s (OrderTable s)
initOrders icTable finalDemand inhouseRatio = do
    ((tMin, c1Min, c2Min), (tMax, c1Max, c2Max)) <- getBounds (_ictable icTable)
    ordersArr <- newArray ((tMin, c1Min, c2Min), (tMax, c1Max, c2Max)) 0
    forM_ [(t, c1, c2) | t <- [tMin .. tMax]
                       , c1 <- [c1Min .. c1Max]
                       , c2 <- [c2Min .. c2Max]] $ \(t, c1, c2) -> do
        when (c2 == finalDemandSector) $ do
            c <- readArray (_ictable icTable) (t, c1,c2)
            writeArray ordersArr (t, c1, c2) ((c / inhouseRatio) * finalDemand)
    return ordersArr

-- ** STArray s (ID, Term) NN.Double
instance Updatable Term EnvVar ST s (OrderTable s) where
    initialize g _ e = do
        icTable <- initICTables g (_inhouseRatio e)  -- ICTable を初期化
        initOrders icTable (_finalDemand e) (_inhouseRatio e)  -- それを基に OrdersTable を作成

    updatePattern _ _ _  = return Modify

    -- 前期の残りの分を追加
    modify t e x  =  case (t == initTerm, t == 20)of
                    (True, _)   -> return ()
                    (_, True)   -> forM_ [fstEnt..lastEnt] $ \e1
                                -> forM_ [fstEnt..lastEnt] $ \e2
                                -> case (e1 == 10, e2==9) of -- 9の最終需要の増加
                                    (True,True) -> readArray x (t-1, e2,e1) >>= \y
                                                -> modifyArray x (t,e2,e1) (\x -> x + y + (_addedDemend e))
                                    ------------------------------------------------------------------
                                    _           -> readArray x (t-1, e2,e1) >>= \y
                                                -> modifyArray x (t,e2,e1) (\x -> x + y)
                    ------------------------------------------------------------------
                    _           -> forM_ [fstEnt..lastEnt] $ \e1
                                -> forM_ [fstEnt..lastEnt] $ \e2
                                -> readArray x (t-1, e2,e1) >>= \y
                                -> modifyArray x (t,e2,e1) (\x -> x + y)

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
data World s = World { _ledger  :: Ledger s
                     , _prices  :: Prices s
                     , _ics     :: ICTable s
                     , _orders  :: OrderTable s
                     , _mss     :: MSs s
                     , _irs     :: IRs s
                     , _gen     :: Gen s }
                     deriving (Generic)

-- deriving Generic をしていれば
-- 空のインスタンス宣言で自動でinitSS,updateSSが使えるようになる
instance StateSpace Term EnvVar ST s (World s)


------------------------------------------------------------------
-- * 汎用関数
------------------------------------------------------------------

-- | 生産可能量の計算
-- 現在の原材料在庫に基づいて生産可能量を計算する
-- 在庫量 / 投入係数の最小値が生産可能量
getPossibleVolume :: World s -> Term -> Entity -> ST s Double
getPossibleVolume wld t e1 = do
    let ics = _ictable (_ics wld)
    le  <- readSTRef (_ledger wld)

    -- 各 e2 (原材料) に対して、生産可能量の計算
    mbps <- CM.forM industries $ \e2 -> do
        -- 原材料在庫
        let n = norm
              $ projWithBase [Not :< (Products, e2, e1, Amount)]
              $ (.-)
              $ termJournal t le

        -- 投入係数
        c <- readArray ics (t, e2, e1)
        -- 0除算防止
        case (c == 0) of
            False -> return (Just (n / c))
            True  -> return Nothing

    let possibleVolumes = L.map (\(Just x) -> x)
                        $ L.filter (\x -> if x == Nothing then False else True) mbps
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


-- 記帳
journal :: World s -> Term -> Transaction -> ST s ()
journal wld t Zero = return ()
journal wld t js   = modifySTRef (_ledger wld) (\x -> x .+ js)

event :: World s -> Term -> EventName -> ST s ()

--  通常簿記を物量簿記に変換する
event wld t ToAmount = readSTRef (_prices wld) >>= \pt
              -> modifySTRef (_ledger wld) $ \le
              -> EJT.transferKeepWiledcard le (toAmountTable pt)

--  物量簿記を通常簿記に変換する
event wld t ToPrice = readSTRef (_prices wld) >>= \pt
              -> modifySTRef (_ledger wld) $ \bk
              -> EJT.transferKeepWiledcard bk (toCashTable pt)


------------------------------------------------------------------
-- 受注分販売・購入する
-- 在庫を受注の割合で販売する
-- 売れた分受注を減らす
event wld t SalesPurchase = do
    le <- readSTRef (_ledger wld)  -- 簿記の状態を取得
    let os = _orders wld  -- OrdersTable を取得

    forM_ industries $ \e1 -> do
        -- 受注総量を取得
        totalOrder <- getOrderTotal wld t e1
        -- 在庫保有量
        let stock = culcStock t e1 le
        -- 各 e2 (注文元) に対して処理
        forM_ [fstEnt..lastEnt] $ \e2 -> do
            orderAmount <- readArray os (t, e1, e2)  -- e2 から e1 への受注量

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
                    writeArray os (t, e1, e2) (orderAmount - sellAmount)

------------------------------------------------------------------
-- | (在庫比率 * 受注量 - 在庫の量) のうち,現在保有している原材料在庫で生産する
event wld t Production = do
    le <- readSTRef (_ledger wld)  -- Book を取得
    let os = (_orders wld)  -- OrdersTable を取得
    forM_ industries $ \e1 -> do
        r <- getInventoryRatio wld e1
        -- 販売後の総受注量
        n <- getOrderTotal wld t e1
        -- 販売量
        let sales = (norm . (.-))
                  $ projWithNoteBase [(SalesPurchase, t)] [Hat:<(Products,e1,e1,Amount)] le
        -- 在庫保有量
        let stock = culcStock t e1 le

        -- 今期の発注量に対するr倍以上のストックを保つだけの生産
        let plan = case compare (r * (n + sales)) stock of
                        -- 在庫が受注と次期の在庫より少ない場合
                        GT -> (r * (n + sales)) - stock
                        -- 等しいか多い場合 生産する必要なし
                        -- ただし
                        _ -> 0.5 * (n + sales)

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
event wld t Order = do
    let os = (_orders wld)  -- OrdersTable を取得
    le <- readSTRef (_ledger wld)  -- Book を取得


    forM_ industries $ \e1 -> do
        r <- getInventoryRatio wld e1
        n <- getOrderTotal wld t e1
        m <- getMaterialStock wld e1
        let sales = (norm . (.-))
                  $ projWithNoteBase [(SalesPurchase, t)] [Hat:<(Products,e1,e1,Amount)] le
        -- 今期の生産量
        let prod = norm
                 $ projWithNoteBase [(Production, t)] [Not:<(Products,e1,e1,Amount)] le
        -- 在庫保有量
        let stock = culcStock t e1 le
        -- 来期の不足分
        let short = case compare (r * (n + sales)) stock of
                        GT -> (r * (n + sales)) - stock
                        _  -> 0
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
            let short_material = short * c

            -- 前回の需要量を次期生産できるだけの原材料在庫
            let next_material = m * (n + sales) * c

            let total = case compare (short_material + next_material) materialStock of
                                    GT -> (short_material + next_material) - materialStock
                                    _  -> 0
            -- when (e1==9) $ do
            --     trace (show t ++ ":total:" ++ show total) return ()
            --     trace (show t ++ ":short_material:" ++ show short_material) return ()
            --     trace (show t ++ ":next_material:" ++ show next_material) return ()
            --     trace (show t ++ ":stock:" ++ show materialStock) return ()

            -- 今期の生産分のm倍
            modifyArray os (t, e2, e1) (\x -> x + total)

------------------------------------------------------------------
-- 最終需要部門が購入したものを消費する
event wld t Consumption = do
    -- 保有している消費財
    le <- readSTRef (_ledger wld)
    let total_consume = projWithNoteBase [(SalesPurchase,t)]
                                         [Not:<(Products,(.#),finalDemandSector,(.#))]
                                         le
    journal wld t $ gather (Consumption,t)
                  $ (.^) total_consume
------------------------------------------------------------------
event wld t Plank = return ()

------------------------------------------------------------------
-- | Transaction全体を結合
eventAll :: forall s.  World s -> Term ->  ST s ()
eventAll wld t =  CM.forM_ ([minBound .. maxBound] :: [EventName])
                $ \e -> event wld t e

------------------------------------------------------------------
-- * Simulation
------------------------------------------------------------------
simulate ::  World s -> EnvVar -> ST s ()
simulate wld e = loop wld initTerm e
  where
  {-# INLINE loop #-}
  loop ::  World s -> Term -> EnvVar -> ST s ()
  loop wld t e
    | t == lastTerm =  updateAll t e wld >>= \_
                    -> eventAll wld t
    | otherwise     =  updateAll t e wld >>= \_
                    -> eventAll wld t
                    >> loop wld (nextTerm t) e

-- | 初期化 → 指定されたイベントの実行までをこなす
runSimulation :: Term -> EnvVar -> StdGen -> ST s (World s)
runSimulation t e gen = initAll gen t e >>= \wld'
                    -> simulate wld' e
                    >> return wld'

------------------------------------------------------------------
-- * 実行
------------------------------------------------------------------
main :: IO ()
main = do
    print("-----1------")
    let gen = mkStdGen seed
        env1 = EnvVar {_initInv      = 100
                      ,_initIR       = 2.1
                      ,_initMS       = 2.1
                      ,_addedDemend  = 0
                      ,_finalDemand  = 200
                      ,_inhouseRatio = 0.4}
    wld1 <- stToIO $ runSimulation (initTerm :: Term) env1 gen
    le <- stToIO $ readSTRef (_ledger wld1)
    print $ projWithBase [HatNot :<(Products,2,1,(.#))] $ termJournal (-1) le
    print $ norm $ projWithBase [HatNot :<(Products,2,1,(.#))] $ (.-) $ termJournal 2 le
    op <- stToIO $ getOneProduction wld1 1 1
    plotGridLine (productionPath env1 ) wld1 "exsample/result/fig/ripple1/normal/" "Production"
    plotGridLine (stockPath env1 ) wld1 "exsample/result/fig/ripple1/normal/" "Stock"
    plotGridLine grossProfitPath wld1 "exsample/result/fig/ripple1/normal/" "Cash(Gross Profit)"
    let ics = _ictable (_ics wld1)
    arr <- stToIO (freeze ics :: ST RealWorld (Array (Term, Row, Col) InputCoefficient))
    ioics <- thaw arr :: IO (IOArray (Term, Row, Col) InputCoefficient)
    writeTermIO "exsample/result/csv/ripple1_io.csv" 1 ioics

    {-
    ------------------------------------------------------------------
    print("-----2------")
    let env2 = EnvVar {_initInv      = 100
                      ,_initIR       = 2.2
                      ,_initMS       = 1.1
                      ,_addedDemend  = 10
                      ,_finalDemand  = 10
                      ,_inhouseRatio = 0.4}
    wld2 <- stToIO $ runSimulation (initTerm :: Term) env2 gen

    plotGridLine (productionPath env2 ) wld2 "exsample/result/fig/ripple1/addedDemand/" "Production-add100"
    plotGridLine (productionDiffPath env1 env2 wld2) wld1 "exsample/result/fig/ripple1/addedDemand/" "Production-Diff-add100"
    plotGridLineMultiColor (productionTwoSeriesPath env2 env1 wld2) wld1 "exsample/result/fig/ripple1/addedDemand/" "Production-Compare-add100"

    ------------------------------------------------------------------
    print("-----3------")
    let env3 = EnvVar {_initInv      = 100
                      ,_initIR       = 2.2
                      ,_initMS       = 1.1
                      ,_addedDemend  = 50
                      ,_finalDemand  = 10
                      ,_inhouseRatio = 0.4}
    wld3 <- stToIO $ runSimulation (initTerm :: Term) env3 gen
    plotGridLine (productionPath env3 ) wld3 "exsample/result/fig/ripple1/addedDemand/" "Production-add500"
    plotGridLine (productionDiffPath env1 env3 wld3) wld1 "exsample/result/fig/ripple1/addedDemand/" "Production-Diff-add500"
    plotGridLineMultiColor (productionTwoSeriesPath env3 env1 wld3) wld1 "exsample/result/fig/ripple1/addedDemand/" "Production-Compare-add500"
    ------------------------------------------------------------------
    print("-----4------")
    let env4 = EnvVar {_initInv      = 100
                      ,_initIR       = 1
                      ,_initMS       = 1.1
                      ,_addedDemend  = 0
                      ,_finalDemand  = 10
                      ,_inhouseRatio = 0.4}
    wld4 <- stToIO $ runSimulation (initTerm :: Term) env4 gen
    plotGridLine (productionPath env4 ) wld4 "exsample/result/fig/ripple1/smallstock/" "Production-Small-Stock"
    plotGridLine grossProfitPath wld4 "exsample/result/fig/ripple1/smallstock/" "Cash(Gross Profit)-Small-Stock"
    ---------------------------------------------------------------22---
    print("-----5------")
    let env5 = EnvVar {_initInv      = 100
                      ,_initIR       = 3
                      ,_initMS       = 1.1
                      ,_addedDemend  = 0
                      ,_finalDemand  = 10
                      ,_inhouseRatio = 0.4}
    wld5 <- stToIO $ runSimulation (initTerm :: Term) env5 gen
    plotGridLine (productionPath env5 ) wld5 "exsample/result/fig/ripple1/largestock/" "Production-Large-Stock"
    plotGridLine grossProfitPath wld5 "exsample/result/fig/ripple1/largestock/" "Cash(Gross Profit)-Large-Stock"
    ------------------------------------------------------------------
    -}
    -- bk <- stToIO $ readSTRef (_book wld)
    -- print $ proj [Not :<(Products,1,1,2,(.#))] bk
    -- print $ norm $ proj [Not :<(Products,1,1,2,(.#))] bk
    -- writeBS "exsample/result/csv/ripple1.csv" $ EA.grossProfitTransferKeepWiledcard bk



