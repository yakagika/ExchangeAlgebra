
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}

{- |
    Module     : ExchangeAlgebra
    Copyright  : (c) Kaya Akagi. 2025
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}


module ExchangeAlgebraMap.Simulate.Visualize (gridLine
                                             ,plotLine
                                             ,plotMultiLines
                                             ,plotWldsDiffLine) where

import              ExchangeAlgebraMap.Simulate
import qualified    Data.List as L
import              Graphics.Rendering.Chart.Easy            hiding ( (:<),(.~))
import              Graphics.Rendering.Chart.Backend.Cairo
import              Graphics.Rendering.Chart.Axis
import              Graphics.Rendering.Chart.Axis.Int
import              Graphics.Rendering.Chart.Grid
import qualified    Control.Monad                   as CM
import              Control.Monad.ST
import              Data.Array.ST
import              Data.STRef
import qualified Data.Set as Set

type Title          = String
type FileName       = String
type Label          = String

type TimeSeries t    = (Label, [[(t, Double)]])
type TimeSerieses t  = [(TimeSeries t)]
type GridColumns t   = [TimeSerieses t]
type GridMatrix t    = [GridColumns t]


------------------------------------------------------------------
-- * Grid Graph
------------------------------------------------------------------
------------------------------------------------------------------
-- ** Single Line
------------------------------------------------------------------

-- | 各エージェントの指定された出力をグリッドで分割して折れ線グラフにする
plotLine
    :: ( StateTime t
       , Enum t
       , PlotValue t
       , Ord i
       , Show i
       , Ix i
       , Enum i
       )
    => (a RealWorld -> t -> i -> ST RealWorld Double)
    -> (i,i)  -- ^ 範囲 (start, end)
    -> a RealWorld                 -- ^ worldなどの状態
    -> FileName                    -- ^ 出力ファイルを置くディレクトリ名
    -> Title                       -- ^ グラフのタイトル
    -> IO ()
plotLine f (start,end) wld fileDir titleStr = do
    gridMatrix <- funcArray f (start,end) wld
    let gridRenderable = createGrid gridMatrix
    CM.void $ renderableToFile def (fileDir ++ "/" ++ titleStr ++ ".png")
            $ fillBackground def
            $ gridToRenderable
            $ createTitle titleStr `wideAbove` gridRenderable

  where
    -- | グリッド全体を垂直方向に並べて1つのGridを作成
    createGrid = aboveN . map createRow

    -- | 各行を横方向に並べて1つの行を作成
    createRow = besideN . map createColumn

    -- | 各カラムに含まれる時系列データをプロットとして配置
    createColumn seriesGroup = layoutToGrid $ execEC $ do
        CM.forM_ seriesGroup $ \(label, seriesData) ->
            plot $ linePlot label seriesData

    -- | 1つの時系列データをラインプロットとして描画
    linePlot label seriesData = liftEC $ do
        plot_lines_values .= [concat seriesData]
        plot_lines_title  .= label
        plot_lines_style . line_color .= opaque blue

    -- | グラフのタイトルを設定
    createTitle name = setPickFn nullPickFn $
        label titleStyle HTA_Centre VTA_Centre name

    -- | タイトルのフォントスタイルを定義
    titleStyle :: FontStyle
    titleStyle = def
        { _font_size   = 15
        , _font_weight = FontWeightBold
        }

{-# INLINE funcArray #-}
funcArray :: ( StateTime t
             , Enum t
             , PlotValue t
             , Ord i
             , Show i
             , Ix i
             , Enum i)
          => (a RealWorld -> t -> i -> ST RealWorld Double)
          -> (i,i)  -- ^ 範囲 (start, end)
          -> a RealWorld
          -> IO (GridMatrix t)

funcArray f (start,end) wld = stToIO $ do
    arr <- newArray ((start, initTerm), (end, lastTerm)) 0
    CM.forM_ [initTerm .. lastTerm ] $ \t
        -> CM.forM_ [start .. end] $ \i
            -> f wld t i >>= \v
            -> modifyArray arr (i, t) (\x -> x + v)

    gridLine arr



-- | データをグリッドに分割する
gridLine  :: (Ord a, Show a, Ix a, StateTime t)
                    => STArray s (a,t) Double
                    -> ST s (GridMatrix t)
gridLine arr = do
        grid    <- newSTRef [] :: ST s (STRef s (GridMatrix t))
        col     <- newSTRef [] :: ST s (STRef s (GridColumns t))
        count   <- newSTRef 1  :: ST s (STRef s Int)

        idx <- getBounds arr
        let as = Set.toAscList
               $ Set.fromList
               $ L.map fst
               $ range idx

        CM.forM_ as ( \e -> do
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
                False   ->  case e == L.last as of
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

------------------------------------------------------------------
-- ** Two lines
------------------------------------------------------------------
-- | Color Palette
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

-- | 複数のラインを各グリッドにプロットする
{-# INLINE plotMultiLines #-}
plotMultiLines
    ::  ( StateTime t
       , Enum t
       , PlotValue t
       , Ord i
       , Show i
       , Ix i
       , Enum i
       )
    => [Label]
    -> [(a RealWorld -> t -> i -> ST RealWorld Double)]  -- ^ グラフ用データを生成する関数
    -> (i,i)  -- ^ 範囲 (start, end)
    -> [a RealWorld]               -- ^ worldなどの状態
    -> FilePath                    -- ^ 出力先ディレクトリ
    -> String                      -- ^ グラフタイトル
    -> IO ()
plotMultiLines xs fs (start,end) wlds fileDir titleStr = do
    gridMatrix <- funcArrays xs fs (start,end) wlds
    let gridRenderable = createGrid gridMatrix
    CM.void $ renderableToFile def (fileDir ++ "/" ++ titleStr ++ ".png")
            $ fillBackground def
            $ gridToRenderable
            $ createTitle titleStr `wideAbove` gridRenderable
  where
    -- | グリッド全体を垂直方向に配置
    createGrid = aboveN . map createRow

    -- | 各行のグリッドを水平方向に配置
    createRow = besideN . map createColumn

    -- | 各カラムに含まれる複数の時系列データをプロット（色分け）
    createColumn seriesGroup = layoutToGrid $ execEC $
        CM.forM_ (zip [0..] seriesGroup) $ \(index, (label, seriesData)) ->
            plot $ linePlot index label seriesData

    -- | 時系列データをカラーラインプロットとして描画
    linePlot idx label seriesData = liftEC $ do
        plot_lines_values .= [concat seriesData]
        plot_lines_title  .= label
        plot_lines_style . line_color .= colorPalette !! (idx `mod` length colorPalette)

    -- | グラフのタイトルを設定
    createTitle name = setPickFn nullPickFn $
        label titleStyle HTA_Centre VTA_Centre name

    -- | タイトルのフォントスタイル
    titleStyle :: FontStyle
    titleStyle = def
        { _font_size   = 15
        , _font_weight = FontWeightBold
        }

{-# INLINE funcArrays #-}
funcArrays :: ( StateTime t
             , Enum t
             , PlotValue t
             , Ord i
             , Show i
             , Ix i
             , Enum i)
          => [Label]
          -> [(a RealWorld -> t -> i -> ST RealWorld Double)]
          -> (i,i)  -- ^ 範囲 (start, end)
          -> [a RealWorld]
          -> IO (GridMatrix t)

funcArrays xs fs (start,end) wlds = stToIO $ do
    arrs <- CM.forM (zip wlds fs) $ \(wld,f) -> do
        arr <- newArray ((start, initTerm), (end, lastTerm)) 0
        CM.forM_ [initTerm .. lastTerm ] $ \t
            -> CM.forM_ [start .. end] $ \i
                -> f wld t i >>= \v
                -> modifyArray arr (i, t) (\x -> x + v)
        return arr

    gridLines xs arrs


-- | gridLine を拡張して,
--   同じセル内に複数の系列をまとめて入れる
gridLines
  :: (Ord a, Show a, Ix a, StateTime t)
  => [Label]
  -> [STArray s (a, t) Double]
  -> ST s (GridMatrix t)
gridLines xs arrs = do
    -- GridMatrix全体，列(3つ毎に1列にまとめたい等)，カウンタを用意
    gridRef  <- newSTRef []        :: ST s (STRef s (GridMatrix t))
    colRef   <- newSTRef []        :: ST s (STRef s (GridColumns t))
    countRef <- newSTRef 1  :: ST s (STRef s Int)
    idx <- getBounds (head arrs)
    let as = Set.toAscList
           $ Set.fromList
           $ L.map fst
           $ range idx

    CM.forM_ as $ \ e -> do
      count' <- readSTRef countRef
      timeVals <- CM.forM arrs $ \ arr -> do
            -- シリーズごとのタイム系列
            ts <- CM.forM [initTerm .. lastTerm] $ \ t -> do
                        v <- readArray arr (e,t)
                        return (t, v)
            return ts
      -- 同じセルに2系列をまとめる
      -- (String, [[(Term, Double)]]) がTimeSeriesの型
      -- シリーズが複数あるので TimeSerieses = [TimeSeries]
      let cell = map (\(l,vs) -> (show e ++ "_" ++ show l, [vs]))
               $ zip xs timeVals

      -------------------------------------------------------------------------
      -- 以下は gridPathSingleLine と同じく, 3つで1カラムにまとめるロジック
      -------------------------------------------------------------------------
      if count' >= 3
        then do
          col' <- readSTRef colRef
          -- いままで溜めたcol'に今回のセルを追加して，
          -- それらをひとまとめ(1列分)としてgridRefに積む
          modifySTRef gridRef (\acc -> acc ++ [col' ++ [cell]])
          -- カラムとカウンタをリセット
          writeSTRef colRef []
          writeSTRef countRef 1

        else if e == last as
          then do
            -- 最後の要素の場合は，ここまで溜まったものをまとめて追加
            col' <- readSTRef colRef
            modifySTRef gridRef (\acc -> acc ++ [col' ++ [cell]])
            writeSTRef colRef []
            writeSTRef countRef 1

          else do
            -- まだ3列に達していない場合は，colRefに追加のみ
            modifySTRef colRef (\acc -> acc ++ [cell])
            modifySTRef countRef (+1)

    -- すべて終わったらGridMatrixを返す
    readSTRef gridRef

------------------------------------------------------------------
plotWldsDiffLine :: ( StateTime t
               , Enum t
               , PlotValue t
               , Ord i
               , Show i
               , Ix i
               , Enum i
               )
            => (a RealWorld -> t -> i -> ST RealWorld Double)
            -> (i,i)
            -> (a RealWorld, a RealWorld)
            -> FilePath                    -- ^ 出力先ディレクトリ
            -> String                      -- ^ グラフタイトル
            -> IO ()

plotWldsDiffLine f (start,end) wlds fileDir titleStr = do
    gridMatrix <- funcDiffArray f (start,end) wlds
    let gridRenderable = createGrid gridMatrix
    CM.void $ renderableToFile def (fileDir ++ "/" ++ titleStr ++ ".png")
            $ fillBackground def
            $ gridToRenderable
            $ createTitle titleStr `wideAbove` gridRenderable

  where
    -- | グリッド全体を垂直方向に並べて1つのGridを作成
    createGrid = aboveN . map createRow

    -- | 各行を横方向に並べて1つの行を作成
    createRow = besideN . map createColumn

    -- | 各カラムに含まれる時系列データをプロットとして配置
    createColumn seriesGroup = layoutToGrid $ execEC $ do
        CM.forM_ seriesGroup $ \(label, seriesData) ->
            plot $ linePlot label seriesData

    -- | 1つの時系列データをラインプロットとして描画
    linePlot label seriesData = liftEC $ do
        plot_lines_values .= [concat seriesData]
        plot_lines_title  .= label
        plot_lines_style . line_color .= opaque blue

    -- | グラフのタイトルを設定
    createTitle name = setPickFn nullPickFn $
        label titleStyle HTA_Centre VTA_Centre name

    -- | タイトルのフォントスタイルを定義
    titleStyle :: FontStyle
    titleStyle = def
        { _font_size   = 15
        , _font_weight = FontWeightBold
        }

{-# INLINE funcDiffArray #-}
funcDiffArray :: ( StateTime t
                 , Enum t
                 , PlotValue t
                 , Ord i
                 , Show i
                 , Ix i
                 , Enum i)
              => (a RealWorld -> t -> i -> ST RealWorld Double)
              -> (i,i)  -- ^ 範囲 (start, end)
              -> (a RealWorld,a RealWorld)
              -> IO (GridMatrix t)

funcDiffArray f (start,end) wlds = stToIO $ do
    arr <- newArray ((start, initTerm), (end, lastTerm)) 0
    CM.forM_ [initTerm .. lastTerm ] $ \t
        -> CM.forM_ [start .. end] $ \i
            -> f (fst wlds) t i >>= \v1
            -> f (snd wlds) t i >>= \v2
            -> modifyArray arr (i, t) (\x -> x + (v1-v2))

    gridLine arr
