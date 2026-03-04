{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}

{- |
    Module     : ExchangeAlgebra
    Copyright  : (c) Kaya Akagi. 2025
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}


module ExchangeAlgebra.Simulate.Visualize   (gridLine
                                            ,plotLine
                                            ,plotMultiLines
                                            ,plotWldsDiffLine
                                            ,plotLineVector
                                            ,writeFuncResults
                                            ,writeFuncResultsWithContext) where

import              ExchangeAlgebra.Simulate
import qualified    Data.Text as T
import qualified    Data.Text.IO as TIO
import qualified    Data.List as L
import              Graphics.Rendering.Chart.Easy            hiding ( (:<),(.~))
import              Graphics.Rendering.Chart.Backend.Cairo
import              Graphics.Rendering.Chart.Axis
import              Graphics.Rendering.Chart.Axis.Int
import              Graphics.Rendering.Chart.Grid
import qualified    Control.Monad                   as CM
import              Control.Monad.ST
import qualified    Data.Vector.Unboxed      as VU
import qualified    Data.Vector.Unboxed.Mutable as VUM
import              Data.Array.ST
import qualified Data.Set as Set
import              System.IO (IOMode(WriteMode), withFile)

type Title          = String
type FileName       = String
type Label          = String

type TimeSeries t    = (Label, [[(t, Double)]])
type TimeSerieses t  = [(TimeSeries t)]
type GridColumns t   = [TimeSerieses t]
type GridMatrix t    = [GridColumns t]

chunkN :: Int -> [a] -> [[a]]
chunkN _ [] = []
chunkN n xs =
    let (front, back) = splitAt n xs
     in front : chunkN n back


------------------------------------------------------------------
-- * Grid Graph
------------------------------------------------------------------
------------------------------------------------------------------
-- ** Single Line
------------------------------------------------------------------

-- | Plot the specified output of each agent as a line graph divided into a grid.
-- Splits into grid columns of 3, and outputs as a PNG file.
--
-- Complexity: O(|range| * T) (T = number of terms)
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
    -> (i,i)  -- ^ Range (start, end)
    -> a RealWorld                 -- ^ World state or similar
    -> FileName                    -- ^ Directory name for output files
    -> Title                       -- ^ Graph title
    -> IO ()
plotLine f (start,end) wld fileDir titleStr = do
    gridMatrix <- funcArray f (start,end) wld
    let gridRenderable = createGrid gridMatrix
    CM.void $ renderableToFile def (fileDir ++ "/" ++ titleStr ++ ".png")
            $ fillBackground def
            $ gridToRenderable
            $ createTitle titleStr `wideAbove` gridRenderable

  where
    -- | Arrange the entire grid vertically to create a single Grid
    createGrid = aboveN . map createRow

    -- | Arrange each row horizontally to create a single row
    createRow = besideN . map createColumn

    -- | Plot the time series data contained in each column
    createColumn seriesGroup = layoutToGrid $ execEC $ do
        CM.forM_ seriesGroup $ \(label, seriesData) ->
            plot $ linePlot label seriesData

    -- | Render a single time series as a line plot
    linePlot label seriesData = liftEC $ do
        plot_lines_values .= [concat seriesData]
        plot_lines_title  .= label
        plot_lines_style . line_color .= opaque blue

    -- | Set the graph title
    createTitle name = setPickFn nullPickFn $
        label titleStyle HTA_Centre VTA_Centre name

    -- | Define the title font style
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
          -> (i,i)  -- ^ Range (start, end)
          -> a RealWorld
          -> IO (GridMatrix t)

funcArray f (start,end) wld = stToIO $ do
    arr <- newArray ((start, initTerm), (end, lastTerm)) 0
    CM.forM_ [initTerm .. lastTerm ] $ \t
        -> CM.forM_ [start .. end] $ \i
            -> writeArray arr (i, t) =<< f wld t i

    gridLine arr



-- | Split STArray data into a grid with 3 columns and return as a GridMatrix.
-- Each cell contains the time series data for one agent (index).
--
-- Complexity: O(|agents| * T) (T = number of terms)
gridLine  :: (Ord a, Show a, Ix a, StateTime t)
                    => STArray s (a,t) Double
                    -> ST s (GridMatrix t)
gridLine arr = do
    idx <- getBounds arr
    let as = Set.toAscList
           $ Set.fromList
           $ L.map fst
           $ range idx

    cells <- CM.forM as $ \e -> do
        xs <- CM.forM [initTerm .. lastTerm] $ \t -> do
            v <- readArray arr (e, t)
            pure (t, v)
        pure [(show e, [xs])]

    pure (chunkN 3 cells)

------------------------------------------------------------------
-- ** Multi lines
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

-- | Plot the results of multiple worlds/functions with color coding in each grid cell.
-- Used for comparing different scenarios.
--
-- Complexity: O(|worlds| * |range| * T)
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
    -> [(a RealWorld -> t -> i -> ST RealWorld Double)]  -- ^ Functions to generate graph data
    -> (i,i)  -- ^ Range (start, end)
    -> [a RealWorld]               -- ^ World states or similar
    -> FilePath                    -- ^ Output directory
    -> String                      -- ^ Graph title
    -> IO ()
plotMultiLines xs fs (start,end) wlds fileDir titleStr = do
    gridMatrix <- funcArrays xs fs (start,end) wlds
    let gridRenderable = createGrid gridMatrix
    CM.void $ renderableToFile def (fileDir ++ "/" ++ titleStr ++ ".png")
            $ fillBackground def
            $ gridToRenderable
            $ createTitle titleStr `wideAbove` gridRenderable
  where
    -- | Arrange the entire grid vertically
    createGrid = aboveN . map createRow

    -- | Arrange each row's grid horizontally
    createRow = besideN . map createColumn

    -- | Plot multiple time series in each column with color coding
    createColumn seriesGroup = layoutToGrid $ execEC $
        CM.forM_ (zip [0..] seriesGroup) $ \(index, (label, seriesData)) ->
            plot $ linePlot index label seriesData

    -- | Render time series data as a colored line plot
    linePlot idx label seriesData = liftEC $ do
        plot_lines_values .= [concat seriesData]
        plot_lines_title  .= label
        plot_lines_style . line_color .= colorPalette !! (idx `mod` length colorPalette)

    -- | Set the graph title
    createTitle name = setPickFn nullPickFn $
        label titleStyle HTA_Centre VTA_Centre name

    -- | Title font style
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
          -> (i,i)  -- ^ Range (start, end)
          -> [a RealWorld]
          -> IO (GridMatrix t)

funcArrays xs fs (start,end) wlds = stToIO $ do
    arrs <- CM.forM (zip wlds fs) $ \(wld,f) -> do
        arr <- newArray ((start, initTerm), (end, lastTerm)) 0
        CM.forM_ [initTerm .. lastTerm ] $ \t
            -> CM.forM_ [start .. end] $ \i
                -> writeArray arr (i, t) =<< f wld t i
        return arr

    gridLines xs arrs


-- | Extension of gridLine that
--   groups multiple series into the same cell
gridLines
  :: (Ord a, Show a, Ix a, StateTime t)
  => [Label]
  -> [STArray s (a, t) Double]
  -> ST s (GridMatrix t)
gridLines xs arrs = do
    idx <- getBounds (head arrs)
    let as = Set.toAscList
           $ Set.fromList
           $ L.map fst
           $ range idx

    cells <- CM.forM as $ \e -> do
      timeVals <- CM.forM arrs $ \arr -> do
            -- Time series for each series
            ts <- CM.forM [initTerm .. lastTerm] $ \ t -> do
                        v <- readArray arr (e,t)
                        return (t, v)
            return ts
      -- Group two series into the same cell
      -- TimeSeries has type (String, [[(Term, Double)]])
      -- Multiple series form TimeSerieses = [TimeSeries]
      let cell = map (\(l,vs) -> (show e ++ "_" ++ show l, [vs]))
               $ zip xs timeVals
      pure cell

    pure (chunkN 3 cells)

------------------------------------------------------------------
-- | Plot the difference between two worlds in a grid.
-- Each cell plots @f wld1 t i - f wld2 t i@.
--
-- Complexity: O(|range| * T)
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
            -> FilePath                    -- ^ Output directory
            -> String                      -- ^ Graph title
            -> IO ()

plotWldsDiffLine f (start,end) wlds fileDir titleStr = do
    gridMatrix <- funcDiffArray f (start,end) wlds
    let gridRenderable = createGrid gridMatrix
    CM.void $ renderableToFile def (fileDir ++ "/" ++ titleStr ++ ".png")
            $ fillBackground def
            $ gridToRenderable
            $ createTitle titleStr `wideAbove` gridRenderable

  where
    -- | Arrange the entire grid vertically to create a single Grid
    createGrid = aboveN . map createRow

    -- | Arrange each row horizontally to create a single row
    createRow = besideN . map createColumn

    -- | Plot the time series data contained in each column
    createColumn seriesGroup = layoutToGrid $ execEC $ do
        CM.forM_ seriesGroup $ \(label, seriesData) ->
            plot $ linePlot label seriesData

    -- | Render a single time series as a line plot
    linePlot label seriesData = liftEC $ do
        plot_lines_values .= [concat seriesData]
        plot_lines_title  .= label
        plot_lines_style . line_color .= opaque blue

    -- | Set the graph title
    createTitle name = setPickFn nullPickFn $
        label titleStyle HTA_Centre VTA_Centre name

    -- | Define the title font style
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
              -> (i,i)  -- ^ Range (start, end)
              -> (a RealWorld,a RealWorld)
              -> IO (GridMatrix t)

funcDiffArray f (start,end) wlds = stToIO $ do
    arr <- newArray ((start, initTerm), (end, lastTerm)) 0
    CM.forM_ [initTerm .. lastTerm ] $ \t
        -> CM.forM_ [start .. end] $ \i
            -> f (fst wlds) t i >>= \v1
            -> f (snd wlds) t i >>= \v2
            -> writeArray arr (i, t) (v1-v2)

    gridLine arr

--------------------------------------------------------------------------------
-- 1. Function to create an (i, t) -> Double array using Vector
--------------------------------------------------------------------------------

-- | Corresponds to the original funcArray. Treats (i, t) as a 2D index and stores the value of f wld t i.
--   A version that replaces the internal implementation from STArray with Unboxed Vector.
funcArrayVector
  :: ( Show i, Enum i, Enum t, StateTime t )
  => (a RealWorld -> t -> i -> ST RealWorld Double)  -- ^ Computation function f
  -> ((i,t),(i,t))                                   -- ^ Range (start, end)
  -> a RealWorld                                     -- ^ World state or similar
  -> ST RealWorld (GridMatrix t)                -- ^ Returns the completed immutable Vector
funcArrayVector f ((i1,t1),(i2,t2)) wld = do
    let iCount = fromEnum i2 - fromEnum i1 + 1
        tCount = fromEnum t2 - fromEnum t1 + 1
        totalCount = iCount * tCount

    -- Initialize a Mutable Vector of length totalCount with 0.0
    mvec <- VUM.replicate totalCount 0.0

    -- Compute and write f wld t i in a nested loop
    CM.forM_ [fromEnum t1 .. fromEnum t2] $ \te -> do
      let t = toEnum te
      CM.forM_ [fromEnum i1 .. fromEnum i2] $ \ie -> do
        let i = toEnum ie
            idx = (te - fromEnum t1) * iCount
                + (ie - fromEnum i1)
        VUM.write mvec idx =<< f wld t i

    -- Finally freeze into an immutable Vector
    gridLineVector ((i1,t1),(i2,t2)) =<< VU.freeze mvec

--------------------------------------------------------------------------------
-- 2. Build GridMatrix t from a Vector (equivalent to the original gridLine)
--------------------------------------------------------------------------------

-- | Convert 2D (i, t) data into columns split every 3 cells, forming a GridMatrix.
gridLineVector
  :: forall t i. ( Enum i, Enum t, StateTime t, Show i )
  => ((i,t),(i,t))           -- ^ (start, end)
  -> VU.Vector Double -- ^ Element count = (end - start + 1) * (lastTerm - initTerm + 1)
  -> ST RealWorld (GridMatrix t)
gridLineVector ((i1,t1),(i2,t2)) vec = do
    -- List of possible values of i (ascending)
    let iVals  = [fromEnum i1 .. fromEnum i2]
    let iCount = length iVals
    let tVals  = [fromEnum t1 .. fromEnum t2]
    cells <- CM.forM iVals $ \ie -> do
      let iVal = (toEnum (ie :: Int) :: i)  -- i :: i
      -- Extract the (t, value) series
      let series :: [(t, Double)]
          series =
            [ (toEnum te, VU.unsafeIndex vec (flattenIndex iCount (ie - fromEnum i1) (te - fromEnum t1)))
            | te <- tVals
            ]
          -- TimeSeries has type (Label, [[(t,Double)]]) so
          -- shape it into [[(t,Double)]]
          ts :: [(Label, [[(t,Double)]])]
          ts = [(show iVal, [series])]
      pure ts

    pure (chunkN 3 cells)
  where
    -- flattenIndex iCount iIndex tIndex = tIndex * iCount + iIndex
    flattenIndex :: Int -> Int -> Int -> Int
    flattenIndex width x y = y * width + x

--------------------------------------------------------------------------------
-- 3. Function to draw (i, t) line graphs using a Vector-based approach
--------------------------------------------------------------------------------

-- | Draw line graphs using Unboxed Vectors.
-- Equivalent to 'plotLine', but uses Unboxed Vectors instead of STArray internally,
-- providing better memory efficiency for large-scale data.
--
-- Complexity: O(|range| * T)
plotLineVector
  :: ( Enum i, Enum t, StateTime t, Show i, PlotValue t )
  => (a RealWorld -> t -> i -> ST RealWorld Double)
  -> ((i,t),(i,t))    -- ^ (start, end)
  -> a RealWorld      -- ^ World state
  -> FilePath         -- ^ Output directory
  -> String           -- ^ Graph title
  -> IO ()
plotLineVector f idx wld outDir titleStr = do
    -- 2. Convert to GridMatrix (gridLineVector)
    gridMatrix :: GridMatrix t <- stToIO $ funcArrayVector f idx wld

    -- 3. Render the chart
    let gridRenderable = createGrid gridMatrix
    CM.void $ renderableToFile def (outDir ++ "/" ++ titleStr ++ ".png")
            $ fillBackground def
            $ gridToRenderable
            $ createTitle titleStr `wideAbove` gridRenderable

  where
    -- | Arrange the entire grid vertically
    createGrid = aboveN . map createRow

    -- | Arrange one row (GridColumns t) horizontally
    createRow = besideN . map createColumn

    -- | Plot multiple TimeSeries contained in one column (TimeSerieses t)
    createColumn seriesGroup = layoutToGrid $ execEC $ do
      CM.forM_ seriesGroup $ \(label, seriesData) ->
        plot $ linePlot label seriesData

    linePlot label seriesData = liftEC $ do
      plot_lines_values .= [concat seriesData]
      plot_lines_title  .= label
      plot_lines_style . line_color .= opaque blue

    createTitle name =
      setPickFn nullPickFn $
        label titleStyle HTA_Centre VTA_Centre name

    titleStyle :: FontStyle
    titleStyle = def
      { _font_size   = 15
      , _font_weight = FontWeightBold
      }


type Header = T.Text

-- | Build a context for each term, then evaluate multiple functions together and output to CSV.
-- By sharing each term's context, expensive preprocessing (e.g., termJournal, transfer) is reduced to once per term.
-- Uses streaming output, so memory usage does not depend on the number of terms.
--
-- Complexity: O(T * (cost(buildCtx) + |funcs| * cost(f)))
writeFuncResultsWithContext
  :: ( StateTime t
     , Show x
     , Num x
     )
  => (a RealWorld -> t -> ST RealWorld c)
  -> [(Header, c -> ST RealWorld x)]
  -> (t,t)
  -> a RealWorld
  -> FilePath
  -> IO ()
writeFuncResultsWithContext buildCtx funcs (tStart,tEnd) wld path = do
    withFile path WriteMode $ \h -> do
        TIO.hPutStrLn h (toCsvRow (T.pack "Time" : map fst funcs))
        CM.forM_ [tStart .. tEnd] $ \t -> do
            vals <- stToIO $ do
                ctx <- buildCtx wld t
                CM.forM funcs $ \(_, f) -> f ctx
            let row = T.pack (show t) : map (T.pack . show) vals
            TIO.hPutStrLn h (toCsvRow row)

-- | Output the results of given functions as CSV time series data.
-- Internally uses streaming output via 'writeFuncResultsWithContext'.
--
-- Complexity: O(T * |funcs| * cost(f))
writeFuncResults
  :: ( StateTime t
     , Show x
     , Num x
     )
  => [(Header,(a RealWorld -> t -> ST RealWorld x))]
  -> (t,t)
  -> a RealWorld
  -> FilePath
  -> IO ()
writeFuncResults funcs range wld path =
    writeFuncResultsWithContext
        (\_ t -> return t)
        (map (\(header, f) -> (header, \t -> f wld t)) funcs)
        range
        wld
        path

{-# INLINE toCsvRow #-}
toCsvRow :: [T.Text] -> T.Text
toCsvRow = T.intercalate (T.pack ",") . map escapeCsv

{-# INLINE escapeCsv #-}
escapeCsv :: T.Text -> T.Text
escapeCsv t
    | T.any isSpecial t = T.concat [T.pack "\"", T.replace (T.pack "\"") (T.pack "\"\"") t, T.pack "\""]
    | otherwise         = t
  where
    isSpecial c = c == ',' || c == '"' || c == '\n' || c == '\r'
