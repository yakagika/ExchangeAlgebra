{- |
    Module     : ExchangeAlgebra.Simulation.Output
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    CSV output of simulation results. Import this module to stream function
    results as time-series CSV without requiring Chart.

    == Operations

    +---------------------------------+---------------------------------------+
    | operation                       | CSV output                            |
    +=================================+=======================================+
    | 'writeFuncResults'              | Evaluate each function at each time.  |
    +---------------------------------+---------------------------------------+
    | 'writeFuncResultsWithContext'   | Build one context per time and share  |
    |                                 | it across the functions.              |
    +---------------------------------+---------------------------------------+

    Both operations use "ExchangeAlgebra.Simulate" for its 'StateTime'
    axis. Spill-file restoration remains in
    "ExchangeAlgebra.Simulate.Spill". Use 'writeTermIO' for term slices
    and 'writeIOMatrix' for two-dimensional matrices.
-}

module ExchangeAlgebra.Simulation.Output
    ( Header
    , writeFuncResults
    , writeFuncResultsWithContext
    , writeTermIO
    , writeIOMatrix
    ) where

import Control.Monad (forM)
import Data.Array.IO
import qualified Data.List as L
import ExchangeAlgebra.Algebra.Base.Representation (BaseClass)
import ExchangeAlgebra.Algebra.Value.Class (HatVal)
import ExchangeAlgebra.IO.Output.Csv (writeCSV)
import qualified Control.Monad as CM
import           Control.Monad.ST
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO (IOMode(WriteMode), withFile)

import           ExchangeAlgebra.Simulate (StateTime)

-- | CSV column header text.
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
writeFuncResults funcs termRange wld path =
    writeFuncResultsWithContext
        (\_ t -> return t)
        (map (\(header, f) -> (header, \t -> f wld t)) funcs)
        termRange
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

-- | Convert a value to a text cell.
--
-- Complexity: O(show cost)
tshow :: (Show a) => a -> T.Text
tshow = T.pack . show


-- | Output the Input-Output Table for a specified term in CSV format.
-- Outputs a slice of the specified term from a 3D array (term, row industry, column industry).
--
-- Complexity: O(r * c) (r = number of rows, c = number of columns)
writeTermIO :: (HatVal n,BaseClass b, Ix t, Ix b, Enum b)
            => FilePath -> t -> IOArray (t, b, b) n  -> IO ()
writeTermIO path t arr = do
    ((_, c1Min, c2Min), (_, c1Max, c2Max)) <- getBounds arr
    let rows = [c1Min .. c1Max]
    let cols = [c2Min .. c2Max]
    body <- forM rows $ \r -> do
        cells <- forM cols $ \c -> tshow <$> readArray arr (t, r, c)
        pure (tshow r : cells)
    writeCSV path ((T.pack "" : L.map tshow cols) : body)

-- | Output a 2D IOArray (Input-Output Table or ripple effect matrix) in CSV format.
--
-- Complexity: O(r * c) (r = number of rows, c = number of columns)
writeIOMatrix :: FilePath -> IOArray (Int, Int) Double -> IO ()
writeIOMatrix path arr = do
    ((r1, c1), (r2, c2)) <- getBounds arr
    let rows = [r1 .. r2]
    let cols = [c1 .. c2]
    body <- forM rows $ \r -> do
        cells <- forM cols $ \c -> tshow <$> readArray arr (r, c)
        pure (tshow r : cells)
    writeCSV path ((T.pack "" : L.map tshow cols) : body)
