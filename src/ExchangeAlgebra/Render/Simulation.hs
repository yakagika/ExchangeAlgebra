{- |
    Module     : ExchangeAlgebra.Render.Simulation
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    CSV and file dumps of simulation results. 'writeFuncResults' and
    'writeFuncResultsWithContext' stream function results as time-series CSV
    without requiring Chart. 'writeTermIO' and 'writeIOMatrix' are re-exported
    unchanged from "ExchangeAlgebra.Write". Spill-file restoration is not a
    rendering concern and stays in "ExchangeAlgebra.Simulate.Spill".
-}

module ExchangeAlgebra.Render.Simulation
    ( Header
    , writeFuncResults
    , writeFuncResultsWithContext
    , writeTermIO
    , writeIOMatrix
    ) where

import qualified Control.Monad as CM
import           Control.Monad.ST
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO (IOMode(WriteMode), withFile)

import           ExchangeAlgebra.Simulate (StateTime)
import           ExchangeAlgebra.Write (writeTermIO, writeIOMatrix)

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
