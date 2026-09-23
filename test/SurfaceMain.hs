module Main (main) where

import           Surface.Accounting ()
import           Surface.Algebra.Exact ()
import           Surface.Algebra.Readout.Net ()
import           Surface.Foundation ()
import           Surface.Journal.Exact ()
import           Surface.Render.Bookkeeping ()
import           Surface.Render.Csv ()
import           Surface.Render.Simulation ()
import           Surface.Simulate.Analysis ()
import           Surface.Simulate.Engine ()
import           Surface.Simulate.Random ()

-- | Report successful compilation of every surface lock module.
main :: IO ()
main = putStrLn "surface ok"
