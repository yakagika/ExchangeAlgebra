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
import           CompatInstance (checkInstances)
import           System.Process (callProcess)

-- | Check the public export snapshot and client-owned instance behavior.
main :: IO ()
main = do
    checkInstances
    callProcess "python3" ["tools/check-export-surface.py", "--direct-ghc", "--suite"]
    putStrLn "surface ok"
