-- | Execute every coffee-shop retry scenario and derive accepted reports.
-- This executable composes the IO adapters, fixed trust data, and report consumer.
-- Run check.sh from any directory; it supplies CALLS.md and calls.schema.json.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import ModelRetry (runScenario, scenarios)
import Reports (report)
import System.Environment (getArgs)
import Trusted (augustAdjustment, specification)

-- | Assert the requested scenario outcomes and print reports for each accepted run.
main :: IO ()
main = do
    arguments <- getArgs
    protocol <- case arguments of
        [callsPath, schemaPath] -> do
            calls <- Text.readFile callsPath
            schema <- Text.readFile schemaPath
            pure (calls <> Text.pack "\nJSON schema:\n" <> schema)
        _ -> fail "Usage: harness CALLS.md calls.schema.json"
    trusted <- either fail pure specification
    putStrLn "Trusted August adjustment (context only; already in opening balance):"
    print augustAdjustment
    forM_ scenarios $ \(scenario, expectedAttempts, expectedSuccess) -> do
        (accepted, attempts) <- runScenario protocol trusted scenario
        unless (attempts == expectedAttempts && isJust accepted == expectedSuccess) $
            fail "Scenario outcome or attempt count differs from the script."
        case accepted of
            Nothing -> pure ()
            Just value -> report value
    putStrLn "All five scenarios completed with the expected outcomes."
