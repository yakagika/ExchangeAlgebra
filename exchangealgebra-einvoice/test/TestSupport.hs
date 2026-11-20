module TestSupport (assert, checkProperty) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.QuickCheck
    ( Args(chatty, maxSuccess), Testable, isSuccess, output, quickCheckWithResult, stdArgs )

-- | Check an example and print the observed value on failure.
assert :: Show a => String -> a -> (a -> Bool) -> IO ()
assert label actual predicate =
    report label (predicate actual) (show actual)

-- | Check a law on 250 generated examples.
checkProperty :: Testable prop => String -> prop -> IO ()
checkProperty label property = do
    result <- quickCheckWithResult stdArgs { maxSuccess = 250, chatty = False } property
    report label (isSuccess result) (output result)

-- | Print one result line and abort the suite on failure.
report :: String -> Bool -> String -> IO ()
report label ok details = do
    putStrLn ((if ok then "[PASS] " else "[FAIL] ") ++ label)
    unless ok (putStrLn details >> exitFailure)
