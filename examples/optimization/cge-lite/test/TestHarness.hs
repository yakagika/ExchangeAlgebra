{- |
  TestHarness — the tiny named-check harness shared by the cge-lite test
  suites (@cge-lite-test@ \/ @cge-lite-solver-test@): a check is a name plus
  'Maybe' a failure message, 'runChecks' prints the tally and exits nonzero
  on any failure. Deliberately dependency-free (no test framework) so the
  example package's dependency footprint stays untouched.
-}
module TestHarness
    ( Check
    , ok
    , bad
    , require
    , approx
    , exact
    , runChecks
    ) where

import           Control.Monad (unless)
import           System.Exit   (exitFailure)

-- | A named check: 'Nothing' = pass, 'Just msg' = failure detail.
type Check = (String, Maybe String)

ok :: String -> Check
ok name = (name, Nothing)

bad :: String -> String -> Check
bad name msg = (name, Just msg)

-- | Boolean condition with a fixed failure message.
require :: String -> Bool -> String -> Check
require name cond msg = if cond then ok name else bad name msg

-- | @approx tol name expected actual@ — absolute-error comparison.
approx :: Double -> String -> Double -> Double -> Check
approx tol name expected actual
    | abs (expected - actual) <= tol = ok name
    | otherwise = bad name $
        "expected " ++ show expected ++ ", got " ++ show actual
        ++ " (|diff| = " ++ show (abs (expected - actual)) ++ " > tol " ++ show tol ++ ")"

-- | Exact-in-Double comparison (1e-9 only guards float division noise;
-- the compared quantities are exact small integers or their ratios).
exact :: String -> Double -> Double -> Check
exact = approx 1e-9

-- | Print the tally (and each failure), exit nonzero on any failure.
runChecks :: String -> [Check] -> IO ()
runChecks suite checks = do
    let failures = [ (name, msg) | (name, Just msg) <- checks ]
    putStrLn $ suite ++ ": " ++ show (length checks) ++ " checks, "
             ++ show (length failures) ++ " failures"
    mapM_ (\(name, msg) -> putStrLn ("  FAIL " ++ name ++ ": " ++ msg)) failures
    unless (null failures) exitFailure
