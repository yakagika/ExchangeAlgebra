{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Shared assertions, generators, and fixture bases for the domain suites.
-- The domain suites share these fixtures and their instances.
-- Read the comparison helpers before the generators and fixture bases.
module Support (eps
               , assertEqual
               , assertNear
               , TestComparison(..)
               , runTestComparison
               , TestAlg
               , TransferAlg
               , TransferJournal
               , removeSpillTestFile
               , SimTerm
               , SimCompany
               , SimHatBase2
               , readFileStrict
               , quickProp
               , genUnit
               , genSide
               , genBase
               , genNNDouble
               , netByBase
               , epsEq
               , CheckedAlgM
               , exactBalancedForTest
               , withTestTemporaryFile
               ) where

import ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Journal  as EJ
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.Simulate (StateTime, initTerm, lastTerm)
import qualified Data.Map.Strict     as M
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import System.Exit (exitFailure)
import System.IO (openBinaryTempFile, hClose)
import System.Directory (removeFile, getTemporaryDirectory)
import Control.Exception (try, SomeException, bracket)
import Test.QuickCheck hiding (Fixed)

eps :: Double
eps = 1e-9

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | expected == actual = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

assertNear :: String -> Double -> Double -> IO ()
assertNear label expected actual
    | abs (expected - actual) <= eps = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

-- | A comparison row keeps its expected value, observed value, and equality mode.
-- Exact comparisons may use different value types without converting their expectations.
data TestComparison
    = forall a. (Eq a, Show a) => EqualComparison String a a
      -- ^ Compare exact values using their original Eq instance.
    | NearComparison String Double Double
      -- ^ Compare Double values using the existing epsilon.

-- | Execute one comparison and retain the suite's existing PASS and FAIL diagnostics.
runTestComparison :: TestComparison -> IO ()
runTestComparison (EqualComparison label expected actual) = assertEqual label expected actual
runTestComparison (NearComparison label expected actual) = assertNear label expected actual

type TestAlg = EA.Alg Double (HatBase CountUnit)


-- ================================================================
-- Transfer regression tests
-- ================================================================

type TransferAlg = EA.Alg Double SimHatBase2
type TransferJournal = EJ.Journal String Double SimHatBase2


removeSpillTestFile :: FilePath -> IO ()
removeSpillTestFile path = do
    _ <- try (removeFile path) :: IO (Either SomeException ())
    pure ()


-- ================================================================
-- SimulateEx1 reproduction (default scenario only, no parallelism)
-- ================================================================

type SimTerm = Int

instance StateTime SimTerm where
    initTerm = 1
    lastTerm = 100

type SimCompany = Int

instance Element SimCompany where
    wildcard = -1

instance BaseClass SimCompany where

type SimHatBase2 = HatBase (AccountTitles, SimCompany, SimCompany, CountUnit)

instance ExBaseClass SimHatBase2 where
    getAccountTitle (h :< (a, _, _, _)) = a
    setAccountTitle (h :< (_, c, e, u)) b = h :< (b, c, e, u)


-- | Strict file read helper for tests
readFileStrict :: FilePath -> IO String
readFileStrict p = do
    bs <- TIO.readFile p
    return (T.unpack bs)

-- run a QuickCheck property in the existing IO-style harness
quickProp :: Testable p => String -> p -> IO ()
quickProp label p = do
    r <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = False } p
    if isSuccess r
        then putStrLn ("[PASS] " ++ label)
        else do putStrLn ("[FAIL] " ++ label); putStr (output r); exitFailure

-- generators: concrete (non-wildcard) bases, intentional collisions
genUnit :: Gen CountUnit
genUnit = elements [Yen, Dollar, Amount]

genSide :: Gen Hat
genSide = elements [Hat, Not]

genBase :: Gen (HatBase CountUnit)
genBase = (:<) <$> genSide <*> genUnit

genNNDouble :: Gen Double          -- non-negative, finite
genNNDouble = do
    NonNegative x <- arbitrary
    if isNaN x || isInfinite x then genNNDouble else pure x

-- exact per-base signed net (Not +, Hat -) via Rational; the observable
-- accounting content. Robust to seq order; catches base misassociation.
netByBase :: (HatVal v, Real v) => EA.Alg v (HatBase CountUnit) -> M.Map CountUnit Rational
netByBase = EA.foldEntries step M.empty
  where
    step m v b = M.insertWith (+) (part b) (signed v b) m
    part (_ :< u) = u
    signed v b = if isHat b then negate (toRational v) else toRational v

epsEq :: Double -> Double -> Bool
epsEq a b = abs (a - b) <= 1e-9 * (1 + max (abs a) (abs b))

-- ================================================================
-- ExchangeAlgebra.IO.Input: checked construction for generated entries.
-- ================================================================

type CheckedAlgM = EA.Alg MoneyDecimal (HatBase AccountTitles)

exactBalancedForTest :: (EA.HatVal v, EA.ExBaseClass b) => EA.Alg v b -> Bool
exactBalancedForTest x = EA.norm (EA.decL x) == EA.norm (EA.decR x)

-- | Allocate an isolated test file and remove it even if a check raises an exception.
-- Uses base and directory so the test suite needs no additional package dependency.
withTestTemporaryFile :: (FilePath -> IO a) -> IO a
withTestTemporaryFile action = bracket acquire removeSpillTestFile action
  where
    acquire = do
        directory <- getTemporaryDirectory
        (path, handle) <- openBinaryTempFile directory "exchangealgebra-test.tmp"
        hClose handle
        pure path
