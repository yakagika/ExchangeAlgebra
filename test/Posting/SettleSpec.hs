{-# LANGUAGE ScopedTypeVariables #-}

-- | Ordered settlement pairs preserve source nets and exact accounting balance.
module Posting.SettleSpec (runTests) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (forM_, unless)
import Data.List (foldl', sort)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
import Test.QuickCheck hiding (label)

import ExchangeAlgebra.Algebra hiding (filter, map, toAlg)
import ExchangeAlgebra.Algebra.Transfer.Rule (ClosingSide(..), closingSide)
import ExchangeAlgebra.Posting

-- | Two coordinates exercise complete-base ordering and axis preservation.
type TestBase = HatBase (CountUnit, AccountTitles)

-- | Signed input amounts indexed by complete base coordinates.
type Nets = Map.Map (CountUnit, AccountTitles) Double

-- | Cover zero, both signs, fractions, subnormals, and amounts above Posted.
genNet :: Gen Double
genNet = do
    magnitude <- frequency
        [ (3, elements [0, 0.1, 0.2, 1, 2 ^ (53 :: Int), 2 ^ (950 :: Int)
                       , encodeFloat 1 (-1074), encodeFloat (2 ^ (53 :: Int) - 1) 971])
        , (2, do
              significand <- chooseInteger (1, 2 ^ (53 :: Int) - 1)
              exponent <- chooseInt (-1074, 971)
              pure (encodeFloat significand exponent))
        ]
    sign <- elements [1, -1]
    pure (sign * magnitude)

-- | Include closing, non-closing, and destination accounts on several units.
genNets :: Gen Nets
genNets = do
    count <- chooseInt (0, 30)
    entries <- vectorOf count $ do
        unit <- elements [Yen, Dollar, Amount]
        title <- elements [Sales, Purchases, Depreciation, Cash, RetainedEarnings, NetIncome]
        amount <- genNet
        pure ((unit, title), amount)
    pure (Map.fromList entries)

-- | Read scalar entries without cancellation or approximate comparisons.
scalars :: Alg Double TestBase -> [(Double, TestBase)]
scalars = foldEntries (\previous value postingBase -> (value, postingBase) : previous) []

-- | The exact signed net at one complete base.
netAt :: (CountUnit, AccountTitles) -> Alg Double TestBase -> Rational
netAt coordinates = foldEntries add 0
  where
    add total value postingBase
        | base postingBase /= coordinates = total
        | hat postingBase == Not = total + toRational value
        | otherwise = total - toRational value

-- | Sum magnitudes as rationals rather than through floating-point norm.
exactMagnitude :: Alg Double TestBase -> Rational
exactMagnitude = foldEntries (\total value _ -> total + toRational value) 0

-- | The key set, pair structure, signs, and debit/credit equality hold exactly.
propSettlement :: Property
propSettlement = forAll genNets $ \amounts ->
    let steps = settlementSteps
            (settleEntries retainedEarningsRule amounts :: SettlementBatch TestBase)
        expectedKeys = Map.keys (Map.filterWithKey eligible amounts)
        keys = map fst steps
        checkPair (coordinates@(unit, title), algebra) =
            let amount = Map.findWithDefault 0 coordinates amounts
                sourceSide = if amount > 0 then Not else Hat
                reverseSide = if sourceSide == Not then Hat else Not
                targetSide = case closingSide title of
                    Just ClosingKeep -> sourceSide
                    _ -> reverseSide
                expected = sort
                    [ (abs amount, reverseSide :< coordinates)
                    , (abs amount, targetSide :< (unit, RetainedEarnings))
                    ]
                valid (value, postingBase) =
                    value >= 0 && not (isNaN value || isInfinite value)
                    && hat postingBase /= HatNot
            in conjoin
                [ sort (scalars algebra) === expected
                , length (scalars algebra) === 2
                , netAt coordinates algebra + toRational amount === 0
                , exactMagnitude (decL algebra) === exactMagnitude (decR algebra)
                , property (all valid (scalars algebra))
                ]
    in conjoin
        [ keys === expectedKeys
        , property (and (zipWith (<) keys (drop 1 keys)))
        , conjoin (map checkPair steps)
        ]
  where
    eligible (_, title) amount = amount /= 0 && title /= RetainedEarnings
        && closingSide title /= Nothing

-- | Report fixed regression failures through the ordinary test executable.
assertTest :: String -> Bool -> IO ()
assertTest label success = unless success $ do
    putStrLn ("[FAIL] settlement: " ++ label)
    exitFailure

-- | Exclusion, both directions, and large finite magnitudes are deterministic.
testFixed :: IO ()
testFixed = do
    let steps amounts = settlementSteps
            (settleEntries retainedEarningsRule amounts :: SettlementBatch TestBase)
        excluded = Map.fromList
            [ ((Yen, Sales), 0)
            , ((Dollar, Purchases), -0.0)
            , ((Yen, Cash), 10)
            , ((Yen, RetainedEarnings), 12)
            , ((Yen, NetIncome), 5)
            ]
    assertTest "empty input" (null (steps Map.empty))
    assertTest "only excluded keys" (null (steps excluded))
    assertTest "known keep and flip accounts" $
        closingSide Sales == Just ClosingKeep && closingSide Purchases == Just ClosingFlip
    forM_ [Sales, Purchases] $ \title ->
        forM_ [1, -1] $ \sign -> do
            let magnitude = 2 ^ (950 :: Int)
                amount = sign * magnitude
                coordinates = (Yen, title)
                result = steps (Map.singleton coordinates amount)
            assertTest "finite 2^950 accepted without Posted validation" $
                case result of
                    [(source, algebra)] -> source == coordinates
                        && length (scalars algebra) == 2
                        && all ((== magnitude) . fst) (scalars algebra)
                        && netAt source algebra == negate (toRational amount)
                        && exactMagnitude (decL algebra) == exactMagnitude (decR algebra)
                    _ -> False
    -- Select three ClosingKeep accounts by their actual key order so the
    -- destination increments are T, 1, -T, independent of enum ordering.
    let titles = take 3 [title | title <- [minBound .. maxBound]
                             , closingSide title == Just ClosingKeep]
        sourceKeys = sort [(Yen, title) | title <- titles]
        large = 2 ^ (53 :: Int)
        result = steps (Map.fromList (zip sourceKeys [large, 1, -large]))
        increments = [fromRational (netAt (Yen, RetainedEarnings) algebra) :: Double
                     | (_, algebra) <- result]
    assertTest "three distinct keep accounts in fixture" (length sourceKeys == 3)
    assertTest "sequential destination increments preserve source order" $
        increments == [large, 1, -large] && foldl' (+) 0 increments == 0
        && foldl' (+) 0 [large, -large, 1] == 1

-- | All input keys obey the finite-input contract, even excluded accounts.
testNonFinite :: IO ()
testNonFinite = forM_ [0 / 0, 1 / 0, -1 / 0] $ \amount ->
    forM_ [Sales, Cash, RetainedEarnings] $ \title -> do
        result <- try (evaluate
            (settleEntries retainedEarningsRule
                (Map.fromList [((Yen, Sales), 1), ((Dollar, title), amount)])
                :: SettlementBatch TestBase))
            :: IO (Either ErrorCall (SettlementBatch TestBase))
        assertTest "non-finite input raises ErrorCall" $ case result of
            Left _ -> True
            Right _ -> False

-- | Run generated laws and boundary regressions with the main test suite.
runTests :: IO ()
runTests = do
    testFixed
    testNonFinite
    result <- quickCheckWithResult stdArgs { maxSuccess = 500, chatty = False } propSettlement
    unless (isSuccess result) $ do
        putStrLn ("[FAIL] settlement properties: " ++ output result)
        exitFailure
    putStrLn "[PASS] settlement (order, keys, exact pairs, sides, axes, bounds, and errors)"
