{-# LANGUAGE ScopedTypeVariables #-}

-- | Exact accumulator laws in the Algebra layer. The public HatVal methods
-- are observed against Rational, with no tolerance and no readout operations.
-- Generators cover binary64 boundaries and decimal scales before the fixed cases.
module Algebra.AccumSpec (runTests) where

import Control.Monad (forM_, unless)
import qualified Data.Decimal as Decimal
import Data.Hashable (hashWithSalt)
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Ratio ((%), denominator)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import System.Exit (exitFailure)
import Test.QuickCheck hiding (label)

import ExchangeAlgebra.Algebra.Value
    ( HatVal(..)
    , MoneyDecimal(..)
    , MoneyDouble(..)
    , RationalAccum(..)
    , Rounding(..)
    , ValueError(..)
    , isErrorValue
    , rationalAddAccum
    , rationalRoundAccum
    , roundScaled
    , sumValues
    )
import ExchangeAlgebra.Algebra.Posting
    ( PostedError(..)
    , posted
    , unPosted
    )

-- * Inputs and observations

-- | Largest finite binary64 value.
maximumFinite :: Double
maximumFinite = encodeFloat (2 ^ (53 :: Int) - 1) 971

-- | Smallest positive binary64 value.
subnormal :: Double
subnormal = encodeFloat 1 (-1074)

-- | First integer whose successor is not representable in binary64.
threshold :: Double
threshold = 2 ^ (53 :: Int)

-- | Accumulate without intermediate rounding; signed inputs are intentional.
accumulate :: HatVal n => [n] -> Accum n
accumulate = List.foldl' (flip addAccum) emptyAccum

-- | Bias toward zero, subnormals, 2^53, maximum values, and two decimal places.
genFinite :: Gen Double
genFinite = frequency
    [ (2, elements [0, -0])
    , (3, do
        coefficient <- chooseInteger (1, 2 ^ (52 :: Int) - 1)
        sign <- elements [1, -1]
        pure (sign * encodeFloat coefficient (-1074)))
    , (3, do
        offset <- chooseInteger (-8, 8)
        sign <- elements [1, -1]
        pure (sign * fromRational (toRational threshold + fromInteger offset)))
    , (3, do
        offset <- chooseInteger (0, 8)
        sign <- elements [1, -1]
        pure (sign * encodeFloat (2 ^ (53 :: Int) - 1 - offset) 971))
    , (5, do
        cents <- chooseInteger (-1000000, 1000000)
        pure (fromRational (cents % 100)))
    , (4, do
        coefficient <- chooseInteger (1, 2 ^ (53 :: Int) - 1)
        power <- chooseInt (-1074, 971)
        sign <- elements [1, -1]
        pure (sign * encodeFloat coefficient power))
    ]

-- | Include the scale limit and large exact coefficients as well as cents.
genDecimal :: Gen MoneyDecimal
genDecimal = do
    places <- frequency [(5, pure 2), (2, elements [0, 1, 254, 255]), (1, choose (0, 255))]
    coefficient <- frequency
        [ (6, chooseInteger (-1000000, 1000000))
        , (1, elements [0, 1, -1, 10 ^ (300 :: Int), negate (10 ^ (300 :: Int))])
        ]
    pure (MoneyDecimal (Decimal.Decimal places coefficient))

-- | Rational inputs include nonbinary fractions and floating overflow boundaries.
genRatio :: Gen Rational
genRatio = frequency
    [ (5, do
        numerator <- chooseInteger (-1000000, 1000000)
        divisor <- chooseInteger (1, 10000)
        pure (numerator % divisor))
    , (3, do
        value <- genFinite
        divisor <- chooseInteger (1, 17)
        pure (toRational value / fromInteger divisor))
    , (2, elements
        [ toRational maximumFinite + toRational (encodeFloat 1 970 :: Double)
        , 2 * toRational maximumFinite
        , toRational subnormal / 2
        , 3 * toRational subnormal / 2
        ])
    ]

-- | Report a fixed-case failure with the exact observed values.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual = unless (expected == actual) $ do
    putStrLn ("[FAIL] accum " ++ label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
    exitFailure

-- | Run enough cases to repeatedly exercise the biased exponent and scale ranges.
propertyCheck :: String -> Property -> IO ()
propertyCheck label proposition = do
    result <- quickCheckWithResult stdArgs { maxSuccess = 600, chatty = False } proposition
    unless (isSuccess result) $ do
        putStrLn ("[FAIL] accum " ++ label ++ ": " ++ output result)
        exitFailure
    putStrLn ("[PASS] accum " ++ label ++ " (600 cases)")

-- * Laws

-- | A1-A6, A5', E1, E2, T3, S1, V1-V3, B1, and P1 for each finite value type.
-- Exact sums and signs use Rational; rounded observations use the value type's ==.
propFiniteLaws :: forall n. (HatVal n, Real n) => Gen n -> Property
propFiniteLaws generator =
    forAll (chooseInt (0, 20) >>= flip vectorOf generator) $ \values ->
    forAll (shuffle values) $ \permuted ->
    forAll generator $ \value ->
    forAll (chooseInt (0, length values)) $ \firstCut ->
    forAll (chooseInt (firstCut, length values)) $ \secondCut ->
        let (left, rest) = splitAt firstCut values
            (middle, right) = splitAt (secondCut - firstCut) rest
            first = accumulate left
            second = accumulate middle
            third = accumulate right
            total = accumulate values
            expected = sum (map toRational values)
            merged = mergeAccum first (mergeAccum second third)
            rounded = roundAccum total
            roundedFinite = isJust (exactAccum (addAccum rounded emptyAccum))
            successfulSplit = expected > 0 && roundedFinite
            checked = fmap unPosted (posted value)
            expectedPosted = case valueError value of
                Just failure -> Left (InvalidValue failure)
                Nothing -> case postingUpperBound :: Maybe n of
                    Just bound | value > bound -> Left AboveBound
                    _ | value == zeroValue -> Right zeroValue
                      | otherwise -> Right value
        in conjoin
            [ counterexample "A1 commutative" $
                exactAccum (mergeAccum first second) === exactAccum (mergeAccum second first)
            , counterexample "A1 associative" $
                exactAccum (mergeAccum (mergeAccum first second) third) === exactAccum merged
            , counterexample "A1 left identity" $
                exactAccum (mergeAccum emptyAccum total) === Just expected
            , counterexample "A1 right identity" $
                exactAccum (mergeAccum total emptyAccum) === Just expected
            , counterexample "A2 singleton merge" $
                exactAccum (addAccum value total)
                    === exactAccum (mergeAccum total (addAccum value emptyAccum))
            , counterexample "A3 involution" $
                exactAccum (negateAccum (negateAccum total)) === Just expected
            , counterexample "A3/A6 cancellation" $
                exactAccum (mergeAccum total (negateAccum total)) === Just 0
            , counterexample "A3 signed oracle" $
                exactAccum (negateAccum total) === Just (negate expected)
            , counterexample "A4 singleton" $
                roundAccum (addAccum value emptyAccum) === value
            , counterexample "A5/A5' domain and reconstruction" $ case splitAccum total of
                Nothing -> property (not successfulSplit)
                Just components -> conjoin
                    [ property successfulSplit
                    , property (all (\x -> x > 0 && valueError x == Nothing) components)
                    , sum (map toRational components) === expected
                    ]
            , counterexample "A5 path independence" $
                splitAccum total === splitAccum (accumulate permuted)
            , counterexample "A5 merge independence" $
                splitAccum total === splitAccum merged
            , counterexample "E1 exact value" $ exactAccum total === Just expected
            , counterexample "E1 permutation" $
                exactAccum (accumulate permuted) === Just expected
            , counterexample "E1 merge tree" $ exactAccum merged === Just expected
            , counterexample "exact sign" $ signAccum total === Just (compare expected 0)
            , counterexample "E2 finite rounding" $ case roundRational Nearest expected of
                Just result -> rounded === result
                Nothing -> property (not roundedFinite)
            , counterexample "T3 representable input" $
                roundRational Nearest (toRational value) === Just value
            , counterexample "S1 definition" $
                sumValues values === roundAccum (foldr addAccum emptyAccum values)
            , counterexample "S1 permutation" $ sumValues values === sumValues permuted
            , counterexample "V1 zero" $ valueError (zeroValue :: n) === Nothing
            , counterexample "V2 valid is finite and nonnegative" $
                property (valueError value /= Nothing ||
                    (value >= zeroValue && isJust (exactAccum (addAccum value emptyAccum))))
            , counterexample "V3 finite signed values" $
                valueError value === case compare value zeroValue of
                    LT -> Just NegativeValue
                    _ -> Nothing
            , counterexample "isErrorValue is a function of valueError" $
                isErrorValue value === isJust (valueError value)
            , counterexample "B1 bound" $ case postingUpperBound :: Maybe n of
                Nothing -> property True
                Just bound -> property (bound > zeroValue && valueError bound == Nothing)
            , counterexample "P1 priority and accepted value" $ checked === expectedPosted
            ]

-- | A5 additionally fixes each Double component to the largest value below the remainder.
propGreedySplit :: Property
propGreedySplit = forAll (chooseInt (0, 20) >>= flip vectorOf genFinite) $ \values ->
    let expected = sum (map toRational values)
        verify _ [] = True
        verify residual (component : rest) =
            toRational component <= residual
                && (isInfinite successor || toRational successor > residual)
                && verify (residual - toRational component) rest
          where
            successor = castWord64ToDouble (castDoubleToWord64 component + 1)
    in case splitAccum (accumulate values) of
        Nothing -> property (expected <= 0 || isInfinite (fromRational expected :: Double))
        Just components -> property (verify expected components)

-- | T1 and T3 for a positive exact unit, including ties and negative multiples.
propDecimalUnit :: Property
propDecimalUnit = forAll genDecimal $ \rawUnit ->
    forAll genRatio $ \value ->
    forAll (chooseInteger (-10000, 10000)) $ \multiple ->
        let unit = abs rawUnit + MoneyDecimal (Decimal.Decimal 255 1)
            rationalUnit = toRational unit
            tied = (fromInteger multiple + 1 % 2) * rationalUnit
            evenMultiple = multiple + multiple `mod` 2
            checkUnit rational = case roundRational (ToMultipleOf unit) rational of
                Nothing -> counterexample "positive exact unit rejected" False
                Just rounded ->
                    let quotient = toRational rounded / rationalUnit
                    in conjoin
                        [ denominator quotient === 1
                        , property (abs (toRational rounded - rational) <= rationalUnit / 2)
                        , property (abs (toRational rounded - rational) /= rationalUnit / 2
                            || even (round quotient :: Integer))
                        ]
        in conjoin
            [ checkUnit value
            , checkUnit tied
            , fmap toRational (roundRational (ToMultipleOf unit) tied)
                === Just (fromInteger evenMultiple * rationalUnit)
            , roundRational (ToMultipleOf unit) (fromInteger multiple * rationalUnit)
                === Just (fromInteger multiple * unit)
            , roundRational (ToMultipleOf (negate unit)) value === Nothing
            , roundRational (ToMultipleOf (0 :: MoneyDecimal)) value === Nothing
            ]

-- | T2: a reduced denominator is representable precisely when it divides 10^255.
-- This oracle does not repeat the implementation's stepwise factorization.
propDecimalNearest :: Property
propDecimalNearest =
    forAll (chooseInteger (-10000, 10000)) $ \coefficient ->
    forAll (chooseInt (0, 260)) $ \twos ->
    forAll (chooseInt (0, 260)) $ \fives ->
    forAll (elements [1, 3, 7, 11 :: Integer]) $ \other ->
        let value = coefficient % (2 ^ twos * 5 ^ fives * other)
            result = roundRational Nearest value :: Maybe MoneyDecimal
            representable = 10 ^ (255 :: Int) `mod` denominator value == 0
        in conjoin
            [ isJust result === representable
            , case result of
                Nothing -> property True
                Just rounded -> toRational rounded === value
            ]

-- | T4 and T5 for both floating types; the oracle rounds Rational directly.
propFloatingRounding :: Property
propFloatingRounding = forAll genRatio $ \value -> forAll genFinite $ \unit ->
    let rounded = fromRational value :: Double
        expected
            | isInfinite rounded = Nothing
            | otherwise = Just rounded
    in conjoin
        [ (roundRational Nearest value :: Maybe Double) === expected
        , roundRational Nearest value === fmap MoneyDouble expected
        , roundRational (ToMultipleOf unit) value === Nothing
        , roundRational (ToMultipleOf (MoneyDouble unit)) value === Nothing
        ]

-- | The Rational helper preserves finite values, signs, and the supplied conversion.
propRationalHelper :: Property
propRationalHelper = forAll (chooseInt (0, 20) >>= flip vectorOf genFinite) $ \values ->
    let state = List.foldl' (flip rationalAddAccum) (RationalAccum 0) values
        expected = sum (map toRational values)
    in conjoin
        [ state === RationalAccum expected
        , rationalRoundAccum id state === expected
        , rationalRoundAccum negate state === negate expected
        ]

-- * Fixed boundaries

-- | Integer overflow, cancellation, two decimal places, subnormals, and greedy splits.
-- Each case also checks the MoneyDouble delegation against the same Rational oracle.
testFloatingBoundaries :: IO ()
testFloatingBoundaries = do
    let machineHalf = encodeFloat 1 (finiteBitSizeInt - 2)
        machineMinimum = fromIntegral (minBound :: Int) :: Double
        belowMidpoint = encodeFloat (2 ^ (53 :: Int) - 1) 917
        midpoint = encodeFloat 1 970
        aboveMidpoint = encodeFloat (2 ^ (52 :: Int) + 1) 918
        cases =
            [ ("Int positive overflow", [machineHalf, machineHalf])
            , ("Int negative overflow", [machineMinimum, -1])
            , ("above 2^53 then fraction", [threshold, 1, 0.25])
            , ("above 2^53 then fraction then cancel", [threshold, 1, 0.25, -threshold])
            , ("two decimal places", [12.34, 56.78, -12.34, 0.01])
            , ("0.1+0.2", [0.1, 0.2])
            , ("M+M-M", [maximumFinite, maximumFinite, -maximumFinite])
            , ("M+s", [maximumFinite, subnormal])
            , ("M+M", [maximumFinite, maximumFinite])
            , ("subnormal cancellation", [maximumFinite, subnormal, -maximumFinite])
            , ("smallest normal minus subnormal", [encodeFloat 1 (-1022), -subnormal])
            , ("three residuals", [0.1, encodeFloat 1 (-60), subnormal])
            , ("tie down", [threshold, 1])
            , ("tie up", [threshold + 2, 1])
            , ("beyond tie", [threshold, 1, subnormal])
            , ("overflow midpoint cancellation", [maximumFinite, midpoint, -midpoint])
            , ("zero", [-0, 0])
            ]
    forM_ cases $ \(label, values) -> do
        let expected = sum (map toRational values)
            state = accumulate values
            wrapped = accumulate (map MoneyDouble values)
        check (label ++ " exact") (Just expected) (exactAccum state)
        check (label ++ " round") (fromRational expected :: Double) (roundAccum state)
        check (label ++ " sign") (Just (compare expected 0)) (signAccum state)
        check (label ++ " wrapped exact") (Just expected) (exactAccum wrapped)
        check (label ++ " wrapped round") (MoneyDouble (fromRational expected)) (roundAccum wrapped)
        check (label ++ " negate") (Just (negate expected)) (exactAccum (negateAccum state))
        check (label ++ " split delegation") (fmap (map MoneyDouble) (splitAccum state))
            (splitAccum wrapped)
    check "minBound negation" (Just (negate (toRational machineMinimum)))
        (exactAccum (negateAccum (accumulate [machineMinimum])))
    check "minBound negation rounds" (negate machineMinimum)
        (roundAccum (negateAccum (accumulate [machineMinimum])))
    check "negative zero normalized" False (isNegativeZero (sumValues [-0 :: Double]))
    check "0.1+0.2 greedy" (Just [0.3, encodeFloat 1 (-55)])
        (splitAccum (accumulate [0.1 :: Double, 0.2]))
    check "M+s greedy" (Just [maximumFinite, subnormal])
        (splitAccum (accumulate [maximumFinite, subnormal]))
    check "below overflow midpoint split" (Just [maximumFinite, belowMidpoint])
        (splitAccum (accumulate [maximumFinite, belowMidpoint]))
    check "at overflow midpoint split" Nothing
        (splitAccum (accumulate [maximumFinite, midpoint]))
    check "above overflow midpoint split" Nothing
        (splitAccum (accumulate [maximumFinite, aboveMidpoint]))
    check "M+M split" Nothing (splitAccum (accumulate [maximumFinite, maximumFinite]))
    check "M+M scaled once" (Just maximumFinite)
        (roundScaled Nearest (1 % 2) (accumulate [maximumFinite, maximumFinite]))
    check "M+M scaled once wrapped" (Just (MoneyDouble maximumFinite))
        (roundScaled Nearest (1 % 2) (accumulate (map MoneyDouble [maximumFinite, maximumFinite])))
    check "positive subnormal split" (Just [subnormal]) (splitAccum (accumulate [subnormal]))
    check "negative state split" Nothing (splitAccum (accumulate [-subnormal]))
    check "zero state split" Nothing (splitAccum (accumulate [0 :: Double]))
    check "subnormal tie to zero" (Just 0 :: Maybe Double)
        (roundRational Nearest (toRational subnormal / 2))
    check "subnormal tie to even" (Just (2 * subnormal))
        (roundRational Nearest (3 * toRational subnormal / 2))
  where
    finiteBitSizeInt = length (takeWhile (> 0) (iterate (`div` 2) (maxBound :: Int))) + 1

-- | A6 and V3 over the IEEE table, for add, merge, negation, and both floating types.
testSpecialValues :: IO ()
testSpecialValues = do
    let values = [0, 1, -1, 1 / 0, -1 / 0, 0 / 0 :: Double]
        observe value
            | isNaN value = Nothing
            | otherwise = Just value
    forM_ values $ \left -> do
        let singleton = accumulate [left]
            expectedError
                | isNaN left || isInfinite left = Just NonFiniteValue
                | left < 0 = Just NegativeValue
                | otherwise = Nothing
        check "V3 Double" expectedError (valueError left)
        check "V3 MoneyDouble" expectedError (valueError (MoneyDouble left))
        check "special negation" (observe (negate left))
            (observe (roundAccum (negateAccum singleton)))
        forM_ values $ \right -> do
            let expected = left + right
                states = [addAccum right singleton, mergeAccum singleton (accumulate [right])]
                wrapped = accumulate (map MoneyDouble [left, right])
            forM_ states $ \state -> do
                check "A6 addition table" (observe expected) (observe (roundAccum state))
                check "A6 exact classification" (not (isNaN expected || isInfinite expected))
                    (isJust (exactAccum state))
                check "A6 sign" (fmap (`compare` 0) (observe expected)) (signAccum state)
            check "A6 wrapped table" (fmap MoneyDouble (observe expected))
                (case roundAccum wrapped of
                    MoneyDouble value -> fmap MoneyDouble (observe value))
            check "A6 wrapped merge" (observe expected)
                (case roundAccum (mergeAccum (accumulate [MoneyDouble left])
                                            (accumulate [MoneyDouble right])) of
                    MoneyDouble value -> observe value)
    forM_ [1 / 0, -1 / 0, 0 / 0 :: Double] $ \special -> do
        let state = accumulate [special]
        check "nonfinite split" Nothing (splitAccum state)
        check "nonfinite exact" Nothing (exactAccum state)
        check "nonfinite scaled" Nothing (roundScaled Nearest 0 state)
        check "P1 nonfinite before bound or sign" (Left (InvalidValue NonFiniteValue))
            (fmap unPosted (posted special))
        check "P1 wrapped nonfinite before bound or sign" (Left (InvalidValue NonFiniteValue))
            (fmap unPosted (posted (MoneyDouble special)))
        forM_ [[], [0.1], [0.1, 0.2], [maximumFinite, maximumFinite]] $ \finite -> do
            let finiteState = accumulate finite
            check "special absorbs finite state" (observe special)
                (observe (roundAccum (mergeAccum state finiteState)))
            check "finite state absorbs special" (observe special)
                (observe (roundAccum (mergeAccum finiteState state)))
    forM_ [0, -1, 0.01, 1 / 0, -1 / 0, 0 / 0 :: Double] $ \unit -> do
        check "T5 Double all units" Nothing (roundRational (ToMultipleOf unit) (1 % 3))
        check "T5 MoneyDouble all units" Nothing
            (roundRational (ToMultipleOf (MoneyDouble unit)) (1 % 3))

-- | T1-T3 at exact decimal scale boundaries and the Eq/Hashable contract.
testDecimalBoundaries :: IO ()
testDecimalBoundaries = do
    let tiny = MoneyDecimal (Decimal.Decimal 255 1)
        cents = MoneyDecimal (Decimal.Decimal 2 1)
        one = MoneyDecimal (Decimal.Decimal 1 10)
        sameOne = MoneyDecimal (Decimal.Decimal 2 100)
    check "T2 10^-255" (Just tiny) (roundRational Nearest (1 % 10 ^ (255 :: Int)))
    check "T2 10^-256" (Nothing :: Maybe MoneyDecimal)
        (roundRational Nearest (1 % 10 ^ (256 :: Int)))
    check "T2 reduced denominator" (Just tiny)
        (roundRational Nearest (10 % 10 ^ (256 :: Int)))
    check "T2 1/3" (Nothing :: Maybe MoneyDecimal) (roundRational Nearest (1 % 3))
    check "T2 2^-255" (Just (1 % 2 ^ (255 :: Int)))
        (toRational <$> (roundRational Nearest (1 % 2 ^ (255 :: Int)) :: Maybe MoneyDecimal))
    check "T2 5^-255" (Just (1 % 5 ^ (255 :: Int)))
        (toRational <$> (roundRational Nearest (1 % 5 ^ (255 :: Int)) :: Maybe MoneyDecimal))
    check "T2 signed boundary" (Just (negate tiny))
        (roundRational Nearest ((-1) % 10 ^ (255 :: Int)))
    check "A5' decimal positive" (Just [1 + tiny]) (splitAccum (accumulate [1, tiny]))
    check "A5' decimal zero" Nothing (splitAccum (accumulate [tiny, negate tiny]))
    check "A5' decimal negative" Nothing (splitAccum (accumulate [negate tiny]))
    check "S1 decimal cents" (3 % 10)
        (toRational (sumValues [MoneyDecimal (Decimal.Decimal 1 1),
                               MoneyDecimal (Decimal.Decimal 1 2)]))
    check "A6 decimal exact cancellation" (Just 0)
        (exactAccum (mergeAccum (accumulate [1, tiny]) (negateAccum (accumulate [1, tiny]))))
    forM_ [(1 % 200, 0), (3 % 200, 2), ((-1) % 200, 0), ((-3) % 200, -2)] $
        \(value, multiple) ->
            check "T1 ties to even cents" (Just (fromInteger multiple * cents))
                (roundRational (ToMultipleOf cents) value)
    check "T1 nonpositive unit" Nothing (roundRational (ToMultipleOf (0 :: MoneyDecimal)) 1)
    check "decimal representations equal" True (one == sameOne)
    forM_ [0, 1, -1, maxBound, minBound :: Int] $ \salt -> do
        check "equal decimal hash" (hashWithSalt salt one) (hashWithSalt salt sameOne)
        check "decimal zero hash" (hashWithSalt salt (MoneyDecimal (Decimal.Decimal 0 0)))
            (hashWithSalt salt (MoneyDecimal (Decimal.Decimal 255 0)))

-- | B1 and P1 at adjacent floating values and an unbounded decimal magnitude.
testPostingBoundaries :: IO ()
testPostingBoundaries = do
    let bound = 2 ^ (900 :: Int) :: Double
        below = castWord64ToDouble (castDoubleToWord64 bound - 1)
        above = castWord64ToDouble (castDoubleToWord64 bound + 1)
        huge = MoneyDecimal (Decimal.Decimal 0 (10 ^ (1000 :: Int)))
    check "B1 Double bound" (Just bound) (postingUpperBound :: Maybe Double)
    check "B1 MoneyDouble bound" (Just (MoneyDouble bound)) (postingUpperBound :: Maybe MoneyDouble)
    check "B1 MoneyDecimal unbounded" Nothing (postingUpperBound :: Maybe MoneyDecimal)
    forM_ [0, -0, below, bound] $ \value -> do
        check "P1 accepted Double" (Right value) (fmap unPosted (posted value))
        check "P1 accepted MoneyDouble" (Right (MoneyDouble value))
            (fmap unPosted (posted (MoneyDouble value)))
    check "P1 above Double bound" (Left AboveBound) (fmap unPosted (posted above))
    check "P1 above MoneyDouble bound" (Left AboveBound)
        (fmap unPosted (posted (MoneyDouble above)))
    check "P1 decimal has no bound" (Right huge) (fmap unPosted (posted huge))
    check "P1 negative decimal" (Left (InvalidValue NegativeValue))
        (fmap unPosted (posted (negate huge)))
    check "P1 positive zero Double" (Right False)
        (fmap (isNegativeZero . unPosted) (posted (-0 :: Double)))
    check "P1 positive zero MoneyDouble" (Right False)
        (fmap (\checked -> case unPosted checked of MoneyDouble value -> isNegativeZero value)
            (posted (MoneyDouble (-0))))

-- | Run the laws and fixed cases from the package's test suite.
runTests :: IO ()
runTests = do
    testFloatingBoundaries
    testSpecialValues
    testDecimalBoundaries
    testPostingBoundaries
    propertyCheck "Double A1-A6/A5'/E1/E2/T3/S1/V1-V3/B1/P1" (propFiniteLaws genFinite)
    propertyCheck "MoneyDouble A1-A6/A5'/E1/E2/T3/S1/V1-V3/B1/P1"
        (propFiniteLaws (MoneyDouble <$> genFinite))
    propertyCheck "MoneyDecimal A1-A6/A5'/E1/T3/S1/V1-V3/B1/P1" (propFiniteLaws genDecimal)
    propertyCheck "A5 greedy Double split" propGreedySplit
    propertyCheck "T1/T3 decimal unit and ties" propDecimalUnit
    propertyCheck "T2 decimal representability" propDecimalNearest
    propertyCheck "T4/T5 floating rounding" propFloatingRounding
    propertyCheck "RationalAccum helper" propRationalHelper
    putStrLn "[PASS] accumulator boundaries and IEEE table"
