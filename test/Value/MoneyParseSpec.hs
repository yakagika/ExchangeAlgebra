-- | Acceptance and property tests for exact money input conversion.
module Value.MoneyParseSpec (runTests) where

import Control.Monad (unless)
import Data.Decimal (DecimalRaw (Decimal), decimalMantissa, decimalPlaces)
import Data.Scientific (Scientific, scientific)
import qualified Data.Text as Text
import System.Exit (exitFailure)
import Test.QuickCheck

import ExchangeAlgebra.Algebra.Value
    ( MoneyDecimal (..)
    , MoneyParseError (..)
    , moneyDecimalFromScientific
    , moneyDecimalFromText
    , toDecimal
    )

-- | Fail the suite on the first incorrect acceptance result.
check :: (Eq result, Show result) => String -> result -> result -> IO ()
check label expected actual = unless (expected == actual) $ do
    putStrLn ("[FAIL] money parse " ++ label)
    putStrLn ("  expected: " ++ show expected)
    putStrLn ("  actual:   " ++ show actual)
    exitFailure

-- | Construct a successful result with explicit scale and mantissa.
money :: Int -> Integer -> Either MoneyParseError MoneyDecimal
money places mantissa = Right (MoneyDecimal (Decimal (fromIntegral places) mantissa))

-- | Check the exact representation, including trailing zeroes.
checkParts :: String -> String -> (Int, Integer) -> IO ()
checkParts label source expected =
    check label (Right expected) $ do
        value <- moneyDecimalFromText (Text.pack source)
        let decimal = toDecimal value
        pure (fromIntegral (decimalPlaces decimal), decimalMantissa decimal)

-- | Check Scientific scale and coefficient without normalizing the input.
checkScientificParts :: String -> Scientific -> (Int, Integer) -> IO ()
checkScientificParts label source expected =
    check label (Right expected) $ do
        value <- moneyDecimalFromScientific source
        let decimal = toDecimal value
        pure (fromIntegral (decimalPlaces decimal), decimalMantissa decimal)

-- | Check each grammar, priority, boundary, and exactness example.
testExamples :: IO ()
testExamples = do
    check "ordinary decimal" (money 2 1999) (parse "19.99")
    check "integer" (money 0 5) (parse "5")
    check "exponent and plus" (money 1 150) (parse "1.50e+1")
    check "uppercase exponent" (money 0 150) (parse "1.5E2")
    check "negative exponent" (money 2 1) (parse "1e-2")
    check "negative zero" (money 0 0) (parse "-0")
    check "negative fractional zero" (money 2 0) (parse "-0.00")
    check "negative zero exponent" (money 0 0) (parse "-0e5")
    check "negative zero exponent range" (Left MoneyExponentOutOfRange) (parse "-0e10001")
    check "negative zero scale" (Left MoneyScaleOutOfRange) (parse "-0e-256")
    mapM_ (\(label, source) -> check label (Left MalformedMoney) (parse source))
        [ ("empty", "")
        , ("bare minus", "-")
        , ("space prefix", " 1")
        , ("space suffix", "1 ")
        , ("plus prefix", "+1")
        , ("missing integer", ".5")
        , ("missing fraction", "5.")
        , ("fraction before exponent missing", "1.e5")
        , ("missing integer after minus", "-.5")
        , ("leading zero", "01")
        , ("two leading zeroes", "00")
        , ("double sign", "--1")
        , ("NaN", "NaN")
        , ("Infinity", "Infinity")
        , ("group separator", "1,000")
        , ("exponent digits missing", "1e+")
        , ("negative exponent digits missing", "1e-")
        , ("uppercase exponent digits missing", "1E")
        , ("exponent without integer", "e5")
        , ("second decimal point", "1.5.5")
        , ("fullwidth digit", "１")
        , ("trailing invalid character", "1x")
        , ("malformed beats negative and exponent", "-1e99999x")
        , ("malformed beats exponent", "01e99999")
        ]
    check "negative" (Left NegativeMoney) (parse "-1")
    check "negative beats exponent" (Left NegativeMoney) (parse "-1e99999")
    check "negative beats scale" (Left NegativeMoney) (parse "-1e-300")
    check "exponent beats scale" (Left MoneyExponentOutOfRange) (parse "1e-20000")
    check "scale" (Left MoneyScaleOutOfRange) (parse "1e-300")
    check "positive exponent over bound" (Left MoneyExponentOutOfRange) (parse "1e10001")
    check "negative exponent over bound" (Left MoneyExponentOutOfRange) (parse "1e-10001")
    check "positive exponent at bound" (Right (10 ^ (10000 :: Int))) (parse "1e10000")
    check "negative exponent at bound" (Left MoneyScaleOutOfRange) (parse "1e-10000")
    check "zero at positive exponent bound" (money 0 0) (parse "0e10000")
    check "zero at negative exponent bound" (Left MoneyScaleOutOfRange) (parse "0e-10000")
    check "scale at bound" (money 255 1) (parse "1e-255")
    check "scale above bound" (Left MoneyScaleOutOfRange) (parse "1e-256")
    checkParts "fraction scale at bound" ("0." ++ replicate 255 '0') (255, 0)
    check "fraction scale above bound" (Left MoneyScaleOutOfRange)
        (parse ("0." ++ replicate 256 '0'))
    checkParts "exponent offsets fraction scale" ("0." ++ replicate 260 '0' ++ "e5")
        (255, 0)
    check "zero-padded exponent at bound" (Right (10 ^ (10000 :: Int)))
        (parse "1e010000")
    check "zero-padded exponent above bound" (Left MoneyExponentOutOfRange)
        (parse "1e010001")
    check "long exponent" (Left MoneyExponentOutOfRange)
        (parse ("1e" ++ replicate 100000 '9'))
    check "long zero exponent" (money 0 10)
        (parse ("1e" ++ replicate 100000 '0' ++ "1"))
    checkParts "written trailing zeros" "1.50" (2, 150)
    checkParts "written zero scale" "0.00" (2, 0)
    checkParts "written negative zero scale" "-0.00" (2, 0)
    checkParts "written negative zero integer" "-0" (0, 0)
    checkParts "written negative zero exponent" "-0e5" (0, 0)
    checkParts "written exponent scale" "1.50e1" (1, 150)
    check "one tenth is exact" (Right (1 / 10))
        (toRational <$> parse "0.1")
    check "scientific raw scale" (money 2 150)
        (moneyDecimalFromScientific (scientific 150 (-2)))
    checkScientificParts "scientific raw places" (scientific 150 (-2)) (2, 150)
    checkScientificParts "scientific equivalent with scale" (scientific 10 (-1)) (1, 10)
    checkScientificParts "scientific equivalent without scale" (scientific 1 0) (0, 1)
    check "scientific negative" (Left NegativeMoney)
        (moneyDecimalFromScientific (scientific (-1) maxBound))
    check "scientific negative before minBound" (Left NegativeMoney)
        (moneyDecimalFromScientific (scientific (-1) minBound))
    check "scientific negative before scale" (Left NegativeMoney)
        (moneyDecimalFromScientific (scientific (-1) (-300)))
    check "scientific positive bound" (Right (10 ^ (10000 :: Int)))
        (moneyDecimalFromScientific (scientific 1 10000))
    check "scientific negative bound" (Left MoneyScaleOutOfRange)
        (moneyDecimalFromScientific (scientific 1 (-10000)))
    check "scientific exponent over bound" (Left MoneyExponentOutOfRange)
        (moneyDecimalFromScientific (scientific 1 10001))
    check "scientific negative exponent over bound" (Left MoneyExponentOutOfRange)
        (moneyDecimalFromScientific (scientific 1 (-10001)))
    checkScientificParts "scientific scale at bound" (scientific 1 (-255)) (255, 1)
    check "scientific scale above bound" (Left MoneyScaleOutOfRange)
        (moneyDecimalFromScientific (scientific 1 (-256)))
    check "scientific minBound" (Left MoneyExponentOutOfRange)
        (moneyDecimalFromScientific (scientific 0 minBound))
    check "scientific maxBound" (Left MoneyExponentOutOfRange)
        (moneyDecimalFromScientific (scientific 0 maxBound))
    check "scientific zero scale" (money 2 0)
        (moneyDecimalFromScientific (scientific 0 (-2)))
    checkScientificParts "scientific zero scale retained" (scientific 0 (-2)) (2, 0)
  where
    parse = moneyDecimalFromText . Text.pack

-- | Generated decimal renderings recover the same exact value.
propShowRoundTrip :: Property
propShowRoundTrip = forAll (chooseInteger (0, 10 ^ (30 :: Int))) $ \mantissa ->
    forAll (chooseInt (0, 255)) $ \places ->
    let decimal = Decimal (fromIntegral places) mantissa
        result = moneyDecimalFromText (Text.pack (show decimal))
        observe value =
            let raw = toDecimal value
            in (decimalPlaces raw, decimalMantissa raw)
    in fmap observe result === Right (decimalPlaces decimal, decimalMantissa decimal)

-- | Successful text and raw Scientific inputs have equal rational values.
propTextScientificValue :: Property
propTextScientificValue = forAll (chooseInteger (0, 10 ^ (30 :: Int))) $ \mantissa ->
    forAll (chooseInt (-30, 255)) $ \exponent ->
    let scientificValue = scientific mantissa exponent :: Scientific
        textValue = Text.pack (show mantissa ++ "e" ++ show exponent)
        fromText = moneyDecimalFromText textValue
        fromScientific = moneyDecimalFromScientific scientificValue
    in case (fromText, fromScientific) of
        (Right left, Right right) -> toRational left === toRational right
        _ -> counterexample ("unexpected rejection: " ++ show (fromText, fromScientific)) False

-- | Run a property with the same sample count as the main suite.
checkProperty :: String -> Property -> IO ()
checkProperty label propertyValue = do
    result <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = False } propertyValue
    unless (isSuccess result) $ do
        putStrLn ("[FAIL] money parse " ++ label)
        putStr (output result)
        exitFailure

-- | Execute all money parser acceptance cases and properties.
runTests :: IO ()
runTests = do
    testExamples
    checkProperty "Decimal show roundtrip" propShowRoundTrip
    checkProperty "Text and Scientific rational value" propTextScientificValue
    putStrLn "[PASS] money parser grammar, boundaries, priority, exactness, and properties"
