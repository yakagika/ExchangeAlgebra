{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Validation, serialization, and algebraic laws for checked ledger postings.
module Posting.PostingSpec (runTests) where

import Control.DeepSeq (force)
import Control.Exception (TypeError, evaluate, try)
import Control.Monad (unless)
import qualified Data.Binary as Binary
import Data.Hashable (hash)
import System.Exit (exitFailure)
import Test.QuickCheck hiding (label)
import ExchangeAlgebra.Algebra hiding (map, filter, toHat)
import qualified ExchangeAlgebra.Posting as Posting
import ExchangeAlgebra.Posting
    ( Posted
    , PostSide(..)
    , Posting
    , entry
    , posted
    , postedUpperBound
    , toAlg
    , toHat
    , unPosted
    )
import qualified Posting.NoNumPosted as NoNum

-- | Account and unit coordinates used for every algebraic property.
type TestBase = HatBase (AccountTitles, CountUnit)

-- | Values stay exact after all generated postings are added.
type TestEntry = (PostSide, Posted, BasePart TestBase)

-- | Generate accepted magnitudes across normal and subnormal exponents.
genAccepted :: Gen Double
genAccepted = frequency
    [ (1, pure 0)
    , (2, do
          significand <- chooseInteger (1, 2 ^ (52 :: Int) - 1)
          pure (encodeFloat significand (-1074)))
    , (7, do
          exponent <- chooseInt (-1022, 899)
          significand <- chooseInteger (2 ^ (52 :: Int), 2 ^ (53 :: Int) - 1)
          pure (encodeFloat significand (exponent - 52)))
    , (1, pure postedUpperBound)
    ]

-- | Smart-constructor identity throughout its accepted domain.
propAcceptedIdentity :: Property
propAcceptedIdentity = forAll genAccepted $ \value ->
    case posted value of
        Left failure -> counterexample (show (value, failure)) False
        Right checked -> counterexample (show value) (unPosted checked == value)

-- | Generate checked integer amounts without floating-point rounding in sums.
genExactPosted :: Gen Posted
genExactPosted = do
    value <- chooseInteger (0, 1000)
    case posted (fromInteger value) of
        Right checked -> pure checked
        Left failure -> error ("integer test amount rejected: " ++ show failure)

-- | Generate every concrete side and occasional wildcard coordinates.
genEntry :: Gen TestEntry
genEntry = do
    side <- elements [PHat, PNot]
    value <- genExactPosted
    title <- elements [Cash, Products, Sales, wildcard]
    unit <- elements [Yen, Amount, Dollar, wildcard]
    pure (side, value, (title, unit))

-- | Generate queries with empty, duplicate, overlapping, partial, and full cases.
genQueries :: Gen [TestBase]
genQueries = frequency
    [ (1, pure [])
    , (2, (: []) <$> genQuery)
    , (2, do
          query <- genQuery
          pure [query, query])
    , (2, do
          title <- elements [Cash, Products, Sales]
          pure [Hat :< (title, Yen), HatNot :< (title, wildcard)])
    , (2, do
          title <- elements [Cash, Products, Sales]
          unit <- elements [Yen, Amount, Dollar]
          pure [Not :< (title, unit)])
    , (1, pure [HatNot :< (wildcard, wildcard)])
    , (2, do
          count <- chooseInt (1, 4)
          vectorOf count genQuery)
    ]

-- | Sample every query hat and wildcard positions independently.
genQuery :: Gen TestBase
genQuery = do
    queryHat <- elements [Hat, Not, HatNot]
    title <- elements [Cash, Products, Sales, wildcard]
    unit <- elements [Yen, Amount, Dollar, wildcard]
    pure (queryHat :< (title, unit))

-- | Generate posting lists and queries without forcing every projection to match.
genAlgebraCase :: Gen ([TestEntry], [TestBase])
genAlgebraCase = do
    count <- chooseInt (0, 20)
    entries <- vectorOf count genEntry
    queries <- genQueries
    pure (entries, queries)

-- | Convert one checked entry through the public constructor.
single :: TestEntry -> Posting TestBase
single (side, value, part) = entry side value part

-- | Ignore only the internal sequence order, preserving each posting's value and base.
sameMultiset :: Alg Double TestBase -> Alg Double TestBase -> Bool
sameMultiset left right = toASCList left == toASCList right

-- | IX-8a: projection distributes across the checked posting list.
propProjection :: Property
propProjection = forAll genAlgebraCase $ \(entries, queries) ->
    let checked = toAlg (foldMap single entries)
        raw = foldr (.+) Zero [toAlg (single item) | item <- entries]
        projected = foldr (.+) Zero
            [proj queries (toAlg (single item)) | item <- entries]
    in counterexample (show (entries, queries)) $
        sameMultiset checked raw && sameMultiset (proj queries checked) projected

-- | Checked concatenation has both identities and is associative in the algebra.
propMonoid :: Property
propMonoid = forAll genAlgebraCase $ \(entries, _) ->
    let (first, rest) = splitAt (length entries `div` 3) entries
        (second, third) = splitAt (length rest `div` 2) rest
        x = foldMap single first
        y = foldMap single second
        z = foldMap single third
        zero = mempty :: Posting TestBase
    in counterexample (show entries) $
        sameMultiset (toAlg (zero <> x)) (toAlg x)
        && sameMultiset (toAlg (x <> zero)) (toAlg x)
        && sameMultiset (toAlg ((x <> y) <> z)) (toAlg (x <> (y <> z)))

-- | Conversion preserves a single append with the algebra's structural equality.
propConversion :: Property
propConversion = forAll genAlgebraCase $ \(entries, _) ->
    let (leftEntries, rightEntries) = splitAt (length entries `div` 2) entries
        left = foldMap single leftEntries
        right = foldMap single rightEntries
    in counterexample (show entries) $
        toAlg (left <> right) == (toAlg left .+ toAlg right)

-- | Fail an ordinary Boolean assertion through the same test harness.
assertTest :: String -> Bool -> IO ()
assertTest label success = unless success $ do
    putStrLn ("[FAIL] ledger posting: " ++ label)
    exitFailure

-- | Run a named QuickCheck property and fail the executable on a counterexample.
quickProperty :: Testable property => String -> property -> IO ()
quickProperty label proposition = do
    result <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = False } proposition
    unless (isSuccess result) $ do
        putStrLn ("[FAIL] " ++ label ++ ": " ++ output result)
        exitFailure
    putStrLn ("[PASS] " ++ label)

-- | Check every validation boundary, including negative zero normalization.
testValidation :: IO ()
testValidation = do
    let positiveInfinity = 1 / 0 :: Double
        negativeInfinity = -1 / 0 :: Double
        nanValue = 0 / 0 :: Double
        nextAboveBound = postedUpperBound * (1 + 2 ** (-52))
        smallestSubnormal = encodeFloat 1 (-1074) :: Double
    assertTest "NaN rejected" (posted nanValue == Left Posting.NonFinite)
    assertTest "positive Infinity rejected" $
        posted positiveInfinity == Left Posting.NonFinite
    assertTest "negative Infinity rejected" $
        posted negativeInfinity == Left Posting.NonFinite
    assertTest "negative finite value rejected" $
        posted (-1e-300) == Left Posting.Negative
    assertTest "next representable value above bound rejected" $
        posted nextAboveBound == Left Posting.AboveBound
    assertTest "2^1000 rejected" (posted (2 ** 1000) == Left Posting.AboveBound)
    assertTest "zero accepted" (fmap unPosted (posted 0) == Right 0)
    assertTest "negative zero normalized" $ case posted (-0.0) of
        Right checked -> isPositiveZero (unPosted checked)
        Left _ -> False
    assertTest "smallest subnormal accepted" $
        fmap unPosted (posted smallestSubnormal) == Right smallestSubnormal
    assertTest "one accepted" (fmap unPosted (posted 1) == Right 1)
    assertTest "upper bound accepted" $
        fmap unPosted (posted postedUpperBound) == Right postedUpperBound
  where
    isPositiveZero value = value == 0 && isInfinite (1 / value) && 1 / value > 0

-- | Binary instances round-trip valid values and reject invalid Posted bytes.
testBinary :: IO ()
testBinary = do
    let smallestSubnormal = encodeFloat 1 (-1074) :: Double
        checkedValues =
            [value | Right value <- map posted [0, smallestSubnormal, 1, postedUpperBound]]
        sides = [PHat, PNot]
        rejects value = case Binary.decodeOrFail (Binary.encode (value :: Double)) of
            Left _ -> True
            Right (_, _, (_ :: Posted)) -> False
        decodedNegativeZero = Binary.decodeOrFail (Binary.encode (-0.0 :: Double))
    assertTest "Posted Binary round trip" $
        all (\value -> Binary.decode (Binary.encode value) == value) checkedValues
    assertTest "PostSide Binary round trip" $
        all (\side -> Binary.decode (Binary.encode side) == side) sides
    assertTest "Posted Binary decoder validates" $
        all rejects
            [ 0 / 0
            , 1 / 0
            , -1 / 0
            , -1e-300
            , postedUpperBound * (1 + 2 ** (-52))
            , 2 ** 1000
            ]
    assertTest "Posted Binary normalizes negative zero" $ case decodedNegativeZero of
        Left _ -> False
        Right (_, _, (value :: Posted)) ->
            unPosted value == 0 && 1 / unPosted value > 0
    assertTest "NFData and Hashable instances" $
        force checkedValues `seq` force sides `seq`
        sum (map hash checkedValues) `seq` sum (map hash sides) `seq` True

-- | Both posting sides map to concrete Hat values only.
testSides :: IO ()
testSides = do
    assertTest "PHat maps to Hat" (toHat PHat == Hat)
    assertTest "PNot maps to Not" (toHat PNot == Not)
    assertTest "PostSide excludes HatNot" $
        all ((/= HatNot) . toHat) [minBound .. maxBound]

-- | Three equal-base entries expose structural ordering without breaking the multiset law.
testAssociativityRegression :: IO ()
testAssociativityRegression = case traverse posted [1, 2, 3] of
    Left failure -> do
        putStrLn ("[FAIL] ledger posting fixture: " ++ show failure)
        exitFailure
    Right [first, second, third] -> do
        let part = (Cash, Yen)
            x = entry PHat first part :: Posting TestBase
            y = entry PHat second part :: Posting TestBase
            z = entry PHat third part :: Posting TestBase
            left = toAlg ((x <> y) <> z)
            right = toAlg (x <> (y <> z))
        assertTest "equal-base grouping changes structural order" (left /= right)
        assertTest "equal-base grouping preserves the multiset" (sameMultiset left right)
    Right _ -> assertTest "three checked fixture values" False

-- | Fixed queries cover each Hat value and both coordinate wildcard positions.
testQueryCoverage :: IO ()
testQueryCoverage = case posted 3 of
    Left failure -> do
        putStrLn ("[FAIL] ledger posting fixture: " ++ show failure)
        exitFailure
    Right value -> do
        let entries =
                [ (PHat, value, (Cash, Yen))
                , (PNot, value, (Products, Amount))
                , (PHat, value, (Sales, Dollar))
                ]
            queries =
                [ Hat :< (Cash, wildcard)
                , Not :< (wildcard, Amount)
                , HatNot :< (Sales, wildcard)
                ]
            checked = toAlg (foldMap single entries)
            projected = foldr (.+) Zero
                [proj queries (toAlg (single item)) | item <- entries]
        assertTest "three query hats and coordinate wildcards" $
            sameMultiset (proj queries checked) projected

-- | Deferred errors prove that clients cannot request Num for Posted.
testNoNum :: IO ()
testNoNum = do
    literal <- try (evaluate NoNum.literalPosted) :: IO (Either TypeError Posted)
    assertTest "Posted numeric literal is rejected" (isTypeError literal)
    case posted 1 of
        Left failure -> do
            putStrLn ("[FAIL] ledger posting fixture: " ++ show failure)
            exitFailure
        Right checked -> do
            arithmetic <- try (evaluate (NoNum.addPosted checked))
                :: IO (Either TypeError Posted)
            assertTest "Posted arithmetic is rejected" (isTypeError arithmetic)
            generic <- try (evaluate (NoNum.genericPosted checked))
                :: IO (Either TypeError ())
            assertTest "Posted Generic is rejected" (isTypeError generic)
            container <- try (evaluate (NoNum.addPosting (mempty :: Posting TestBase)))
                :: IO (Either TypeError (Posting TestBase))
            assertTest "Posting arithmetic is rejected" (isTypeError container)
  where
    isTypeError (Left _) = True
    isTypeError (Right _) = False

-- | Register checked-posting tests with ExchangeAlgebra-test.
runTests :: IO ()
runTests = do
    testValidation
    testBinary
    testSides
    testAssociativityRegression
    testQueryCoverage
    testNoNum
    quickProperty "V-1 accepted-domain identity" propAcceptedIdentity
    quickProperty "IX-8a projection and raw conversion" propProjection
    quickProperty "Posting monoid laws" propMonoid
    quickProperty "Posting conversion preserves structural append" propConversion
    putStrLn "[PASS] checked ledger posting"
