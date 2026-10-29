{-# LANGUAGE ScopedTypeVariables #-}

-- | Rational-oracle and bit-level acceptance tests for exact readouts.
module Algebra.ExactSumSpec (runTests) where

import qualified ExchangeAlgebra.Accounting.Exchange as Exchange
import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Accounting.Exchange
import Control.Monad (unless)
import Control.Exception (SomeException, evaluate, try)
import qualified Data.Decimal as Decimal
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64)
import System.Exit (exitFailure)
import Test.QuickCheck hiding (collect)

import ExchangeAlgebra.Algebra.Base hiding (equal)
import ExchangeAlgebra.Algebra
    ( Alg
    , (.@)
    , norm
    , bar
    , balanceMapBy
    , netPairMapBy
    , postFromNetBy
    , projNetNorm
    )
import qualified ExchangeAlgebra.Algebra as Algebra
import qualified ExchangeAlgebra.Journal as Journal
import ExchangeAlgebra.Accounting.TrialBalance.Balance (AccountBalance(..), accountBalances)
import ExchangeAlgebra.Algebra.Value
    ( HatVal(..)
    , sumValues
    , MoneyDecimal(..)
    , MoneyDouble(..)
    )

-- | Concrete complete bases with no wildcard ordering in the oracle.
type TestBase = HatBase AccountTitles

-- | Double posting ledger used by the readout acceptance cases.
type TestAlg = Alg Double TestBase

-- | Notes are strings, including the existing empty-string plank wildcard.
type TestJournal = Journal.Journal String Double TestBase

-- | Largest finite binary64 input.
maximumFinite :: Double
maximumFinite = encodeFloat (2 ^ (53 :: Int) - 1) 971

-- | Smallest positive binary64 subnormal input.
subnormal :: Double
subnormal = encodeFloat 1 (-1074)

-- | First integer whose successor cannot be represented in binary64.
threshold :: Double
threshold = 2 ^ (53 :: Int)

-- | Compare successful outputs by all 64 bits, including the sign of zero.
bits :: Double -> Word64
bits = castDoubleToWord64

-- | Independent nearest-even conversion, including IEEE overflow.
oracle :: Rational -> Double
oracle = fromRational

-- | Build an accumulator without rounding its intermediate states.
accumulate :: HatVal n => [n] -> Accum n
accumulate = List.foldl' (flip addAccum) emptyAccum

-- | Generate finite non-negative inputs across all binary64 exponent ranges.
genValue :: Gen Double
genValue = frequency
    [ (3, elements [0, subnormal, maximumFinite, threshold, 1, 1e308])
    , (7, do
        mantissa <- chooseInteger (1, 2 ^ (53 :: Int) - 1)
        power <- chooseInt (-1074, 971)
        pure (encodeFloat mantissa power))
    ]

-- | Moderate-sized lists include both valid and overflowing exact totals.
genValues :: Gen [Double]
genValues = chooseInt (0, 25) >>= flip vectorOf genValue

-- | Permutations, construction by split, and merge association share one oracle.
propSum :: Property
propSum = forAll genValues $ \values -> forAll (shuffle values) $ \permuted ->
    forAll (chooseInt (0, length values)) $ \cut ->
    let (left, right) = splitAt cut values
        expected = bits (oracle (sum (fmap toRational values)))
        first = accumulate left
        second = accumulate right
        (middle, lastPart) = splitAt (length right `div` 2) right
        middleState = accumulate middle
        lastState = accumulate lastPart
    in conjoin
        [ bits (sumValues values) === expected
        , bits (sumValues permuted) === expected
        , bits (roundAccum (mergeAccum first second)) === expected
        , bits (roundAccum (mergeAccum second first)) === expected
        , bits (roundAccum (mergeAccum emptyAccum (mergeAccum first second))) === expected
        , bits (roundAccum (mergeAccum (mergeAccum first emptyAccum) second)) === expected
        , bits (roundAccum (mergeAccum first (mergeAccum middleState lastState))) === expected
        , bits (roundAccum (mergeAccum (mergeAccum first middleState) lastState)) === expected
        ]

-- | Subtraction rounds the exact signed difference, including overflowing sides.
propNet :: Property
propNet = forAll genValues $ \left -> forAll genValues $ \right ->
    let first = sum (fmap toRational left)
        second = sum (fmap toRational right)
        difference = mergeAccum (accumulate left) (negateAccum (accumulate right))
    in bits (roundAccum difference) === bits (oracle (first - second))

-- | Merging residual states preserves their exact non-negative values.
propResidualMerge :: Property
propResidualMerge = forAll genValue $ \a -> forAll genValue $ \b ->
    forAll genValue $ \c -> forAll genValue $ \d ->
    let firstSigned = mergeAccum (accumulate [a]) (negateAccum (accumulate [b]))
        secondSigned = mergeAccum (accumulate [c]) (negateAccum (accumulate [d]))
        first = case signAccum firstSigned of
            Just LT -> negateAccum firstSigned
            _       -> firstSigned
        second = case signAccum secondSigned of
            Just LT -> negateAccum secondSigned
            _       -> secondSigned
        expected = bits (oracle (abs (toRational a - toRational b)
            + abs (toRational c - toRational d)))
    in conjoin
        [ bits (roundAccum (mergeAccum first second)) === expected
        , bits (roundAccum (mergeAccum second first)) === expected
        ]

-- | Bias toward high-exponent ties that create a negative low partial.
genBoundarySide :: Gen [Double]
genBoundarySide = frequency
    [ (1, pure [maximumFinite])
    , (3, do
        high <- chooseInteger (2 ^ (53 :: Int) - 16, 2 ^ (53 :: Int) + 16)
        low <- chooseInteger (1, 31)
        pure [encodeFloat high 970, encodeFloat low 970])
    ]

-- | Both subtraction directions stay finite for near-boundary states.
propBoundaryNet :: Property
propBoundaryNet = forAll genBoundarySide $ \left -> forAll genBoundarySide $ \right ->
    let first = sum (fmap toRational left)
        second = sum (fmap toRational right)
        difference = mergeAccum (accumulate left) (negateAccum (accumulate right))
    in bits (roundAccum difference) === bits (oracle (first - second))

-- | Subtraction also accepts previously netted residual states.
propNestedNet :: Property
propNestedNet = forAll genValue $ \a -> forAll genValue $ \b ->
    forAll genValue $ \c -> forAll genValue $ \d ->
    let first = abs (toRational a - toRational b)
        second = abs (toRational c - toRational d)
        leftSigned = mergeAccum (accumulate [a]) (negateAccum (accumulate [b]))
        rightSigned = mergeAccum (accumulate [c]) (negateAccum (accumulate [d]))
        left = case signAccum leftSigned of
            Just LT -> negateAccum leftSigned
            _       -> leftSigned
        right = case signAccum rightSigned of
            Just LT -> negateAccum rightSigned
            _       -> rightSigned
        difference = mergeAccum left (negateAccum right)
    in bits (roundAccum difference) === bits (oracle (first - second))

-- | Generate small ledgers with exact, independently calculable group residuals.
genEntries :: Gen [(Double, TestBase)]
genEntries = do
    count <- chooseInt (0, 20)
    vectorOf count $ do
        value <- elements [subnormal, 1, 2, 3, threshold, threshold + 2]
        postingBase <- (:<) <$> elements [Not, Hat] <*> elements [Cash, Deposits, Sales]
        pure (value, postingBase)

-- | Construct only through the public checked posting constructor.
build :: [(Double, TestBase)] -> TestAlg
build = Algebra.fromList . fmap (uncurry (.@))

-- | Observe scalar bits without relying on HashMap traversal or Alg equality.
observeAlg :: TestAlg -> [(String, Word64)]
observeAlg = List.sort . Algebra.foldEntries
    (\entries value postingBase -> (show postingBase, castDoubleToWord64 value) : entries) []

-- | Independently sum signed Rational values per concrete account title.
exactGroups :: [(Double, TestBase)] -> Map.Map AccountTitles Rational
exactGroups = List.foldl' collect Map.empty
  where
    collect totals (value, postingBase) = Map.insertWith (+) (base postingBase)
        (signed postingBase (toRational value)) totals
    signed postingBase value
        | isHat postingBase = negate value
        | otherwise = value

-- | Regrouping a multiset preserves bar bits; residual merging rounds only once.
propReadouts :: Property
propReadouts = forAll genEntries $ \entries -> forAll (shuffle entries) $ \permuted ->
    let groups = exactGroups entries
        original = build entries
        reordered = mconcat (fmap (uncurry (.@)) permuted)
        gross = sum (fmap (toRational . fst) entries)
        residual = sum (fmap abs (Map.elems groups))
        expectedBalances = fmap oracle groups
        expectedPair =
            ( oracle (sum [value | value <- Map.elems groups, value > 0])
            , oracle (sum [abs value | value <- Map.elems groups, value < 0])
            )
        pairs = netPairMapBy (const (Just ())) original
        actualPair = Map.findWithDefault (0, 0) () pairs
        queries = [HatNot :< wildcard]
    in conjoin
        [ observeAlg (bar original) === observeAlg (bar reordered)
        , bits (norm original) === bits (oracle gross)
        , bits (projNetNorm queries original) === bits (oracle residual)
        , balanceMapBy Just original === expectedBalances
        , actualPair === expectedPair
        ]

-- | Journal permutation and assembly preserve per-note projection residuals.
propJournal :: Property
propJournal = forAll genEntries $ \entries ->
    let annotated = zip (cycle ["a", "b", "c"]) entries
        make (note, (value, postingBase)) = value .@ postingBase Journal..| note
        original = mconcat (fmap make annotated) :: TestJournal
        query = [HatNot :< wildcard]
        noteGroups = List.foldl' collect Map.empty annotated
        collect totals (note, (value, postingBase)) =
            Map.insertWith (+) (note, base postingBase) (signed postingBase value) totals
        signed postingBase value
            | isHat postingBase = negate (toRational value)
            | otherwise = toRational value
        expected = bits (oracle (sum (fmap abs (Map.elems noteGroups))))
    in forAll (shuffle annotated) $ \permuted ->
        let reordered = Journal.fromList (fmap make permuted)
        in conjoin
            [ bits (Journal.projWithBaseNetNorm query original) === expected
            , bits (Journal.projWithBaseNetNorm query reordered) === expected
            , balanceMapBy Just (Journal.toAlg reordered)
                === balanceMapBy Just (Journal.toAlg original)
            , accountBalances (Journal.toAlg reordered)
                === accountBalances (Journal.toAlg original)
            ]

-- | Handwritten wrappers preserve the Double oracle; Decimal merge is exact.
propInstances :: Property
propInstances = forAll genValues $ \values ->
    let expected = bits (sumValues values)
        money = fmap MoneyDouble values
        unwrapMoney (MoneyDouble value) = value
        decimalValues = fmap (MoneyDecimal . Decimal.Decimal 2 . toInteger) [1 .. length values]
        decimalState = accumulate decimalValues
    in conjoin
        [ bits (unwrapMoney (sumValues money)) === expected
        , roundAccum (mergeAccum decimalState emptyAccum) === sum decimalValues
        , sumValues (reverse decimalValues) === sum decimalValues
        ]

-- | Run a deterministic check and stop the suite on failure.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check name expected actual = unless (expected == actual) $ do
    putStrLn ("[FAIL] exact " ++ name ++ ": expected " ++ show expected ++ ", got " ++ show actual)
    exitFailure

-- | Run a reproducible-size property batch with failure details on demand.
propertyCheck :: String -> Property -> IO ()
propertyCheck name proposition = do
    result <- quickCheckWithResult stdArgs { maxSuccess = 1000, chatty = False } proposition
    unless (isSuccess result) $ do
        putStrLn ("[FAIL] exact " ++ name ++ ": " ++ output result)
        exitFailure
    putStrLn ("[PASS] exact " ++ name ++ " (1000 cases)")

-- | Floating boundaries, exact cancellation, and invalid posting rejection.
testAccumulator :: IO ()
testAccumulator = do
    check "2^53,1,1" (threshold + 2) (sumValues [threshold, 1, 1])
    check "subnormals" (3 * subnormal) (sumValues [subnormal, subnormal, subnormal])
    check "tie even down" threshold (sumValues [threshold, 1])
    check "tie even up" (threshold + 4) (sumValues [threshold + 2, 1])
    check "beyond tie" (threshold + 2) (sumValues [threshold, 1, subnormal])
    check "net below tie" threshold
        (roundAccum (mergeAccum (accumulate [threshold + 2])
            (negateAccum (accumulate [1, subnormal]))))
    check "zero sign" 0 (bits (sumValues [-0, 0 :: Double]))
    check "net zero sign" 0
        (bits (roundAccum (mergeAccum (accumulate [-0 :: Double])
            (negateAccum (accumulate [0])))))
    check "M" maximumFinite (sumValues [maximumFinite])
    check "M boundary inside" maximumFinite
        (sumValues [encodeFloat (2 ^ (53 :: Int) - 2) 971, encodeFloat 1 971])
    check "M plus smallest rounds M" maximumFinite (sumValues [maximumFinite, subnormal])
    check "both sides overflow but cancel" 0
        (roundAccum (mergeAccum (accumulate [1e308, 1e308 :: Double])
            (negateAccum (accumulate [1e308, 1e308]))))
    check "overflow rounds to infinity" True (isInfinite (sumValues [maximumFinite, maximumFinite]))
    let boundaryValues = [encodeFloat 1 1023, 3 * encodeFloat 1 970]
        boundaryState = accumulate boundaryValues
        expectedDifference = fromRational
            (toRational maximumFinite - sum (fmap toRational boundaryValues))
        boundaryLedger = build
            [(value, Not :< Cash) | value <- boundaryValues]
            <> build [(maximumFinite, Hat :< Cash)]
    check "negative low versus M" (negate expectedDifference)
        (roundAccum (mergeAccum boundaryState (negateAccum (accumulate [maximumFinite]))))
    check "M versus negative low" expectedDifference
        (roundAccum (mergeAccum (accumulate [maximumFinite]) (negateAccum boundaryState)))
    check "boundary projection finite" expectedDifference
        (projNetNorm [HatNot :< Cash] boundaryLedger)
    check "boundary bar does not throw" expectedDifference (norm (bar boundaryLedger))
    mapM_ (\value -> do
        result <- try (evaluate (norm (value .@ (Not :< Cash) :: TestAlg)))
            :: IO (Either SomeException Double)
        case result of
            Left _  -> pure ()
            Right _ -> fail "invalid posting accepted") [-1, 0 / 0, 1 / 0, -1 / 0]
    mapM_ (\value -> do
        result <- try (evaluate (norm (MoneyDouble value .@ (Not :< Cash)
            :: Alg MoneyDouble TestBase))) :: IO (Either SomeException MoneyDouble)
        case result of
            Left _  -> pure ()
            Right _ -> fail "invalid MoneyDouble posting accepted") [-1, 1 / 0]
    decimalResult <- try (evaluate (norm ((-1 :: MoneyDecimal) .@ (Not :< Cash))))
        :: IO (Either SomeException MoneyDecimal)
    case decimalResult of
        Left _  -> pure ()
        Right _ -> fail "negative MoneyDecimal posting accepted"
    let nearMaximum = accumulate [maximumFinite]
        tiny = accumulate [subnormal]
        below = mergeAccum nearMaximum (negateAccum tiny)
    check "M-small rounds M" maximumFinite (roundAccum below)
    check "M-small plus small" maximumFinite (roundAccum (mergeAccum below tiny))
    check "small plus M-small" maximumFinite (roundAccum (mergeAccum tiny below))
    check "nested residual" subnormal
        (roundAccum (mergeAccum nearMaximum (negateAccum below)))
    check "MoneyDouble" (MoneyDouble (threshold + 2))
        (sumValues [MoneyDouble threshold, 1, 1])
    let decimal = MoneyDecimal (Decimal.Decimal 2 1)
    check "MoneyDecimal scale" (MoneyDecimal (Decimal.Decimal 2 101))
        (sumValues [1, decimal])
    check "MoneyDecimal unlimited" (2 * 10 ^ (400 :: Int) :: MoneyDecimal)
        (sumValues [10 ^ (400 :: Int), 10 ^ (400 :: Int)])
    check "MoneyDecimal net" (negate decimal)
        (roundAccum (mergeAccum (accumulate [1]) (negateAccum (accumulate [1, decimal]))))

-- | Fixed grouping, projection, and accounting cases from the accepted contract.
testReadouts :: IO ()
testReadouts = do
    let pairExample = build [(10, Not :< Cash), (7, Hat :< Deposits)]
        sameBase = build [(10, Not :< Cash), (7, Hat :< Cash)]
        selectHat postingBase
            | isHat postingBase = Just ()
            | otherwise = Nothing
        post _ value = value .@ (Not :< Sales)
        query = [HatNot :< wildcard]
        large = build [(1e308, Not :< Cash), (1e308, Not :< Deposits)]
        roundExample = build [(threshold, Not :< Cash), (1, Not :< Cash), (1, Not :< Deposits)]
        equal = build [(10, Not :< Cash), (10, Hat :< Cash)]
    check "pair retains base distinction" (Map.singleton () (10, 7))
        (netPairMapBy (const (Just ())) pairExample)
    check "post selects after netting" []
        (observeAlg (postFromNetBy selectHat post sameBase))
    check "balance direction" (Map.fromList [(Cash, 10), (Deposits, -7)])
        (balanceMapBy Just pairExample)
    check "zero key retained" (Map.singleton Cash 0)
        (balanceMapBy Just equal)
    check "bar zero removed" [] (observeAlg (bar equal))
    check "gross overflow" True (isInfinite (norm large))
    check "residual overflow" True (isInfinite (projNetNorm query large))
    check "pair overflow" (Map.singleton () (1 / 0, 0))
        (netPairMapBy (const (Just ())) large)
    check "post overflow reaches callback" (observeAlg sameBase)
        (observeAlg (postFromNetBy (const (Just ()))
            (\_ value -> case value == 1 / 0 of
                True  -> sameBase
                False -> mempty)
            (build [(1e308, Not :< Cash), (1e308, Not :< Cash)])))
    check "projection rounds once" (threshold + 2) (projNetNorm query roundExample)
    check "pair rounds once per key side" (Map.singleton () (threshold + 2, 0))
        (netPairMapBy (const (Just ())) roundExample)
    check "post rounds once per key" (threshold + 2)
        (norm (postFromNetBy (const (Just ())) post roundExample))
    check "two rounds differ" threshold (norm (bar roundExample))
    check "duplicate base query" (projNetNorm query roundExample)
        (projNetNorm (query ++ query) roundExample)
    check "debit difference" (Debit, 3) (diffRL sameBase)
    check "exact balance" True (balance equal)
    check "account balances" (Map.singleton Cash (DebitBalance 3))
        (accountBalances sameBase)
    let journal = (10 .@ (Not :< Cash) Journal..| "a")
               <> (10 .@ (Hat :< Cash) Journal..| "b") :: TestJournal
        roundedJournal = (build [(threshold, Not :< Cash), (1, Not :< Cash)] Journal..| "a")
                      <> (build [(1, Not :< Cash)] Journal..| "b")
    check "journal separate notes" 20 (Journal.projWithBaseNetNorm query journal)
    check "journal duplicate base queries" 20
        (Journal.projWithBaseNetNorm (query ++ query) journal)
    check "journal note selection" 10
        (Journal.projWithNoteBaseNetNorm ["a", "a"] query journal)
    check "journal empty notes" 20
        (Journal.projWithNoteBaseNetNorm [] query journal)
    check "journal plank notes" 20
        (Journal.projWithNoteBaseNetNorm ["a", ""] query journal)
    check "journal once across notes" (threshold + 2)
        (Journal.projWithBaseNetNorm query roundedJournal)
    check "journal bar gathers" 0 (Journal.norm (Journal.bar journal))
    check "journal balance" True (Exchange.balance journal)
    check "journal diff" (Side, 0) (Exchange.diffRL journal)
    check "journal account" (Map.singleton Cash NoBalance)
        (accountBalances (Journal.toAlg journal))
    check "journal pair gathers" Map.empty
        (netPairMapBy Just (Journal.toAlg journal))
    check "journal signed gathers" (Map.singleton Cash 0)
        (balanceMapBy Just (Journal.toAlg journal))
    check "journal post gathers" 0
        (Journal.norm (Journal.postFromNetBy selectHat
            (\key value -> post key value Journal..| "new") journal))
    check "journal post rounds once" (threshold + 2)
        (Journal.norm (Journal.postFromNetBy (const (Just ()))
            (\key value -> post key value Journal..| "new") roundedJournal))
    check "journal residual overflow" True
        (isInfinite (Journal.projWithBaseNetNorm query (large Journal..| "a")))

-- | Execute all exact-readout acceptance checks in the main test suite.
runTests :: IO ()
runTests = do
    testAccumulator
    testReadouts
    propertyCheck "sum, permutation, merge, and Rational oracle" propSum
    propertyCheck "net and Rational oracle" propNet
    propertyCheck "residual merge and Rational oracle" propResidualMerge
    propertyCheck "high-exponent tie netting" propBoundaryNet
    propertyCheck "nested netting and Rational oracle" propNestedNet
    propertyCheck "readouts, construction order, and Rational oracle" propReadouts
    propertyCheck "journal multiset and note-local Rational oracle" propJournal
    propertyCheck "handwritten instances" propInstances
    putStrLn "[PASS] exact boundary, grouping, journal, and instance acceptance cases"
