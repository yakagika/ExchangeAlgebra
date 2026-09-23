{-# LANGUAGE ScopedTypeVariables #-}

-- | Rational-oracle and bit-level acceptance tests for checked exact readouts.
module Algebra.ExactSumSpec (runTests) where

import Control.Monad (unless)
import qualified Data.Decimal as Decimal
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64)
import qualified Number.NonNegative as NN
import System.Exit (exitFailure)
import Test.QuickCheck hiding (collect)

import ExchangeAlgebra.Algebra.Base hiding (equal)
import ExchangeAlgebra.Algebra (Alg, (.@))
import qualified ExchangeAlgebra.Algebra as Algebra
import ExchangeAlgebra.Algebra.Exact
import qualified ExchangeAlgebra.Journal as Journal
import qualified ExchangeAlgebra.Journal.Exact as JournalExact
import ExchangeAlgebra.TrialBalance.Balance (AccountBalance(..))
import ExchangeAlgebra.Value (MoneyDecimal(..), MoneyDouble(..))

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
bits :: Either ExactSumError Double -> Either ExactSumError Word64
bits = fmap castDoubleToWord64

-- | Exact reference with a range check before nearest-even conversion.
oracle :: Rational -> Either ExactSumError Double
oracle value
    | value > toRational maximumFinite = Left SumOutOfRange
    | otherwise = Right (fromRational value)

-- | Build an accumulator without rounding its intermediate states.
accumulate :: ExactSum n => [n] -> Accum n
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
        [ bits (sumExact values) === expected
        , bits (sumExact permuted) === expected
        , bits (roundAccum (mergeAccum first second)) === expected
        , bits (roundAccum (mergeAccum second first)) === expected
        , bits (roundAccum (mergeAccum emptyAccum (mergeAccum first second))) === expected
        , bits (roundAccum (mergeAccum (mergeAccum first emptyAccum) second)) === expected
        , bits (roundAccum (mergeAccum first (mergeAccum middleState lastState))) === expected
        , bits (roundAccum (mergeAccum (mergeAccum first middleState) lastState)) === expected
        ]

-- | Netting checks both exact side ranges before subtracting, without tolerance.
propNet :: Property
propNet = forAll genValues $ \left -> forAll genValues $ \right ->
    let first = sum (fmap toRational left)
        second = sum (fmap toRational right)
        expected
            | max first second > toRational maximumFinite = Left SumOutOfRange
            | otherwise = (,) (compare first second) <$> oracle (abs (first - second))
        observe = fmap (\(direction, value) -> (direction, castDoubleToWord64 value))
    in observe (netAccum (accumulate left) (accumulate right)) === observe expected

-- | Merging signed residual expansions preserves their exact non-negative values.
propResidualMerge :: Property
propResidualMerge = forAll genValue $ \a -> forAll genValue $ \b ->
    forAll genValue $ \c -> forAll genValue $ \d ->
    let result = do
            (_, first) <- netAccumState (accumulate [a]) (accumulate [b])
            (_, second) <- netAccumState (accumulate [c]) (accumulate [d])
            let left = mergeAccum first second
                right = mergeAccum second first
            pure (bits (roundAccum left), bits (roundAccum right))
        expected = bits (oracle (abs (toRational a - toRational b)
            + abs (toRational c - toRational d)))
    in result === Right (expected, expected)

-- | Bias toward high-exponent ties that create a negative low partial.
genBoundarySide :: Gen [Double]
genBoundarySide = frequency
    [ (1, pure [maximumFinite])
    , (3, do
        high <- chooseInteger (2 ^ (53 :: Int) - 16, 2 ^ (53 :: Int) + 16)
        low <- chooseInteger (1, 31)
        pure [encodeFloat high 970, encodeFloat low 970])
    ]

-- | Both subtraction directions must stay finite for valid near-boundary states.
propBoundaryNet :: Property
propBoundaryNet = forAll genBoundarySide $ \left -> forAll genBoundarySide $ \right ->
    let first = sum (fmap toRational left)
        second = sum (fmap toRational right)
        observe = fmap (\(direction, value) -> (direction, castDoubleToWord64 value))
        expected = (,) (compare first second) <$> oracle (abs (first - second))
    in observe (netAccum (accumulate left) (accumulate right)) === observe expected

-- | Subtraction also accepts previously netted states with signed low components.
propNestedNet :: Property
propNestedNet = forAll genValue $ \a -> forAll genValue $ \b ->
    forAll genValue $ \c -> forAll genValue $ \d ->
    let first = abs (toRational a - toRational b)
        second = abs (toRational c - toRational d)
        result = do
            (_, left) <- netAccumState (accumulate [a]) (accumulate [b])
            (_, right) <- netAccumState (accumulate [c]) (accumulate [d])
            netAccum left right
        expected = (,) (compare first second) <$> oracle (abs (first - second))
        observe = fmap (\(direction, value) -> (direction, castDoubleToWord64 value))
    in observe result === observe expected

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
        expectedBalances = traverse (\value ->
            (,) (compare value 0) <$> oracle (abs value)) groups
        expectedPair = (,)
            <$> oracle (sum [value | value <- Map.elems groups, value > 0])
            <*> oracle (sum [abs value | value <- Map.elems groups, value < 0])
        pairs = netPairMapByExact (const (Just ())) original
        actualPair = fmap (Map.findWithDefault (0, 0) ()) pairs
        queries = [HatNot :< wildcard]
    in conjoin
        [ fmap observeAlg (barExact original) === fmap observeAlg (barExact reordered)
        , bits (normExact original) === bits (oracle gross)
        , bits (projNetNormExact queries original) === bits (oracle residual)
        , balanceMapByExact Just original === expectedBalances
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
            [ bits (JournalExact.projNetNormExact query original) === expected
            , bits (JournalExact.projNetNormExact query reordered) === expected
            , JournalExact.balanceMapByExact Just reordered
                === JournalExact.balanceMapByExact Just original
            , JournalExact.accountBalancesExact reordered
                === JournalExact.accountBalancesExact original
            ]

-- | Handwritten wrappers preserve the Double oracle; Decimal merge is exact.
propInstances :: Property
propInstances = forAll genValues $ \values -> forAll (shuffle values) $ \permuted ->
    let expected = bits (sumExact values)
        money = fmap MoneyDouble values
        nonNegative = fmap NN.fromNumber permuted :: [NN.Double]
        unwrapMoney (MoneyDouble value) = value
        decimalValues = fmap (MoneyDecimal . Decimal.Decimal 2 . toInteger) [1 .. length values]
        decimalState = accumulate decimalValues
    in conjoin
        [ bits (fmap unwrapMoney (sumExact money)) === expected
        , bits (fmap NN.toNumber (sumExact nonNegative)) === expected
        , roundAccum (mergeAccum decimalState emptyAccum) === Right (sum decimalValues)
        , sumExact (reverse decimalValues) === Right (sum decimalValues)
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

-- | Floating boundaries and sticky validation errors, independent of algebra construction.
testAccumulator :: IO ()
testAccumulator = do
    check "2^53,1,1" (Right (threshold + 2)) (sumExact [threshold, 1, 1])
    check "subnormals" (Right (3 * subnormal)) (sumExact [subnormal, subnormal, subnormal])
    check "tie even down" (Right threshold) (sumExact [threshold, 1])
    check "tie even up" (Right (threshold + 4)) (sumExact [threshold + 2, 1])
    check "beyond tie" (Right (threshold + 2)) (sumExact [threshold, 1, subnormal])
    check "net below tie" (Right (GT, threshold))
        (netAccum (accumulate [threshold + 2]) (accumulate [1, subnormal]))
    check "zero sign" (Right 0) (bits (sumExact [-0, 0 :: Double]))
    check "net zero sign" (Right (EQ, 0))
        (fmap (\(direction, value) -> (direction, castDoubleToWord64 value))
            (netAccum (accumulate [-0 :: Double]) (accumulate [0])))
    check "M" (Right maximumFinite) (sumExact [maximumFinite])
    check "M boundary inside" (Right maximumFinite)
        (sumExact [encodeFloat (2 ^ (53 :: Int) - 2) 971, encodeFloat 1 971])
    check "M plus smallest" (Left SumOutOfRange) (sumExact [maximumFinite, subnormal])
    check "both sides overflow" (Left SumOutOfRange)
        (netAccum (accumulate [1e308, 1e308 :: Double]) (accumulate [1e308, 1e308]))
    let boundaryValues = [encodeFloat 1 1023, 3 * encodeFloat 1 970]
        boundaryState = accumulate boundaryValues
        expectedDifference = fromRational
            (toRational maximumFinite - sum (fmap toRational boundaryValues))
        boundaryLedger = build
            [(value, Not :< Cash) | value <- boundaryValues]
            <> build [(maximumFinite, Hat :< Cash)]
    check "negative low versus M" (Right (LT, expectedDifference))
        (netAccum boundaryState (accumulate [maximumFinite]))
    check "M versus negative low" (Right (GT, expectedDifference))
        (netAccum (accumulate [maximumFinite]) boundaryState)
    check "boundary projection finite" (Right expectedDifference)
        (projNetNormExact [HatNot :< Cash] boundaryLedger)
    check "boundary bar does not throw" (Right expectedDifference)
        (barExact boundaryLedger >>= normExact)
    check "negative" (Left NegativeInput) (sumExact [-1 :: Double])
    check "NaN" (Left NonFiniteInput) (sumExact [0 / 0 :: Double])
    check "positive infinity" (Left NonFiniteInput) (sumExact [1 / 0 :: Double])
    check "negative infinity" (Left NonFiniteInput) (sumExact [-1 / 0 :: Double])
    let failed = accumulate [-1 :: Double]
    check "sticky add" (Left NegativeInput) (roundAccum (addAccum 1 failed))
    check "sticky merge left" (Left NegativeInput) (roundAccum (mergeAccum failed emptyAccum))
    check "sticky merge right" (Left NegativeInput) (roundAccum (mergeAccum emptyAccum failed))
    check "sticky net" (Left NegativeInput) (netAccum failed failed)
    let nearMaximum = accumulate [maximumFinite]
        tiny = accumulate [subnormal]
    case netAccumState nearMaximum tiny of
        Left failure -> check "M-small state" (Right ()) (Left failure)
        Right (_, below) -> do
            check "M-small rounds M" (Right maximumFinite) (roundAccum below)
            check "M-small plus small" (Right maximumFinite) (roundAccum (mergeAccum below tiny))
            check "small plus M-small" (Right maximumFinite) (roundAccum (mergeAccum tiny below))
            check "nested residual" (Right (GT, subnormal)) (netAccum nearMaximum below)
    check "MoneyDouble" (Right (MoneyDouble (threshold + 2)))
        (sumExact [MoneyDouble threshold, 1, 1])
    check "MoneyDouble nonfinite" (Left NonFiniteInput) (sumExact [MoneyDouble (1 / 0)])
    check "MoneyDouble negative" (Left NegativeInput) (sumExact [MoneyDouble (-1)])
    check "NN.Double" (Right (NN.fromNumber (threshold + 2)))
        (sumExact [NN.fromNumber threshold, 1, 1] :: Either ExactSumError NN.Double)
    check "NN magnitude only" (Right (LT, 7 :: NN.Double))
        (netAccum (accumulate [3]) (accumulate [10]))
    check "MoneyDecimal negative" (Left NegativeInput) (sumExact [-1 :: MoneyDecimal])
    let decimal = MoneyDecimal (Decimal.Decimal 2 1)
    check "MoneyDecimal scale" (Right (MoneyDecimal (Decimal.Decimal 2 101)))
        (sumExact [1, decimal])
    check "MoneyDecimal unlimited" (Right (2 * 10 ^ (400 :: Int) :: MoneyDecimal))
        (sumExact [10 ^ (400 :: Int), 10 ^ (400 :: Int)])
    check "MoneyDecimal net" (Right (LT, decimal))
        (netAccum (accumulate [1]) (accumulate [1, decimal]))

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
    check "pair retains base distinction" (Right (Map.singleton () (10, 7)))
        (netPairMapByExact (const (Just ())) pairExample)
    check "post selects after netting" (Right [])
        (fmap observeAlg (postFromNetByExact selectHat post sameBase))
    check "balance direction" (Right (Map.fromList [(Cash, (GT, 10)), (Deposits, (LT, 7))]))
        (balanceMapByExact Just pairExample)
    check "zero key retained" (Right (Map.singleton Cash (EQ, 0)))
        (balanceMapByExact Just equal)
    check "bar zero removed" (Right []) (fmap observeAlg (barExact equal))
    check "gross overflow" (Left SumOutOfRange) (normExact large)
    check "residual overflow" (Left SumOutOfRange) (projNetNormExact query large)
    check "pair overflow" (Left SumOutOfRange) (netPairMapByExact (const (Just ())) large)
    check "post overflow" (Left SumOutOfRange)
        (fmap observeAlg (postFromNetByExact (const (Just ())) post large))
    check "projection rounds once" (Right (threshold + 2)) (projNetNormExact query roundExample)
    check "pair rounds once per key side" (Right (Map.singleton () (threshold + 2, 0)))
        (netPairMapByExact (const (Just ())) roundExample)
    check "post rounds once per key" (Right (threshold + 2))
        (postFromNetByExact (const (Just ())) post roundExample >>= normExact)
    check "two rounds differ" (Right threshold) (barExact roundExample >>= normExact)
    check "duplicate base query" (projNetNormExact query roundExample)
        (projNetNormExact (query ++ query) roundExample)
    check "debit difference" (Right (Debit, 3)) (diffRLExact sameBase)
    check "exact balance" (Right True) (balanceExact equal)
    check "account balances" (Right (Map.singleton Cash (DebitBalance 3)))
        (accountBalancesExact sameBase)
    let journal = (10 .@ (Not :< Cash) Journal..| "a")
               <> (10 .@ (Hat :< Cash) Journal..| "b") :: TestJournal
        roundedJournal = (build [(threshold, Not :< Cash), (1, Not :< Cash)] Journal..| "a")
                      <> (build [(1, Not :< Cash)] Journal..| "b")
    check "journal separate notes" (Right 20) (JournalExact.projWithBaseNetNormExact query journal)
    check "journal duplicate base queries" (Right 20)
        (JournalExact.projWithBaseNetNormExact (query ++ query) journal)
    check "journal note selection" (Right 10)
        (JournalExact.projWithNoteBaseNetNormExact ["a", "a"] query journal)
    check "journal empty notes" (Right 20)
        (JournalExact.projWithNoteBaseNetNormExact [] query journal)
    check "journal plank notes" (Right 20)
        (JournalExact.projWithNoteBaseNetNormExact ["a", ""] query journal)
    check "journal once across notes" (Right (threshold + 2))
        (JournalExact.projWithBaseNetNormExact query roundedJournal)
    check "journal bar gathers" (Right 0) (JournalExact.barExact journal >>= JournalExact.normExact)
    check "journal balance" (Right True) (JournalExact.balanceExact journal)
    check "journal diff" (Right (Side, 0)) (JournalExact.diffRLExact journal)
    check "journal account" (Right (Map.singleton Cash NoBalance))
        (JournalExact.accountBalancesExact journal)
    check "journal pair gathers" (Right Map.empty)
        (JournalExact.netPairMapByExact Just journal)
    check "journal signed gathers" (Right (Map.singleton Cash (EQ, 0)))
        (JournalExact.balanceMapByExact Just journal)
    check "journal post gathers" (Right 0)
        (JournalExact.postFromNetByExact selectHat
            (\key value -> post key value Journal..| "new") journal >>= JournalExact.normExact)
    check "journal post rounds once" (Right (threshold + 2))
        (JournalExact.postFromNetByExact (const (Just ()))
            (\key value -> post key value Journal..| "new") roundedJournal
                >>= JournalExact.normExact)
    check "journal alias" (Right 20) (JournalExact.projNetNormExact query journal)
    check "journal residual overflow" (Left SumOutOfRange)
        (JournalExact.projWithBaseNetNormExact query (large Journal..| "a"))

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
