-- | Acceptance properties for explicit journal carryover.
module Journal.CarrySpec (runTests) where

import Control.Monad (unless)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
import Test.QuickCheck

import ExchangeAlgebra.Algebra ((.@), foldEntries)
import ExchangeAlgebra.Algebra.Base
    ( AccountTitles(..)
    , CountUnit(..)
    , Hat(..)
    , HatBase(..)
    )
import ExchangeAlgebra.Journal (Journal, (.|))
import qualified ExchangeAlgebra.Journal as Journal
import ExchangeAlgebra.Journal.Transfer.Rule (carryBefore, carryEntries)
import ExchangeAlgebra.Journal.Exact (ExactSumError(..))

-- | Test postings use two base axes and integer note labels.
type Part = (CountUnit, AccountTitles)

-- | One scalar posting with its complete base coordinates.
type Row = (Int, Part, Hat, Double)

-- | The generated journal's concrete base has unit and account coordinates.
type TestJournal = Journal Int Double (HatBase Part)

-- | Include decimal values and the binary64 rounding boundary.
valueGen :: Gen Double
valueGen = elements [0.1, 0.2, 0.3, 1, 2, 2 ^ (53 :: Int), 2 ^ (53 :: Int) + 2]

-- | Vary selected notes, retained notes, sides, and complete bases.
rowsGen :: Gen [Row]
rowsGen = do
    count <- chooseInt (0, 18)
    vectorOf count $ do
        note <- elements [0, 1, 2, 3, 4, 9]
        unit <- elements [Yen, Dollar]
        account <- elements [Cash, Deposits, Products]
        side <- elements [Not, Hat]
        value <- valueGen
        pure (note, (unit, account), side, value)

-- | Retain the redundancy of each original scalar posting.
build :: [Row] -> TestJournal
build = foldMap (\(note, coordinates, side, value) ->
    (value .@ (side :< coordinates)) .| note)

-- | Extract individual scalar entries, including their original note.
rows :: TestJournal -> [Row]
rows journal = concatMap entries (HashMap.toList (Journal.toMap journal))
  where
    entries (note, algebra) = foldEntries
        (\previous value (side :< coordinates) ->
            (note, coordinates, side, value) : previous)
        []
        algebra

-- | Count repeated entries without relying on their storage order.
multiset :: Ord a => [a] -> Map.Map a Int
multiset = Map.fromListWith (+) . fmap (\entry -> (entry, 1))

-- | Compute exact signed balances directly from the scalar entries.
balances :: [Row] -> Map.Map Part Rational
balances = Map.fromListWith (+) . fmap contribution
  where
    contribution (_, coordinates, side, value) =
        (coordinates, case side of
            Not    -> toRational value
            Hat    -> negate (toRational value)
            HatNot -> error "CarrySpec: concrete-side invariant violated")

-- | Carry preserves retained scalars and rounds only each selected net.
propCarry :: Property
propCarry = forAll rowsGen $ \original ->
    case carryBefore (< 3) 9 (build original) of
      Left failure -> counterexample (show failure) False
      Right after  -> checkCarry original (rows after)

-- | Forgetting selected notes after adding carry entries matches replacement.
-- The carry note 9 is outside the selected notes 0, 1, and 2.
propCarryEntriesDecomposition :: Property
propCarryEntriesDecomposition = forAll rowsGen $ \original ->
    let journal = build original
        observed = do
            additions <- carryEntries (< 3) 9 journal
            pure (Journal.filterWithNote (\note _ -> note >= 3) (journal <> additions))
    in fmap (multiset . filter nonzero . rows) observed
        === fmap (multiset . filter nonzero . rows) (carryBefore (< 3) 9 journal)
  where
    nonzero (_, _, _, value) = value /= 0

-- | Each base's exact balance changes by the rounding of its selected net.
propCarryEntriesBalance :: Property
propCarryEntriesBalance = forAll rowsGen $ \original ->
    let journal = build original
        selectedBalances = balances
            (filter (\(note, _, _, _) -> note < 3) original)
        originalBalances = balances original
        observed = do
            additions <- carryEntries (< 3) 9 journal
            pure (balances (rows (journal <> additions)))
        check result = all (matches result) allCoordinates
        allCoordinates = Map.keys (Map.union originalBalances selectedBalances)
        matches result coordinates =
            let before = Map.findWithDefault 0 coordinates originalBalances
                after = Map.findWithDefault 0 coordinates result
                selected = Map.findWithDefault 0 coordinates selectedBalances
                rounded = toRational (fromRational selected :: Double)
            in after - before == rounded - selected
    in fmap check observed === Right True

-- | Compare exact balances and entry multisets after a successful carry.
checkCarry :: [Row] -> [Row] -> Property
checkCarry original observed =
    let retained = filter (\(note, _, _, _) -> note >= 3) original
        retainedCounts = multiset retained
        observedCounts = multiset observed
        originalBalances = balances original
        observedBalances = balances observed
        selectedBalances = balances
            (filter (\(note, _, _, _) -> note < 3) original)
        expectedCarried =
            [ (9, coordinates, netSide amount, fromRational (abs amount))
            | (coordinates, amount) <- Map.toAscList selectedBalances
            , amount /= 0
            ]
        allCoordinates = Map.keys (Map.unions
            [originalBalances, observedBalances, selectedBalances])
        retainedPresent = all (\(entry, count) ->
            Map.findWithDefault 0 entry observedCounts >= count)
            (Map.toList retainedCounts)
        otherRetained = filter (\(note, _, _, _) -> note /= 9) retained
        otherObserved = filter (\(note, _, _, _) -> note /= 9) observed
        roundedExactlyOnce coordinates =
            let beforeBalance = Map.findWithDefault 0 coordinates originalBalances
                afterBalance = Map.findWithDefault 0 coordinates observedBalances
                selectedBalance = Map.findWithDefault 0 coordinates selectedBalances
                roundedSelected = toRational (fromRational selectedBalance :: Double)
            in afterBalance - beforeBalance == roundedSelected - selectedBalance
        valid (_, _, side, value) =
            side /= HatNot && value > 0 && not (isNaN value) && not (isInfinite value)
    in conjoin
        [ multiset observed === multiset (retained ++ expectedCarried)
        , counterexample "retained scalar multiset" (property retainedPresent)
        , multiset otherObserved === multiset otherRetained
        , counterexample "carry difference is not the selected net's one rounding"
            (property (all roundedExactlyOnce allCoordinates))
        , counterexample "carry generated an invalid magnitude or side"
            (property (all valid observed))
        ]
  where
    netSide amount
        | amount > 0 = Not
        | otherwise  = Hat

-- | An exact cancellation adds nothing, even with an existing carry note.
propZeroAndExistingNote :: Property
propZeroAndExistingNote =
    let original =
            [ (0, (Yen, Cash), Not, 0.1)
            , (1, (Yen, Cash), Hat, 0.1)
            , (9, (Yen, Deposits), Not, 3)
            ]
        after = fmap rows (carryBefore (< 3) 9 (build original))
    in fmap multiset after === Right (multiset [(9, (Yen, Deposits), Not, 3)])

-- | An existing entry on the carry note and complete base is appended to.
propCarryNoteCollision :: Property
propCarryNoteCollision =
    let original =
            [ (0, (Yen, Cash), Not, 2)
            , (9, (Yen, Cash), Not, 2)
            , (0, (Dollar, Cash), Hat, 3)
            ]
        expected =
            [ (9, (Yen, Cash), Not, 2)
            , (9, (Yen, Cash), Not, 2)
            , (9, (Dollar, Cash), Hat, 3)
            ]
    in fmap (multiset . rows) (carryBefore (< 3) 9 (build original))
        === Right (multiset expected)

-- | Carry entries keep the existing carry-note entry and add a separate net.
propCarryEntriesCollision :: Property
propCarryEntriesCollision =
    let original =
            [ (0, (Yen, Cash), Not, 2)
            , (9, (Yen, Cash), Not, 2)
            ]
        expected =
            [ (0, (Yen, Cash), Hat, 2)
            , (9, (Yen, Cash), Not, 2)
            ]
    in fmap (multiset . rows) (carryEntries (< 3) 9 (build original))
        === Right (multiset expected)

-- | Both carry operations report the same checked aggregation failure.
propCarryEntriesFailureParity :: Property
propCarryEntriesFailureParity =
    let maximumFinite = encodeFloat (2 ^ (53 :: Int) - 1) 971 :: Double
        original = build
            [ (0, (Yen, Cash), Not, maximumFinite)
            , (1, (Yen, Cash), Not, maximumFinite)
            ]
        before = fmap (const ()) (carryBefore (< 3) 9 original)
        entries = fmap (const ()) (carryEntries (< 3) 9 original)
    in conjoin
        [ before === Left SumOutOfRange
        , entries === before
        ]

-- | Selection of every or no note follows the same exact balance contract.
propSelectionExtremes :: Property
propSelectionExtremes =
    let original =
            [ (0, (Yen, Cash), Not, 0.1)
            , (1, (Yen, Cash), Not, 0.2)
            , (9, (Dollar, Cash), Hat, 4)
            ]
        none = fmap (multiset . rows) (carryBefore (const False) 12 (build original))
        allEntries = fmap (multiset . rows) (carryBefore (const True) 12 (build original))
        expectedAll =
            [ (12, (Yen, Cash), Not, fromRational
                (toRational (0.1 :: Double) + toRational (0.2 :: Double)))
            , (12, (Dollar, Cash), Hat, 4)
            ]
    in conjoin
        [ none === Right (multiset original)
        , allEntries === Right (multiset expectedAll)
        ]

-- | A selected complete base whose exact side sum exceeds Double fails.
testOverflow :: IO ()
testOverflow = do
    let maximumFinite = encodeFloat (2 ^ (53 :: Int) - 1) 971 :: Double
        original =
            [ (0, (Yen, Cash), Not, maximumFinite)
            , (1, (Yen, Cash), Not, maximumFinite)
            ]
        carried = carryBefore (< 3) 9 (build original)
        additions = carryEntries (< 3) 9 (build original)
    unless (fmap (const ()) carried == fmap (const ()) additions) $
        failTest "carryEntries and carryBefore disagree on overflow"
    case carried of
        Left SumOutOfRange -> pure ()
        Left failure     -> failTest ("unexpected overflow error: " ++ show failure)
        Right _          -> failTest "overflowing selected side sum succeeded"

-- | Report a deterministic acceptance failure and stop the suite.
failTest :: String -> IO ()
failTest message = do
    putStrLn ("[FAIL] journal carry: " ++ message)
    exitFailure

-- | Run journal carry properties as part of the package test suite.
runTests :: IO ()
runTests = do
    quickProperty 500 "carry" propCarry
    quickProperty 500 "carry decomposition" propCarryEntriesDecomposition
    quickProperty 500 "carry balance" propCarryEntriesBalance
    quickProperty 1 "carry zero" propZeroAndExistingNote
    quickProperty 1 "carry collision" propCarryNoteCollision
    quickProperty 1 "carry entries collision" propCarryEntriesCollision
    quickProperty 1 "carry error parity" propCarryEntriesFailureParity
    quickProperty 1 "carry selection" propSelectionExtremes
    testOverflow
    putStrLn "[PASS] journal carry (retention, rounding, zero, and carry note)"

-- | Run one named QuickCheck property and fail on a counterexample.
quickProperty :: Testable property => Int -> String -> property -> IO ()
quickProperty count label proposition = do
    result <- quickCheckWithResult stdArgs { maxSuccess = count, chatty = False } proposition
    unless (isSuccess result) $ failTest (label ++ ": " ++ output result)
