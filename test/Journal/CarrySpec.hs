{-# LANGUAGE ScopedTypeVariables #-}

-- | Acceptance properties for explicit journal carryover.
module Journal.CarrySpec (runTests) where

import ExchangeAlgebra.Accounting.Account (AccountTitles(..))
import qualified ExchangeAlgebra.Algebra.Internal as Internal
import Control.Monad (unless)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
import Test.QuickCheck

import ExchangeAlgebra.Algebra (Alg, (.@), foldEntries)
import ExchangeAlgebra.Algebra.Value (HatVal, MoneyDecimal)
import GHC.Float (castDoubleToWord64)
import ExchangeAlgebra.Algebra.Base
    ( CountUnit(..)
    , Hat(..)
    , HatBase(..)
    )
import ExchangeAlgebra.Journal (Journal, (.|))
import qualified ExchangeAlgebra.Journal as Journal
import ExchangeAlgebra.Journal.Transfer.Rule (CarryError(..), carryBefore, carryEntries)

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
        [ before === Left ResultOutOfRange
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

-- | K4: a selected complete base whose rounded net exceeds Double fails.
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
        Left ResultOutOfRange -> pure ()
        Left failure     -> failTest ("unexpected overflow error: " ++ show failure)
        Right _          -> failTest "overflowing selected side sum succeeded"

-- | K1: side overflow and a tiny addend still succeed when the rounded net is finite.
testExpandedRange :: IO ()
testExpandedRange = do
    let maximumFinite = encodeFloat (2 ^ (53 :: Int) - 1) 971 :: Double
        leastSubnormal = encodeFloat 1 (-1074) :: Double
        originals =
            [ [(0, (Yen, Cash), Not, maximumFinite)
              ,(1, (Yen, Cash), Not, maximumFinite)
              ,(2, (Yen, Cash), Hat, maximumFinite)]
            , [(0, (Yen, Cash), Not, maximumFinite)
              ,(1, (Yen, Cash), Not, leastSubnormal)]
            ]
    mapM_ (\original -> do
        let journal = build original
        case (carryBefore (< 3) 9 journal, carryEntries (< 3) 9 journal) of
            (Right carried, Right additions) -> do
                unless (fmap (castDoubleToWord64 . valueOf) (rows carried)
                    == [castDoubleToWord64 maximumFinite]) $
                    failTest "K1 expanded range has wrong rounded net"
                unless (all (\(_, _, _, value) -> not (isInfinite value)) (rows additions)) $
                    failTest "K1 carry entries contain infinity"
            failures -> failTest (show failures)) originals
  where
    valueOf :: Row -> Double
    valueOf (_, _, _, value) = value

-- | K2-K3: decimal carry preserves exact balances and the replacement decomposition.
propDecimalCarry :: Property
propDecimalCarry = forAll rowsGen $ \original ->
    let journal = foldMap posting original
        posting :: Row -> Journal Int MoneyDecimal (HatBase Part)
        posting (note, coordinates, side, value) =
            (fromRational (toRational value) .@ (side :< coordinates)) .| note
        scalarRows :: HatVal v => Journal Int v (HatBase Part) -> [(Int, Part, Hat, v)]
        scalarRows input = concatMap entries
                         $ HashMap.toList
                         $ Journal.toMap input
          where
            entries :: HatVal v => (Int, Alg v (HatBase Part)) -> [(Int, Part, Hat, v)]
            entries (note, algebra) = foldEntries
                (\previous value (side :< coordinates) ->
                    (note, coordinates, side, value) : previous) [] algebra
        exactBalances :: Journal Int MoneyDecimal (HatBase Part) -> Map.Map Part Rational
        exactBalances = Map.fromListWith (+) . fmap contribution . scalarRows
        contribution :: (Int, Part, Hat, MoneyDecimal) -> (Part, Rational)
        contribution (_, coordinates, side, value)
            | side == Hat = (coordinates, negate (toRational value))
            | otherwise   = (coordinates, toRational value)
    in case (carryEntries (< 3) 9 journal, carryBefore (< 3) 9 journal) of
        (Right additions, Right replaced) ->
            let decomposed = Journal.filterWithNote (\note _ -> note >= 3)
                    (journal <> additions)
            in conjoin
                [ property $ all (== 0) $ Map.elems $ Map.unionWith (-)
                    (exactBalances journal) (exactBalances (journal <> additions))
                , property $ all (== 0) $ Map.elems $ Map.unionWith (-)
                    (exactBalances journal) (exactBalances replaced)
                , multiset (scalarRows replaced) === multiset (scalarRows decomposed)
                ]
        failures -> counterexample (show failures) False

-- | K5: all selected inputs are checked before rounding, in a fixed priority.
propFailurePriority :: Property
propFailurePriority = forAll (shuffle [0 :: Int, 1, 2, 3, 4]) $ \order ->
    let maximumFinite = encodeFloat (2 ^ (53 :: Int) - 1) 971 :: Double
        invalid = [(HatNot, 1), (Not, 0 / 0), (Hat, -1)
                  ,(Not, maximumFinite), (Not, maximumFinite)]
        assemble :: [(Hat, Double)] -> TestJournal
        assemble postings = Journal.fromMap $ HashMap.fromList
            [ (note, value `Internal.Single` (side :< (Yen, Cash)))
            | (note, (side, value)) <- zip order postings ]
        check :: [(Hat, Double)] -> CarryError -> Property
        check postings expected = conjoin
            [ fmap (const ()) (carryBefore (const True) 9 (assemble postings)) === Left expected
            , fmap (const ()) (carryEntries (const True) 9 (assemble postings)) === Left expected
            ]
    in conjoin
        [ check invalid WildcardSide
        , check (drop 1 invalid) NonFiniteValue
        , check (drop 2 invalid) NegativeValue
        , check (drop 3 invalid) ResultOutOfRange
        ]

-- | K5: raw zero HatNot is absent from both the net and reversal.
testZeroWildcard :: IO ()
testZeroWildcard = do
    let wildcardJournal = Journal.fromMap $ HashMap.singleton 0
            (0 `Internal.Single` (HatNot :< (Yen, Cash))) :: TestJournal
        input = wildcardJournal <> build [(1, (Yen, Cash), Not, 3)]
        expected = build [(9, (Yen, Cash), Not, 3)]
    unless (fmap (multiset . rows) (carryBefore (< 3) 9 input)
        == Right (multiset (rows expected))) $ failTest "zero wildcard carryBefore"
    case carryEntries (< 3) 9 input of
        Left failure -> failTest (show failure)
        Right additions -> unless
            (multiset (rows additions) == multiset
                [(1, (Yen, Cash), Hat, 3), (9, (Yen, Cash), Not, 3)]) $
            failTest "zero wildcard reversal"

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
    testExpandedRange
    quickProperty 500 "K2-K3 decimal carry" propDecimalCarry
    quickProperty 200 "K5 failure priority" propFailurePriority
    testZeroWildcard
    putStrLn "[PASS] journal carry (retention, rounding, zero, and carry note)"

-- | Run one named QuickCheck property and fail on a counterexample.
quickProperty :: Testable property => Int -> String -> property -> IO ()
quickProperty count label proposition = do
    result <- quickCheckWithResult stdArgs { maxSuccess = count, chatty = False } proposition
    unless (isSuccess result) $ failTest (label ++ ": " ++ output result)
