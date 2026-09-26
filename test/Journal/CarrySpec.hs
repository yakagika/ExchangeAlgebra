-- | Acceptance properties for explicit journal carryover.
module Journal.CarrySpec (runTests) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import qualified Data.HashMap.Strict as HashMap
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
import Test.QuickCheck

import ExchangeAlgebra.Algebra ((.@), foldEntries)
import ExchangeAlgebra.Algebra.Base
    (AccountTitles(..), CountUnit(..), Hat(..), HatBase(..))
import ExchangeAlgebra.Journal (Journal, (.|))
import qualified ExchangeAlgebra.Journal as Journal
import ExchangeAlgebra.Journal.Carry (carryBefore)

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
            Not -> toRational value
            Hat -> negate (toRational value)
            HatNot -> error "CarrySpec: concrete-side invariant violated")

-- | Carry preserves retained scalars and rounds only each selected net.
propCarry :: Property
propCarry = forAll rowsGen $ \original ->
    let before = build original
        after = carryBefore (< 3) 9 before
        observed = rows after
        retained = filter (\(note, _, _, _) -> note >= 3) original
        retainedCounts = multiset retained
        observedCounts = multiset observed
        originalBalances = balances original
        observedBalances = balances observed
        selectedBalances = balances
            (filter (\(note, _, _, _) -> note < 3) original)
        expectedCarried =
            [ (9, coordinates, if amount > 0 then Not else Hat, fromRational (abs amount))
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

-- | An exact cancellation adds nothing, even with an existing carry note.
propZeroAndExistingNote :: Property
propZeroAndExistingNote =
    let original =
            [ (0, (Yen, Cash), Not, 0.1)
            , (1, (Yen, Cash), Hat, 0.1)
            , (9, (Yen, Deposits), Not, 3)
            ]
        after = rows (carryBefore (< 3) 9 (build original))
    in multiset after === multiset [(9, (Yen, Deposits), Not, 3)]

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
    in multiset (rows (carryBefore (< 3) 9 (build original))) === multiset expected

-- | Selection of every or no note follows the same exact balance contract.
propSelectionExtremes :: Property
propSelectionExtremes =
    let original =
            [ (0, (Yen, Cash), Not, 0.1)
            , (1, (Yen, Cash), Not, 0.2)
            , (9, (Dollar, Cash), Hat, 4)
            ]
        none = rows (carryBefore (const False) 12 (build original))
        allEntries = rows (carryBefore (const True) 12 (build original))
        expectedAll =
            [ (12, (Yen, Cash), Not, fromRational
                (toRational (0.1 :: Double) + toRational (0.2 :: Double)))
            , (12, (Dollar, Cash), Hat, 4)
            ]
    in conjoin
        [ multiset none === multiset original
        , multiset allEntries === multiset expectedAll
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
    outcome <- try (evaluate (length (rows carried))) :: IO (Either SomeException Int)
    case outcome of
        Left failure -> unless
            ("selected complete-base side sums must fit Double" `isInfixOf` show failure)
            (failTest "overflow failure did not name the precondition")
        Right _ -> failTest "overflowing selected side sum succeeded"

-- | Report a deterministic acceptance failure and stop the suite.
failTest :: String -> IO ()
failTest message = do
    putStrLn ("[FAIL] journal carry: " ++ message)
    exitFailure

-- | Run journal carry properties as part of the package test suite.
runTests :: IO ()
runTests = do
    carryResult <- quickCheckWithResult
        stdArgs { maxSuccess = 500, chatty = False } propCarry
    unless (isSuccess carryResult) $ do
        putStrLn ("[FAIL] journal carry: " ++ output carryResult)
        exitFailure
    zeroResult <- quickCheckWithResult
        stdArgs { maxSuccess = 1, chatty = False } propZeroAndExistingNote
    unless (isSuccess zeroResult) $ do
        putStrLn ("[FAIL] journal carry zero: " ++ output zeroResult)
        exitFailure
    collisionResult <- quickCheckWithResult
        stdArgs { maxSuccess = 1, chatty = False } propCarryNoteCollision
    unless (isSuccess collisionResult) $ do
        putStrLn ("[FAIL] journal carry collision: " ++ output collisionResult)
        exitFailure
    extremesResult <- quickCheckWithResult
        stdArgs { maxSuccess = 1, chatty = False } propSelectionExtremes
    unless (isSuccess extremesResult) $ do
        putStrLn ("[FAIL] journal carry selection: " ++ output extremesResult)
        exitFailure
    testOverflow
    putStrLn "[PASS] journal carry (retention, rounding, zero, and carry note)"
