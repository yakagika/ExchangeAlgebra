{-# LANGUAGE ScopedTypeVariables #-}

-- | Exact balance laws for Accounting transfers and their Journal lifting.
-- This Accounting test suite uses independent Rational observations and a
-- sequential posting rewrite oracle. Read the fixtures before 'runTests'.
module Transfer.BalanceSpec (runTests) where

import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Accounting.Exchange
import Control.Monad (forM_, unless)
import Data.List (sort)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import System.Exit (exitFailure)
import Test.QuickCheck hiding (collect, label)
import ExchangeAlgebra.Algebra hiding (base, filter, map)
import qualified ExchangeAlgebra.Algebra as Algebra
import qualified ExchangeAlgebra.Algebra.Internal as Internal
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.Accounting.Closing (ClosingSide(..), closingEntries, closingSide)
import qualified ExchangeAlgebra.Accounting.Transfer as Transfer
import qualified ExchangeAlgebra.Journal.Core as Journal
import ExchangeAlgebra.Journal.Core ((.|))

-- | Two-coordinate base using the library's existing account instance.
type TestBase = HatBase (CountUnit, AccountTitles)

-- | A posting retains its value and complete base for raw pair observations.
type Posting v = (v, TestBase)

-- | A row fixture has independent source and destination account titles.
type AccountMove = (AccountTitles, AccountTitles)

-- | Stop the suite immediately on a failed law or fixture.
assertLaw :: String -> Bool -> IO ()
assertLaw label passed = unless passed $ do
    putStrLn ("[FAIL] " ++ label)
    exitFailure

-- | Require successful validation without using partial extraction.
requireRight :: Show e => String -> Either e a -> IO a
requireRight _ (Right value) = pure value
requireRight label (Left failure) = do
    putStrLn ("[FAIL] " ++ label ++ ": " ++ show failure)
    exitFailure

-- | Run properties with their law identifiers in test output.
checkLaw :: Testable property => String -> property -> IO ()
checkLaw label lawProperty = do
    putStrLn ("[LAW] " ++ label)
    result <- quickCheckWithResult stdArgs { maxSuccess = 100 } lawProperty
    assertLaw label (isSuccess result)

-- | Observe every raw posting, preserving repetitions and traversal order.
postings :: forall v. HatVal v => Alg v TestBase -> [Posting v]
postings = foldEntries append []
  where
    append :: [Posting v] -> v -> TestBase -> [Posting v]
    append entries value base = entries ++ [(value, base)]

-- | Read one finite value as an exact Rational through its accumulator.
exactValue :: HatVal v => v -> Maybe Rational
exactValue value = exactAccum (addAccum value emptyAccum)

-- | Observe exact Not-minus-Hat balances, omitting zero coordinates.
exactBalances :: forall v. HatVal v => Alg v TestBase -> Maybe (Map.Map TestBase Rational)
exactBalances ledger = fmap (Map.filter (/= 0)) $ foldl' collect (Just Map.empty)
    (postings ledger)
  where
    collect :: Maybe (Map.Map TestBase Rational)
            -> Posting v
            -> Maybe (Map.Map TestBase Rational)
    collect previous (value, side :< coordinates) = do
        balances <- previous
        amount   <- exactValue value
        let signed :: Rational
            signed = case side of
                Not -> amount
                Hat -> negate amount
                HatNot -> 0
        pure (Map.insertWith (+) (Not :< coordinates) signed balances)

-- | Sum selected debit or credit values without any floating-point addition.
exactTotal :: HatVal v => Alg v TestBase -> Maybe Rational
exactTotal ledger = fmap sum $ traverse (exactValue . fst) (postings ledger)

-- | Construct a wildcard-unit row without Hat/Not in its endpoints.
moveRow :: AccountMove -> Transfer.TransferRow TestBase
moveRow (source, target) = (wildcard, source) Transfer..-> (wildcard, target)
    Transfer.|% Transfer.Relabel

-- | Build the ordered two-stage chain used by the rewrite oracle.
chainMoves :: [AccountMove]
chainMoves = [(Cash, Deposits), (Deposits, Sales)]

-- | Generate valid redundant ledgers with positive and negative net balances.
genLedger :: HatVal v => Gen (Alg v TestBase)
genLedger = do
    count <- chooseInt (0, 35)
    entries <- vectorOf count $ do
        amount <- elements [1, 2, 3, 1 / 10, 1 / 5, 9007199254740992]
        side   <- elements [Hat, Not]
        unit   <- elements [Yen, Dollar, wildcard]
        title  <- elements [Cash, Deposits, Sales, Products]
        pure (fromRational amount .@ side :< (unit, title))
    pure (Algebra.fromList entries)

-- | Rewrite each posting row by row, independently of the transfer machinery.
rewriteLedger :: forall v. HatVal v => [AccountMove] -> Alg v TestBase -> Alg v TestBase
rewriteLedger moves ledger = Algebra.fromList
    [value .@ foldl' rewrite base moves | (value, base) <- postings ledger]
  where
    rewrite :: TestBase -> AccountMove -> TestBase
    rewrite base@(side :< (unit, title)) (source, target)
        | title /= source = base
        | whichSide (Not :< (unit, source)) == whichSide (Not :< (unit, target)) =
            side :< (unit, target)
        | otherwise = revHat (side :< (unit, target))

-- | Check the pair structure without comparing rounded totals.
validPairs :: forall v. HatVal v => [Posting v] -> Bool
validPairs entries = sideValues (whichSide (Not :< (Yen, Cash) :: TestBase))
    == sideValues (whichSide (Hat :< (Yen, Cash) :: TestBase))
  where
    sideValues :: Side -> [Maybe Rational]
    sideValues side = sort [exactValue value | (value, base) <- entries, whichSide base == side]

-- | T1-T5 use exact Rational observations, including nonrepresentable Double nets.
propRelabel :: forall v. HatVal v => Proxy v -> Property
propRelabel _ = forAll (genLedger :: Gen (Alg v TestBase)) $ \ledger ->
    case Transfer.mkTransferTable (map moveRow chainMoves) of
        Left failure -> counterexample (show failure) False
        Right table -> case Transfer.transferEntries table ledger of
            Left failure -> counterexample (show failure) False
            Right entries ->
                let combined :: Alg v TestBase
                    combined = ledger .+ entries
                    sourceBalances :: Maybe (Map.Map TestBase Rational)
                    sourceBalances = fmap (Map.filterWithKey isSource) (exactBalances combined)
                    isSource :: TestBase -> Rational -> Bool
                    isSource (_ :< (_, title)) _ = title `elem` [Cash, Deposits]
                    positive :: Posting v -> Bool
                    positive (value, _) = value > 0 && not (isErrorValue value)
                in conjoin
                    [ counterexample "T1 exact debit/credit and equal pair values" $
                        exactTotal (decL entries) == exactTotal (decR entries)
                        && validPairs (postings entries)
                    , counterexample "T2 finite positive complete pairs" $
                        all positive (postings entries)
                    , counterexample "T3 exact source closure" $
                        sourceBalances == Just Map.empty
                    , counterexample "T4 exact empty second result" $
                        Transfer.transferEntries table combined == Right Zero
                    , counterexample "T5 sequential independent rewrite" $
                        exactBalances combined == exactBalances (rewriteLedger chainMoves ledger)
                    ]

-- | T6 covers every ordered distinct pair of registry accounts and both sides.
testAllAccountPairs :: forall v. HatVal v => Proxy v -> IO ()
testAllAccountPairs _ = forM_ concreteAccountTitles $ \source ->
    forM_ concreteAccountTitles $ \target -> unless (source == target) $
        forM_ [Hat, Not] $ \side -> do
            table <- requireRight "T6 table" $ Transfer.mkTransferTable [moveRow (source, target)]
            entries <- requireRight "T6 entries" $ Transfer.transferEntries table
                (3 .@ side :< (Yen, source) :: Alg v TestBase)
            let expectedSide :: Hat
                expectedSide
                    | whichSide (Not :< (Yen, source)) == whichSide (Not :< (Yen, target)) =
                        side
                    | otherwise = hat (revHat (side :< (Yen, target)))
                expected :: Alg v TestBase
                expected = 3 .@ revHat (side :< (Yen, source))
                    .+ 3 .@ expectedSide :< (Yen, target)
            assertLaw "T6 registry pair Hat/Not and T2 one representable pair" $
                sort (postings entries) == sort (postings expected)

-- | T7 first checks the whole closing registry before testing any transfers.
testClosingHomeSides :: IO ()
testClosingHomeSides = forM_ concreteAccountTitles $ \title -> case closingSide title of
    Nothing -> pure ()
    Just decision -> do
        let sameHome :: Bool
            sameHome = whichSide (Not :< (Yen, title) :: TestBase)
                == whichSide (Not :< (Yen, RetainedEarnings) :: TestBase)
        assertLaw ("T7 STOP: closing/home mismatch " ++ show title) $
            (decision == ClosingKeep) == sameHome

-- | T7 compares raw posting multisets over all eligible closing accounts.
testClosingMultiset :: forall v. HatVal v => Proxy v -> IO ()
testClosingMultiset _ = do
    let titles :: [AccountTitles]
        titles = filter (isJust . closingSide) concreteAccountTitles
        ledger :: Alg v TestBase
        ledger = Algebra.fromList
            [ amount .@ side :< (unit, title)
            | title <- titles
            , unit <- [Yen, Dollar, wildcard]
            , (amount, side) <- [(0.1, Not), (0.2, Not), (0.05, Hat)]
            ]
    table <- requireRight "T7 table" $ Transfer.mkTransferTable
        [moveRow (title, RetainedEarnings) | title <- titles]
    closing <- requireRight "T7 closing" (closingEntries ledger)
    entries <- requireRight "T7 transfer" (Transfer.transferEntries table ledger)
    assertLaw "T7 closing raw multiset" $ sort (postings closing) == sort (postings entries)

-- | T2 and T3 pin the Double balance that cannot fit in a single posting.
testDoubleSplit :: IO ()
testDoubleSplit = do
    table <- requireRight "T2 split table" $ Transfer.mkTransferTable [moveRow (Cash, Sales)]
    let ledger :: Alg Double TestBase
        ledger = 9007199254740992 .@ Not :< (Yen, Cash) .+ 1 .@ Not :< (Yen, Cash)
    entries <- requireRight "T2 split entries" (Transfer.transferEntries table ledger)
    assertLaw "T2 split balance has two exact pairs" $
        length (postings entries) == 4 && validPairs (postings entries)
    assertLaw "T3 split residual is exactly zero" $
        exactBalances (ledger .+ entries) == Just (Map.singleton (Not :< (Yen, Sales))
            (-9007199254740993))

-- | T8 fixes once-rounded shares, zero results, and non-idempotent portions.
testPortions :: IO ()
testPortions = do
    let row :: Transfer.TransferRow TestBase
        row = (wildcard, Cash) Transfer..-> (wildcard, Deposits) Transfer.|% Transfer.Portion 1 3
        one :: Alg MoneyDecimal TestBase
        one = 1 .@ Not :< (Yen, Cash)
    table <- requireRight "T8 thirds" (Transfer.mkTransferTable [row])
    assertLaw "T8 decimal third is unroundable" $
        Transfer.transferEntries table one
            == Left (Transfer.UnroundableAmount row (Not :< (Yen, Cash)))
    rounded <- requireRight "T8 cents" $
        Transfer.transferEntriesWith (ToMultipleOf 0.01) table one
    assertLaw "T8 cents leave exactly 0.67" $
        exactBalances (one .+ rounded) == Just (Map.fromList
            [(Not :< (Yen, Cash), 67 / 100), (Not :< (Yen, Deposits), 33 / 100)])
    thirds <- requireRight "T8 representable third" $
        Transfer.transferEntries table (3 .@ Not :< (Yen, Cash) :: Alg MoneyDecimal TestBase)
    assertLaw "T8 three divided by three moves one" $
        exactBalances thirds == Just (Map.fromList
            [(Not :< (Yen, Cash), -1), (Not :< (Yen, Deposits), 1)])
    half <- requireRight "T8 half table" $ Transfer.mkTransferTable
        [(wildcard, Cash) Transfer..-> (wildcard, Deposits) Transfer.|% Transfer.Portion 1 2]
    first <- requireRight "T8 first half" (Transfer.transferEntries half one)
    second <- requireRight "T8 second half" (Transfer.transferEntries half (one .+ first))
    assertLaw "T8 portion is not idempotent" (second /= Zero)
    zero <- requireRight "T2 rounded zero" $
        Transfer.transferEntriesWith (ToMultipleOf 1) table one
    assertLaw "T2 rounded zero emits no pair" (zero == Zero)

-- | T8 checks exact half shares on both supported value instances.
propHalfPortion :: forall v. HatVal v => Proxy v -> Property
propHalfPortion _ = forAll (chooseInteger (1, 1000000)) $ \amount ->
    let row :: Transfer.TransferRow TestBase
        row = (wildcard, Cash) Transfer..-> (wildcard, Sales) Transfer.|% Transfer.Portion 1 2
        ledger :: Alg v TestBase
        ledger = fromInteger amount .@ Hat :< (Dollar, Cash)
        halfAmount :: Rational
        halfAmount = fromInteger amount / 2
    in case Transfer.mkTransferTable [row] of
        Left failure -> counterexample (show failure) False
        Right table -> case Transfer.transferEntries table ledger of
            Left failure -> counterexample (show failure) False
            Right entries -> counterexample "T8 exact remainder and side-preserving target" $
                exactBalances (ledger .+ entries) == Just (Map.fromList
                    [(Not :< (Dollar, Cash), negate halfAmount),
                     (Not :< (Dollar, Sales), halfAmount)])
                && length (postings entries) == 2

-- | Wildcards in source patterns match, while concrete queries miss ledger wildcards.
testWildcardMatching :: forall v. HatVal v => Proxy v -> IO ()
testWildcardMatching _ = do
    let ledger :: Alg v TestBase
        ledger = 7 .@ Not :< (wildcard, Cash)
        concreteRow :: Transfer.TransferRow TestBase
        concreteRow = (Yen, Cash) Transfer..-> (wildcard, Deposits) Transfer.|% Transfer.Relabel
    concrete <- requireRight "T9 concrete pattern" $ Transfer.mkTransferTable [concreteRow]
    assertLaw "T9 concrete source misses wildcard ledger coordinate" $
        Transfer.transferEntries concrete ledger == Right Zero
    broad <- requireRight "T9 wildcard pattern" $
        Transfer.mkTransferTable [moveRow (Cash, Deposits)]
    entries <- requireRight "T9 wildcard ledger selection" $ Transfer.transferEntries broad ledger
    assertLaw "T9 wildcard destination keeps literal ledger wildcard" $
        exactBalances (ledger .+ entries)
            == Just (Map.singleton (Not :< (wildcard, Deposits)) 7)

-- | T9 rejects backward edges, cycles, self edges, and wildcard backflow.
testOrder :: IO ()
testOrder = do
    let forward :: [Transfer.TransferRow TestBase]
        forward = map moveRow [(Cash, Deposits), (Deposits, Securities)]
        backward :: [Transfer.TransferRow TestBase]
        backward = reverse forward
        rejectsBackward :: [Transfer.TransferRow TestBase] -> Bool
        rejectsBackward rows = case Transfer.mkTransferTable rows of
            Left (Transfer.BackwardFlow _ _) -> True
            _ -> False
        wildcardBackflow :: [Transfer.TransferRow TestBase]
        wildcardBackflow =
            [ (Yen, Deposits) Transfer..-> (wildcard, Securities) Transfer.|% Transfer.Relabel
            , (wildcard, Cash) Transfer..-> (wildcard, Deposits) Transfer.|% Transfer.Relabel
            ]
    table <- requireRight "T9 forward" (Transfer.mkTransferTable forward)
    assertLaw "T9 table preserves written order" (Transfer.tableRows table == forward)
    assertLaw "T9 backward" (rejectsBackward backward)
    assertLaw "T9 self" (rejectsBackward [moveRow (Cash, Cash)])
    assertLaw "T9 cycle" (rejectsBackward (map moveRow [(Cash, Deposits), (Deposits, Cash)]))
    assertLaw "T9 wildcard backward regression" (rejectsBackward wildcardBackflow)
    wildcardForward <- requireRight "T9 wildcard forward regression" $
        Transfer.mkTransferTable (reverse wildcardBackflow)
    let wildcardLedger :: Alg Double TestBase
        wildcardLedger = 0.1 .@ Not :< (Yen, Cash)
            .+ 0.2 .@ Not :< (Yen, Cash)
            .+ 3 .@ Hat :< (Dollar, Cash)
            .+ 2 .@ Not :< (Yen, Deposits)
    wildcardEntries <- requireRight "T9 wildcard forward entries" $
        Transfer.transferEntries wildcardForward wildcardLedger
    assertLaw "T4 wildcard forward second result is exactly empty" $
        Transfer.transferEntries wildcardForward (wildcardLedger .+ wildcardEntries) == Right Zero
    entries <- requireRight "T9 chain entries" $ Transfer.transferEntries table
        (7 .@ Not :< (Yen, Cash) :: Alg MoneyDecimal TestBase)
    assertLaw "T9 forward chain reaches final account" $
        exactBalances (7 .@ Not :< (Yen, Cash) .+ entries)
            == Just (Map.singleton (Not :< (Yen, Securities)) 7)

-- | T9 fixes validation priority and the written j-then-i candidate order.
testTableFailureOrder :: IO ()
testTableFailureOrder = do
    let invalidFirst :: Transfer.TransferRow TestBase
        invalidFirst = (Yen, Cash) Transfer..-> (Yen, Cash) Transfer.|% Transfer.Portion 0 1
        invalidLater :: Transfer.TransferRow TestBase
        invalidLater = (Yen, Cash) Transfer..-> (Yen, Cash) Transfer.|% Transfer.Portion 2 1
        overlapFirst :: Transfer.TransferRow TestBase
        overlapFirst = (Yen, Cash) Transfer..-> (Dollar, Cash) Transfer.|% Transfer.Relabel
        overlapSecond :: Transfer.TransferRow TestBase
        overlapSecond = (Dollar, Cash) Transfer..-> (Yen, Cash) Transfer.|% Transfer.Relabel
        overlapThird :: Transfer.TransferRow TestBase
        overlapThird = (wildcard, Cash) Transfer..-> (wildcard, Sales) Transfer.|% Transfer.Relabel
        overlapFourth :: Transfer.TransferRow TestBase
        overlapFourth = (wildcard, Cash) Transfer..-> (wildcard, Securities)
            Transfer.|% Transfer.Relabel
        backwardFirst :: Transfer.TransferRow TestBase
        backwardFirst = (Yen, Cash) Transfer..-> (Yen, Deposits) Transfer.|% Transfer.Relabel
        backwardSecond :: Transfer.TransferRow TestBase
        backwardSecond = (Dollar, Cash) Transfer..-> (Dollar, Deposits) Transfer.|% Transfer.Relabel
        backwardThird :: Transfer.TransferRow TestBase
        backwardThird = (wildcard, Deposits) Transfer..-> (wildcard, Cash)
            Transfer.|% Transfer.Relabel
        backwardFourth :: Transfer.TransferRow TestBase
        backwardFourth = (Yen, Securities) Transfer..-> (Yen, Cash) Transfer.|% Transfer.Relabel
    assertLaw "T9 invalid portion precedes overlaps and backflow, first invalid row wins" $
        Transfer.mkTransferTable [overlapFirst, overlapSecond, invalidFirst, invalidLater]
            == Left (Transfer.InvalidPortion invalidFirst)
    assertLaw "T9 overlap precedes backflow, first j then first i wins" $
        Transfer.mkTransferTable [overlapFirst, overlapSecond, overlapThird, overlapFourth]
            == Left (Transfer.OverlappingRows overlapFirst overlapThird)
    assertLaw "T9 backflow first j then first i wins among multiple candidates" $
        Transfer.mkTransferTable [backwardFirst, backwardSecond, backwardThird, backwardFourth]
            == Left (Transfer.BackwardFlow backwardFirst backwardThird)

-- | T10 supplies several simultaneous failures and shuffles their raw postings.
propFailureOrder :: Property
propFailureOrder = forAll (shuffle fixtures) $ \permuted ->
    case Transfer.mkTransferTable [row] of
        Left failure -> counterexample (show failure) False
        Right table -> conjoin
            [ counterexample "T10 nonfinite precedes negative on the same base" $
                apply table permuted
                    == Left (Transfer.InvalidPosting invalidBase NonFiniteValue)
            , counterexample "T10 unclassified precedes unroundable" $
                apply table (filter isValidSideValue permuted)
                    == Left (Transfer.UnclassifiedAccount unclassifiedBase)
            , counterexample "T10 final failure is unroundable" $
                apply table (filter isClassifiedValid permuted)
                    == Left (Transfer.UnroundableAmount row validBase)
            ]
  where
    row :: Transfer.TransferRow TestBase
    row = (Yen, wildcard) Transfer..-> (Dollar, Deposits) Transfer.|% Transfer.Portion 1 3
    invalidBase :: TestBase
    invalidBase = Not :< (Yen, Cash)
    unclassifiedBase :: TestBase
    unclassifiedBase = Not :< (Yen, wildcard)
    validBase :: TestBase
    validBase = Not :< (Yen, Products)
    fixtures :: [Posting Double]
    fixtures =
        [ (-2, invalidBase)
        , (0 / 0, invalidBase)
        , (1 / 0, Not :< (Yen, Sales))
        , (1, unclassifiedBase)
        , (1, validBase)
        ]
    isValidSideValue :: Posting Double -> Bool
    isValidSideValue (value, _) = not (isErrorValue value)
    isClassifiedValid :: Posting Double -> Bool
    isClassifiedValid posting@(_, base) = isValidSideValue posting
        && getAccountTitle base /= wildcard
    apply :: Transfer.TransferTable TestBase
          -> [Posting Double]
          -> Either (Transfer.TransferError TestBase) (Alg Double TestBase)
    apply table entries = Transfer.transferEntriesWith (ToMultipleOf 0.01) table
        (Algebra.fromList [value `Internal.Single` base | (value, base) <- entries])

-- | T10 rejects raw singleton wildcard sides before checking their scalar.
testWildcardFailure :: IO ()
testWildcardFailure = do
    table <- requireRight "T10 wildcard table" $
        Transfer.mkTransferTable [moveRow (Cash, Deposits)]
    let base :: TestBase
        base = HatNot :< (Yen, Cash)
    forM_ [1, -1, 0 / 0] $ \value ->
        assertLaw "T10 wildcard precedes invalid scalar on the same posting" $
            Transfer.transferEntries table (Internal.Single value base :: Alg Double TestBase)
                == Left (Transfer.WildcardPosting (Not :< (Yen, Cash)))
    assertLaw "T10 zero wildcard contributes no failure" $
        Transfer.transferEntries table (Internal.Single 0 base :: Alg Double TestBase) == Right Zero

-- | A stable target groups notes 1 and 2, while note 4 forms a second group.
groupNote :: Int -> Int
groupNote note
    | note <= 3 = 3
    | otherwise = 5

-- | J1 combines target groups before rounding and preserves other target groups.
testJournalGroups :: forall v. HatVal v => Proxy v -> IO ()
testJournalGroups _ = do
    table <- requireRight "J1 table" $ Transfer.mkTransferTable
        [(wildcard, Cash) Transfer..-> (wildcard, Deposits) Transfer.|% Transfer.Portion 1 3]
    let ledger :: Journal.Journal Int v TestBase
        ledger = (1 .@ Not :< (Yen, Cash) .| 1)
            .+ (2 .@ Not :< (Yen, Cash) .| 2)
            .+ (6 .@ Not :< (Dollar, Cash) .| 4)
        entriesAt :: Int -> Journal.Journal Int v TestBase -> Alg v TestBase
        entriesAt note = Journal.toAlg . Journal.filterWithNote (\actual _ -> actual == note)
    result <- requireRight "J1 grouped exact thirds" $
        Transfer.transferEntriesBy groupNote table ledger
    forM_ [3, 5] $ \target -> do
        expected <- requireRight "J1 independent group" $ Transfer.transferEntries table
            (Journal.toAlg (Journal.filterWithNote (\note _ -> groupNote note == target) ledger))
        assertLaw "J1 target entries equal independently grouped transfer" $
            sort (postings (entriesAt target result)) == sort (postings expected)
    let expectedThree :: Alg v TestBase
        expectedThree = 1 .@ Hat :< (Yen, Cash) .+ 1 .@ Not :< (Yen, Deposits)
        expectedFive :: Alg v TestBase
        expectedFive = 2 .@ Hat :< (Dollar, Cash) .+ 2 .@ Not :< (Dollar, Deposits)
    assertLaw "J1 target three has the manually specified one-unit pair" $
        sort (postings (entriesAt 3 result)) == sort (postings expectedThree)
    assertLaw "J1 target five has the manually specified two-unit pair" $
        sort (postings (entriesAt 5 result)) == sort (postings expectedFive)
    assertLaw "J1 original notes absent from added entries" $
        all (Algebra.isZero . (`entriesAt` result)) [1, 2, 4]

-- | J2 rejects moving input targets before evaluating invalid posting values.
testJournalMoves :: forall v. HatVal v => Proxy v -> IO ()
testJournalMoves _ = do
    table <- requireRight "J2 table" $ Transfer.mkTransferTable [moveRow (Cash, Deposits)]
    let ledger :: Journal.Journal Int v TestBase
        ledger = (1 .@ Not :< (Yen, Cash) .| 1)
            .+ (Internal.Single (-1) (Not :< (Yen, Cash)) .| 2)
        next :: Int -> Int
        next = (+ 1)
    assertLaw "J2 note movement checked before invalid values" $
        case Transfer.transferEntriesBy next table ledger of
            Left (Transfer.TransferNoteMoves 2 3) -> True
            _ -> False
    accepted <- requireRight "J2 absent next-period target" $ Transfer.transferEntriesBy next table
        (1 .@ Not :< (Yen, Cash) .| 1 :: Journal.Journal Int v TestBase)
    assertLaw "J2 absent target accepted" $
        exactBalances (Journal.toAlg accepted) == Just (Map.fromList
            [(Not :< (Yen, Cash), -1), (Not :< (Yen, Deposits), 1)])

-- | J3 repeats a Relabel transfer after its target notes have been added.
propJournalIdempotence :: forall v. HatVal v => Proxy v -> Property
propJournalIdempotence _ = forAll (genLedger :: Gen (Alg v TestBase)) $ \ledger ->
    case Transfer.mkTransferTable (map moveRow chainMoves) of
        Left failure -> counterexample (show failure) False
        Right table ->
            let journal :: Journal.Journal Int v TestBase
                journal = ledger .| 1
                stableTarget :: Int -> Int
                stableTarget _ = 3
            in case Transfer.transferEntriesBy stableTarget table journal of
                Left failure -> counterexample (show failure) False
                Right result -> counterexample "J3 second result is exactly empty" $
                    case Transfer.transferEntriesBy stableTarget table (journal .+ result) of
                        Right repeated -> HashMap.null (Journal.toMap repeated)
                        Left _ -> False

-- | Register the gate, both value instances, and all balance and Journal laws.
runTests :: IO ()
runTests = do
    testClosingHomeSides
    checkLaw "T1-T5 Double" (propRelabel (Proxy :: Proxy Double))
    checkLaw "T1-T5 MoneyDecimal" (propRelabel (Proxy :: Proxy MoneyDecimal))
    testAllAccountPairs (Proxy :: Proxy Double)
    testAllAccountPairs (Proxy :: Proxy MoneyDecimal)
    testClosingMultiset (Proxy :: Proxy Double)
    testClosingMultiset (Proxy :: Proxy MoneyDecimal)
    testDoubleSplit
    testPortions
    checkLaw "T8 Double half share" (propHalfPortion (Proxy :: Proxy Double))
    checkLaw "T8 MoneyDecimal half share" (propHalfPortion (Proxy :: Proxy MoneyDecimal))
    testWildcardMatching (Proxy :: Proxy Double)
    testWildcardMatching (Proxy :: Proxy MoneyDecimal)
    testOrder
    testTableFailureOrder
    checkLaw "T10 deterministic failure precedence" propFailureOrder
    testWildcardFailure
    testJournalGroups (Proxy :: Proxy Double)
    testJournalGroups (Proxy :: Proxy MoneyDecimal)
    testJournalMoves (Proxy :: Proxy Double)
    testJournalMoves (Proxy :: Proxy MoneyDecimal)
    checkLaw "J3 Double" (propJournalIdempotence (Proxy :: Proxy Double))
    checkLaw "J3 MoneyDecimal" (propJournalIdempotence (Proxy :: Proxy MoneyDecimal))
    putStrLn "[PASS] T1-T10 and J1-J3 exact transfer laws"
