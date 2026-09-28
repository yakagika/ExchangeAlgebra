-- | Acceptance properties for exact pre-cancellation journal side totals.
module Journal.SideTotalsSpec (runTests) where

import Control.Monad (unless)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64)
import System.Exit (exitFailure)
import Test.QuickCheck

import ExchangeAlgebra.Algebra (Alg((:@)), (.@))
import ExchangeAlgebra.Algebra.Base
    ( AccountTitles(..), CountUnit(..), Hat(..), HatBase(..) )
import ExchangeAlgebra.Algebra.Exact (ExactSum(..))
import ExchangeAlgebra.Journal (Journal, (.|))
import qualified ExchangeAlgebra.Journal as Journal
import ExchangeAlgebra.Journal.Transfer.Rule
    ( CarryError(..), SideTotals(..), sideTotal, sideTotalsBy )
import ExchangeAlgebra.Algebra.Posting (PostSide(..))

type Part = (CountUnit, AccountTitles)
type Row = (Int, Part, Hat, Double)
type TestJournal = Journal Int Double (HatBase Part)

-- | Keep the original scalar postings apart from the invalid raw-entry tests.
build :: [Row] -> TestJournal
build = foldMap (\(note, coordinates, side, value) ->
    (value .@ (side :< coordinates)) .| note)

buildLeft :: [Row] -> TestJournal
buildLeft = List.foldl' (\journal (note, coordinates, side, value) ->
    journal <> ((value .@ (side :< coordinates)) .| note)) mempty

-- | Exercise integer, decimal, ulp, subnormal, and widely separated exponents.
valueGen :: Gen Double
valueGen = elements
    [ 0, 1, 0.01, 0.1, 0.2, 0.3
    , 2 ^ (53 :: Int), 2 ^ (53 :: Int) + 2
    , encodeFloat 1 (-1074), encodeFloat 1 500, encodeFloat 1 900
    ]

rowsGen :: Gen [Row]
rowsGen = do
    count <- chooseInt (0, 16)
    vectorOf count $ do
        note <- chooseInt (0, 5)
        unit <- elements [Yen, Dollar]
        account <- elements [Cash, Deposits]
        side <- elements [Not, Hat]
        value <- valueGen
        pure (note, (unit, account), side, value)

-- | Compare the binary64 representations, including the sign of zero.
bits :: Double -> Word64
bits = castDoubleToWord64

sameBits :: SideTotals -> SideTotals -> Bool
sameBits left right = bits (notTotal left) == bits (notTotal right)
    && bits (hatTotal left) == bits (hatTotal right)

-- | A Rational oracle rounds each independent key and side once.
oracle :: [Row] -> Either CarryError (Map.Map Part SideTotals)
oracle original = traverse finish grouped
  where
    grouped = Map.fromListWith (++)
        [ (coordinates, [(side, toRational value)])
        | (_, coordinates, side, value) <- original, value /= 0 ]
    finish entries = do
        notValue <- rounded [amount | (Not, amount) <- entries]
        hatValue <- rounded [amount | (Hat, amount) <- entries]
        pure (SideTotals notValue hatValue)
    rounded amounts =
        let value = fromRational (sum amounts) :: Double
        in if isInfinite value then Left ResultOutOfRange
           else Right (if value == 0 then 0 else value)

-- | The output equals the Rational oracle for each generated journal.
propOracle :: Property
propOracle = forAll rowsGen $ \original ->
    case (sideTotalsBy id (build original), oracle original) of
        (Right actual, Right expected) ->
            property (Map.keys actual == Map.keys expected
                && and (Map.elems (Map.intersectionWith sameBits actual expected)))
        (Left actual, Left expected) -> actual === expected
        (actual, expected) -> counterexample
            ("actual " ++ show actual ++ ", expected " ++ show expected) False

-- | The same entries keep their bits when reordered or assigned new notes.
propRearrangement :: Property
propRearrangement = forAll rowsGen $ \original ->
    let renoted = zipWith (\note (_, coordinates, side, value) ->
            (note, coordinates, side, value)) (cycle [8, 9, 10]) (reverse original)
        first = sideTotalsBy id (build original)
        second = sideTotalsBy id (buildLeft renoted)
    in case (first, second) of
        (Right left, Right right) -> property (Map.keys left == Map.keys right
            && and (Map.elems (Map.intersectionWith sameBits left right)))
        (Left left, Left right) -> left === right
        _ -> counterexample "rearrangement changed success or failure" False

-- | Where ExactSum succeeds, its rounded bits match each independent side.
propExactParity :: Property
propExactParity = forAll rowsGen $ \original ->
    let checked = sideTotalsBy id (build original)
        grouped = Map.fromListWith (++)
            [ ((coordinates, side), [value])
            | (_, coordinates, side, value) <- original, value /= 0 ]
        compareSide ((coordinates, side), values) =
            let exact = roundAccum (foldr addAccum emptyAccum values)
                observed = do
                    totals <- checked
                    totalsForKey <- maybe (Left ResultOutOfRange) Right
                        (Map.lookup coordinates totals)
                    pure (sideTotal (if side == Hat then HatSide else NotSide)
                        totalsForKey)
            in case exact of
                Right expected -> fmap bits observed == Right (bits expected)
                Left _ -> True
    in counterexample (show checked) (property (all compareSide (Map.toList grouped)))

-- | Build one raw scalar whose invalid value bypasses the smart constructor.
raw :: Int -> Hat -> Double -> TestJournal
raw note side value = Journal.fromMap $ HashMap.singleton note
    (value :@ (side :< (Yen, Cash)))

propBoundaries :: Property
propBoundaries =
    let maximumFinite = encodeFloat (2 ^ (53 :: Int) - 1) 971 :: Double
        leastSubnormal = encodeFloat 1 (-1074) :: Double
        success = sideTotalsBy id (build
            [(0, (Yen, Cash), Not, maximumFinite),
             (1, (Yen, Cash), Not, leastSubnormal)])
        failure = sideTotalsBy id (build
            [(0, (Yen, Cash), Not, maximumFinite),
             (1, (Yen, Cash), Not, maximumFinite)])
        mixed = sideTotalsBy id (build
            [(0, (Yen, Cash), Not, maximumFinite),
             (1, (Yen, Cash), Not, leastSubnormal),
             (2, (Yen, Cash), Hat, 7),
             (3, (Yen, Deposits), Not, 3)])
    in conjoin
        [ fmap (fmap (bits . notTotal) . Map.lookup (Yen, Cash)) success
            === Right (Just (bits maximumFinite))
        , failure === Left ResultOutOfRange
        , fmap (fmap (\totals -> (bits (notTotal totals), bits (hatTotal totals)))
            . Map.lookup (Yen, Cash)) mixed
            === Right (Just (bits maximumFinite, bits 7))
        , fmap (Map.lookup (Yen, Deposits)) mixed
            === Right (Just (SideTotals 3 0))
        ]

propCases :: Property
propCases =
    let both = sideTotalsBy id (build
            [(0, (Yen, Cash), Not, 2), (1, (Yen, Cash), Hat, 3),
             (2, (Yen, Deposits), Not, 5)])
        merged = sideTotalsBy (const ()) (build
            [(0, (Yen, Cash), Not, 2), (1, (Dollar, Deposits), Not, 3)])
        only = sideTotalsBy id (build [(0, (Yen, Cash), Hat, 2)])
    in conjoin
        [ sideTotalsBy id (mempty :: TestJournal) === Right Map.empty
        , both === Right (Map.fromList
            [((Yen, Cash), SideTotals 2 3), ((Yen, Deposits), SideTotals 5 0)])
        , merged === Right (Map.singleton () (SideTotals 5 0))
        , only === Right (Map.singleton (Yen, Cash) (SideTotals 0 2))
        , fmap (\totals -> (bits (sideTotal NotSide totals),
                            bits (sideTotal HatSide totals)))
            (Map.lookup (Yen, Cash) =<< either (const Nothing) Just only)
            === Just (bits 0, bits 2)
        ]

-- | Input errors outrank overflow and do not depend on note traversal.
propFailures :: Property
propFailures = forAll (shuffle invalid) $ \permutation ->
    let assemble :: [(Int, Hat, Double)] -> TestJournal
        assemble entries = Journal.fromMap (HashMap.fromList
            [ (note, value :@ (side :< (Yen, Cash)))
            | (note, side, value) <- entries ])
        reassigned = zipWith (\note (_, side, value) -> (note, side, value))
            [11, 12, 13, 14] permutation
    in conjoin
        [ sideTotalsBy id (assemble invalid) === Left WildcardSide
        , sideTotalsBy id (assemble reassigned) === Left WildcardSide
        , sideTotalsBy id (raw 0 HatNot 0) === Left WildcardSide
        , sideTotalsBy id (raw 0 HatNot 1) === Left WildcardSide
        , sideTotalsBy id (raw 0 Not (0 / 0)) === Left NonFiniteValue
        , sideTotalsBy id (raw 0 Hat (1 / 0)) === Left NonFiniteValue
        , sideTotalsBy id (raw 0 Hat ((-1) / 0)) === Left NonFiniteValue
        , sideTotalsBy id (raw 0 Not (-1)) === Left NegativeValue
        , sideTotalsBy id (assemble
            [(1, Not, 0 / 0), (2, Hat, 1 / 0), (3, Not, -1)])
            === Left NonFiniteValue
        , sideTotalsBy id (assemble
            [(1, Not, maximumFinite), (2, Not, maximumFinite),
             (3, Hat, -1)]) === Left NegativeValue
        ]
  where
    maximumFinite = encodeFloat (2 ^ (53 :: Int) - 1) 971 :: Double
    invalid :: [(Int, Hat, Double)]
    invalid =
        [ (0, HatNot, 0), (1, Not, 0 / 0), (2, Hat, 1 / 0),
          (3, Not, -1) ]

failTest :: String -> IO ()
failTest message = putStrLn ("[FAIL] journal side totals: " ++ message) >> exitFailure

quickProperty :: Testable property => Int -> String -> property -> IO ()
quickProperty count label proposition = do
    result <- quickCheckWithResult stdArgs { maxSuccess = count, chatty = False } proposition
    unless (isSuccess result) $ failTest (label ++ ": " ++ output result)

-- | Run the side-total properties in the package test suite.
runTests :: IO ()
runTests = do
    quickProperty 500 "Rational oracle" propOracle
    quickProperty 500 "reordering and renoting" propRearrangement
    quickProperty 500 "ExactSum parity" propExactParity
    quickProperty 1 "range boundaries" propBoundaries
    quickProperty 1 "keys and side selection" propCases
    quickProperty 100 "input failures" propFailures
    putStrLn "[PASS] journal side totals"
