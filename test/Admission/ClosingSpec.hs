{-# LANGUAGE OverloadedStrings #-}

-- | Exact closing observations and input diagnostics.
module Admission.ClosingSpec (runTests) where

import Control.Monad (forM_, unless)
import Data.Decimal (DecimalRaw(Decimal))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import System.Exit (exitFailure)
import Test.QuickCheck

import ExchangeAlgebra.Algebra (Alg(..), Hat(..), HatBase((:<)), (.+), (.@))
import ExchangeAlgebra.Algebra.Base (ExBaseClass(whichSide))
import ExchangeAlgebra.Accounting.Account
    ( AccountDivision(..), AccountSpec(..), AccountTitles(..), Side(..)
    , accountSpec, concreteAccountTitles )
import ExchangeAlgebra.IO.Input.Admission (EntityId(..), PeriodId(..), TxId(..), TxKey(..), Entry)
import ExchangeAlgebra.Accounting.Equivalence
    ( ClosingDifference(..), ClosingSource(..), closingDifferences, isClosingEquivalent )
import ExchangeAlgebra.Algebra.Value (MoneyDecimal(..))

-- | Report an exact assertion failure through the suite convention.
assertTest :: String -> Bool -> IO ()
assertTest label success = unless success $ do
    putStrLn ("[FAIL] closing: " ++ label)
    exitFailure

-- | Construct distinct keys in one entity and period.
key :: String -> TxKey
key identity = TxKey (EntityId "A") (PeriodId "2026") (TxId (Text.pack identity))

-- | Construct a posting after the fixture has established a valid amount.
posting :: MoneyDecimal -> Hat -> AccountTitles -> Entry
posting amount label title = amount .@ (label :< title)

-- | Preserve separate postings when building a test entry.
entry :: [(MoneyDecimal, Hat, AccountTitles)] -> Entry
entry = foldr (.+) Zero . map (\(amount, label, title) -> posting amount label title)

-- | Put one entry at one transaction key.
at :: String -> Entry -> Map TxKey Entry
at identity = Map.singleton (key identity)

-- | Compare fixed examples, including exact output order.
testExamples :: IO ()
testExamples = do
    let cash = entry [(10, Not, Cash), (10, Hat, Sales)]
        split = entry [(4, Not, Cash), (6, Not, Cash), (10, Hat, Sales)]
        gross = entry [(12, Not, RetainedEarnings), (7, Hat, RetainedEarnings)]
        net = entry [(5, Not, RetainedEarnings)]
        debitHeavy = entry [(12, Hat, RetainedEarnings), (7, Not, RetainedEarnings)]
        sameSides = entry [(5, Not, RetainedEarnings), (5, Hat, RetainedEarnings)]
        tiny = MoneyDecimal (Decimal 255 1)
    assertTest "same journal" $ isClosingEquivalent RetainedEarnings (at "a" cash) (at "a" cash)
    assertTest "split rows" $ isClosingEquivalent RetainedEarnings (at "a" cash) (at "a" split)
    assertTest "retained gross and net" $
        isClosingEquivalent RetainedEarnings (at "a" gross) (at "a" net)
    assertTest "retained debit excess" $
        closingDifferences RetainedEarnings (at "a" debitHeavy) (at "a" Zero)
            == [RetainedEarningsDifference (key "a") (Debit, 5) (Credit, 0)]
    assertTest "retained zero equals absent" $
        isClosingEquivalent RetainedEarnings (at "a" sameSides) (at "a" Zero)
    assertTest "tiny nonzero retained difference" $
        closingDifferences RetainedEarnings
            (at "a" (posting tiny Not RetainedEarnings)) (at "a" Zero)
            == [RetainedEarningsDifference (key "a") (Credit, tiny) (Credit, 0)]
    assertTest "retained credit difference" $
        closingDifferences RetainedEarnings
            (at "a" (posting 7 Not RetainedEarnings))
            (at "a" (posting 6 Not RetainedEarnings))
            == [RetainedEarningsDifference (key "a") (Credit, 7) (Credit, 6)]
    assertTest "other account total difference" $
        closingDifferences RetainedEarnings (at "a" (posting 3 Not Cash)) (at "a" Zero)
            == [SideTotalDifference (key "a") Cash Debit 3 0]
    assertTest "equal opposite sides do not cancel" $
        closingDifferences RetainedEarnings
            (at "a" (entry [(2, Not, Cash), (2, Hat, Cash)])) (at "a" Zero)
            == [ SideTotalDifference (key "a") Cash Credit 2 0
               , SideTotalDifference (key "a") Cash Debit 2 0 ]
    assertTest "empty maps" $
        closingDifferences RetainedEarnings Map.empty Map.empty == []
    assertTest "empty entry differs from absent key" $
        closingDifferences RetainedEarnings (at "a" Zero) Map.empty
            == [TransactionOnlyInCandidate (key "a")]
    assertTest "reference-only key" $
        closingDifferences RetainedEarnings Map.empty (at "a" Zero)
            == [TransactionOnlyInReference (key "a")]
    assertTest "contra allowance sides" $
        closingDifferences RetainedEarnings
            (at "a" (entry [(1, Not, AllowanceForDoubtfulAccounts)
                            , (1, Hat, AllowanceForDoubtfulAccounts)]))
            (at "a" Zero)
            == [ SideTotalDifference (key "a") AllowanceForDoubtfulAccounts Credit 1 0
               , SideTotalDifference (key "a") AllowanceForDoubtfulAccounts Debit 1 0 ]
    assertTest "contra depreciation sides" $
        closingDifferences RetainedEarnings
            (at "a" (entry [(1, Not, AccumulatedDepreciation)
                            , (1, Hat, AccumulatedDepreciation)]))
            (at "a" Zero)
            == [ SideTotalDifference (key "a") AccumulatedDepreciation Credit 1 0
               , SideTotalDifference (key "a") AccumulatedDepreciation Debit 1 0 ]

-- | Pin key, account, side, and retained-net ordering in one result list.
testOrdering :: IO ()
testOrdering = do
    let candidate = Map.fromList
            [ (key "a", Zero)
            , (key "c", entry [(1, Not, Sales), (2, Hat, Sales)
                              , (3, Not, Cash), (4, Hat, Cash)
                              , (5, Not, RetainedEarnings)])
            , (key "d", Zero) ]
        reference = Map.fromList [(key "b", Zero), (key "c", Zero), (key "d", Zero)]
    assertTest "complete ordered differences" $
        closingDifferences RetainedEarnings candidate reference ==
            [ TransactionOnlyInCandidate (key "a")
            , TransactionOnlyInReference (key "b")
            , SideTotalDifference (key "c") Cash Credit 4 0
            , SideTotalDifference (key "c") Cash Debit 3 0
            , SideTotalDifference (key "c") Sales Credit 1 0
            , SideTotalDifference (key "c") Sales Debit 2 0
            , RetainedEarningsDifference (key "c") (Credit, 5) (Credit, 0) ]

-- | Check all diagnostics before accounting, with duplicate elimination.
testDiagnostics :: IO ()
testDiagnostics = do
    let blank = TxKey (EntityId " ") (PeriodId "2026") (TxId "x")
        allKinds = (-1) :@ (HatNot :< AccountTitle)
        repeated = ((-1) :@ (Not :< AccountTitle))
            .+ ((-2) :@ (Not :< AccountTitle))
        candidate = Map.fromList
            [(blank, allKinds), (key "y", 1 :@ (HatNot :< Sales)), (key "z", repeated)]
        reference = Map.fromList [(blank, allKinds), (key "y", Zero), (key "z", Zero)]
    assertTest "diagnostics ordered, deduplicated, and non-comparing" $
        closingDifferences RetainedEarnings candidate reference ==
            [ BlankTransactionKey Candidate blank
            , WildcardPosting Candidate blank AccountTitle
            , UnclassifiedAccount Candidate blank AccountTitle
            , NegativeAmount Candidate blank AccountTitle
            , BlankTransactionKey Reference blank
            , WildcardPosting Reference blank AccountTitle
            , UnclassifiedAccount Reference blank AccountTitle
            , NegativeAmount Reference blank AccountTitle
            , WildcardPosting Candidate (key "y") Sales
            , UnclassifiedAccount Candidate (key "z") AccountTitle
            , NegativeAmount Candidate (key "z") AccountTitle ]
    assertTest "invalid one-sided key is inspected" $
        closingDifferences RetainedEarnings (at "z" repeated) Map.empty ==
            [ UnclassifiedAccount Candidate (key "z") AccountTitle
            , NegativeAmount Candidate (key "z") AccountTitle ]
    assertTest "unclassified retained argument is invalid when posted" $
        closingDifferences AccountTitle (at "a" (1 :@ (Not :< AccountTitle)))
            (at "a" Zero) == [UnclassifiedAccount Candidate (key "a") AccountTitle]

-- | Pin classification for every concrete title against registry divisions
-- and contra flags; Hat reverses the resulting accounting side.
testClassifications :: IO ()
testClassifications = forM_ concreteAccountTitles $ \title -> case accountSpec title of
    Nothing -> assertTest ("missing classification: " ++ show title) False
    Just spec -> do
        let divisionSide = case asDivision spec of
                Assets    -> Debit
                Cost      -> Debit
                Equity    -> Credit
                Liability -> Credit
                Revenue   -> Credit
            homeSide = if asIsContra spec then opposite divisionSide else divisionSide
        assertTest ("Not side: " ++ show title) $
            whichSide (Not :< title) == homeSide
        assertTest ("Hat side: " ++ show title) $
            whichSide (Hat :< title) == opposite homeSide
  where
    opposite Credit = Debit
    opposite Debit = Credit
    opposite Side = Side

-- | Generate valid small entries without using decimal division.
genEntry :: Gen Entry
genEntry = do
    rows <- listOf $ do
        amount <- choose (0, 1000 :: Integer)
        label <- elements [Not, Hat]
        title <- elements [Cash, Sales, RetainedEarnings, AllowanceForDoubtfulAccounts]
        pure (fromInteger amount, label, title)
    pure (entry rows)

-- | Reflexivity holds for valid finite maps.
propReflexive :: Property
propReflexive = forAll genEntry $ \value ->
    closingDifferences RetainedEarnings (at "a" value) (at "a" value) == []

-- | Splitting integer coefficients at the same decimal place preserves sums.
propSplit :: Property
propSplit = forAll (choose (0, 255 :: Int)) $ \places ->
    forAll (choose (0, 1000 :: Integer)) $ \first ->
    forAll (choose (0, 1000 :: Integer)) $ \second ->
        let amount coefficient = MoneyDecimal (Decimal (fromIntegral places) coefficient)
            whole = posting (amount (first + second)) Not Cash
            pieces = entry [(amount first, Not, Cash), (amount second, Not, Cash)]
        in isClosingEquivalent RetainedEarnings (at "a" whole) (at "a" pieces)

-- | Posting order leaves exact sums unchanged.
propPermutation :: Property
propPermutation = forAll (listOf $ choose (0, 100 :: Integer)) $ \amounts ->
    let forward = entry [(fromInteger amount, Not, Cash) | amount <- amounts]
        backward = entry [(fromInteger amount, Not, Cash) | amount <- reverse amounts]
    in isClosingEquivalent RetainedEarnings (at "a" forward) (at "a" backward)

-- | Reversing inputs reverses each reported amount pair and presence side.
propSwap :: Property
propSwap = forAll genEntry $ \left -> forAll genEntry $ \right ->
    let candidate = Map.fromList [(key "a", left), (key "c", right)]
        reference = Map.fromList [(key "b", right), (key "c", left)]
    in closingDifferences RetainedEarnings reference candidate
        == map swapDifference (closingDifferences RetainedEarnings candidate reference)
  where
    swapDifference difference = case difference of
        TransactionOnlyInCandidate transaction -> TransactionOnlyInReference transaction
        TransactionOnlyInReference transaction -> TransactionOnlyInCandidate transaction
        SideTotalDifference transaction title side left right ->
            SideTotalDifference transaction title side right left
        RetainedEarningsDifference transaction left right ->
            RetainedEarningsDifference transaction right left
        other -> other

-- | Run fixed cases and bounded exact-arithmetic properties.
runTests :: IO ()
runTests = do
    testExamples
    testOrdering
    testDiagnostics
    testClassifications
    check "reflexivity" propReflexive
    check "split rows" propSplit
    check "posting permutation" propPermutation
    check "candidate/reference swap" propSwap
    putStrLn "[PASS] closing comparison"
  where
    check label proposition = do
        result <- quickCheckWithResult stdArgs { maxSuccess = 100, chatty = False } proposition
        assertTest label (isSuccess result)
