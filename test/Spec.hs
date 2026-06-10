{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Algebra.Transfer as EAT
import qualified ExchangeAlgebra.Journal  as EJ
import qualified ExchangeAlgebra.Journal.Transfer as EJT
import           ExchangeAlgebra.Value    (MoneyDecimal, bankersRound)
import qualified ExchangeAlgebra.Simulate as ES
import           ExchangeAlgebra.Simulate
import qualified ExchangeAlgebra.Simulate.Lite as Lite
import           ExchangeAlgebra.Simulate.Network
                     ( TradeNetwork, InputCoefficients, NetworkError(..)
                     , tradeNetwork, inputCoefficients
                     , nodes, edges, suppliersOf, buyersOf, edgeCount
                     , coefficient, inputsOf, sigmaEdges
                     , completeNetwork, kRegular, erdosRenyi, scaleFree, sectorBlock
                     , CoefOptions(..), defaultCoefOptions, randomCoefficients
                     , networkFromTable, coefficientsFromTable, fromCoefficientMatrix
                     , parseEdgeCsv, parseCoefCsv )
import           ExchangeAlgebra.Simulate.Lite
                     ( InitT, RefT, SnapT, HK
                     , Field(..), carry, resetEach, updateEach
                     , Stage, stage, stageFor
                     , Par(..), SimSpec, mkSimSpec, runLite, runLiteWithPolicy )
import qualified ExchangeAlgebra.Simulate.Policy as Policy
import           ExchangeAlgebra.Value    (MoneyDouble)
import qualified ExchangeAlgebra.Write    as EW
import           ExchangeAlgebra.Write

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
import qualified Data.List           as L
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Control.Monad       (forM_)
import           Control.Monad.ST
import           Data.Array.ST
import           Data.STRef
import           System.Exit         (exitFailure)
import           System.IO           (IOMode(WriteMode), withFile)
import           System.Directory    (removeFile)
import           System.Random       (StdGen, mkStdGen, randomR, split)
import           Control.Monad       (replicateM)
import           Control.Monad.State (runState, state)
import           Control.Exception   (try, evaluate, SomeException)
import           Test.QuickCheck
import           GHC.Generics        (Generic)
import           System.Random       (randomR)

-- ================================================================
-- Unit test helpers
-- ================================================================

eps :: Double
eps = 1e-9

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | expected == actual = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

assertNear :: String -> Double -> Double -> IO ()
assertNear label expected actual
    | abs (expected - actual) <= eps = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

-- ================================================================
-- Existing pure algebra tests
-- ================================================================

type TestAlg = EA.Alg Double (HatBase CountUnit)
type TestJournal = EJ.Journal String Double (HatBase CountUnit)
type AxisJournal = EJ.Journal (String, Int) Double (HatBase CountUnit)

algSample :: TestAlg
algSample =
       (1 :@ (Hat    :< Yen))
    .+ (1 :@ (Not    :< Amount))
    .+ (2 :@ (Not    :< Yen))
    .+ (2 :@ (Hat    :< Amount))
    .+ (3 :@ (Hat    :< Yen))

journalSample :: TestJournal
journalSample = EJ.fromList [x, y, z]
  where
    x = ((1 :@ (Hat :< Yen)) .+ (1 :@ (Not :< Amount))) .| "cat"  :: TestJournal
    y = ((2 :@ (Not :< Yen)) .+ (2 :@ (Hat :< Amount))) .| "dog"  :: TestJournal
    z = ((3 :@ (Hat :< Yen)) .+ (3 :@ (Not :< Amount))) .| "fish" :: TestJournal

testProjMultiPatternOnePass :: IO ()
testProjMultiPatternOnePass = do
    let qs :: [HatBase CountUnit]
        qs = [Hat :< Yen, HatNot :< Amount, Hat :< Yen]
        expected = L.foldl' (\acc q -> acc .+ EA.proj [q] algSample) EA.Zero qs
        actual = EA.proj qs algSample
    assertEqual "Alg.proj multi-pattern preserves behavior" expected actual

testProjNormFastPath :: IO ()
testProjNormFastPath = do
    let qs :: [HatBase CountUnit]
        qs = [Hat :< Yen, HatNot :< Amount, Hat :< Yen]
        expected = norm $ (.-) $ EA.proj qs algSample
        actual = EA.projNorm qs algSample
    assertNear "Alg.projNorm fast path matches existing semantics" expected actual

testProjWithBaseNorm :: IO ()
testProjWithBaseNorm = do
    let bs :: [HatBase CountUnit]
        bs = [Not :< Amount]
        expected = norm $ EJ.projWithBase bs journalSample
        actual = EJ.projWithBaseNorm bs journalSample
    assertNear "Journal.projWithBaseNorm matches norm . projWithBase" expected actual

testProjWithNoteNorm :: IO ()
testProjWithNoteNorm = do
    let bs :: [HatBase CountUnit]
        bs = [HatNot :< Amount, Hat :< Yen]
        ns1 = ["dog", "cat"]
        ns2 = [plank]
        expected1 = norm $ EJ.projWithNoteBase ns1 bs journalSample
        actual1 = EJ.projWithNoteNorm ns1 bs journalSample
        expected2 = norm $ EJ.projWithNoteBase ns2 bs journalSample
        actual2 = EJ.projWithNoteNorm ns2 bs journalSample
    assertNear "Journal.projWithNoteNorm (selected notes)" expected1 actual1
    assertNear "Journal.projWithNoteNorm (plank wildcard)" expected2 actual2

-- | Regression test for the `bases` typo bug.
--
-- Before the fix at Algebra.hs:868, `bases` ignored the `_notSide` Seq and
-- iterated `_hatSide` twice (with `Hat` and `Not` labels). As a result,
-- `length (bases x) != length (vals x)` whenever Hat/Not Seq lengths differed.
--
-- This test constructs an Alg where the Hat Seq for `Yen` has length 1 and
-- the Not Seq has length 2, plus a separate basis whose Hat Seq is empty.
-- That makes the divergence detectable in both directions.
testBasesNotSideRegression :: IO ()
testBasesNotSideRegression = do
    let alg :: TestAlg
        alg =  (100 :@ (Hat :< Yen))      -- Yen: hatSide = [100]
            .+ (50  :@ (Not :< Yen))      -- Yen: notSide = [50]
            .+ (30  :@ (Not :< Yen))      -- Yen: notSide = [50, 30]
            .+ (20  :@ (Not :< Amount))   -- Amount: notSide = [20], hatSide = []
        vs = EA.vals alg
        bs = EA.bases alg
        hatCount = length (L.filter isHat bs)
        notCount = length (L.filter (not . isHat) bs)
    -- vals and bases must agree on total count (one label per scalar entry)
    assertEqual "bases/vals same length (regression for hs/ns typo)"
        (length vs) (length bs)
    -- Expected: 1 Hat label (Hat:<Yen) and 3 Not labels (50:<Yen, 30:<Yen, 20:<Amount)
    assertEqual "bases Hat label count" 1 hatCount
    assertEqual "bases Not label count" 3 notCount

testSigmaMergePath :: IO ()
testSigmaMergePath = do
    let xs = [1 .. 5 :: Int]
        f :: Int -> TestAlg
        f i
            | i == 3 = EA.Zero
            | odd i = fromIntegral i :@ (Hat :< Yen)
            | otherwise = fromIntegral i :@ (Not :< Amount)
        expected :: TestAlg
        expected = EA.unionsMerge (L.map f xs)
        actual :: TestAlg
        actual = EA.sigma xs f
    assertEqual "Alg.sigma bulk-merge path matches unionsMerge" expected actual

testSigma2When :: IO ()
testSigma2When = do
    let xs = [1 .. 3 :: Int]
        ys = [1 .. 4 :: Int]
        cond i j = i /= j && even (i + j)
        f :: Int -> Int -> TestAlg
        f i j =
            let v = fromIntegral (i * 10 + j)
            in if odd i
                then v :@ (Hat :< Yen)
                else v :@ (Not :< Amount)
        expected :: TestAlg
        expected =
            EA.unionsMerge
                [ f i j
                | i <- xs
                , j <- ys
                , cond i j
                ]
        actual :: TestAlg
        actual = EA.sigma2When xs ys cond f
    assertEqual "Alg.sigma2When matches list-comprehension sum" expected actual

testSigmaFromMap :: IO ()
testSigmaFromMap = do
    let kvs = M.fromList
            [ ((1, 2), 5.0)
            , ((2, 3), 0.0)
            , ((3, 1), 7.0)
            ] :: M.Map (Int, Int) Double
        f :: (Int, Int) -> Double -> TestAlg
        f (i, j) v
            | i < j = v :@ (Hat :< Yen)
            | otherwise = v :@ (Not :< Amount)
        expected :: TestAlg
        expected = EA.unionsMerge
            [ f (1, 2) 5.0
            , f (3, 1) 7.0
            ]
        actual :: TestAlg
        actual = EA.sigmaFromMap kvs f
    assertEqual "Alg.sigmaFromMap iterates non-zero map entries only" expected actual

testJournalFromListStrict :: IO ()
testJournalFromListStrict = do
    -- fromList is now a strict left fold (L.foldl' (.+) mempty). Verify it still
    -- preserves the posting multiset by matching the old lazy right-fold reference
    -- (foldr (.+) mempty). Colliding note keys (i `mod` 30) force same-note/same-base
    -- postings into one Alg sequence, where the two folds accumulate in opposite
    -- order; with MoneyDecimal (exact, associative) the aggregate (norm) is identical.
    let mk i = ((fromIntegral (i `mod` 7 + 1) :: MoneyDecimal)
                  :@ ((if even i then Hat else Not) :< ([Yen, Amount] !! (i `mod` 2))))
               .| show (i `mod` 30)
        xs :: [Journal String MoneyDecimal (HatBase CountUnit)]
        xs = [ mk i | i <- [1 .. 400 :: Int] ]
        strict  = EJ.fromList xs
        lazyRef = foldr (.+) mempty xs
    -- exact value type ⇒ norm identical regardless of seq order (multiset preserved)
    assertEqual "Journal.fromList (strict): norm matches lazy foldr reference (MoneyDecimal exact)"
        (norm strict) (norm lazyRef)
    -- distinct note keys ⇒ no seq collision ⇒ exact structural equality with foldr
    let ys :: [Journal String MoneyDecimal (HatBase CountUnit)]
        ys = [ ((fromIntegral i :: MoneyDecimal) :@ (Not :< Yen)) .| show i
             | i <- [1 .. 20 :: Int] ]
    assertEqual "Journal.fromList (strict): structurally equal to foldr for distinct notes"
        (EJ.toMap (EJ.fromList ys)) (EJ.toMap (foldr (.+) mempty ys))

-- | Regression test for the @union@ zero-singleton base-relabel bug
-- (Algebra.hs). When one operand of @(.+)@ is a /zero-valued/ singleton on base
-- @b1@ and the other a /real/ singleton on a different base @b2@, the result must
-- keep the real value on its OWN base (@v2:@b2@), not relabel it onto the zero
-- posting's base. The old code returned @v2:@b1@ / @v1:@b2@, which silently moved
-- a value to the wrong base. It preserved @norm@ (total unchanged) but corrupted
-- per-base projection, and surfaced as construction-order-dependent simulation
-- results (sparsified coefficients build explicit @0:@base@ singletons via raw
-- @(:@)@). See plans/in-progress/SELECTABLE_VALUE_TYPE_PLAN.md (Stage D).
testUnionZeroSingletonBase :: IO ()
testUnionZeroSingletonBase = do
    let zb = 0 :@ (Hat :< Yen)    :: TestAlg   -- zero value, base Yen
        rb = 5 :@ (Hat :< Amount) :: TestAlg   -- real value, base Amount
    -- both fold directions of the singleton/singleton union
    assertEqual "union zero(.+)real keeps real value on its own base"
        rb (EA.proj [Hat :< Amount] (zb .+ rb))
    assertEqual "union real(.+)zero keeps real value on its own base"
        rb (EA.proj [Hat :< Amount] (rb .+ zb))
    -- the real value must NOT appear on the zero posting's base
    assertEqual "union zero(.+)real: nothing relabeled onto the zero's base"
        (EA.Zero :: TestAlg) (EA.proj [Hat :< Yen] (zb .+ rb))
    assertEqual "union real(.+)zero: nothing relabeled onto the zero's base"
        (EA.Zero :: TestAlg) (EA.proj [Hat :< Yen] (rb .+ zb))

-- | Regression for audit divergence C: scalar product (.*) must reject a
-- negative / non-finite scalar instead of silently producing negative
-- (out-of-domain) postings. (Pre-fix, (.*) used raw (:@) and bypassed the
-- isErrorValue check that (.@) performs.)
testScalarRejectsNegative :: IO ()
testScalarRejectsNegative = do
    let xD = 10 :@ (Not :< Yen) :: TestAlg
    rD <- try (evaluate (norm ((-1) .* xD))) :: IO (Either SomeException Double)
    case rD of
        Left _  -> putStrLn "[PASS] (.*) rejects negative scalar (Double)"
        Right v -> do putStrLn ("[FAIL] (.*) negative scalar leaked (Double): " ++ show v); exitFailure
    let xN = 10 :@ (Not :< Yen) :: EA.Alg MoneyDecimal (HatBase CountUnit)
    rN <- try (evaluate (norm ((-1) .* xN))) :: IO (Either SomeException MoneyDecimal)
    case rN of
        Left _  -> putStrLn "[PASS] (.*) rejects negative scalar (MoneyDecimal)"
        Right v -> do putStrLn ("[FAIL] (.*) negative scalar leaked (MoneyDecimal): " ++ show v); exitFailure
    -- non-negative scalar still works
    assertNear "(.*) non-negative scalar works" 20.0 (norm (2 .* xD))

-- Step 1 (concrete projection keeps the axis index lazy): the module is compiled
-- @Strict@, so a concrete (non-wildcard) 'projNorm' must NOT force the lazy
-- @_axisPosting@ index (it should be a plain 'Map.lookup'); a wildcard 'projNorm'
-- must use (force) it. We poison the index fields with 'error' and check which
-- projection crashes. Guards the projExactMap/projWildMap split.
testProjConcreteNoIndexForce :: IO ()
testProjConcreteNoIndexForce = do
    let alg :: EA.Alg Double SimHatBase2
        alg = EA.fromList [ 10 :@ Not :< (Cash, 1, 1, Yen)
                          , 20 :@ Not :< (Products, 2, 2, Amount)
                          , 30 :@ Hat :< (Cash, 3, 3, Yen) ]
    case alg of
      EA.Liner m _ _ _ _ _ -> do
        let poison = EA.Liner m (error "POISON") (error "POISON")
                                (error "POISON") (error "POISON") (error "POISON")
        rc <- try (evaluate (EA.projNorm [Not :< (Cash, 1, 1, Yen)] poison))
                :: IO (Either SomeException Double)
        case rc of
          Right v | v == 10.0 -> putStrLn "[PASS] concrete projNorm does not force the axis index"
          Right v             -> do putStrLn ("[FAIL] concrete projNorm wrong value: " ++ show v); exitFailure
          Left _              -> do putStrLn "[FAIL] concrete projNorm forced the (poisoned) axis index"; exitFailure
        rw <- try (evaluate (EA.projNorm [Not :< (Cash, (.#), 1, Yen)] poison))
                :: IO (Either SomeException Double)
        case rw of
          Left _   -> putStrLn "[PASS] wildcard projNorm uses the axis index (forced, as required)"
          Right v  -> do putStrLn ("[FAIL] wildcard projNorm did not use the index: " ++ show v); exitFailure
      _ -> do putStrLn "[FAIL] expected a Liner"; exitFailure

testJournalSigmaMergePath :: IO ()
testJournalSigmaMergePath = do
    let xs = [1 .. 4 :: Int]
        f :: Int -> TestJournal
        f i = case i of
            1 -> (1 :@ (Hat :< Yen)) .| "A"
            2 -> EJ.Zero
            3 -> (EA.Zero :: TestAlg) .| "A"
            _ -> (2 :@ (Not :< Amount)) .| "B"
        expected :: TestJournal
        expected = EJ.fromMap $ HM.fromList
            [ ("A", 1 :@ (Hat :< Yen))
            , ("B", 2 :@ (Not :< Amount))
            ]
        actual = EJ.sigma xs f
    assertEqual "Journal.sigma bulk-merge path skips zero postings" (EJ.toMap expected) (EJ.toMap actual)

testJournalSigma2When :: IO ()
testJournalSigma2When = do
    let xs = [1 .. 3 :: Int]
        ys = [1 .. 3 :: Int]
        cond i j = i < j
        f :: Int -> Int -> TestJournal
        f i j
            | i == 1 && j == 2 = (EA.Zero :: TestAlg) .| "N"
            | odd (i + j) = (fromIntegral (i + j) :@ (Hat :< Yen)) .| "N"
            | otherwise = EJ.Zero
        expected :: TestJournal
        expected = EJ.fromMap $ HM.fromList [("N", 5 :@ (Hat :< Yen))]
        actual = EJ.sigma2When xs ys cond f
    assertEqual "Journal.sigma2When matches filtered pair sum" (EJ.toMap expected) (EJ.toMap actual)

testJournalSigmaOn :: IO ()
testJournalSigmaOn = do
    let xs = [1 .. 4 :: Int]
        f :: Int -> TestAlg
        f i
            | i <= 2 = EA.Zero
            | otherwise = fromIntegral i :@ (Hat :< Yen)
        expected :: TestJournal
        expected = (EA.sigma xs f) .| "SalesPurchase"
        actual :: TestJournal
        actual = EJ.sigmaOn "SalesPurchase" xs f
        zeroExpected = EJ.Zero :: TestJournal
        zeroActual = EJ.sigmaOn "SalesPurchase" xs (\_ -> EA.Zero :: TestAlg)
    assertEqual "Journal.sigmaOn attaches note after EA.sigma" (EJ.toMap expected) (EJ.toMap actual)
    assertEqual "Journal.sigmaOn returns Zero when EA.sigma is Zero" (EJ.toMap zeroExpected) (EJ.toMap zeroActual)

testJournalSigmaOnFromMap :: IO ()
testJournalSigmaOnFromMap = do
    let kvs = M.fromList
            [ ((1, 2), 4.0)
            , ((2, 3), 0.0)
            , ((2, 1), 6.0)
            ] :: M.Map (Int, Int) Double
        f :: (Int, Int) -> Double -> TestAlg
        f (i, j) v
            | i < j = v :@ (Hat :< Yen)
            | otherwise = v :@ (Not :< Amount)
        expected :: TestJournal
        expected = (EA.sigmaFromMap kvs f) .| "SalesPurchase"
        actual :: TestJournal
        actual = EJ.sigmaOnFromMap "SalesPurchase" kvs f
        zeroActual :: TestJournal
        zeroActual = EJ.sigmaOnFromMap "SalesPurchase" (M.singleton (1, 1) 0.0) f
    assertEqual "Journal.sigmaOnFromMap matches EA.sigmaFromMap + note" (EJ.toMap expected) (EJ.toMap actual)
    assertEqual "Journal.sigmaOnFromMap returns Zero for empty-effective map" (EJ.toMap (EJ.Zero :: TestJournal)) (EJ.toMap zeroActual)

testFilterByAxisEquivalent :: IO ()
testFilterByAxisEquivalent = do
    let ledger :: AxisJournal
        ledger = EJ.fromList
            [ (10 :@ (Hat :< Yen)) .| ("A", 1)
            , (20 :@ (Not :< Amount)) .| ("B", 1)
            , (30 :@ (Hat :< Yen)) .| ("A", 2)
            ]
        expected = EJ.filterWithNote (\(_, t') _ -> t' == 1) ledger
        actual = EJ.filterByAxis 1 (EJ.NoteAxisKey (1 :: Int)) ledger
        mismatch = EJ.filterByAxis 1 (EJ.NoteAxisKey ("1" :: String)) ledger
    assertEqual "Journal.filterByAxis matches filterWithNote on axis=1"
        (EJ.toMap expected)
        (EJ.toMap actual)
    assertEqual "Journal.filterByAxis type mismatch returns empty"
        (EJ.toMap (EJ.Zero :: AxisJournal))
        (EJ.toMap mismatch)

testFilterByAxisWithDeltaUpdates :: IO ()
testFilterByAxisWithDeltaUpdates = do
    let base :: AxisJournal
        base = EJ.fromMap $ HM.fromList
            [ (("A", 1), 10 :@ (Hat :< Yen))
            , (("C", 2), 5 :@ (Not :< Amount))
            ]
        rhs :: AxisJournal
        rhs = EJ.fromMap $ HM.fromList
            [ (("A", 1), 3 :@ (Not :< Amount))
            , (("B", 1), 7 :@ (Hat :< Yen))
            ]
        ledger = base .+ rhs
        expected = EJ.filterWithNote (\(_, t') _ -> t' == 1) ledger
        actual = EJ.filterByAxis 1 (EJ.NoteAxisKey (1 :: Int)) ledger
    assertEqual "Journal.filterByAxis works after append updates"
        (EJ.toMap expected)
        (EJ.toMap actual)

-- ================================================================
-- Transfer regression tests
-- ================================================================

type TransferAlg = EA.Alg Double SimHatBase2
type TransferJournal = EJ.Journal String Double SimHatBase2

transferAlgSample :: TransferAlg
transferAlgSample = EA.fromList
    [ 7  :@ Not :<(WageExpenditure, 1, 1, Yen)
    , 3  :@ Hat :<(Depreciation, 2, 2, Yen)
    , 11 :@ Not :<(Purchases, 3, 3, Yen)
    , 13 :@ Not :<(ValueAdded, 1, 2, Yen)
    , 17 :@ Hat :<(Sales, 2, 1, Yen)
    , 19 :@ Not :<(InterestEarned, 4, 4, Yen)
    , 23 :@ Hat :<(InterestExpense, 5, 5, Yen)
    , 29 :@ Not :<(TaxesRevenue, 2, 2, Yen)
    , 31 :@ Hat :<(TaxesExpense, 3, 3, Yen)
    , 37 :@ Not :<(WageEarned, 6, 6, Yen)
    , 41 :@ Hat :<(ConsumptionExpenditure, 6, 6, Yen)
    , 43 :@ Not :<(CentralBankPaymentIncome, 1, 1, Yen)
    , 47 :@ Hat :<(CentralBankPaymentExpense, 1, 1, Yen)
    , 53 :@ Not :<(GrossProfit, 7, 7, Yen)
    , 59 :@ Hat :<(OrdinaryProfit, 8, 8, Yen)
    , 61 :@ Not :<(Cash, 1, 1, Yen)
    ]

transferJournalSample :: TransferJournal
transferJournalSample = EJ.fromList
    [ transferAlgSample .| "A"
    , ((5 :@ Not :<(Sales, 2, 1, Yen)) .+ (2 :@ Hat :<(WageExpenditure, 1, 1, Yen))) .| "B"
    , ((3 :@ Hat :<(TaxesExpense, 3, 3, Yen)) .+ (4 :@ Not :<(InterestEarned, 4, 4, Yen))) .| "C"
    ]

testFinalStockTransferAlgEquivalence :: IO ()
testFinalStockTransferAlgEquivalence = do
    let ref =
            (.-)
                . EAT.retainedEarningTransfer
                . EAT.ordinaryProfitTransfer
                . EAT.grossProfitTransfer
                $ transferAlgSample
        actual = EAT.finalStockTransfer transferAlgSample
    assertEqual "Algebra.finalStockTransfer matches composed transfer" ref actual

testFinalStockTransferJournalEquivalence :: IO ()
testFinalStockTransferJournalEquivalence = do
    let ref =
            (.-)
                . EJT.retainedEarningTransfer
                . EJT.ordinaryProfitTransfer
                . EJT.grossProfitTransfer
                $ transferJournalSample
        actual = EJT.finalStockTransfer transferJournalSample
    assertEqual "Journal.finalStockTransfer matches composed transfer" (EJ.toMap ref) (EJ.toMap actual)

type SpillRestoreJournal = EJ.Journal (String, Int) Double (HatBase CountUnit)

testRestoreJournalFromBinarySpill :: IO ()
testRestoreJournalFromBinarySpill = do
    let spillPath = "/tmp/exchangealgebra_spill_restore_test.bin"
        chunk1 :: SpillRestoreJournal
        chunk1 = EJ.fromList
            [ (1 :@ (Hat :< Yen)) .| ("A", 1)
            , (2 :@ (Not :< Amount)) .| ("B", 2)
            ]
        chunk2 :: SpillRestoreJournal
        chunk2 = (3 :@ (Hat :< Yen)) .| ("C", 3)
        currentLedger :: SpillRestoreJournal
        currentLedger = EJ.fromList
            [ (4 :@ (Not :< Amount)) .| ("Tail", 4)
            , (8 :@ (Hat :< Yen)) .| ("AlreadySpilled", 2)
            ]
        expected :: SpillRestoreJournal
        expected = chunk1 .+ chunk2 .+ ((4 :@ (Not :< Amount)) .| ("Tail", 4))

    withFile spillPath WriteMode $ \h -> do
        ES.defaultBinarySpillWriter h (1 :: Int, 2 :: Int) chunk1
        ES.defaultBinarySpillWriter h (3 :: Int, 3 :: Int) chunk2

    actual <- restoreJournalFromBinarySpill spillPath snd currentLedger
    assertEqual "Write.restoreJournalFromBinarySpill merges spill + tail remainder"
        (EJ.toMap expected)
        (EJ.toMap actual)

-- ================================================================
-- SimulateEx1 reproduction (default scenario only, no parallelism)
-- ================================================================

type SimTerm = Int

instance StateTime SimTerm where
    initTerm = 1
    lastTerm = 100
data SimInitVar = SimInitVar
    { _simInitStock        :: Double
    , _simSteadyProduction :: Double
    , _simInhouseRatio     :: Double
    } deriving (Eq, Show)

instance InitVariables SimInitVar where

data SimEvent
    = SimSalesPurchase
    | SimProduction
    | SimPlank
    deriving (Ord, Show, Enum, Eq, Bounded, Generic)

instance Hashable SimEvent where

instance Note SimEvent where
    plank = SimPlank

instance Event SimEvent where

type SimCompany = Int

instance Element SimCompany where
    wiledcard = -1

instance BaseClass SimCompany where

simFstC, simLastC :: SimCompany
simFstC = 1
simLastC = 6

simCompanies :: [SimCompany]
simCompanies = [simFstC .. simLastC]

type SimHatBase2 = HatBase (AccountTitles, SimCompany, SimCompany, CountUnit)

instance ExBaseClass SimHatBase2 where
    getAccountTitle (h :< (a, _, _, _)) = a
    setAccountTitle (h :< (_, c, e, u)) b = h :< (b, c, e, u)

-- Accounting value type is MoneyDecimal (exact): ledger arithmetic is exact and
-- construction-order-independent. ABM parameters / input coefficients / random
-- draws remain Double and are converted (realToFrac) at the boundary where they
-- enter the ledger; reported stock/profit convert back to Double.
type SimTransaction = EJ.Journal (SimEvent, SimTerm) MoneyDecimal SimHatBase2

simCompressPreviousTerm :: SimTerm -> SimTransaction -> SimTransaction
simCompressPreviousTerm t le =
    EJ.fromMap $
        L.foldl' (\acc ev -> HM.adjust compress (ev, t) acc)
                 (EJ.toMap le)
                 [fstEvent .. lastEvent]

newtype SimLedger s = SimLedger (STRef s SimTransaction)

instance UpdatableSTRef SimLedger s SimTransaction where
    _unwrapURef (SimLedger x) = x
    _wrapURef x = SimLedger x

simInitLedger :: Double -> ST s (SimLedger s)
simInitLedger d = newURef $ EJ.fromList
    [ realToFrac d :@ Not :<(Products, e, e, Amount) .| (plank, initTerm)  -- Double param -> MoneyDecimal
    | e <- simCompanies
    ]

instance Updatable SimTerm SimInitVar SimLedger s where
    type Inner SimLedger s = STRef s SimTransaction
    unwrap = _unwrapURef
    initialize _ _ e = simInitLedger (_simInitStock e)
    updatePattern _ = return Modify
    modify _ t _ x = do
        le <- readURef x
        let added = EJ.gather (plank, t)
                  $ EJT.finalStockTransfer
                  $ (.-) $ simTermJournal (t - 1) le
            next = simCompressPreviousTerm (t - 1) (le .+ added)
        writeURef x next

type SimInputCoefficient = Double

newtype SimICTable s = SimICTable (STArray s (SimCompany, SimCompany) SimInputCoefficient)

instance UpdatableSTArray SimICTable s (SimCompany, SimCompany) SimInputCoefficient where
    _unwrapUArray (SimICTable arr) = arr
    _wrapUArray arr = SimICTable arr

simGenerateRandomList :: StdGen -> Int -> ([Double], StdGen)
simGenerateRandomList g n =
    let (xs, g') = runState (replicateM n (state (randomR (0, 1.0))))
                            (updateGen g 1000)
        ys = L.map (\v -> if v < 0.1 then 0 else v) xs
    in (ys, g')

simInitTermCoefficients :: StdGen -> Double -> M.Map SimCompany [SimInputCoefficient]
simInitTermCoefficients g inhouseRatio =
    fst $ L.foldl' buildRow (M.empty, g) simCompanies
  where
    buildRow (acc, g0) c2 =
        let (row, g1) = generateRow g0
        in (M.insert c2 row acc, g1)
    generateRow g0 =
        let (vals, g1) = simGenerateRandomList g0 simLastC
            total = sum vals
            normalized = L.map (\v -> (v / total) * inhouseRatio) vals
        in (normalized, g1)

simInitICTables :: StdGen -> Double -> ST s (SimICTable s)
simInitICTables g inhouseRatio = do
    arr <- newUArray ((simFstC, simFstC), (simLastC, simLastC)) 0
    let termCoefficients = simInitTermCoefficients g inhouseRatio
    forM_ simCompanies $ \c2 -> do
        let row = termCoefficients M.! c2
        forM_ (zip simCompanies row) $ \(c1, coef) ->
            writeUArray arr (c1, c2) coef
    return arr

instance Updatable SimTerm SimInitVar SimICTable s where
    type Inner SimICTable s = STArray s (SimCompany, SimCompany) SimInputCoefficient
    unwrap (SimICTable a) = a
    initialize g _ e = simInitICTables g (_simInhouseRatio e)
    updatePattern _ = return DoNothing

type SimSteadyProd = Double

newtype SimSP s = SimSP (STRef s SimSteadyProd)

instance UpdatableSTRef SimSP s SimSteadyProd where
    _unwrapURef (SimSP x) = x
    _wrapURef x = SimSP x

instance Updatable SimTerm SimInitVar SimSP s where
    type Inner SimSP s = STRef s SimSteadyProd
    unwrap = _unwrapURef
    initialize _ _ e = newURef (_simSteadyProduction e)
    updatePattern _ = return DoNothing

data SimWorld s = SimWorld
    { _simLedger :: SimLedger s
    , _simIcs    :: SimICTable s
    , _simSp     :: SimSP s
    } deriving (Generic)

-- helper functions

simTermJournal :: SimTerm -> SimTransaction -> SimTransaction
simTermJournal t = EJ.filterWithNote (\(_, t') _ -> t' == t)

simGetOneProduction :: SimWorld s -> SimTerm -> SimCompany -> ST s SimTransaction
simGetOneProduction wld t c = do
    let arr = _simIcs wld
    inputs <- mapM (\c2 -> do
        coef <- readUArray arr (c2, c)
        return $ realToFrac coef :@ Hat :<(Products, c2, c, Amount) .| (SimProduction, t)  -- Double coef -> MoneyDecimal
        ) simCompanies
    let totalInput = EJ.fromList inputs
        result = (1 :@ Not :<(Products, c, c, Amount) .| (SimProduction, t)) .+ totalInput
    return result

simJournal :: SimWorld s -> SimTransaction -> ST s ()
simJournal _ Zero = return ()
simJournal wld js = modifyURef (_simLedger wld) (\x -> x .+ js)

-- Values come from the MoneyDecimal ledger (via EA.toList), so the shortage map is
-- MoneyDecimal-valued; no conversion is needed and the amounts re-enter the ledger exactly.
simBuildShortageMap :: SimTerm -> SimTransaction -> M.Map (SimCompany, SimCompany) MoneyDecimal
simBuildShortageMap t le =
    let termAlg = EJ.toAlg $ (.-) $ simTermJournal t le
    in L.foldl' go M.empty (EA.toList termAlg)
  where
    go acc (v :@ (Hat :< (Products, j, i, Amount))) = M.insertWith (+) (i, j) v acc
    go acc _ = acc

simPurchases :: SimTerm -> SimWorld s -> ST s SimTransaction
simPurchases t wld = do
    le <- readURef (_simLedger wld)
    let shortageMap = simBuildShortageMap t le
        o i j = M.findWithDefault 0 (i, j) shortageMap
    return $ sigma simCompanies $ \i
           -> sigma (simCompanies L.\\ [i]) $ \j
           -> (o i j) :@ Not :<(Products, j, i, Amount)
           .+ (o i j) :@ Hat :<(Cash, (.#), i, Yen)
           .+ (o i j) :@ Not :<(Purchases, (.#), i, Yen)
           .+ (o i j) :@ Not :<(Cash, (.#), j, Yen)
           .+ (o i j) :@ Not :<(Sales, (.#), j, Yen)
           .+ (o i j) :@ Hat :<(Products, j, j, Amount)
           .| (SimSalesPurchase, t)

instance StateSpace SimTerm SimInitVar SimEvent SimWorld s where
    event = simEvent

simEvent :: SimWorld s -> SimTerm -> SimEvent -> ST s ()

simEvent wld t SimSalesPurchase = do
    toAdd <- simPurchases t wld
    simJournal wld toAdd

simEvent wld t SimProduction = do
    sp <- readURef (_simSp wld)
    forM_ simCompanies $ \e1 -> do
        op <- simGetOneProduction wld t e1
        simJournal wld (realToFrac sp .* op)  -- Double steady-production multiplier -> MoneyDecimal scalar

simEvent _ _ SimPlank = return ()

simGetTermStock :: SimWorld s -> SimTerm -> SimCompany -> ST s Double
simGetTermStock wld t e = do
    le <- readURef (_simLedger wld)
    let tj = (.-) $ simTermJournal t le
        plusStock  = norm $ EJ.projWithBase [Not :<(Products, e, e, Amount)] tj
        minusStock = norm $ EJ.projWithBase [Hat :<(Products, e, e, Amount)] tj
    return $ realToFrac (plusStock - minusStock)  -- exact MoneyDecimal stock -> Double for reporting

simGetTermGrossProfit :: SimWorld s -> SimTerm -> SimCompany -> ST s Double
simGetTermGrossProfit wld t e = do
    le <- readURef (_simLedger wld)
    let termTr = simTermJournal t le
        tr     = EJT.grossProfitTransfer termTr
        plus   = norm $ EJ.projWithBase [Not :<(GrossProfit, (.#), e, Yen)] tr
        minus  = norm $ EJ.projWithBase [Hat :<(GrossProfit, (.#), e, Yen)] tr
    return $ realToFrac (plus - minus)  -- exact MoneyDecimal -> Double for reporting

-- ================================================================
-- Simulation integration test
-- ================================================================

simEps :: Double
simEps = 1e-6

assertSimNear :: String -> Double -> Double -> IO ()
assertSimNear label expected actual
    | abs (expected - actual) <= simEps = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

testSimulateEx1Default :: IO ()
testSimulateEx1Default = do
    let gen = mkStdGen 2025
        defaultEnv = SimInitVar
            { _simInitStock        = 20
            , _simInhouseRatio     = 0.4
            , _simSteadyProduction = 10
            }

    wld <- ES.runSimulation gen defaultEnv

    -- Stock at term 1 for each company
    stocks1 <- stToIO $ mapM (simGetTermStock wld 1) simCompanies
    -- Stock at term 50 for each company
    stocks50 <- stToIO $ mapM (simGetTermStock wld 50) simCompanies
    -- Stock at term 100 for each company
    stocks100 <- stToIO $ mapM (simGetTermStock wld 100) simCompanies
    -- Gross profit at term 50 for each company
    profits50 <- stToIO $ mapM (simGetTermGrossProfit wld 50) simCompanies

    -- Stock at t=1
    assertSimNear "sim1 stock(t=1,c=1)" 28.487224703666264 (stocks1 !! 0)
    assertSimNear "sim1 stock(t=1,c=3)" 30.0               (stocks1 !! 2)
    assertSimNear "sim1 stock(t=1,c=6)" 30.0               (stocks1 !! 5)  -- re-baselined: union zero-base fix removed a phantom self-input
    -- Stock at t=50
    assertSimNear "sim1 stock(t=50,c=1)" 304.9028131162567  (stocks50 !! 0)
    assertSimNear "sim1 stock(t=50,c=4)" 292.4764622201871  (stocks50 !! 3)
    -- Stock at t=100
    assertSimNear "sim1 stock(t=100,c=1)" 586.9595359862476  (stocks100 !! 0)
    assertSimNear "sim1 stock(t=100,c=6)" 767.9605634804993  (stocks100 !! 5)  -- re-baselined: union zero-base fix (bug compounded over terms)
    -- Gross profit at t=50
    assertSimNear "sim1 profit(t=50,c=1)" 0.35886554260018855 (profits50 !! 0)
    assertSimNear "sim1 profit(t=50,c=2)" 1.572544209772035   (profits50 !! 1)

-- ================================================================
-- CSV Write tests
-- ================================================================

testCsvTranspose :: IO ()
testCsvTranspose = do
    -- Square matrix
    let input1 = [ [T.pack "a", T.pack "b"]
                 , [T.pack "c", T.pack "d"] ]
        expected1 = [ [T.pack "a", T.pack "c"]
                    , [T.pack "b", T.pack "d"] ]
    assertEqual "CSV.transpose square matrix" expected1 (EW.csvTranspose input1)

    -- Ragged matrix (shorter rows padded with empty)
    let input2 = [ [T.pack "a", T.pack "b", T.pack "c"]
                 , [T.pack "d"] ]
        expected2 = [ [T.pack "a", T.pack "d"]
                    , [T.pack "b", T.empty]
                    , [T.pack "c", T.empty] ]
    assertEqual "CSV.transpose ragged matrix" expected2 (EW.csvTranspose input2)

    -- Single row
    let input3 = [[T.pack "x", T.pack "y", T.pack "z"]]
        expected3 = [[T.pack "x"], [T.pack "y"], [T.pack "z"]]
    assertEqual "CSV.transpose single row" expected3 (EW.csvTranspose input3)

    -- Empty
    assertEqual "CSV.transpose empty" ([] :: [[T.Text]]) (EW.csvTranspose [])

testCsvWriteCSV :: IO ()
testCsvWriteCSV = do
    let path = "/tmp/exchangealgebra_csv_test.csv"
        input = [ [T.pack "Name", T.pack "Value"]
                , [T.pack "Alice", T.pack "100"]
                , [T.pack "Bob", T.pack "200"] ]
    EW.writeCSV path input
    raw <- readFileStrict path
    -- Each cell should be quoted
    let lns = lines raw
    assertEqual "CSV writeCSV line count" 3 (length lns)
    assertEqual "CSV writeCSV header" "\"Name\",\"Value\"" (lns !! 0)
    assertEqual "CSV writeCSV row 1"  "\"Alice\",\"100\"" (lns !! 1)
    assertEqual "CSV writeCSV row 2"  "\"Bob\",\"200\""   (lns !! 2)
    removeFile path

testCsvWriteCSVWithQuotes :: IO ()
testCsvWriteCSVWithQuotes = do
    let path = "/tmp/exchangealgebra_csv_quote_test.csv"
        input = [[T.pack "say \"hello\"", T.pack "a,b"]]
    EW.writeCSV path input
    raw <- readFileStrict path
    let lns = lines raw
    -- Internal quotes should be escaped as ""
    assertEqual "CSV writeCSV escapes quotes" "\"say \"\"hello\"\"\",\"a,b\"" (lns !! 0)
    removeFile path

testCsvWriteCSVEmpty :: IO ()
testCsvWriteCSVEmpty = do
    let path = "/tmp/exchangealgebra_csv_empty_test.csv"
        input = [[T.pack "", T.pack "x"]]
    EW.writeCSV path input
    raw <- readFileStrict path
    let lns = lines raw
    assertEqual "CSV writeCSV empty cell" "\"\",\"x\"" (lns !! 0)
    removeFile path

-- | Regression tests for scale-aware numeric tolerance (WI-11/12/14).
-- These exercise large magnitudes that the previous fixed @1e-13@ absolute
-- tolerance handled incorrectly (retaining pure rounding noise as a residual);
-- small-scale behavior is unchanged. See plans LAZY_EVAL_AUDIT.md s4.6.
testNumericToleranceScaleAware :: IO ()
testNumericToleranceScaleAware = do
    assertEqual "nearlyEqScaled: large-scale rounding treated as equal"
        True  (EA.nearlyEqScaled (1e10 + 0.1 + 0.2) (1e10 + 0.3 :: Double))
    assertEqual "isNearlyNum 1e-13: large-scale rounding rejected (documents old flaw)"
        False (EA.isNearlyNum (1e10 + 0.1 + 0.2) (1e10 + 0.3) (1e-13 :: Double))
    assertEqual "nearlyEqScaled: small-scale noise treated as equal"
        True  (EA.nearlyEqScaled (0.1 + 0.2) (0.3 :: Double))
    assertEqual "nearlyEqScaled: genuine residual kept (not swallowed)"
        False (EA.nearlyEqScaled (1e10 + 5.0) (1e10 :: Double))
    assertEqual "nearlyEqScaled: NaN guarded (no crash, not equal)"
        False (EA.nearlyEqScaled (0/0) (1.0 :: Double))
    let big = (1e10 :@ (Hat :< Yen)) .+ (0.1 :@ (Hat :< Yen)) .+ (0.2 :@ (Hat :< Yen))
           .+ (1e10 :@ (Not :< Yen)) .+ (0.3 :@ (Not :< Yen)) :: TestAlg
    assertEqual "bar cancels balanced large-scale element to Zero"
        True (EA.isZero ((.-) big))

-- | Smoke test for the exact non-negative decimal value type 'MoneyDecimal' (Stage B).
-- The point of an exact value type is that summation is associative, so @norm@ is
-- *independent of construction order* — the property that makes the fromList O(N)
-- optimization safe (Stage D). Note the raw @Seq@ order (and hence @toMap@/@Eq@)
-- still depends on construction; only the numeric results are order-independent.
testMoneyDecimalExactOrderIndependent :: IO ()
testMoneyDecimalExactOrderIndependent = do
    assertEqual "MoneyDecimal: 0.1 + 0.2 == 0.3 exactly"
        True (0.1 + 0.2 == (0.3 :: MoneyDecimal))
    let mk i = ((fromIntegral (i `mod` 7 + 1) :: MoneyDecimal)
                  :@ ((if even i then Hat else Not) :< ([Yen, Amount] !! (i `mod` 2))))
               .| show (i `mod` 150)
        xs       :: [Journal String MoneyDecimal (HatBase CountUnit)]
        xs       = [ mk i | i <- [1 .. 400 :: Int] ]
        viaFoldr = foldr (.+) mempty xs
        viaFoldl = L.foldl' (.+) mempty xs
    -- exact ⇒ norm is identical for the two construction orders
    assertEqual "MoneyDecimal Journal: norm is construction-order-independent"
        (norm viaFoldr) (norm viaFoldl)
    -- banker's rounding (round half to even)
    assertEqual "bankersRound 0 2.5 = 2 (half to even)" (2 :: MoneyDecimal) (bankersRound 0 2.5)
    assertEqual "bankersRound 0 3.5 = 4 (half to even)" (4 :: MoneyDecimal) (bankersRound 0 3.5)
    assertEqual "bankersRound 2 0.125 = 0.12 (half to even)" (0.12 :: MoneyDecimal) (bankersRound 2 0.125)

-- | Strict file read helper for tests
readFileStrict :: FilePath -> IO String
readFileStrict p = do
    bs <- TIO.readFile p
    return (T.unpack bs)

-- ================================================================
-- Main
-- ================================================================

-- ================================================================
-- Redundant-algebra axiom property tests (QuickCheck)
--
-- Encodes the Definition 6 axioms (paper Appendix A) + derived lemmas as
-- QuickCheck properties, plus regression generalizations for the union
-- zero-base bug and construction-order independence. Property suite, additive.
-- ================================================================

type NNAlg = EA.Alg MoneyDecimal (HatBase CountUnit)

-- run a QuickCheck property in the existing IO-style harness
quickProp :: Testable p => String -> p -> IO ()
quickProp label p = do
    r <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = False } p
    if isSuccess r
        then putStrLn ("[PASS] " ++ label)
        else do putStrLn ("[FAIL] " ++ label); putStr (output r); exitFailure

-- generators: concrete (non-wildcard) bases, intentional collisions
genUnit :: Gen CountUnit
genUnit = elements [Yen, Dollar, Amount]

genSide :: Gen Hat
genSide = elements [Hat, Not]

genBase :: Gen (HatBase CountUnit)
genBase = (:<) <$> genSide <*> genUnit

genNNDouble :: Gen Double          -- non-negative, finite
genNNDouble = do
    NonNegative x <- arbitrary
    if isNaN x || isInfinite x then genNNDouble else pure x

genAlgD :: Gen TestAlg
genAlgD = sized $ \n -> do
    k  <- choose (0, min 40 n)
    ps <- vectorOf k ((,) <$> genNNDouble <*> genBase)
    pure (EA.fromList [ v .@ b | (v, b) <- ps ])

genAlgN :: Gen NNAlg
genAlgN = sized $ \n -> do
    k  <- choose (0, min 40 n)
    ps <- vectorOf k ((,) <$> (realToFrac <$> genNNDouble) <*> genBase)
    pure (EA.fromList [ v .@ b | (v, b) <- ps ])

-- exact per-base signed net (Not +, Hat -) via Rational; the observable
-- accounting content. Robust to seq order; catches base misassociation.
netByBase :: (HatVal v, Real v) => EA.Alg v (HatBase CountUnit) -> M.Map CountUnit Rational
netByBase = EA.foldEntries step M.empty
  where
    step m v b = M.insertWith (+) (part b) (signed v b) m
    part (_ :< u) = u
    signed v b = if isHat b then negate (toRational v) else toRational v

epsEq :: Double -> Double -> Bool
epsEq a b = abs (a - b) <= 1e-9 * (1 + max (abs a) (abs b))

axiomProperties :: IO ()
axiomProperties = do
    -- Definition 6 axioms (Double; semantic equality via exact per-base nets)
    quickProp "axiom: Hat involution (x^^ = x)" $
        forAll genAlgD $ \x -> netByBase ((.^) ((.^) x)) == netByBase x
    quickProp "axiom: scalar on singleton (a*(v:@b) = (a*v):@b)" $
        forAll genNNDouble $ \a -> forAll genNNDouble $ \v -> forAll genBase $ \b ->
            netByBase (a .* (v .@ b)) == netByBase (((a * v) .@ b) :: TestAlg)
    quickProp "axiom: scalar distributes over (.+)" $
        forAll genNNDouble $ \a -> forAll genAlgD $ \x -> forAll genAlgD $ \y ->
            netByBase (a .* (x .+ y)) == netByBase ((a .* x) .+ (a .* y))
    quickProp "axiom: norm additivity (norm(x+y) = norm x + norm y)" $
        forAll genAlgD $ \x -> forAll genAlgD $ \y ->
            epsEq (norm (x .+ y)) (norm x + norm y)
    quickProp "axiom: norm homogeneity (norm(a*x) = a*norm x, a>=0)" $
        forAll genNNDouble $ \a -> forAll genAlgD $ \x ->
            epsEq (norm (a .* x)) (a * norm x)
    -- derived lemmas
    quickProp "lemma: bar idempotent (bar(bar x) = bar x)" $
        forAll genAlgD $ \x -> netByBase (bar (bar x)) == netByBase (bar x)
    quickProp "lemma: zero identity (x .+ Zero = x)" $
        forAll genAlgD $ \x -> netByBase (x .+ EA.Zero) == netByBase x
    quickProp "lemma: (.+) associative" $
        forAll genAlgD $ \x -> forAll genAlgD $ \y -> forAll genAlgD $ \z ->
            netByBase ((x .+ y) .+ z) == netByBase (x .+ (y .+ z))
    -- regression: union must not relabel a value onto a zero posting's base
    -- (the 0.4.1.1 bug; raw (:@) so zero-valued singletons are exercised)
    quickProp "regression: union preserves per-base net (zero-base bug)" $
        forAll genNNDouble $ \v1 -> forAll genBase $ \b1 ->
        forAll genNNDouble $ \v2 -> forAll genBase $ \b2 ->
            let s1 = v1 :@ b1 :: TestAlg
                s2 = v2 :@ b2 :: TestAlg
            in netByBase (s1 .+ s2)
                 == M.unionWith (+) (netByBase s1) (netByBase s2)
    -- construction-order independence for the exact value type (MoneyDecimal)
    quickProp "MoneyDecimal: fromList per-base net is construction-order independent" $
        forAll (listOf ((,) <$> (realToFrac <$> genNNDouble) <*> genBase)) $ \ps ->
            let singles = [ v :@ b | (v, b) <- ps ] :: [NNAlg]
                viaList  = EA.fromList singles
                viaFoldr = foldr   (.+) EA.Zero singles
                viaFoldl = L.foldl' (.+) EA.Zero singles
            in netByBase viaList == netByBase viaFoldr
               && netByBase viaFoldr == netByBase viaFoldl
    -- mapBasePart (Phase 3): identity + norm preservation (no value lost on collision)
    quickProp "mapBasePart id preserves per-base net (MoneyDecimal)" $
        forAll genAlgN $ \x -> netByBase (EA.mapBasePart id x :: NNAlg) == netByBase x
    quickProp "mapBasePart preserves norm under base collapse (MoneyDecimal)" $
        forAll genAlgN $ \x -> norm (EA.mapBasePart (const Amount) x :: NNAlg) == norm x

-- ================================================================
-- Journal-algebra axiom properties (Phase 1.5)
-- ================================================================

type NNJournal = EJ.Journal String MoneyDecimal (HatBase CountUnit)

genNote :: Gen String
genNote = elements ["a", "b", "c"]

genPosNN :: Gen MoneyDecimal               -- strictly positive (avoids zero-note drop)
genPosNN = (\d -> realToFrac (1 + d)) <$> genNNDouble

genJournalN :: Gen NNJournal
genJournalN = sized $ \n -> do
    k  <- choose (0, min 30 n)
    ps <- vectorOf k ((,,) <$> genPosNN <*> genBase <*> genNote)
    pure (EJ.fromList [ (v :@ b) .| nt | (v, b, nt) <- ps ])

-- per-(note, base) signed net; exact (Rational)
netJournal :: NNJournal -> M.Map (String, CountUnit) Rational
netJournal j = M.fromList
    [ ((nt, u), r)
    | (nt, alg) <- HM.toList (EJ.toMap j)
    , (u, r)    <- M.toList (netByBase alg) ]

journalProperties :: IO ()
journalProperties = do
    quickProp "journal: norm additivity (norm(j1.+j2) = norm j1 + norm j2, MoneyDecimal)" $
        forAll genJournalN $ \j1 -> forAll genJournalN $ \j2 ->
            norm (j1 .+ j2) == norm j1 + norm j2
    quickProp "journal: Hat preserves the note set" $
        forAll genJournalN $ \j ->
            L.sort (HM.keys (EJ.toMap ((.^) j))) == L.sort (HM.keys (EJ.toMap j))
    quickProp "journal: fromList per-(note,base) net is construction-order independent (MoneyDecimal)" $
        forAll (listOf ((,,) <$> genPosNN <*> genBase <*> genNote)) $ \ps ->
            let js = [ (v :@ b) .| nt | (v, b, nt) <- ps ] :: [NNJournal]
            in netJournal (EJ.fromList js) == netJournal (foldr (.+) mempty js)

-- ================================================================
-- Quotient decomposition properties (Phase 1, feat/quotient-decomposition)
--
-- Encodes the dec_κ / π_κ axioms of the scaling formalization
-- (agent-notes/drafts/scaling-formalization.md §2, §7) as QuickCheck
-- properties, plus fixed sentinels for the side-sensitive non-commutation
-- cases that MUST NOT silently start commuting (they encode a semantic
-- choice, not a bug).
-- ================================================================

-- proper classifier: factors through the base part (never sees Hat/Not)
properKf :: HatBase CountUnit -> Maybe CountUnit
properKf (_ :< u) = Just u

-- partial proper classifier: Yen entries fall into the residual
partialKf :: HatBase CountUnit -> Maybe CountUnit
partialKf (_ :< Yen) = Nothing
partialKf (_ :< u)   = Just u

-- side-sensitive classifier: sees the Hat/Not state (like decP/decM)
sideKf :: HatBase CountUnit -> Maybe Bool
sideKf b = Just (isHat b)

-- residual of a partial classifier (reference implementation via filter)
residualOf :: (HatBase CountUnit -> Maybe CountUnit) -> NNAlg -> NNAlg
residualOf kf = EA.filter (\s -> s /= EA.Zero && kf (EA._hatBase s) == Nothing)

-- per-base nets with exact-zero entries dropped (bar drops zero-net bases,
-- so commutation properties are compared modulo zero nets)
nonZeroNet :: NNAlg -> M.Map CountUnit Rational
nonZeroNet = M.filter (/= 0) . netByBase

quotientProperties :: IO ()
quotientProperties = do
    -- reconstruction: Σ_k x_k (+ residual) = x  (formalization Prop 2.3)
    quickProp "decBy: reconstruction, total classifier (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            netByBase (mconcat (M.elems (EA.decBy properKf x))) == netByBase x
    quickProp "decBy: reconstruction with residual, partial classifier" $
        forAll genAlgN $ \x ->
            netByBase (mconcat (M.elems (EA.decBy partialKf x)) .+ residualOf partialKf x)
                == netByBase x
    -- norm additivity over classes (formalization Prop 2.4(1))
    quickProp "decBy: norm additivity over classes + residual (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            norm x == L.foldl' (+) 0 (L.map norm (M.elems (EA.decBy partialKf x)))
                      + norm (residualOf partialKf x)
    -- proper classifier commutes with bar componentwise (Prop 2.4(4))
    quickProp "decBy: bar commutes componentwise (proper classifier)" $
        forAll genAlgN $ \x ->
            M.filter (not . M.null) (M.map nonZeroNet (EA.decBy properKf (bar x)))
                == M.filter (not . M.null) (M.map (nonZeroNet . bar) (EA.decBy properKf x))
    -- decBy equals the naive per-class filter loop (semantics check)
    quickProp "decBy: equals naive per-class filter (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let d = EA.decBy properKf x
                naive k = EA.filter
                    (\s -> s /= EA.Zero && properKf (EA._hatBase s) == Just k) x
            in all (\(k, alg) -> netByBase alg == netByBase (naive k)) (M.toList d)
    -- postFromNetBy equals an independent per-key projNorm pipeline
    quickProp "postFromNetBy: equals per-key projNorm reference (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let kf b = if isHat b then Just (unitOf b) else Nothing
                unitOf (_ :< u) = u
                post u v = v .@ (Not :< u) :: NNAlg
                viaApi = EA.postFromNetBy kf post x
                viaRef = mconcat
                    [ post u s
                    | u <- [Yen, Dollar, Amount]
                    , let s = EA.projNorm [Hat :< u] (bar x)
                    , s /= 0 ]
            in netByBase viaApi == netByBase viaRef
    -- decTo: flatten reconstructs and norm is preserved (total classifier)
    quickProp "decTo: toAlg . decTo reconstructs (total classifier, MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let j = EJ.decTo (\(_ :< u) -> Just (show u)) x
                    :: EJ.Journal String MoneyDecimal (HatBase CountUnit)
            in netByBase (EJ.toAlg j) == netByBase x && norm j == norm x
    -- sentinel: side-sensitive classifier does NOT commute with bar
    -- (decP/decM-style split; x = v:@Not:<Yen .+ v:@Hat:<Yen nets to zero
    --  globally but each side survives within its own class)
    let xCancel = (5 .@ (Not :< Yen)) .+ (5 .@ (Hat :< Yen)) :: NNAlg
        lhs = M.filter (not . EA.isZero) (M.map bar (EA.decBy sideKf xCancel))
        rhs = EA.decBy sideKf (bar xCancel)
    assertEqual "sentinel: side-sensitive decBy does not commute with bar"
        True (M.keys lhs /= M.keys rhs)
    -- sentinel: whichSide-style classifier is also side-sensitive
    -- (Cash homeSide = Debit, so Hat flips it to Credit: the two sides of one
    --  base land in different classes — Deguchi Def 2.13)
    let xCash = (100 .@ (Not :< Cash)) .+ (100 .@ (Hat :< Cash))
                    :: EA.Alg MoneyDecimal (HatBase AccountTitles)
        bySide = EA.decBy (\b -> Just (whichSide b)) xCash
    assertEqual "sentinel: whichSide splits one base across classes (side-sensitive)"
        [Credit, Debit] (L.sort (M.keys bySide))
    assertEqual "sentinel: whichSide decBy does not commute with bar"
        True (M.filter (not . EA.isZero) (M.map bar bySide)
                /= EA.decBy (\b -> Just (whichSide b)) (bar xCash))
    -- sentinel: π_κ (mapBasePart, non-injective) does not commute with bar
    -- (formalization §2.8: coarsen-then-net /= net-then-coarsen)
    let xPi = (100 .@ (Not :< Yen)) .+ (100 .@ (Hat :< Dollar)) :: NNAlg
    assertEqual "sentinel: bar (mapBasePart const) nets across the class"
        0 (norm (bar (EA.mapBasePart (const Amount) xPi :: NNAlg)))
    assertEqual "sentinel: mapBasePart (bar x) keeps both sides (no cross-base netting)"
        200 (norm (EA.mapBasePart (const Amount) (bar xPi) :: NNAlg))

-- ================================================================
-- Simulate.Lite tests (Phase 2, feat/simulate-lite)
-- ================================================================

-- A concrete Note type for the Lite models: (event tag, term index).
type LNote   = (String, Int)
type LBaseD  = HatBase AccountTitles
type LedgerD = Journal LNote MoneyDouble LBaseD     -- IEEE-754 path (DET-1, BSP, equiv)
type LBaseM  = HatBase AccountTitles
type LedgerM = Journal LNote MoneyDecimal LBaseM    -- exact path (DET-2)

------------------------------------------------------------------
-- Lite test 1: boilerplate acceptance example (3 fields, 2 stages).
-- The body of this function (the World record, the two stages, the spec and
-- the run) is the "~20 line" boilerplate the design targets.
------------------------------------------------------------------

-- A product-only HKD world: a ledger, a scalar price, a scalar tax rate.
data MiniW f = MiniW
  { mwLedger :: HK f LedgerD
  , mwPrice  :: HK f MoneyDouble
  , mwTax    :: HK f Double
  } deriving Generic

-- stage A: each agent buys 1 unit at the snapshot price (a pure message).
buyStage :: Stage MiniW Int LNote MoneyDouble LBaseD
buyStage = stageFor "buy" [1 .. 5 :: Int] $ \w t _g i ->
    let amt = mwPrice w * fromIntegral i
    in ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("buy", t)

-- stage B: a single bookkeeping step paying tax on the snapshot price.
taxStage :: Stage MiniW Int LNote MoneyDouble LBaseD
taxStage = stage "tax" $ \w t ->
    let amt = mwPrice w * realToFrac (mwTax w)
    in ((amt .@ Not :< Sales) .+ (amt .@ Hat :< Cash)) .| ("tax", t)

miniSpec :: SimSpec MiniW Int LNote MoneyDouble LBaseD
miniSpec = mkSimSpec (1, 3) 42 mwLedger [buyStage, taxStage]

testLiteBoilerplate :: IO ()
testLiteBoilerplate = do
    let w0 = MiniW { mwLedger = carry mempty
                   , mwPrice  = carry 10
                   , mwTax    = carry 0.1 }
        n  = runLite miniSpec w0 (realToFrac . norm . mwLedger)
    -- 3 terms * (sum_{i=1..5} 10*i*2  +  10*0.1*2) = 3 * (300 + 2) = 906
    assertNear "Lite: boilerplate mini-model runs (norm)" 906.0 n

------------------------------------------------------------------
-- Lite test 2 (DET-2): MoneyDecimal Sequential vs ParChunk exact match.
------------------------------------------------------------------

data DecW f = DecW
  { dwLedger :: HK f LedgerM
  } deriving Generic

decStage :: Stage DecW Int LNote MoneyDecimal LBaseM
decStage = stageFor "post" [1 .. 50 :: Int] $ \_w t g i ->
    let (k, _) = randomR (1, 9 :: Int) g
        amt    = fromIntegral (i + k) :: MoneyDecimal
    in ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("post", t)

decSpec :: Par -> SimSpec DecW Int LNote MoneyDecimal LBaseM
decSpec par = (mkSimSpec (1, 4) 7 dwLedger [decStage]) { Lite.specParallel = par }

testLiteDet2 :: IO ()
testLiteDet2 = do
    let w0 = DecW { dwLedger = carry mempty }
        runP par = runLite (decSpec par) w0 (toMap . dwLedger)
        seqMap = runP Sequential
        parMap = runP (ParChunk 8)
    assertEqual "Lite DET-2: Sequential and ParChunk produce identical ledgers (exact)"
        seqMap parMap

------------------------------------------------------------------
-- Lite test 3 (DET-1): MoneyDouble reproducibility across two runs.
------------------------------------------------------------------

testLiteDet1 :: IO ()
testLiteDet1 = do
    let w0 = MiniW { mwLedger = carry mempty
                   , mwPrice  = carry 10
                   , mwTax    = carry 0.1 }
        n1 = runLite miniSpec w0 (realToFrac . norm . mwLedger)
        n2 = runLite miniSpec w0 (realToFrac . norm . mwLedger)
    assertNear "Lite DET-1: same spec run twice gives same norm" n1 n2

------------------------------------------------------------------
-- Lite test 4 (BSP intra-stage invisibility sentinel).
-- Every agent in a stage reads the SAME snapshot. We encode the snapshot
-- ledger's norm into each agent's message; if a later agent could see an
-- earlier agent's write within the same stage, the encoded norms would differ
-- from the all-zero baseline (the ledger starts empty for term 1 stage 0).
------------------------------------------------------------------

data BspW f = BspW
  { bwLedger :: HK f LedgerD
  } deriving Generic

-- each agent posts (1 + norm-of-snapshot-ledger). On term 1, stage 0, the
-- snapshot ledger is empty for every agent, so each posts exactly 1.0.
bspStage :: Stage BspW Int LNote MoneyDouble LBaseD
bspStage = stageFor "bsp" [1 .. 10 :: Int] $ \w t _g _i ->
    let seenNorm = norm (bwLedger w)          -- must be 0 for ALL agents (BSP)
        amt = 1 + realToFrac seenNorm :: MoneyDouble
    in ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("bsp", t)

bspSpec :: SimSpec BspW Int LNote MoneyDouble LBaseD
bspSpec = mkSimSpec (1, 1) 0 bwLedger [bspStage]

testLiteBspInvisibility :: IO ()
testLiteBspInvisibility = do
    let w0 = BspW { bwLedger = carry mempty }
        n  = runLite bspSpec w0 (realToFrac . norm . bwLedger)
    -- 10 agents each post Not:<Purchases 1.0 + Hat:<Cash 1.0 = norm 20.
    -- If intra-stage writes were visible, later agents would post > 1.0 and the
    -- norm would exceed 20.
    assertNear "Lite BSP: intra-stage invisibility (all agents see empty ledger)"
        20.0 n

------------------------------------------------------------------
-- Lite test 5: gate toy-model equivalence (3 terms, agents [1..10], norm 3300).
-- Rebuilds the gate-report.md prototype with the Lite API; same norm.
------------------------------------------------------------------

data GateW f = GateW
  { gwLedger :: HK f LedgerD
  , gwPrice  :: HK f MoneyDouble
  } deriving Generic

gateStage :: Stage GateW Int LNote MoneyDouble LBaseD
gateStage = stageFor "buy" [1 .. 10 :: Int] $ \w t _g i ->
    let amt = gwPrice w * fromIntegral i
    in ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("buy", t)

gateSpec :: SimSpec GateW Int LNote MoneyDouble LBaseD
gateSpec = mkSimSpec (1, 3) 1 gwLedger [gateStage]

testLiteGateEquivalence :: IO ()
testLiteGateEquivalence = do
    let w0 = GateW { gwLedger = carry mempty, gwPrice = carry 10 }
        n  = runLite gateSpec w0 (realToFrac . norm . gwLedger)
    -- norm = 3 terms * sum_{i=1..10} (10*i*2) = 3 * 2 * 10 * 55 = 3300
    assertNear "Lite: gate toy-model equivalence (norm 3300)" 3300.0 n

------------------------------------------------------------------
-- Lite test 6: term-boundary Field rules (Carry / ResetEach / UpdateEach).
-- One agent posts (current price) each term; the three runs differ only in the
-- price field's boundary rule, exercising each Field constructor.
------------------------------------------------------------------

data RuleW f = RuleW
  { rwLedger :: HK f LedgerD
  , rwPrice  :: HK f MoneyDouble
  } deriving Generic

ruleStage :: Stage RuleW Int LNote MoneyDouble LBaseD
ruleStage = stage "post" $ \w t ->
    let amt = rwPrice w
    in ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("post", t)

ruleSpec :: SimSpec RuleW Int LNote MoneyDouble LBaseD
ruleSpec = mkSimSpec (1, 3) 0 rwLedger [ruleStage]

runRule :: Field MoneyDouble -> Double
runRule priceField =
    let w0 = RuleW { rwLedger = carry mempty, rwPrice = priceField }
    in realToFrac (runLite ruleSpec w0 (norm . rwLedger))
       -- norm counts both Not:<Purchases and Hat:<Cash, hence 2 * price each term

testLiteFieldRules :: IO ()
testLiteFieldRules = do
    -- Carry 10: price stays 10 every term -> 3 * 2 * 10 = 60
    assertNear "Lite Field: Carry keeps the value" 60.0 (runRule (carry 10))
    -- ResetEach 5: price reset to 5 at each boundary, but stage reads BEFORE
    -- the term-1 boundary commit at the same value -> 3 * 2 * 5 = 30
    assertNear "Lite Field: ResetEach restores each term" 30.0 (runRule (resetEach 5))
    -- UpdateEach 10 (*2): term1 price 10, boundary doubles -> term2 20, term3 40.
    -- norm = 2 * (10 + 20 + 40) = 140
    assertNear "Lite Field: UpdateEach applies the step each boundary"
        140.0 (runRule (updateEach 10 (* 2)))

-- regression: the boundary rule must fire once per TERM, not per stage.
-- (Parser pitfall: a trailing backtick operator after an inner lambda's
-- do-block is swallowed into the lambda body, turning the term-boundary
-- commit into a per-stage commit. Single-stage tests cannot see this.)
-- 2 identical stages x UpdateEach 10 (*2): both stages must read the SAME
-- price within a term -> norm = 2 entries * 2 stages * (10+20+40) = 280.
-- The per-stage-commit bug yields 2 * (10+20 + 40+80 + 160+320) = 1260.
testLiteBoundaryOncePerTerm :: IO ()
testLiteBoundaryOncePerTerm =
    assertNear "Lite: term boundary fires once per term (2 stages)" 280.0
        (let spec2 = mkSimSpec (1, 3) 0 rwLedger [ruleStage, ruleStage]
             w0 = RuleW { rwLedger = carry mempty, rwPrice = updateEach 10 (* 2) }
         in realToFrac (runLite spec2 w0 (norm . rwLedger)))

-- ================================================================
-- Simulate.Policy tests (Phase 4, feat/ledger-policy)
--
-- LedgerPolicy = declarative retention / spill / compaction, applied at the
-- term boundary by runLiteWithPolicy. The exact MoneyDecimal value type lets us
-- assert lossless round-trips and norm/compaction invariants by strict equality.
-- ================================================================

-- A one-field world whose stage posts a few distinct bases per term, so that a
-- closed term has redundant per-base sequences (exercising CompressClosedTerms)
-- and a multi-term history (exercising RetainRecent + spill).
data PolW f = PolW
  { pwLedger :: HK f LedgerM
  } deriving Generic

-- A single-field world for the classic-bridge test: a constructor @a@ of kind
-- @Type -> Type@ whose @a RealWorld@ is an @STRef RealWorld LedgerM@ (so it fits
-- the @SpillOptions t a payload@ shape, where @a@ is applied to the state token).
newtype LedgerRef s = LedgerRef (STRef s LedgerM)

-- Each agent posts twice to the SAME base within the term, so the term's per-base
-- posting sequence has length 2 before compress and length 1 after.
polStage :: Stage PolW Int LNote MoneyDecimal LBaseM
polStage = stageFor "post" [1 .. 4 :: Int] $ \_w t _g i ->
    let amt = fromIntegral i :: MoneyDecimal
        one = 1             :: MoneyDecimal
        m1  = ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("post", t)
        m2  = ((one .@ Not :< Purchases) .+ (one .@ Hat :< Cash)) .| ("post", t)
    in m1 .+ m2 :: Journal LNote MoneyDecimal LBaseM

polSpec :: SimSpec PolW Int LNote MoneyDecimal LBaseM
polSpec = mkSimSpec (1, 5) 0 pwLedger [polStage]

polW0 :: PolW InitT
polW0 = PolW { pwLedger = carry mempty }

-- run a temp spill file, returning (result, path); caller removes the file.
withTempSpill :: String -> (FilePath -> IO a) -> IO a
withTempSpill tag act = do
    let path = "/tmp/exchangealgebra_policy_" ++ tag ++ ".bin"
    -- ensure no stale file from a previous run (append-mode would accumulate)
    _ <- try (removeFile path) :: IO (Either SomeException ())
    r <- act path
    _ <- try (removeFile path) :: IO (Either SomeException ())
    pure r

-- Test 1 (flagship): defaultLedgerPolicy is observationally equal to runLite.
testPolicyEquivalence :: IO ()
testPolicyEquivalence = do
    let pureLedger = runLite polSpec polW0 (toMap . pwLedger)
    polLedger <- runLiteWithPolicy Policy.defaultLedgerPolicy polSpec polW0 (toMap . pwLedger)
    assertEqual "Policy: runLiteWithPolicy defaultLedgerPolicy == runLite (exact)"
        pureLedger polLedger

-- Test 2 (flagship): RetainRecent w + spillTo gives an in-memory window AND a
-- lossless restore that equals the FullAudit ledger.
testPolicyWindowRoundTrip :: IO ()
testPolicyWindowRoundTrip = withTempSpill "window" $ \path -> do
    let full = runLite polSpec polW0 (toMap . pwLedger)   -- FullAudit reference
        pol  = Policy.defaultLedgerPolicy
                 { Policy.retain  = Policy.RetainRecent 2
                 , Policy.spillTo = Just path }
    -- ONE policy run (append-mode spill: a second run would double the file),
    -- projecting the live journal; we derive both checks from it.
    residentJournal <- runLiteWithPolicy pol polSpec polW0 pwLedger
    -- (a) in-memory ledger after the run contains ONLY the most recent 2 terms.
    let residentMap   = toMap residentJournal
        residentTerms = L.sort (L.nub [ t | (_, t) <- HM.keys residentMap ])
    assertEqual "Policy: RetainRecent 2 leaves only the most recent 2 terms resident"
        [4, 5] residentTerms
    -- (b) restoreLedger (spill file + resident remainder) == FullAudit ledger.
    restored <- Policy.restoreLedger path residentJournal :: IO LedgerM
    assertEqual "Policy: restoreLedger (spill + remainder) == FullAudit ledger (lossless, exact)"
        full (toMap restored)

-- Test 3: CompressClosedTerms — norm/balance invariant, closed-term seq length 1,
-- in-progress term keeps full redundancy.
testPolicyCompressClosed :: IO ()
testPolicyCompressClosed = do
    let full = runLite polSpec polW0 (toMap . pwLedger)
    compactedJ <- runLiteWithPolicy
                    (Policy.defaultLedgerPolicy { Policy.compaction = Policy.CompressClosedTerms })
                    polSpec polW0 pwLedger
    let fullJ = runLite polSpec polW0 pwLedger
    -- (a) norm is invariant under compaction.
    assertEqual "Policy: CompressClosedTerms preserves norm (exact)"
        (norm fullJ) (norm compactedJ)
    -- (b) balance result unchanged (still balanced overall).
    assertEqual "Policy: CompressClosedTerms preserves balance"
        (EA.balance fullJ) (EA.balance compactedJ)
    -- (c) each CLOSED term (1..4) has at most one posting per base/side: its Alg
    --     compresses to itself (idempotent), so compress . entry == entry.
    let compactedMap = toMap compactedJ
        closedOk = all
          (\((_, t), alg) -> t == (5 :: Int) || EA.compress alg == alg)
          (HM.toList compactedMap)
    assertEqual "Policy: closed terms are already compressed (compress is a no-op on them)"
        True closedOk
    -- (d) the in-progress term (5) keeps its redundancy: in the FULL ledger term
    --     5's entry has a length-2 sequence, and the compacted ledger keeps the
    --     SAME term-5 entry (untouched), i.e. it differs from its own compress.
    let term5Full = HM.lookup ("post", 5) (toMap fullJ)
        term5Comp = HM.lookup ("post", 5) compactedMap
    assertEqual "Policy: in-progress term is untouched by CompressClosedTerms"
        term5Full term5Comp
    case term5Comp of
      Just alg -> assertEqual "Policy: in-progress term retains its redundant sequence"
                    False (EA.compress alg == alg)
      Nothing  -> assertEqual "Policy: in-progress term present" True False

-- Test 4: deletion-only (spillTo Nothing + RetainRecent) narrows the ledger to
-- the window and reduces its norm by exactly the discarded terms' norm.
testPolicyDeleteOnly :: IO ()
testPolicyDeleteOnly = do
    let pol = Policy.defaultLedgerPolicy { Policy.retain = Policy.RetainRecent 2 }
    residentJournal <- runLiteWithPolicy pol polSpec polW0 pwLedger
    let residentMap = toMap residentJournal
        residentTerms = L.sort (L.nub [ t | (_, t) <- HM.keys residentMap ])
        -- the FullAudit ledger restricted to the same window must match exactly
        -- (deletion is just a filter; the kept terms are untouched).
        full = runLite polSpec polW0 pwLedger
        windowOfFull = EJ.filterWithNote (\(_, t) _ -> t >= 4) full
    assertEqual "Policy: delete-only leaves only the window terms" [4, 5] residentTerms
    assertEqual "Policy: delete-only window equals FullAudit restricted to the window (exact)"
        (toMap windowOfFull) residentMap
    -- norm strictly drops (terms 1..3 were discarded with no spill).
    assertEqual "Policy: discarding older terms strictly reduces norm"
        True (norm residentJournal < norm full)

-- Test 5: DET — policy runs are reproducible and Sequential == ParChunk (exact).
testPolicyDeterminism :: IO ()
testPolicyDeterminism = withTempSpill "det" $ \_ -> do
    let pol = Policy.defaultLedgerPolicy { Policy.retain = Policy.RetainRecent 3 }
        specPar p = polSpec { Lite.specParallel = p }
    r1 <- runLiteWithPolicy pol (specPar Sequential) polW0 (toMap . pwLedger)
    r2 <- runLiteWithPolicy pol (specPar Sequential) polW0 (toMap . pwLedger)
    rP <- runLiteWithPolicy pol (specPar (ParChunk 2)) polW0 (toMap . pwLedger)
    assertEqual "Policy DET-1: same policy run twice is identical" r1 r2
    assertEqual "Policy DET-2: Sequential == ParChunk under policy (exact)" r1 rP

-- Test 6 (classic bridge): policySpillOptions drives the classic engine and the
-- result restores losslessly, mirroring the existing binary-spill restore test.
-- We exercise the derived chunk extraction + eviction directly (no full
-- StateSpace needed) by checking the option fields it builds.
testPolicyClassicBridge :: IO ()
testPolicyClassicBridge = withTempSpill "bridge" $ \path -> do
    -- Build a ledger spanning terms 1..3, spill terms 1..2 via the policy-derived
    -- chunk extractor, keep term 3 as the remainder, then restore == whole ledger.
    let pol = Policy.defaultLedgerPolicy
                { Policy.retain = Policy.RetainRecent 1, Policy.spillTo = Just path }
        whole :: LedgerM
        whole = EJ.fromList
            [ (1 .@ Not :< Purchases) .| ("post", 1)
            , (2 .@ Not :< Purchases) .| ("post", 2)
            , (3 .@ Not :< Purchases) .| ("post", 3) ]
        -- the option built by the bridge; we use its spillExtractChunk to carve
        -- terms 1..2 and write them, exactly as runSimulationWithSpill would.
        opts = Policy.policySpillOptions pol 2
                 (\(LedgerRef r) -> readSTRef r)
                 (\f (LedgerRef r) -> modifySTRef' r f)
                 :: ES.SpillOptions Int LedgerRef LedgerM
    -- emulate a single spill of the [1,2] chunk + eviction of term <= 2.
    ref <- LedgerRef <$> stToIO (newSTRef whole)
    chunk <- case ES.spillExtractChunk opts of
        Just extract -> stToIO (extract (1, 2) ref)
        Nothing      -> error "policySpillOptions must set spillExtractChunk"
    withFile path WriteMode $ \h -> ES.spillWriteChunk opts h (1, 2) chunk
    stToIO (ES.spillDeleteRange opts (1, 2) ref)
    let LedgerRef r0 = ref
    remainder <- stToIO (readSTRef r0)
    -- remainder is now only term 3; restore merges spill + remainder == whole.
    restored <- Policy.restoreLedger path remainder :: IO LedgerM
    assertEqual "Policy bridge: policySpillOptions chunk keeps spilled-range terms"
        (toMap (EJ.filterWithNote (\(_, t) _ -> t >= 1 && t <= 2) whole)) (toMap chunk)
    assertEqual "Policy bridge: after eviction the remainder is only the kept window"
        (toMap (EJ.filterWithNote (\(_, t) _ -> t > 2) whole)) (toMap remainder)
    assertEqual "Policy bridge: restore (spill + remainder) == whole ledger (lossless)"
        (toMap whole) (toMap restored)

-- Test 7 (HasTermAxis): termOf returns the LAST Note component for pair/triple.
testPolicyHasTermAxis :: IO ()
testPolicyHasTermAxis = do
    assertEqual "Policy HasTermAxis: pair termOf = snd" (7 :: Int) (Policy.termOf ("e", 7 :: Int))
    assertEqual "Policy HasTermAxis: triple termOf = 3rd" (9 :: Int)
        (Policy.termOf ("e1", "e2", 9 :: Int))

-- ================================================================
-- Simulate.Network tests (Phase 3, feat/trade-network)
--
-- Property + unit tests for the TradeNetwork / InputCoefficients separation,
-- the deterministic generators, the smart-constructor invariants, and the
-- edge-summation sigmaEdges. All read-outs are Ord-ascending (no hash order).
-- ================================================================

type NetJD = Journal (Int, Int) MoneyDecimal (HatBase CountUnit)

-- a tiny per-edge journal builder used by the sigmaEdges equivalence test
edgeJ :: Int -> Int -> NetJD
edgeJ i j = ((fromIntegral (i + 2 * j) :: MoneyDecimal) .@ Not :< Amount) .| (i, j)

-- Test 1: completeNetwork makes sigmaEdges coincide with the all-pairs sum.
-- "The notation is unchanged; only the set Σ runs over changes."
testNetCompleteEquiv :: IO ()
testNetCompleteEquiv = do
    let ks = [1 .. 6 :: Int]
        viaEdges = sigmaEdges (completeNetwork ks) edgeJ          :: NetJD
        viaPairs = EJ.sigma2When ks ks (/=) edgeJ                 :: NetJD
    assertEqual "Network: sigmaEdges complete == all-pairs sigma2When (exact)"
        (toMap viaPairs) (toMap viaEdges)

-- Test 2: determinism (DET-1). Same StdGen -> identical edges for every
-- generator, checked by running each twice and comparing.
testNetDeterminism :: IO ()
testNetDeterminism = do
    let ks = [1 .. 30 :: Int]
        g  = mkStdGen 42
        twice f = assertEqual ("Network DET-1: " ++ fst f) (edges (snd f g)) (edges (snd f g))
    twice ("kRegular",   \s -> kRegular   s ks 4)
    twice ("erdosRenyi", \s -> erdosRenyi s ks 0.3)
    twice ("scaleFree",  \s -> scaleFree  s ks 3)
    twice ("sectorBlock",\s -> sectorBlock s [(k, k `mod` 3) | k <- ks] (\(a,b) -> if a==b then 0.5 else 0.1))

-- Test 3: smart constructors reject the four illegal cases.
testNetSmartConstructor :: IO ()
testNetSmartConstructor = do
    assertEqual "Network: self-loop rejected"
        (Left SelfLoop) (tradeNetwork [1,2] [(1,1)] :: Either NetworkError (TradeNetwork Int))
    assertEqual "Network: duplicate edge rejected"
        (Left DuplicateEdge) (tradeNetwork [1,2] [(1,2),(1,2)] :: Either NetworkError (TradeNetwork Int))
    let Right g = tradeNetwork [1,2,3] [(1,3)] :: Either NetworkError (TradeNetwork Int)
    assertEqual "Network: coefficient outside network rejected"
        (Left CoefOutsideNetwork)
        (inputCoefficients g [(2,3,0.5)] :: Either NetworkError (InputCoefficients Int Double))
    assertEqual "Network: negative coefficient rejected"
        (Left NegativeCoefficient)
        (inputCoefficients g [(1,3,-0.5)] :: Either NetworkError (InputCoefficients Int Double))
    assertEqual "Network: duplicate coefficient rejected"
        (Left DuplicateCoefficient)
        (inputCoefficients g [(1,3,0.2),(1,3,0.3)] :: Either NetworkError (InputCoefficients Int Double))

-- Test 4: Hawkins-Simon — every buyer's column sum is strictly below 1.
testNetHawkinsSimon :: IO ()
testNetHawkinsSimon = do
    let g  = completeNetwork [1 .. 12 :: Int]
        a  = randomCoefficients (mkStdGen 11) defaultCoefOptions g :: InputCoefficients Int Double
        ok = all (\j -> sum (Prelude.map snd (inputsOf a j)) < 1.0) (nodes g)
    assertEqual "Network: randomCoefficients (hawkinsSimon) all column sums < 1" True ok

-- Test 5: generator structure.
testNetGeneratorStructure :: IO ()
testNetGeneratorStructure = do
    let ks = [1 .. 8 :: Int]
        kr = kRegular (mkStdGen 3) ks 3 :: TradeNetwork Int
    assertEqual "Network: kRegular in-degree = min k (N-1)"
        (replicate (length ks) 3)
        (Prelude.map (length . suppliersOf kr) (nodes kr))
    -- erdosRenyi p=1 == complete, p=0 == empty
    assertEqual "Network: erdosRenyi p=1 == completeNetwork edges"
        (edges (completeNetwork ks))
        (edges (erdosRenyi (mkStdGen 0) ks 1.0 :: TradeNetwork Int))
    assertEqual "Network: erdosRenyi p=0 has no edges"
        0 (edgeCount (erdosRenyi (mkStdGen 0) ks 0.0 :: TradeNetwork Int))
    -- scaleFree edge count is deterministic: C(m+1,2) + (N-m-1)*m
    let n = length ks; m = 2
        expected = (m * (m + 1) `div` 2) + (n - m - 1) * m
    assertEqual "Network: scaleFree edge count matches preferential-attachment formula"
        expected (edgeCount (scaleFree (mkStdGen 9) ks m :: TradeNetwork Int))

-- Test 6: out/in adjacency consistency on an arbitrary generated network.
-- (i,j) in edges  <=>  i in suppliersOf j  <=>  j in buyersOf i
testNetAdjacencyConsistency :: IO ()
testNetAdjacencyConsistency = do
    let ks = [1 .. 25 :: Int]
        g  = erdosRenyi (mkStdGen 77) ks 0.25 :: TradeNetwork Int
        es = edges g
        fwd = all (\(i,j) -> i `elem` suppliersOf g j && j `elem` buyersOf g i) es
        -- and the reverse: every (i,j) reconstructed from suppliersOf equals edges
        viaSuppliers = L.sort [ (i, j) | j <- nodes g, i <- suppliersOf g j ]
        viaBuyers    = L.sort [ (i, j) | i <- nodes g, j <- buyersOf g i ]
    assertEqual "Network: edges <=> suppliersOf (forward)" True fwd
    assertEqual "Network: edges == reconstruction from suppliersOf" (L.sort es) viaSuppliers
    assertEqual "Network: edges == reconstruction from buyersOf" (L.sort es) viaBuyers

-- Test 7: CSV round-trip — parse . render == id (render is a test helper).
renderEdgeCsv :: [(T.Text, T.Text)] -> T.Text
renderEdgeCsv rows = T.unlines (T.pack "from,to" : [ a <> T.pack "," <> b | (a, b) <- rows ])

renderCoefCsv :: [(T.Text, T.Text, Double)] -> T.Text
renderCoefCsv rows =
    T.unlines (T.pack "from,to,coef" :
        [ a <> T.pack "," <> b <> T.pack "," <> T.pack (show c) | (a, b, c) <- rows ])

testNetCsvRoundTrip :: IO ()
testNetCsvRoundTrip = do
    let eRows = [(T.pack "a", T.pack "b"), (T.pack "b", T.pack "c"), (T.pack "a", T.pack "c")]
    assertEqual "Network: edge CSV parse . render == id"
        (Right eRows) (parseEdgeCsv (renderEdgeCsv eRows))
    let cRows = [(T.pack "a", T.pack "b", 0.25), (T.pack "b", T.pack "c", 0.5)]
    assertEqual "Network: coef CSV parse . render == id"
        (Right cRows) (parseCoefCsv (renderCoefCsv cRows))
    -- ingestion helpers agree with the network/coefficient invariants
    let Right (g, a) = coefficientsFromTable [(1,3,0.2),(2,3,0.5)]
                         :: Either NetworkError (TradeNetwork Int, InputCoefficients Int Double)
    assertEqual "Network: coefficientsFromTable edges" [(1,3),(2,3)] (edges g)
    assertEqual "Network: coefficientsFromTable inputsOf" [(1,0.2),(2,0.5)] (inputsOf a 3)
    -- fromCoefficientMatrix drops zero cells from the support
    let m i j = if i < j then fromIntegral (i + j) else 0 :: Double
        (gm, am) = fromCoefficientMatrix [1,2,3 :: Int] m
    assertEqual "Network: fromCoefficientMatrix support drops zeros"
        [(1,2),(1,3),(2,3)] (edges gm)
    assertEqual "Network: fromCoefficientMatrix coefficient" (Just 4.0) (coefficient am 1 3)
    -- networkFromTable derives nodes from rows
    let Right gt = networkFromTable [(1,2),(2,3)] :: Either NetworkError (TradeNetwork Int)
    assertEqual "Network: networkFromTable derives node set" [1,2,3] (nodes gt)

-- ================================================================
-- MarketModel equivalence tests (Phase 5, feat/market-scale-experiments)
--
-- The examples/market/MarketModel.hs core cannot be imported here (it declares
-- an orphan `instance StateTime Int` that would clash with the SICE harness's
-- `instance StateTime SimTerm`), so the trade simple/tuned stages and a small
-- BSP world are re-stated minimally (per the Phase 5 plan §2 commit 3 note).
-- We check the two properties the plan puts in CI:
--   (a) tradeStageSimple ≡ tradeStageTuned, EXACTLY, under MoneyDecimal;
--   (b) Sequential ≡ ParChunk (DET-2) for the whole 3-stage model, exactly.
-- (Perf ratios are out of CI; they live in run-market-experiments.sh.)
-- ================================================================

-- 4-axis base (AccountTitles, owner, counterparty, CountUnit), mirroring
-- MarketModel.MBase. It is exactly the SICE harness's SimHatBase2, so we reuse
-- that type (and its ExBaseClass / Element Int / BaseClass Int instances)
-- instead of re-declaring them.
type MktFirm  = SimCompany           -- = Int
type MktNote  = (String, Int)
type MktBase  = SimHatBase2          -- = HatBase (AccountTitles, Int, Int, CountUnit)
type MktLedgM = Journal MktNote MoneyDecimal MktBase

data MktW v f = MktW
  { mkLedger :: HK f (Journal MktNote v MktBase)
  , mkNet    :: HK f (TradeNetwork MktFirm)
  , mkCoef   :: HK f (InputCoefficients MktFirm v)
  } deriving Generic

-- own-product classifier shared by the mirror.
mktOwnerOfProduct :: BasePart MktBase -> Maybe MktFirm
mktOwnerOfProduct bp = case bp of
    (Products, o, c, _) | o == c -> Just o
    _                            -> Nothing

-- opening stock read from the ("carryover", t) note (indexed per-note),
-- mirroring MarketModel.openingMap (carryover-based O(term) inventory).
mktOpening :: (HatVal v, Real v)
           => Int -> Journal MktNote v MktBase -> M.Map MktFirm v
mktOpening t ledger =
    EA.balanceMapBy mktOwnerOfProduct
        (EJ.toAlg (EJ.projWithNote [("carryover", t)] ledger))

-- inventory-connected demand from opening stock, mirroring MarketModel.demandMap.
mktDemand :: (HatVal v, Real v)
          => Double -> Int -> [MktFirm] -> Journal MktNote v MktBase -> M.Map MktFirm Double
mktDemand target t fs ledger =
    let opening = mktOpening t ledger
    in M.fromList
         [ (j, max 0 (target - realToFrac (M.findWithDefault 0 j opening)))
         | j <- fs ]

mktPurchase :: (HatVal v) => v -> MktFirm -> MktFirm -> EA.Alg v MktBase
mktPurchase amt i j =
       amt .@ Not :< (Products,  j, j, Amount)
  .+   amt .@ Hat :< (Cash,      j, j, Yen)
  .+   amt .@ Not :< (Purchases, j, j, Yen)
  .+   amt .@ Not :< (Cash,      i, i, Yen)
  .+   amt .@ Not :< (Sales,     i, i, Yen)
  .+   amt .@ Hat :< (Products,  i, i, Amount)

mktOrderAmt :: (HatVal v, Real v)
            => InputCoefficients MktFirm v -> M.Map MktFirm Double -> MktFirm -> MktFirm -> Double
mktOrderAmt coef dm i j =
    realToFrac (maybe 0 id (coefficient coef i j)) * M.findWithDefault 0 j dm

-- per-firm trade stage (stageFor over the firm list), mirroring
-- MarketModel.tradeStageSimple: buyer j folds its in-edges (suppliersOf).
mktTradeSimple :: (HatVal v, Real v) => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktTradeSimple fs target = stageFor "trade" fs $ \w t _g j ->
    let net = mkNet w; coef = mkCoef w
        dm  = mktDemand target t fs (mkLedger w)
        sup = suppliersOf net j
        one i = let amt = realToFrac (mktOrderAmt coef dm i j)
                in if amt <= 0 then mempty else mktPurchase amt i j
        alg = EA.sigma sup one
    in if EA.isZero alg then mempty else alg .| ("trade", t)

mktTradeTuned :: (HatVal v, Real v) => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktTradeTuned fs target = stageFor "trade" fs $ \w t _g j ->
    let net = mkNet w; coef = mkCoef w
        dm  = mktDemand target t fs (mkLedger w)
        sup = suppliersOf net j
        accum = L.foldl' step M.empty sup
        step acc i =
            let amt = realToFrac (mktOrderAmt coef dm i j)
            in if amt <= 0 then acc
               else L.foldl' (\m (b, v) -> M.insertWith (+) b v m) acc
                      [ (Not :< (Products,  j, j, Amount), amt)
                      , (Hat :< (Cash,      j, j, Yen),    amt)
                      , (Not :< (Purchases, j, j, Yen),    amt)
                      , (Not :< (Cash,      i, i, Yen),    amt)
                      , (Not :< (Sales,     i, i, Yen),    amt)
                      , (Hat :< (Products,  i, i, Amount), amt) ]
        alg = EA.sigmaFromMap accum (\b v -> v .@ b)
    in if EA.isZero alg then mempty else alg .| ("trade", t)

mktProduction :: (HatVal v, Real v) => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktProduction fs target = stageFor "production" fs $ \w t _g j ->
    let dm  = mktDemand target t fs (mkLedger w)
        amt = realToFrac (M.findWithDefault 0 j dm)
    in if amt <= 0 then mempty
       else ((amt .@ Hat :< (Products,  j, j, Amount))
          .+ (amt .@ Not :< (SalesCost, j, j, Yen)))
            .| ("production", t)

mktReport :: (HatVal v) => Stage (MktW v) Int MktNote v MktBase
mktReport = stage "report" $ \w t ->
    let flow = EJ.toAlg (EJ.projWithNote [("trade", t), ("production", t)] (mkLedger w))
        shortageK b = case b of
            Hat :< (Products, o, c, _) | o == c -> Just o
            _                                   -> Nothing
        sh = EA.postFromNetBy shortageK (\j v -> v .@ Not :< (Products, j, j, Amount)) flow
    in if EA.isZero sh then mempty else sh .| ("report", t)

-- carryover stage (mirror): net this term's own-product stock and roll the
-- positive surplus into ("carryover", t+1). Mirrors MarketModel.carryoverStage.
mktCarryover :: (HatVal v, Real v) => Stage (MktW v) Int MktNote v MktBase
mktCarryover = stage "closing" $ \w t ->
    let termAlg = EJ.toAlg (EJ.filterByAxis 1 (NoteAxisKey (t :: Int)) (mkLedger w))
        netMap  = EA.balanceMapBy mktOwnerOfProduct termAlg
        perFirm (j, v) =
            if v <= 0 then mempty
            else ((v .@ Hat :< (Products, j, j, Amount)) .| ("closing",   t))
              <> ((v .@ Not :< (Products, j, j, Amount)) .| ("carryover", t + 1))
    in mconcat [ perFirm kv | kv <- M.toList netMap ]

-- a fixed small (G, A) used by both equivalence tests.
mktBuild :: (HatVal v) => Int -> (TradeNetwork MktFirm, InputCoefficients MktFirm v)
mktBuild n =
    let (gG, gA) = split (mkStdGen 2025)
        fs  = [1 .. n]
        net = erdosRenyi gG fs 0.3
        a   = randomCoefficients gA defaultCoefOptions net
    in (net, a)

mktSpec :: (HatVal v, Real v)
        => Bool -> Int -> Int -> Par -> SimSpec (MktW v) Int MktNote v MktBase
mktSpec tuned n lastT par =
    let fs = [1 .. n] in
    (mkSimSpec (1, lastT) 2025 mkLedger
        [ (if tuned then mktTradeTuned else mktTradeSimple) fs 10
        , mktProduction fs 10
        , mktReport
        , mktCarryover ])
      { Lite.specParallel = par }

mktW0 :: (HatVal v) => Int -> MktW v InitT
mktW0 n = let (net, a) = mktBuild n
          in MktW { mkLedger = carry mempty, mkNet = carry net, mkCoef = carry a }

-- (a) simple ≡ tuned, exactly, under MoneyDecimal (N=30, T=5).
-- the redundant-algebra-correct "same result": net each note's Alg per base
-- ('bar' drops the cancelled part and any zero-padding), keeping the Hat/Not
-- side. simple and tuned differ ONLY in seq redundancy (simple keeps the
-- per-edge posting sequence; tuned pre-sums per base), so they are equal exactly
-- after netting. (norm additivity already holds; this is the stronger per-base
-- exact check.)
nettedMktMap :: Journal MktNote MoneyDecimal MktBase
             -> HM.HashMap MktNote (EA.Alg MoneyDecimal MktBase)
nettedMktMap = toMap . EJ.map EA.bar

testMarketSimpleTunedEqual :: IO ()
testMarketSimpleTunedEqual = do
    let simpleL = runLite (mktSpec False 30 5 Sequential) (mktW0 30) mkLedger
                    :: MktLedgM
        tunedL  = runLite (mktSpec True  30 5 Sequential) (mktW0 30) mkLedger
    assertEqual "Market: tradeStageSimple == tradeStageTuned (MoneyDecimal, exact per-base net)"
        (nettedMktMap simpleL) (nettedMktMap tunedL)
    -- gross volume must also agree: bar-equality alone cannot detect an
    -- accidental early Hat/Not netting in the tuned path (bar is idempotent,
    -- but the pre-bar norm would shrink). norm pins the gross posting volume.
    assertEqual "Market: simple/tuned gross volume (norm) agrees (no early netting)"
        (norm simpleL) (norm tunedL)

-- (b) DET-2: Sequential ≡ ParChunk, exactly, under MoneyDecimal (simple path).
testMarketSeqParEqual :: IO ()
testMarketSeqParEqual = do
    let seqM = runLite (mktSpec False 30 5 Sequential)   (mktW0 30) (toMap . mkLedger)
                 :: HM.HashMap MktNote (EA.Alg MoneyDecimal MktBase)
        parM = runLite (mktSpec False 30 5 (ParChunk 8)) (mktW0 30) (toMap . mkLedger)
    assertEqual "Market DET-2: Sequential == ParChunk (MoneyDecimal, exact)"
        seqM parM

-- (c) sanity: the report's net shortage is strictly positive (Hawkins-Simon),
-- and the complete-network setting also runs (a participating-set sanity).
testMarketShortagePositive :: IO ()
testMarketShortagePositive = do
    let finalSh = runLite (mktSpec False 24 4 Sequential) (mktW0 24)
                    (\final -> norm (EJ.projWithNote [("report", 4)] (mkLedger final)))
                    :: MoneyDecimal
    assertEqual "Market: final-term net shortage is strictly positive (Hawkins-Simon)"
        True (finalSh > 0)
    -- complete network on a tiny N just exercises the dense edge set end-to-end.
    let (gG, gA) = split (mkStdGen 2025)
        cnet     = completeNetwork [1 .. 8 :: MktFirm]
        ccoef    = randomCoefficients gA defaultCoefOptions cnet :: InputCoefficients MktFirm MoneyDecimal
        cw0      = MktW { mkLedger = carry mempty, mkNet = carry cnet, mkCoef = carry ccoef }
        cfs      = [1 .. 8 :: MktFirm]
        cspec    = (mkSimSpec (1, 3) 2025 mkLedger
                      [ mktTradeSimple cfs 10, mktProduction cfs 10, mktReport, mktCarryover ])
        cNorm    = runLite cspec cw0 (norm . mkLedger) :: MoneyDecimal
        _        = gG
    assertEqual "Market: complete-network run produces a positive ledger norm"
        True (cNorm > 0)

-- (d) WINDOW-TRANSPARENCY SENTINEL (Phase 5 fix, modification 3):
-- the carryover bookkeeping makes the model self-contained per term, so a
-- RetainRecent window must NOT change the observable result. Assert that
-- RetainRecent 2 (+ spill) and RetainAll produce the EXACT SAME final-term
-- report norm AND final carryover map (MoneyDecimal, so equality is exact).
-- This permanently guards against the bug this round fixed (a full-ledger
-- inventory sweep silently re-reading a window-truncated net: 9974.74 vs
-- 9993.56). N=40, T=8 so the window (2) is strictly smaller than the history.
testMarketWindowTransparent :: IO ()
testMarketWindowTransparent = withTempSpill "market_window" $ \path -> do
    let n = 40; lastT = 8
        spec = mktSpec False n lastT Sequential
        w0   = mktW0 n
        -- project the two observables we pin: the final-term report norm and the
        -- final carryover map (the next-term opening, keyed by firm).
        project final =
            let lj = mkLedger final :: MktLedgM
                reportN = norm (EJ.projWithNote [("report", lastT)] lj) :: MoneyDecimal
                carryM  = EA.balanceMapBy mktOwnerOfProduct
                            (EJ.toAlg (EJ.projWithNote [("carryover", lastT + 1)] lj))
                          :: M.Map MktFirm MoneyDecimal
            in (reportN, carryM)
        polAll = Policy.defaultLedgerPolicy { Policy.retain = Policy.RetainAll }
        polWin = Policy.defaultLedgerPolicy
                   { Policy.retain = Policy.RetainRecent 2, Policy.spillTo = Just path }
    (allN, allM) <- runLiteWithPolicy polAll spec w0 project
    (winN, winM) <- runLiteWithPolicy polWin spec w0 project
    assertEqual "Market window-transparency: final report norm equal under RetainAll vs RetainRecent 2 + spill"
        allN winN
    assertEqual "Market window-transparency: final carryover map equal under RetainAll vs RetainRecent 2 + spill"
        allM winM

main :: IO ()
main = do
    testProjMultiPatternOnePass
    testProjNormFastPath
    testProjWithBaseNorm
    testProjWithNoteNorm
    testBasesNotSideRegression
    testNumericToleranceScaleAware
    testMoneyDecimalExactOrderIndependent
    testSigmaMergePath
    testSigma2When
    testSigmaFromMap
    testJournalFromListStrict
    testUnionZeroSingletonBase
    testScalarRejectsNegative
    testProjConcreteNoIndexForce
    testJournalSigmaMergePath
    testJournalSigma2When
    testJournalSigmaOn
    testJournalSigmaOnFromMap
    testFilterByAxisEquivalent
    testFilterByAxisWithDeltaUpdates
    testFinalStockTransferAlgEquivalence
    testFinalStockTransferJournalEquivalence
    testRestoreJournalFromBinarySpill
    testSimulateEx1Default
    testCsvTranspose
    testCsvWriteCSV
    testCsvWriteCSVWithQuotes
    testCsvWriteCSVEmpty
    testLiteBoilerplate
    testLiteDet2
    testLiteDet1
    testLiteBspInvisibility
    testLiteGateEquivalence
    testLiteFieldRules
    testLiteBoundaryOncePerTerm
    testPolicyEquivalence
    testPolicyWindowRoundTrip
    testPolicyCompressClosed
    testPolicyDeleteOnly
    testPolicyDeterminism
    testPolicyClassicBridge
    testPolicyHasTermAxis
    testNetCompleteEquiv
    testNetDeterminism
    testNetSmartConstructor
    testNetHawkinsSimon
    testNetGeneratorStructure
    testNetAdjacencyConsistency
    testNetCsvRoundTrip
    testMarketSimpleTunedEqual
    testMarketSeqParEqual
    testMarketShortagePositive
    testMarketWindowTransparent
    axiomProperties
    journalProperties
    quotientProperties
