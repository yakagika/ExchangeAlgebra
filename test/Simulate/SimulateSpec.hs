{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Classic and Lite engines, ledger policies, spill, and market models.
-- Tests exercise the Simulation layer using the shared Support fixtures.
-- Start with 'runTests' for the suite's execution order.
module Simulate.SimulateSpec (runTests) where

import ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Journal  as EJ
import qualified ExchangeAlgebra.Journal.Transfer as EJT
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import qualified ExchangeAlgebra.Simulate as ES
import ExchangeAlgebra.Simulate
import qualified ExchangeAlgebra.Simulate.Lite as Lite
import ExchangeAlgebra.Simulation.Network (TradeNetwork
                                          , InputCoefficients
                                          , suppliersOf
                                          , coefficient
                                          , completeNetwork
                                          , erdosRenyi
                                          , defaultCoefOptions
                                          , randomCoefficients
                                          )
import ExchangeAlgebra.Simulate.Lite (InitT
                                     , SnapT
                                     , HK
                                     , Field(..)
                                     , carry
                                     , resetEach
                                     , updateEach
                                     , Stage
                                     , stage
                                     , stageFor
                                     , stageOf
                                     , Par(..)
                                     , SimSpec
                                     , mkSimSpec
                                     , runLite
                                     , runLiteFold
                                     , runLiteWithPolicy
                                     , runLiteWithPolicyObs
                                     )
import qualified ExchangeAlgebra.Simulate.Policy as Policy
import ExchangeAlgebra.Algebra.Value (MoneyDouble)
import ExchangeAlgebra.Write (restoreJournalFromBinarySpill, restoreJournalFromBinarySpillChecked)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
import qualified Data.List           as L
import qualified Data.Binary         as Binary
import qualified Data.ByteString.Lazy as BL
import Control.Monad (forM_)
import Control.Monad.ST (ST, stToIO)
import Data.Array.ST (STArray)
import Data.STRef (STRef
                  , newSTRef
                  , readSTRef
                  , modifySTRef'
                  )
import Data.IORef (newIORef, readIORef, modifyIORef')
import System.Exit (exitFailure)
import System.IO (IOMode(WriteMode), withFile)
import System.Directory (removeFile)
import System.Random (StdGen
                     , mkStdGen
                     , randomR
                     , split
                     )
import Control.Monad (replicateM)
import Control.Monad.State (runState, state)
import Control.Exception (try, ErrorCall, SomeException)
import GHC.Generics (Generic)
import Support (TestComparison(..)
               , assertEqual
               , assertNear
               , runTestComparison
               , removeSpillTestFile
               , SimTerm
               , SimCompany
               , SimHatBase2
               , withTestTemporaryFile
               )

type SpillRestoreJournal = EJ.Journal (String, Int) Double (HatBase CountUnit)

-- | Design-review C4: the spill/eviction decision logic is single-sourced in
-- 'ES.stepBackWith' / 'ES.spillDeleteDecision' (previously duplicated inline
-- in the classic spill loop and in Lite's retention loop). Pin the decision
-- table and the equivalence with Lite's former @backByTerms@.
testSpillDecisionSingleSource :: IO ()
testSpillDecisionSingleSource = do
    assertEqual "stepBackWith pred 3 10" (7 :: Int) (ES.stepBackWith pred 3 10)
    assertEqual "stepBackWith is id for n <= 0" (10 :: Int) (ES.stepBackWith pred 0 10)
    assertEqual "stepBackWith is id for negative n" (10 :: Int) (ES.stepBackWith pred (-1) 10)
    assertEqual "NoDelete evicts nothing"
        Nothing (ES.spillDeleteDecision pred (ES.NoDelete :: ES.SpillDeletePolicy Int) (1, 10))
    assertEqual "DeleteSpilledChunk evicts exactly the chunk"
        (Just (1, 10)) (ES.spillDeleteDecision pred ES.DeleteSpilledChunk (1 :: Int, 10))
    assertEqual "KeepRecentTerms 3 keeps the trailing window"
        (Just (1, 7)) (ES.spillDeleteDecision pred (ES.KeepRecentTerms 3) (1 :: Int, 10))
    assertEqual "KeepRecentTerms covering the chunk evicts nothing"
        Nothing (ES.spillDeleteDecision pred (ES.KeepRecentTerms 12) (1 :: Int, 10))
    -- Lite boundary equivalence: former backByTerms w t == stepBackWith pred w t
    let backByTermsRef w t = let go n x | n <= (0 :: Int) = x
                                        | otherwise       = go (n - 1) (pred x)
                             in go w t
    forM_ [(0, 5), (1, 5), (3, 5), (7, 5)] $ \(w, t) ->
        assertEqual ("Lite boundary equivalence w=" ++ show w)
            (backByTermsRef w t) (ES.stepBackWith pred w (t :: Int))

writeSpillTestChunks
    :: FilePath
    -> [((Int, Int), SpillRestoreJournal)]
    -> IO ()
writeSpillTestChunks path chunks = do
    removeSpillTestFile path
    withFile path WriteMode $ \h ->
        forM_ chunks $ \(termRange, chunk) ->
            ES.defaultBinarySpillWriter h termRange chunk

spillCheckedChunk1 :: SpillRestoreJournal
spillCheckedChunk1 = EJ.fromList
    [ (1 .@ (Hat :< Yen)) .| ("A", 1)
    , (2 .@ (Not :< Amount)) .| ("B", 2)
    ]

spillCheckedChunk2 :: SpillRestoreJournal
spillCheckedChunk2 = (3 .@ (Hat :< Yen)) .| ("C", 3)

spillCheckedCurrent :: SpillRestoreJournal
spillCheckedCurrent = EJ.fromList
    [ (4 .@ (Not :< Amount)) .| ("Tail", 4)
    , (8 .@ (Hat :< Yen)) .| ("AlreadySpilled", 2)
    ]

spillCheckedExpected :: SpillRestoreJournal
spillCheckedExpected =
    spillCheckedChunk1 .+ spillCheckedChunk2
    .+ ((4 .@ (Not :< Amount)) .| ("Tail", 4))

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

simFstC, simLastC :: SimCompany
simFstC = 1
simLastC = 6

simCompanies :: [SimCompany]
simCompanies = [simFstC .. simLastC]

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

-- Test 5: policy execution preserves exact Sequential / ParChunk parity.
testPolicyDeterminism :: IO ()
testPolicyDeterminism = withTempSpill "det" $ \_ -> do
    let pol = Policy.defaultLedgerPolicy { Policy.retain = Policy.RetainRecent 3 }
        specPar p = polSpec { Lite.specParallel = p }
    r1 <- runLiteWithPolicy pol (specPar Sequential) polW0 (toMap . pwLedger)
    rP <- runLiteWithPolicy pol (specPar (ParChunk 2)) polW0 (toMap . pwLedger)
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

-- ADT event tag mirroring MarketModel.MTag (typo'd tags become compile errors,
-- not silently-empty projections). 'MktPlank' is the explicit blank tag.
data MktTag = MktPlank | MktTrade | MktProduction | MktReport | MktClosing | MktCarryover
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance Hashable MktTag
-- needed so the spill / runLiteWithPolicy window-transparency test can serialize
-- a @Journal MktNote v b@ (derived structurally from Generic).
instance Binary.Binary MktTag
instance Note MktTag where
    plank = MktPlank

type MktNote  = (MktTag, Int)
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

-- opening stock read from the (MktCarryover, t) note (indexed per-note),
-- mirroring MarketModel.openingMap (carryover-based O(term) inventory).

-- single-firm opening read (indexed per-note + per-base), mirroring
-- MarketModel.openingOf: balanceBy over firm j's own-product base only.
mktOpeningOf :: (HatVal v, Real v)
             => Int -> MktFirm -> Journal MktNote v MktBase -> v
mktOpeningOf t j ledger =
    EA.balanceBy [Not :< (Products, j, j, Amount)]
                 [Hat :< (Products, j, j, Amount)]
                 (EJ.toAlg (EJ.projWithNote [(MktCarryover, t)] ledger))

-- single-firm inventory-connected demand, mirroring MarketModel.demandOf.
mktDemandOf :: (HatVal v, Real v)
            => Double -> Int -> MktFirm -> Journal MktNote v MktBase -> Double
mktDemandOf target t j ledger =
    max 0 (target - realToFrac (mktOpeningOf t j ledger))

mktPurchase :: (HatVal v) => v -> MktFirm -> MktFirm -> EA.Alg v MktBase
mktPurchase amt i j =
       amt .@ Not :< (Products,  j, j, Amount)
  .+   amt .@ Hat :< (Cash,      j, j, Yen)
  .+   amt .@ Not :< (Purchases, j, j, Yen)
  .+   amt .@ Not :< (Cash,      i, i, Yen)
  .+   amt .@ Not :< (Sales,     i, i, Yen)
  .+   amt .@ Hat :< (Products,  i, i, Amount)

mktOrderAmt :: (HatVal v, Real v)
            => InputCoefficients MktFirm v -> Double -> MktFirm -> MktFirm -> Double
mktOrderAmt coef d i j =
    realToFrac (maybe 0 id (coefficient coef i j)) * d

-- per-firm trade stage (stageFor over the firm list), mirroring
-- MarketModel.tradeStageSimple: buyer j folds its in-edges (suppliersOf).
mktTradeSimple :: (HatVal v, Real v) => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktTradeSimple fs target = stageOf MktTrade fs $ \w t _g j ->
    -- single-note stage: emit the bare Alg; the runner attaches (MktTrade, t).
    let net = mkNet w; coef = mkCoef w
        d   = mktDemandOf target t j (mkLedger w)
        sup = suppliersOf net j
        one i = let amt = realToFrac (mktOrderAmt coef d i j)
                in if amt <= 0 then mempty else mktPurchase amt i j
    in EA.sigma sup one

mktTradeTuned :: (HatVal v, Real v) => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktTradeTuned fs target = stageOf MktTrade fs $ \w t _g j ->
    -- single-note stage: see 'mktTradeSimple'.
    let net = mkNet w; coef = mkCoef w
        d   = mktDemandOf target t j (mkLedger w)
        sup = suppliersOf net j
        accum = L.foldl' step M.empty sup
        step acc i =
            let amt = realToFrac (mktOrderAmt coef d i j)
            in if amt <= 0 then acc
               else L.foldl' (\m (b, v) -> M.insertWith (+) b v m) acc
                      [ (Not :< (Products,  j, j, Amount), amt)
                      , (Hat :< (Cash,      j, j, Yen),    amt)
                      , (Not :< (Purchases, j, j, Yen),    amt)
                      , (Not :< (Cash,      i, i, Yen),    amt)
                      , (Not :< (Sales,     i, i, Yen),    amt)
                      , (Hat :< (Products,  i, i, Amount), amt) ]
    in EA.sigmaFromMap accum (\b v -> v .@ b)

mktProduction :: (HatVal v, Real v) => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktProduction fs target = stageOf MktProduction fs $ \w t _g j ->
    -- single-note stage: emit the bare Alg; the runner attaches (MktProduction, t).
    let amt = realToFrac (mktDemandOf target t j (mkLedger w))
    in if amt <= 0 then mempty
       else (amt .@ Hat :< (Products,  j, j, Amount))
         .+ (amt .@ Not :< (SalesCost, j, j, Yen))

mktReport :: (HatVal v) => Stage (MktW v) Int MktNote v MktBase
mktReport = stageOf MktReport [()] $ \w t _g () ->
    -- single-note aggregate stage: emit the bare Alg; runner attaches (MktReport, t).
    let flow = EJ.toAlg (EJ.projWithNote [(MktTrade, t), (MktProduction, t)] (mkLedger w))
        shortageK b = case b of
            Hat :< (Products, o, c, _) | o == c -> Just o
            _                                   -> Nothing
    in EA.postFromNetBy shortageK (\j v -> v .@ Not :< (Products, j, j, Amount)) flow

-- carryover stage (mirror): net this term's own-product stock and roll the
-- positive surplus into (MktCarryover, t+1). Mirrors MarketModel.carryoverStage.
mktCarryover :: (HatVal v, Real v) => Stage (MktW v) Int MktNote v MktBase
mktCarryover = stage "closing" $ \w t ->
    let termAlg = EJ.toAlg (EJ.filterByAxis 1 (NoteAxisKey (t :: Int)) (mkLedger w))
        netMap  = EA.balanceMapBy mktOwnerOfProduct termAlg
        perFirm (j, v) =
            if v <= 0 then mempty
            else ((v .@ Hat :< (Products, j, j, Amount)) .| (MktClosing,   t))
              <> ((v .@ Not :< (Products, j, j, Amount)) .| (MktCarryover, t + 1))
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
-- ('bar' drops the canceled part and any zero-padding), keeping the Hat/Not
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
                    (\final -> norm (EJ.projWithNote [(MktReport, 4)] (mkLedger final)))
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
                reportN = norm (EJ.projWithNote [(MktReport, lastT)] lj) :: MoneyDecimal
                carryM  = EA.balanceMapBy mktOwnerOfProduct
                            (EJ.toAlg (EJ.projWithNote [(MktCarryover, lastT + 1)] lj))
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

-- (e) stageOf AUTO-NOTE SENTINEL: a 'stageOf' stage and the equivalent manual
-- 'stageFor' that writes @.| (tag, t)@ itself must produce the EXACT SAME ledger
-- (MoneyDecimal, so equality is exact). This pins the semantics of the runner's
-- single auto-attachment of @(stTag, t)@: moving the note from the stage body
-- into 'runStage' changes nothing observable (incl. the zero-drop at the sigma
-- commit). The manual stages below are byte-for-byte the bodies of the migrated
-- mirror stages, but tagged explicitly with the OLD @if isZero then mempty@ form.
mktTradeSimpleManual :: (HatVal v, Real v)
                     => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktTradeSimpleManual fs target = stageFor "trade" fs $ \w t _g j ->
    let net = mkNet w; coef = mkCoef w
        d   = mktDemandOf target t j (mkLedger w)
        sup = suppliersOf net j
        one i = let amt = realToFrac (mktOrderAmt coef d i j)
                in if amt <= 0 then mempty else mktPurchase amt i j
        alg = EA.sigma sup one
    in if EA.isZero alg then mempty else alg .| (MktTrade, t)

mktProductionManual :: (HatVal v, Real v)
                    => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktProductionManual fs target = stageFor "production" fs $ \w t _g j ->
    let amt = realToFrac (mktDemandOf target t j (mkLedger w))
    in if amt <= 0 then mempty
       else ((amt .@ Hat :< (Products,  j, j, Amount))
          .+ (amt .@ Not :< (SalesCost, j, j, Yen)))
            .| (MktProduction, t)

mktReportManual :: (HatVal v) => Stage (MktW v) Int MktNote v MktBase
mktReportManual = stage "report" $ \w t ->
    let flow = EJ.toAlg (EJ.projWithNote [(MktTrade, t), (MktProduction, t)] (mkLedger w))
        shortageK b = case b of
            Hat :< (Products, o, c, _) | o == c -> Just o
            _                                   -> Nothing
        sh = EA.postFromNetBy shortageK (\j v -> v .@ Not :< (Products, j, j, Amount)) flow
    in if EA.isZero sh then mempty else sh .| (MktReport, t)

-- the same 4-stage spec as 'mktSpec' but with the three single-note stages
-- expressed via manual stageFor + explicit @.| (tag, t)@ (carryover unchanged).
mktSpecManual :: (HatVal v, Real v)
              => Int -> Int -> Par -> SimSpec (MktW v) Int MktNote v MktBase
mktSpecManual n lastT par =
    let fs = [1 .. n] in
    (mkSimSpec (1, lastT) 2025 mkLedger
        [ mktTradeSimpleManual fs 10
        , mktProductionManual fs 10
        , mktReportManual
        , mktCarryover ])
      { Lite.specParallel = par }

testMarketStageOfAutoNote :: IO ()
testMarketStageOfAutoNote = do
    let stageOfL = runLite (mktSpec False 30 5 Sequential) (mktW0 30) (toMap . mkLedger)
                     :: HM.HashMap MktNote (EA.Alg MoneyDecimal MktBase)
        manualL  = runLite (mktSpecManual    30 5 Sequential) (mktW0 30) (toMap . mkLedger)
    assertEqual "Market stageOf auto-note: stageOf ledger == manual stageFor + .| (tag, t) (MoneyDecimal, exact)"
        stageOfL manualL


-- | Pin the two toy-model norms at 906 and 3300.
testLiteToyModels :: IO ()
testLiteToyModels = do
    let cases = concat
            [ let
                  w0 = MiniW { mwLedger = carry mempty
                             , mwPrice  = carry 10
                             , mwTax    = carry 0.1 }
                  n  = runLite miniSpec w0 (realToFrac . norm . mwLedger)
              in
                  [ NearComparison "Lite: boilerplate mini-model runs (norm)" 906.0 n
                  ]
            , let
                  w0 = GateW { gwLedger = carry mempty, gwPrice = carry 10 }
                  n  = runLite gateSpec w0 (realToFrac . norm . gwLedger)
              in
                  [ NearComparison "Lite: gate toy-model equivalence (norm 3300)" 3300.0 n
                  ]
            ]
    forM_ cases runTestComparison


-- | Check Field rules and one boundary update per term across two stages.
testLiteFieldBoundaries :: IO ()
testLiteFieldBoundaries = do
    let cases = concat
            [ [ NearComparison "Lite Field: Carry keeps the value" 60.0 (runRule (carry 10))
                  , NearComparison
                        "Lite Field: ResetEach restores each term"
                        30.0 (runRule (resetEach 5))
                  , NearComparison "Lite Field: UpdateEach applies the step each boundary"
                        140.0 (runRule (updateEach 10 (* 2)))
                  ]
            ,     [ NearComparison "Lite: term boundary fires once per term (2 stages)" 280.0
                        (let spec2 = mkSimSpec (1, 3) 0 rwLedger [ruleStage, ruleStage]
                             w0 = RuleW { rwLedger = carry mempty, rwPrice = updateEach 10 (* 2) }
                         in realToFrac (runLite spec2 w0 (norm . rwLedger)))
                  ]
            ]
    forM_ cases runTestComparison


-- | Check observer visits, committed postings, and pre-update Field values.
testLiteObserverBoundaries :: IO ()
testLiteObserverBoundaries = do
    let cases :: [(String, IO ())]
        cases =
            [ ("testLiteObserverTerms",
                forM_ [(1, 5), (3, 6), (4, 4), (4, 3)] $ \range@(lo, hi) -> do
                    let spec = polSpec { Lite.specTerms = range }
                        terms = runLiteFold (\t _ acc -> t : acc) [] spec polW0
                                    (\acc _ -> reverse acc)
                    assertEqual "Lite fold observer: term order and count" [lo .. hi] terms
                    seen <- newIORef []
                    _ <- runLiteWithPolicyObs (\t _ -> modifyIORef' seen (t :))
                             Policy.defaultLedgerPolicy spec polW0 (toMap . pwLedger)
                    observed <- reverse <$> readIORef seen
                    assertEqual "Lite IO observer: term order and count" [lo .. hi] observed
              )
            , ("testLiteObserverBoundary", do
                let w0 = MiniW { mwLedger = carry mempty
                               , mwPrice  = updateEach 10 (* 2)
                               , mwTax    = carry 0.1 }
                    snapshots = runLiteFold (\t w acc -> (t, w) : acc) [] miniSpec w0
                                    (\acc _ -> reverse acc)
                    check :: (Int, MiniW SnapT) -> IO ()
                    check (t, w) = do
                        let ledger = toMap (mwLedger w)
                            price  = 10 * (2 ^ (t - 1)) :: MoneyDouble
                            keys   = L.sort [(tag, u) | u <- [1 .. t], tag <- ["buy", "tax"]]
                        assertEqual "Lite observer: all committed notes, no future notes"
                            keys (L.sort (HM.keys ledger))
                        assertEqual "Lite observer: price before Field update" price (mwPrice w)
                        assertEqual "Lite observer: carried tax" 0.1 (mwTax w)
                        assertNear "Lite observer: current buy stage committed"
                            (realToFrac (30 * price))
                            (maybe 0 (realToFrac . norm) (HM.lookup ("buy", t) ledger))
                        assertNear "Lite observer: current final stage committed"
                            (realToFrac (2 * price * realToFrac (mwTax w)))
                            (maybe 0 (realToFrac . norm) (HM.lookup ("tax", t) ledger))
                forM_ snapshots check
                seen <- newIORef []
                finalPrice <- runLiteWithPolicyObs
                    (\t w -> modifyIORef' seen ((t, w) :))
                    Policy.defaultLedgerPolicy miniSpec w0 mwPrice
                ioSnapshots <- reverse <$> readIORef seen
                assertEqual "Lite IO observer: all boundaries saved" [1, 2, 3] (L.map fst ioSnapshots)
                forM_ ioSnapshots check
                assertEqual "Lite continuation: final Field update has fired" 80 finalPrice
              )
            ]
    forM_ cases $ \(_, check) -> check

-- | Check observer equivalence and streaming with retention windows zero and two.
testLiteObserverPolicyCases :: IO ()
testLiteObserverPolicyCases = do
    let cases :: [(String, IO ())]
        cases =
            [ ("testLiteObserverEquivalence", do
                let w0 = MiniW { mwLedger = carry mempty
                               , mwPrice  = updateEach 10 (* 2)
                               , mwTax    = carry 0.1 }
                    project w = (toMap (mwLedger w), mwPrice w, mwTax w)
                    legacy = runLite miniSpec w0 id
                    folded = runLiteFold (\_ _ a -> a) () miniSpec w0 (\_ w -> w)
                assertEqual "Lite fold: unchanged final snapshot" (project legacy) (project folded)
                full <- runLiteWithPolicy Policy.defaultLedgerPolicy miniSpec w0 project
                observed <- runLiteWithPolicyObs (\_ _ -> pure ())
                                Policy.defaultLedgerPolicy miniSpec w0 project
                assertEqual "Lite IO observer: unchanged full world" full observed
                forM_ [Policy.RetainAll, Policy.RetainRecent 2] $ \retention ->
                    withTempSpill "observer_legacy" $ \oldPath ->
                    withTempSpill "observer_new" $ \newPath -> do
                        let policy path = Policy.defaultLedgerPolicy
                                { Policy.retain  = retention
                                , Policy.spillTo = Just path }
                        old <- runLiteWithPolicy (policy oldPath) polSpec polW0 pwLedger
                        new <- runLiteWithPolicyObs (\_ _ -> pure ())
                                   (policy newPath) polSpec polW0 pwLedger
                        assertEqual "Lite IO observer: unchanged final policy ledger"
                            (toMap old) (toMap new)
                        oldRestored <- Policy.restoreLedger oldPath old :: IO LedgerM
                        newRestored <- Policy.restoreLedger newPath new :: IO LedgerM
                        assertEqual "Lite IO observer: unchanged spill contents"
                            (toMap oldRestored) (toMap newRestored)
              )
            , ("testLiteObserverStreamingSpill", do
                full <- runLiteWithPolicy Policy.defaultLedgerPolicy polSpec polW0 pwLedger
                forM_ [0, 2] $ \window -> withTempSpill "observer_stream" $ \path -> do
                    let policy = Policy.defaultLedgerPolicy
                            { Policy.retain  = Policy.RetainRecent window
                            , Policy.spillTo = Just path }
                    seen <- newIORef []
                    resident <- runLiteWithPolicyObs
                        (\t w -> modifyIORef' seen ((t, pwLedger w) :))
                        policy polSpec polW0 pwLedger
                    snapshots <- reverse <$> readIORef seen
                    let entries = L.nub (concatMap (HM.toList . toMap . snd) snapshots)
                        expected = HM.toList (toMap full)
                        streamed = sigma snapshots $ \(t, ledger) ->
                            EJ.filterWithNote (\(_, u) _ -> u == t) ledger
                    assertEqual "Lite streaming: observer term order" [1 .. 5] (L.map fst snapshots)
                    assertEqual "Lite streaming: snapshot entry set equals FullAudit"
                        True (length entries == length expected && all (`elem` entries) expected)
                    assertEqual "Lite streaming: current-term output equals FullAudit exactly"
                        (toMap full) (toMap streamed)
                    assertEqual "Lite streaming: final resident window"
                        [6 - window .. 5] (L.sort (L.map snd (HM.keys (toMap resident))))
                    restored <- Policy.restoreLedger path resident :: IO LedgerM
                    assertEqual "Lite streaming: spill plus resident ledger is lossless"
                        (toMap full) (toMap restored)
              )
            ]
    forM_ cases $ \(_, check) -> check

-- | Check decoded ranges and both restore paths on the original spill fixtures.
testSpillCheckedCases :: IO ()
testSpillCheckedCases = do
    let wellFormed = [((1, 2), spillCheckedChunk1), ((3, 3), spillCheckedChunk2)]
        rangeCases =
            [ ("checked spill reader rejects stale append"
              , wellFormed ++ [((1, 2), spillCheckedChunk1)]
              , Left (ES.SpillRangeError ES.ChunkOutOfOrder (3, 3) (1, 2)))
            , ("checked spill reader rejects overlap"
              , [((1, 3), spillCheckedChunk1), ((2, 4), spillCheckedChunk2)]
              , Left (ES.SpillRangeError ES.ChunkOverlap (1, 3) (2, 4)))
            , ("checked spill reader rejects gap"
              , [((1, 2), spillCheckedChunk1), ((4, 4), spillCheckedChunk2)]
              , Left (ES.SpillRangeError ES.ChunkGap (1, 2) (4, 4)))
            , ("checked spill reader rejects empty range"
              , [((3, 1), spillCheckedChunk1)]
              , Left (ES.SpillEmptyRange (3, 1)))
            , ("checked spill reader accepts empty file", [], Right [])
            ]
        cases :: [FilePath -> IO ()]
        cases =
            [ \path -> do
                writeSpillTestChunks path wellFormed
                readResult <- readChunks path
                case readResult of
                    Left err -> assertEqual "checked spill reader accepts well-formed chunks"
                        "Right with two chunks" (ES.renderSpillReadError err)
                    Right decoded -> assertEqual "checked spill reader returns both chunks"
                        2 (L.length decoded)
                restored <- restoreJournalFromBinarySpillChecked path snd spillCheckedCurrent
                case restored of
                    Left err -> assertEqual "checked spill restore accepts well-formed chunks"
                        "Right restored ledger" (ES.renderSpillReadError err)
                    Right actual -> assertEqual "checked spill restore merges spill + tail remainder"
                        (EJ.toMap spillCheckedExpected) (EJ.toMap actual)
                actual <- restoreJournalFromBinarySpill path snd spillCheckedCurrent
                assertEqual "Write.restoreJournalFromBinarySpill merges spill + tail remainder"
                    (EJ.toMap spillCheckedExpected) (EJ.toMap actual)
            , \path -> do
                let encodedChunk2 = Binary.encode
                        ((3 :: Int, 3 :: Int), spillCheckedChunk2)
                    truncatedChunk2 = BL.take (BL.length encodedChunk2 `div` 2) encodedChunk2
                withFile path WriteMode $ \handle -> do
                    ES.defaultBinarySpillWriter handle (1 :: Int, 2 :: Int) spillCheckedChunk1
                    BL.hPut handle truncatedChunk2
                result <- readChunks path
                case result of
                    Left (ES.SpillDecodeFailure offset chunks _) -> do
                        assertEqual "truncated spill failure follows first chunk" True (offset > 0)
                        assertEqual "truncated spill reports decoded chunk count" 1 chunks
                    other -> assertEqual "truncated spill is a decode failure"
                        "SpillDecodeFailure" (show other)
                caught <- try
                    (restoreJournalFromBinarySpill path snd (mempty :: SpillRestoreJournal))
                    :: IO (Either ErrorCall SpillRestoreJournal)
                case caught of
                    Left _ -> putStrLn "[PASS] unchecked spill restore raises ErrorCall"
                    Right _ -> assertEqual "unchecked spill restore raises ErrorCall" True False
            ] ++ L.map checkRanges rangeCases
        checkRanges (label, chunks, expected) path = do
            writeSpillTestChunks path chunks
            result <- readChunks path
            assertEqual label expected (fmap (fmap fst) result)
    forM_ cases withTestTemporaryFile
  where
    readChunks :: FilePath
        -> IO (Either (ES.SpillReadError Int) [((Int, Int), SpillRestoreJournal)])
    readChunks = ES.readBinarySpillFileChecked

-- | Run this domain in its original relative test order.
runTests :: IO ()
runTests = do
    testSpillCheckedCases
    testLiteObserverPolicyCases
    testLiteObserverBoundaries
    testLiteFieldBoundaries
    testLiteToyModels
    testSpillDecisionSingleSource
    testSimulateEx1Default
    testLiteDet2
    testLiteBspInvisibility
    testPolicyEquivalence
    testPolicyWindowRoundTrip
    testPolicyCompressClosed
    testPolicyDeleteOnly
    testPolicyDeterminism
    testPolicyClassicBridge
    testPolicyHasTermAxis
    testMarketSimpleTunedEqual
    testMarketSeqParEqual
    testMarketShortagePositive
    testMarketWindowTransparent
    testMarketStageOfAutoNote
