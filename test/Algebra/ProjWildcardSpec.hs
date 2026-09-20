{-# LANGUAGE ScopedTypeVariables #-}

-- | The @proj@ family matches wildcards one way: only the query's wildcards
-- act as a pattern, and a wildcard stored in a ledger base is an ordinary
-- value. The result must not depend on the shape of the ledger (a single
-- element or a 'Liner').
module Algebra.ProjWildcardSpec (runTests) where

import           Control.Monad (unless)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           System.Exit (exitFailure)
import           Test.QuickCheck hiding (label)
import           ExchangeAlgebra.Algebra hiding (map, filter)
import qualified ExchangeAlgebra.Algebra as Algebra

-- | Account and unit coordinates; either axis may hold a ledger wildcard.
type TestBase = HatBase (AccountTitles, CountUnit)

type TestAlg = Alg Double TestBase

-- | Generated postings (ledger wildcards allowed) and a query set. Values are
-- small whole numbers, so every 'Double' sum below is exact.
data ProjCase = ProjCase [(Double, TestBase)] [TestBase]
    deriving (Show)

genTitle :: Gen AccountTitles
genTitle = frequency [(1, pure wildcard), (4, elements [Cash, Products, Sales])]

genUnit :: Gen CountUnit
genUnit = frequency [(1, pure wildcard), (4, elements [Yen, Amount, Dollar])]

genEntryBase :: Gen TestBase
genEntryBase = (:<) <$> elements [Hat, Not] <*> ((,) <$> genTitle <*> genUnit)

genQueryBase :: Gen TestBase
genQueryBase = (:<) <$> elements [Hat, Not, HatNot] <*> ((,) <$> genTitle <*> genUnit)

genProjCase :: Gen ProjCase
genProjCase = do
    count <- chooseInt (0, 12)
    entries <- vectorOf count ((,) <$> (fromIntegral <$> chooseInt (1, 1000)) <*> genEntryBase)
    queryCount <- chooseInt (1, 4)
    ProjCase entries <$> vectorOf queryCount genQueryBase

-- | The same generator restricted to concrete ledger bases.
genConcreteCase :: Gen ProjCase
genConcreteCase = do
    ProjCase entries queries <- genProjCase
    pure (ProjCase (filter (not . haveWildcard . snd) entries) queries)

-- | Reference predicate, written per axis and independent of the library's
-- 'ignoreWildcard': a query axis matches when it is a wildcard or equals the
-- entry axis. A ledger wildcard is matched only by a query wildcard.
oneWay :: TestBase -> TestBase -> Bool
oneWay (queryHat :< (queryTitle, queryUnit)) (entryHat :< (entryTitle, entryUnit)) =
    axis queryHat entryHat && axis queryTitle entryTitle && axis queryUnit entryUnit
  where
    axis :: Element a => a -> a -> Bool
    axis query entry = isWildcard query || query == entry

-- | The old symmetric predicate, for the concrete-ledger regression.
symmetric :: TestBase -> TestBase -> Bool
symmetric = (.==)

build :: [(Double, TestBase)] -> TestAlg
build entries = fromList [value :@ postingBase | (value, postingBase) <- entries]

-- | Raw postings keyed by the rendered base. 'HatBase' orders wildcards as
-- equal to anything, so the base itself cannot be the key here.
observe :: TestAlg -> Map.Map String [Double]
observe = Map.map List.sort
        . foldEntries (\result value postingBase ->
              Map.insertWith (++) (show postingBase) [value] result) Map.empty

selected :: (TestBase -> TestBase -> Bool) -> ProjCase -> [(Double, TestBase)]
selected match (ProjCase entries queries) =
    [entry | entry@(_, postingBase) <- entries, any (`match` postingBase) queries]

-- | Bar-netted norm of the selected entries, per base.
netNorm :: [(Double, TestBase)] -> Double
netNorm entries = sum (Map.elems (Map.map abs perBase))
  where
    perBase = Map.fromListWith (+)
        [ (show (base postingBase), if hat postingBase == Hat then negate value else value)
        | (value, postingBase) <- entries ]

propLedger :: ProjCase -> Bool
propLedger testCase@(ProjCase entries queries) =
    observe (proj queries (build entries)) == observe (build (selected oneWay testCase))

propSingle :: ProjCase -> Bool
propSingle (ProjCase entries queries) = all agrees entries
  where
    agrees entry@(value, postingBase) =
        observe (proj queries (value :@ postingBase))
            == observe (build (selected oneWay (ProjCase [entry] queries)))

propNetNorm :: ProjCase -> Bool
propNetNorm testCase@(ProjCase entries queries) =
    projNetNorm queries (build entries) == netNorm (selected oneWay testCase)
        && all single entries
  where
    single entry@(value, postingBase) =
        projNetNorm queries (value :@ postingBase)
            == netNorm (selected oneWay (ProjCase [entry] queries))

propConcreteRegression :: ProjCase -> Bool
propConcreteRegression testCase =
    selected oneWay testCase == selected symmetric testCase && propLedger testCase

assertTest :: String -> Bool -> IO ()
assertTest label success = do
    unless success $ do
        putStrLn ("[FAIL] " ++ label)
        exitFailure
    putStrLn ("[PASS] " ++ label)

quickProperty :: Testable property => String -> property -> IO ()
quickProperty label proposition = do
    result <- quickCheckWithResult stdArgs { maxSuccess = 500, chatty = False } proposition
    unless (isSuccess result) $ do
        putStrLn ("[FAIL] " ++ label ++ ": " ++ output result)
        exitFailure
    putStrLn ("[PASS] " ++ label)

-- | The reported table: cash carries no unit axis in the ledger.
testReportedTable :: IO ()
testReportedTable = do
    let cash = 10 :@ Not :< (Cash, wildcard) :: TestAlg
        both = cash .+ 3 :@ Not :< (Products, Yen)
        anyTitleYen = [Not :< (wildcard, Yen)]
        cashYen = [Not :< (Cash, Yen)]
        cashAny = [Not :< (Cash, wildcard)]
        products = 3 :@ Not :< (Products, Yen) :: TestAlg
    assertTest "proj: wildcard-title query skips ledger-wildcard unit (single)"
        (Algebra.isZero (proj anyTitleYen cash))
    assertTest "proj: wildcard-title query skips ledger-wildcard unit (Liner)"
        (observe (proj anyTitleYen both) == observe products)
    assertTest "proj: concrete query does not match a ledger wildcard (single)"
        (Algebra.isZero (proj cashYen cash))
    assertTest "proj: concrete query does not match a ledger wildcard (Liner)"
        (Algebra.isZero (proj cashYen both))
    assertTest "proj: query wildcard picks up the ledger wildcard (single)"
        (observe (proj cashAny cash) == observe cash)
    assertTest "proj: query wildcard picks up the ledger wildcard (Liner)"
        (observe (proj cashAny both) == observe cash)
    assertTest "projNetNorm: single element and Liner agree on a ledger wildcard"
        (projNetNorm cashYen cash == 0 && projNetNorm cashYen both == 0
            && projNetNorm cashAny cash == 10 && projNetNorm cashAny both == 10)
    assertTest "proj: projecting a projection is stable"
        (observe (proj cashYen (proj cashAny both)) == Map.empty)

-- | 'projByAccountTitle' follows the same one-way rule on the title axis.
testProjByAccountTitle :: IO ()
testProjByAccountTitle = do
    let noTitle = 7 :@ Not :< wildcard :: Alg Double (HatBase AccountTitles)
        cash = 10 :@ Not :< Cash
        ledger = noTitle .+ cash
        postings = List.sort . foldEntries (\result value postingBase ->
            (show postingBase, value) : result) []
    assertTest "projByAccountTitle: concrete title skips a ledger wildcard (single)"
        (Algebra.isZero (projByAccountTitle Cash noTitle))
    assertTest "projByAccountTitle: concrete title skips a ledger wildcard (Liner)"
        (postings (projByAccountTitle Cash ledger) == postings cash)
    assertTest "projByAccountTitle: wildcard title selects every entry"
        (postings (projByAccountTitle wildcard ledger) == postings ledger)

-- | 'Algebra.map' keeps a rewrite between a wildcard and a concrete coordinate
-- in both directions and in both ledger shapes.
testMapRewritesWildcardAxis :: IO ()
testMapRewritesWildcardAxis = do
    let cash = 10 :@ Not :< (Cash, wildcard) :: TestAlg
        cashYen = 10 :@ Not :< (Cash, Yen) :: TestAlg
        products = 3 :@ Not :< (Products, Amount) :: TestAlg
        setCashUnit unit entry@(value :@ (h :< (title, _)))
            | title == Cash = value :@ (h :< (title, unit))
            | otherwise     = entry
        setCashUnit _ entry = entry
    assertTest "map: wildcard axis rewritten to a concrete value (single)"
        (observe (Algebra.map (setCashUnit Yen) cash) == observe cashYen)
    assertTest "map: wildcard axis rewritten to a concrete value (Liner)"
        (observe (Algebra.map (setCashUnit Yen) (cash .+ products))
            == observe (cashYen .+ products))
    assertTest "map: concrete axis rewritten to the wildcard (Liner)"
        (observe (Algebra.map (setCashUnit wildcard) (cashYen .+ products))
            == observe (cash .+ products))

runTests :: IO ()
runTests = do
    testReportedTable
    testProjByAccountTitle
    testMapRewritesWildcardAxis
    quickProperty "proj: Liner result equals the one-way reference (ledger wildcards)"
        (forAll genProjCase propLedger)
    quickProperty "proj: single-element result equals the one-way reference"
        (forAll genProjCase propSingle)
    quickProperty "projNetNorm: equals the one-way reference for both shapes"
        (forAll genProjCase propNetNorm)
    quickProperty "proj: concrete ledgers keep the old symmetric result"
        (forAll genConcreteCase propConcreteRegression)
