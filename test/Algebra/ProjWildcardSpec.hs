{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @proj@ family matches wildcards one way: only the query's wildcards
-- act as a pattern, and a wildcard stored in a ledger base is an ordinary
-- value. The result must not depend on the shape of the ledger (a single
-- element or a 'Liner').
module Algebra.ProjWildcardSpec (runTests) where

import qualified Data.Binary as Binary
import           Data.Time ( Day(ModifiedJulianDay)
                           , TimeOfDay(..)
                           , fromGregorian
                           , midnight
                           )
import           Data.Typeable (Typeable)
import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Accounting.Exchange
import           Control.Monad (unless)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           System.Exit (exitFailure)
import           Test.QuickCheck hiding (label)
import           ExchangeAlgebra.Algebra hiding (map, filter)
import qualified ExchangeAlgebra.Algebra as Algebra
import           ExchangeAlgebra.Algebra.Element (matchesQuery)
import qualified ExchangeAlgebra.Algebra.Transfer as Transfer
import           ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import qualified ExchangeAlgebra.Journal as Journal
import           Support (assertEqual)

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
build entries = fromList [value .@ postingBase | (value, postingBase) <- entries]

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
        observe (proj queries (value .@ postingBase))
            == observe (build (selected oneWay (ProjCase [entry] queries)))

propNetNorm :: ProjCase -> Bool
propNetNorm testCase@(ProjCase entries queries) =
    projNetNorm queries (build entries) == netNorm (selected oneWay testCase)
        && all single entries
  where
    single entry@(value, postingBase) =
        projNetNorm queries (value .@ postingBase)
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
    let cash = 10 .@ Not :< (Cash, wildcard) :: TestAlg
        both = cash .+ 3 .@ Not :< (Products, Yen)
        anyTitleYen = [Not :< (wildcard, Yen)]
        cashYen = [Not :< (Cash, Yen)]
        cashAny = [Not :< (Cash, wildcard)]
        products = 3 .@ Not :< (Products, Yen) :: TestAlg
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
    let noTitle = 7 .@ Not :< wildcard :: Alg Double (HatBase AccountTitles)
        cash = 10 .@ Not :< Cash
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
    let cash = 10 .@ Not :< (Cash, wildcard) :: TestAlg
        cashYen = 10 .@ Not :< (Cash, Yen) :: TestAlg
        products = 3 .@ Not :< (Products, Amount) :: TestAlg
        setCashUnit unit entry@(value :@ (h :< (title, _)))
            | title == Cash = value .@ (h :< (title, unit))
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

-- | Flattening Text coordinates preserves projection norms, net norms,
-- collapse postings, and symmetric matching. Small non-negative whole values
-- make the Double observations exact; either inner query axis may be a wildcard.
propNestedFlattening :: (Text, Text, Text) -> (Double, Double, Double) -> Bool
propNestedFlattening (queryFirst, querySecond, queryThird) (notValue, hatValue, otherValue) =
    norm (proj [nestedQuery] nestedLedger) == norm (proj [flatQuery] flatLedger)
        && projNetNorm [nestedQuery] nestedLedger == projNetNorm [flatQuery] flatLedger
        && List.sort nestedCollapsed == List.sort flatCollapsed
        && (nestedStored .== nestedCoordinates) == (flatStored .== flatCoordinates)
        && ((Not :< nestedStored) .== (Not :< nestedCoordinates))
            == ((Not :< flatStored) .== (Not :< flatCoordinates))
  where
    nestedStored      = (("a", "b"), "c") :: ((Text, Text), Text)
    flatStored        = ("a", "b", "c") :: (Text, Text, Text)
    nestedCoordinates = ((queryFirst, querySecond), queryThird)
    flatCoordinates   = (queryFirst, querySecond, queryThird)
    nestedQuery       = HatNot :< nestedCoordinates
    flatQuery         = HatNot :< flatCoordinates
    nestedLedger :: Alg Double (HatBase ((Text, Text), Text))
    nestedLedger = notValue   .@ Not :< nestedStored
                .+ hatValue   .@ Hat :< nestedStored
                .+ otherValue .@ Not :< (("x", "y"), "z")
    flatLedger :: Alg Double (HatBase (Text, Text, Text))
    flatLedger = notValue   .@ Not :< flatStored
              .+ hatValue   .@ Hat :< flatStored
              .+ otherValue .@ Not :< ("x", "y", "z")
    nestedCollapsed :: [(Double, Hat, (Text, Text, Text))]
    nestedCollapsed = foldEntries
        (\result value (side :< ((first, second), third)) ->
            (value, side, (first, second, third)) : result) []
        (Transfer.collapseEntries [nestedQuery] (const (("a", "b"), "d")) nestedLedger)
    flatCollapsed :: [(Double, Hat, (Text, Text, Text))]
    flatCollapsed = foldEntries
        (\result value (side :< coordinates) -> (value, side, coordinates) : result) []
        (Transfer.collapseEntries [flatQuery] (const ("a", "b", "d")) flatLedger)

-- | The reported inner-only wildcard selects gross 13 and net 7, and
-- 'Transfer.collapseEntries' adds reversal and replacement postings with total norm 26.
testNestedWildcard :: IO ()
testNestedWildcard = do
    let ledger = 10 .@ Not :< (("a", "b"), "c")
              .+  3 .@ Hat :< (("a", "b"), "c")
              .+  2 .@ Not :< (("x", "y"), "z")
              :: Alg Double (HatBase ((Text, Text), Text))
        query = HatNot :< (("a", ""), "c")
    assertTest "nested wildcard: reported projection and collapse values"
        (norm (proj [query] ledger) == 13
            && projNetNorm [query] ledger == 7
            && norm (Transfer.collapseEntries [query] (const (("a", "b"), "d")) ledger) == 26)
    assertTest "nested wildcard: reported case agrees with flat coordinates"
        (propNestedFlattening ("a", "", "c") (10, 3, 2))

-- | A fixture identifier with no Element instance and deliberately colliding hashes.
newtype FixtureId = FixtureId Int
    deriving (Eq, Ord, Show)

instance Hashable FixtureId where
    hashWithSalt salt _ = salt

instance Binary.Binary FixtureId where
    put (FixtureId number) = Binary.put number
    get = FixtureId <$> Binary.get

-- | Compare exact coordinates, sides, and amounts independently of posting order.
observeWild :: forall coordinates. (BaseClass coordinates, AxisDecompose coordinates)
            => Alg MoneyDecimal (HatBase coordinates)
            -> [(Hat, coordinates, MoneyDecimal)]
observeWild = List.sort . foldEntries collect []
  where
    collect :: [(Hat, coordinates, MoneyDecimal)]
            -> MoneyDecimal
            -> HatBase coordinates
            -> [(Hat, coordinates, MoneyDecimal)]
    collect postings amount (side :< coordinates) = (side, coordinates, amount) : postings

-- | Match one coordinate without using the library's matching implementation.
matchesAxis :: Element coordinate => coordinate -> coordinate -> Bool
matchesAxis query entry = isWildcard query || query == entry

-- | Check every ordered pair in an ascending list of concrete coordinates plus Wild.
testWildAxis :: forall coordinate.
                (Ord coordinate, Show coordinate, Hashable coordinate, Typeable coordinate)
             => String
             -> [coordinate]
             -> IO ()
testWildAxis axis concrete = do
    assertEqual (axis ++ " matchesQuery is one-way")
        [query == Wild || query == entry | (query, entry) <- pairs]
        [matchesQuery query entry | (query, entry) <- pairs]
    assertEqual (axis ++ " .== is symmetric")
        [query == Wild || entry == Wild || query == entry | (query, entry) <- pairs]
        [query .== entry | (query, entry) <- pairs]
    assertEqual (axis ++ " compareElement treats Wild as EQ in either position")
        [wildcardOrder query entry | (query, entry) <- pairs]
        [compareElement query entry | (query, entry) <- pairs]
    assertEqual (axis ++ " compare orders Wild first and agrees with Eq")
        [(compare left right, left == right) | (left, right) <- indexPairs]
        [(compare left right, left == right) | (left, right) <- pairs]
  where
    coordinates :: [Wild coordinate]
    coordinates = Wild : fmap Exactly concrete

    pairs :: [(Wild coordinate, Wild coordinate)]
    pairs       = [(query, entry) | query <- coordinates, entry <- coordinates]

    indices :: [Int]
    indices = take (length coordinates) [0 ..]

    indexPairs :: [(Int, Int)]
    indexPairs = [(left, right) | left <- indices, right <- indices]

    wildcardOrder :: Ord payload => Wild payload -> Wild payload -> Ordering
    wildcardOrder Wild _ = EQ
    wildcardOrder _ Wild = EQ
    wildcardOrder left right = compare left right

-- | Account, time, and identifier coordinates shared by both projection shapes.
type WildCoordinates = (AccountTitles, Wild TimeOfDay, Wild Text)

-- | Flat tuples expose each axis key. A nested tuple stays one key at its outer position.
testWildAxisKeys :: IO ()
testWildAxisKeys = do
    assertTest "Wild flat tuple has one wildcard at the time axis" $
        haveWildcard flat && not (haveWildcard (Cash, time, identifier))
    assertTest "Wild nested tuple has an inner wildcard" $
        haveWildcard nested && not (haveWildcard (Cash, (time, identifier)))
    assertTest "Wild flat tuple exposes three axis keys in coordinate order" $
        toAxisKeys flat == [AxisKey Cash, AxisKey noTime, AxisKey identifier]
    assertTest "Wild nested tuple exposes two outer keys with the pair in position two" $
        toAxisKeys nested == [AxisKey Cash, AxisKey (noTime, identifier)]
  where
    noTime :: Wild TimeOfDay
    noTime = Wild

    time :: Wild TimeOfDay
    time = Exactly midnight

    identifier :: Wild Text
    identifier = Exactly ""

    flat :: WildCoordinates
    flat = (Cash, noTime, identifier)

    nested :: (AccountTitles, (Wild TimeOfDay, Wild Text))
    nested = (Cash, (noTime, identifier))

-- | Exercise single and indexed projections against the same per-axis oracle.
testWildProjection :: forall coordinates. (BaseClass coordinates, AxisDecompose coordinates)
                   => String
                   -> (WildCoordinates -> coordinates)
                   -> IO ()
testWildProjection shape pack = mapM_ checkQueries querySets
  where
    entries :: [(MoneyDecimal, Hat, WildCoordinates)]
    entries =
        [ (10, Not, (Cash, Exactly midnight, Exactly ""))
        , ( 4, Hat, (Cash, Exactly midnight, Exactly ""))
        , (20, Not, (Cash, Wild,             Exactly ""))
        , (30, Not, (Cash, Exactly midnight, Wild))
        , (40, Not, (Sales, Exactly midnight, Exactly ""))
        ]

    querySets :: [[(Hat, WildCoordinates)]]
    querySets =
        [ [(HatNot, (Cash, Exactly midnight, Exactly ""))]
        , [(HatNot, (Cash, Wild,             Exactly ""))]
        , [(Not,    (Cash, Exactly midnight, Wild))]
        , [(HatNot, (Cash, Wild, Exactly "")), (Not, (Cash, Exactly midnight, Wild))]
        ]

    matches :: (Hat, WildCoordinates) -> (MoneyDecimal, Hat, WildCoordinates) -> Bool
    matches (querySide, (queryTitle, queryTime, queryId))
            (_, entrySide, (entryTitle, entryTime, entryId)) =
        matchesAxis querySide entrySide
            && matchesAxis queryTitle entryTitle
            && matchesAxis queryTime entryTime
            && matchesAxis queryId entryId

    posting :: (MoneyDecimal, Hat, WildCoordinates) -> Alg MoneyDecimal (HatBase coordinates)
    posting (amount, side, entry) = amount .@ side :< pack entry

    ledger :: Alg MoneyDecimal (HatBase coordinates)
    ledger = fromList (fmap posting entries)

    checkQueries :: [(Hat, WildCoordinates)] -> IO ()
    checkQueries patterns = do
        let queries  = [side :< pack query | (side, query) <- patterns]
            selected = fromList [posting entry | entry <- entries, selects entry]
            context  = "Wild " ++ shape ++ " " ++ show patterns
            note     = "receipts" :: Text
            journal  = Journal.projWithBase queries (ledger Journal..| note)
            singles  = fmap posting entries
            selects :: (MoneyDecimal, Hat, WildCoordinates) -> Bool
            selects entry = any (`matches` entry) patterns
        assertEqual (context ++ " single-posting proj")
            [observeWild (fromList [posting entry | selects entry]) | entry <- entries]
            [observeWild (proj queries single) | single <- singles]
        assertEqual (context ++ " single-posting projNetNorm")
            [norm (fromList [posting entry | selects entry]) | entry <- entries]
            [projNetNorm queries single | single <- singles]
        assertEqual (context ++ " indexed proj") (observeWild selected)
            (observeWild (proj queries ledger))
        assertEqual (context ++ " indexed projNetNorm") (norm (bar selected))
            (projNetNorm queries ledger)
        assertEqual (context ++ " journal base projection")
            (observeWild selected)
            (observeWild (Journal.toAlg (Journal.projWithNote [note] journal)))

-- | Check source matching and target retention with both stored time coordinates.
testWildTransfer :: IO ()
testWildTransfer = do
    assertTest "Wild concrete transfer source skips the stored Wild" $
        agrees (Exactly midnight) (4 .@ Hat :< (Cash, Exactly midnight)
                                .+ 4 .@ Not :< (Deposits, Exactly midnight))
    assertTest "Wild transfer target retains both stored time coordinates" $
        agrees Wild (7 .@ Hat :< (Cash, Wild)
                  .+ 7 .@ Not :< (Deposits, Wild)
                  .+ 4 .@ Hat :< (Cash, Exactly midnight)
                  .+ 4 .@ Not :< (Deposits, Exactly midnight))
  where
    ledger :: Alg MoneyDecimal (HatBase (AccountTitles, Wild TimeOfDay))
    ledger = 7 .@ Not :< (Cash, Wild)
          .+ 4 .@ Not :< (Cash, Exactly midnight)

    agrees :: Wild TimeOfDay -> Alg MoneyDecimal (HatBase (AccountTitles, Wild TimeOfDay)) -> Bool
    agrees source expected =
        case Transfer.mkTransferRules [rule] of
            Left _      -> False
            Right rules -> case Transfer.transferEntries rules ledger of
                Left _         -> False
                Right postings -> observeWild postings == observeWild expected
      where
        rule = Transfer.TransferRule (Not :< (Cash, source)) (Not :< (Deposits, Wild))
            Transfer.Relabel

-- | Replacing an account title preserves the Hat/Not side and every other coordinate.
testWildAccountTitles :: IO ()
testWildAccountTitles = do
    assertEqual "Wild Day: setAccountTitle preserves the day and side"
        (Hat :< (Deposits, epoch)) (setAccountTitle (Hat :< (Cash, epoch)) Deposits)
    assertEqual "Wild Day: setAccountTitle preserves the five-axis base"
        (Not :< (Deposits, item, Yen, subject, noDay))
        (setAccountTitle (Not :< (Cash, item, Yen, subject, noDay)) Deposits)
    assertEqual "Wild Day/TimeOfDay: setAccountTitle preserves the six-axis base"
        (Hat :< (Deposits, item, Yen, subject, epoch, Exactly midnight))
        (setAccountTitle (Hat :< (Cash, item, Yen, subject, epoch, Exactly midnight)) Deposits)
  where
    epoch = Exactly (ModifiedJulianDay 0)
    noDay = Wild :: Wild Day
    item = "notebook" :: Name
    subject = "stock" :: Subject

-- | Wrapped identifiers retain equality, serialization, and indexed lookup under collision.
testWildPayload :: IO ()
testWildPayload = do
    testWildAxis "Wild FixtureId" [FixtureId 1, FixtureId 2]
    assertTest "Wild Text treats empty text as an ordinary coordinate" $
        not (isWildcard empty) && not (haveWildcard empty)
            && matchesQuery empty empty && not (matchesQuery empty Wild)
    quickProperty "Wild equal coordinates hash equally at every salt" $
        forAll arbitrary $ \salt ->
            let pairs = [(Wild, Wild), (Exactly (FixtureId 1), first)]
            in all (\(left, right) -> hashWithSalt salt left == hashWithSalt salt right) pairs
    assertEqual "Wild Binary round trips both constructors"
        [Wild, first, second] (fmap (Binary.decode . Binary.encode) [Wild, first, second])
    assertTest "Wild index keys collide at the same salt" $
        firstKey /= secondKey && hashWithSalt 7 firstKey == hashWithSalt 7 secondKey
    let ledger = 7 .@ Not :< first .+ 9 .@ Not :< second
            :: Alg MoneyDecimal (HatBase (Wild FixtureId))
    assertEqual "Wild index retrieves the first colliding key" 7
        (norm (proj [Not :< first] ledger))
    assertEqual "Wild index retrieves the second colliding key" 9
        (norm (proj [Not :< second] ledger))
  where
    empty = Exactly "" :: Wild Text
    first = Exactly (FixtureId 1)
    second = Exactly (FixtureId 2)
    firstKey = AxisKey first
    secondKey = AxisKey second

-- | Midnight stays concrete on the wrapped axis and remains a wildcard on the legacy axis.
testWildCalendarRegression :: IO ()
testWildCalendarRegression = do
    assertEqual "Wild TimeOfDay: midnight selects only the midnight posting" 100
        (norm (proj [Not :< (Cash, Exactly midnight)] receipts))
    assertEqual "Wild TimeOfDay: Wild selects all three postings" 600
        (norm (proj [Not :< (Cash, Wild)] receipts))
    assertEqual "legacy TimeOfDay: midnight selects all three postings" 600
        (norm (proj [Not :< (Cash, midnight)] receiptsOld))
    assertEqual "Wild Day: 1858-11-17 selects only that day's posting" 500
        (norm (proj [Not :< (Cash, Exactly (ModifiedJulianDay 0))] dated))
    assertEqual "Wild Day: sale totals 500 on both debit and credit" (500, 500)
        (norm (decL sale), norm (decR sale))
  where
    receipts :: Alg MoneyDecimal (HatBase (AccountTitles, Wild TimeOfDay))
    receipts = 100 .@ Not :< (Cash, Exactly midnight)
            .+ 200 .@ Not :< (Cash, Exactly (TimeOfDay 9 0 0))
            .+ 300 .@ Not :< (Cash, Wild)

    receiptsOld :: Alg MoneyDecimal (HatBase (AccountTitles, TimeOfDay))
    receiptsOld = 100 .@ Not :< (Cash, midnight)
               .+ 200 .@ Not :< (Cash, TimeOfDay 9 0 0)
               .+ 300 .@ Not :< (Cash, midnight)

    sale :: Alg MoneyDecimal (HatBase (AccountTitles, Wild Day))
    sale = 500 .@ Not :< (Cash,  Exactly (fromGregorian 2026 1 10))
        .+ 500 .@ Not :< (Sales, Exactly (fromGregorian 2026 1 10))

    dated :: Alg MoneyDecimal (HatBase (AccountTitles, Wild Day))
    dated = sale .+ 500 .@ Not :< (Cash, Exactly (ModifiedJulianDay 0))
                 .+ 300 .@ Not :< (Cash, Wild)

runTests :: IO ()
runTests = do
    testReportedTable
    testProjByAccountTitle
    testMapRewritesWildcardAxis
    testNestedWildcard
    quickProperty "nested wildcard: flattening preserves projection, collapse, and matching"
        (forAll ((,,) <$> elements ["a", ""] <*> elements ["b", ""]
                      <*> elements ["c", ""]) $ \coordinates ->
            forAll ((,,) <$> (fromIntegral <$> chooseInt (0, 1000))
                         <*> (fromIntegral <$> chooseInt (0, 1000))
                         <*> (fromIntegral <$> chooseInt (0, 1000)))
                (propNestedFlattening coordinates))
    quickProperty "proj: Liner result equals the one-way reference (ledger wildcards)"
        (forAll genProjCase propLedger)
    quickProperty "proj: single-element result equals the one-way reference"
        (forAll genProjCase propSingle)
    quickProperty "projNetNorm: equals the one-way reference for both shapes"
        (forAll genProjCase propNetNorm)
    quickProperty "proj: concrete ledgers keep the old symmetric result"
        (forAll genConcreteCase propConcreteRegression)
    testWildAxis "Wild TimeOfDay" [midnight, TimeOfDay 9 0 0]
    testWildAxis "Wild Day" [ModifiedJulianDay (-1), ModifiedJulianDay 0, ModifiedJulianDay 1]
    testWildAxisKeys
    testWildProjection "flat tuple" id
    testWildProjection "nested tuple" (\(title, time, identifier) -> (title, (time, identifier)))
    testWildTransfer
    testWildAccountTitles
    testWildPayload
    testWildCalendarRegression
