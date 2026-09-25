{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Index, flow, and exact-journal laws for the ledger evaluator.
module Ledger.LedgerSpec (runTests) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HashMap
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import System.Exit (exitFailure)
import Test.QuickCheck hiding (label)

import ExchangeAlgebra.Algebra hiding (map, filter)
import ExchangeAlgebra.Ledger
import ExchangeAlgebra.Ledger.Posting ( PostSide(..)
                                     , Posted
                                     , Posting
                                     , Signed(..)
                                     , entry
                                     , posted
                                     , toAlg
                                     )
import qualified ExchangeAlgebra.Journal as Journal
import qualified ExchangeAlgebra.Journal.Exact as Exact

-- | Three axes leave multiple bases in each partition component.
type TestBase = HatBase (AccountTitles, CountUnit, CountUnit)

-- | Coordinates without the posting side.
type TestPart = (AccountTitles, CountUnit, CountUnit)

-- | The first two coordinates identify a component.
type TestKey = (AccountTitles, CountUnit)

-- | Integer notes distinguish several postings in the same component.
type TestLedger = Ledger Int TestBase

-- | A local axis with an NFData instance for the populated Liner fixture.
data Tiny
    = First
    | Second
    | Any
    deriving (Eq, Ord, Show, Enum, Generic)

instance Hashable Tiny where
    hashWithSalt salt = hashWithSalt salt . fromEnum

instance NFData Tiny

instance Element Tiny where
    wildcard = Any

-- | This independent base gives the NFData fixture fully strict axes.
type ForceBase = HatBase (Tiny, Tiny, Tiny)

instance Partition ForceBase where
    type PartKey ForceBase = (Tiny, Tiny)
    partKey _ (first, second, _) = (first, second)
    type Group ForceBase = Tiny
    groupOf _ = fst

instance Partition TestBase where
    type PartKey TestBase = TestKey
    partKey _ (title, unit, _) = (title, unit)
    type Group TestBase = AccountTitles
    groupOf _ = fst

-- | Each input posting may have several entries and a distinct note.
data LedgerCase = LedgerCase [(Int, [(PostSide, Double, TestPart)])] TestBase
    deriving (Show)

-- | Reject a malformed fixed or generated test amount immediately.
checked :: Double -> Posted
checked value = case posted value of
    Right result -> result
    Left failure -> error ("invalid ledger test amount: " ++ show failure)

-- | Combine checked entries through the posting monoid.
makePosting :: [(PostSide, Double, TestPart)] -> Posting TestBase
makePosting = foldMap (\(side, value, part) -> entry side (checked value) part)

-- | Post each generated record in list order.
build :: LedgerCase -> TestLedger
build (LedgerCase records _) = foldl step emptyLedger records
  where
    step ledger (note, entries) = post note (makePosting entries) ledger

-- | Include stored wildcards in every coordinate axis.
genPart :: Gen TestPart
genPart = (,,) <$> elements [Cash, Products, Sales, wildcard]
                  <*> elements [Yen, Amount, Dollar, wildcard]
                  <*> elements [Yen, Amount, Dollar, wildcard]

-- | Independently sample the three query hats.
genQuery :: Gen TestBase
genQuery = (:<) <$> elements [Hat, Not, HatNot] <*> genPart

-- | Generate several notes and posting lists, including empty lists.
genCaseWith :: Gen Double -> Gen LedgerCase
genCaseWith genValue = do
    count <- chooseInt (0, 12)
    records <- vectorOf count $ do
        note <- chooseInt (0, 3)
        entryCount <- chooseInt (0, 5)
        entries <- vectorOf entryCount $ do
            side <- elements [PHat, PNot]
            value <- genValue
            part <- genPart
            pure (side, value, part)
        pure (note, entries)
    LedgerCase records <$> genQuery

-- | Integer inputs keep IX-1 arithmetic exact below 2^53.
genIntegerCase :: Gen LedgerCase
genIntegerCase = genCaseWith (fromIntegral <$> chooseInt (0, 1000))

-- | Decimal fractions and a large value expose floating-point order.
genFractionalCase :: Gen LedgerCase
genFractionalCase = genCaseWith $ elements
    [0, 0.1, 0.2, 0.3, 0.5, 1, 1.25, 2.5, 1000000000000000]

-- | Enumerate the scalar sequence in exactly the order used by post.
scalars :: LedgerCase -> [(Int, PostSide, Double, TestPart)]
scalars (LedgerCase records _) = concatMap expand records
  where
    expand (note, entries) = reverse $ foldEntries collect [] (toAlg (makePosting entries))
      where
        collect previous value postingBase =
            (note, concreteSide (hat postingBase), value, base postingBase) : previous
        concreteSide Hat = PHat
        concreteSide Not = PNot
        concreteSide HatNot = error "scalars: Posting concrete-side invariant violated"

-- | Project one base part to its component key.
keyOf :: TestPart -> TestKey
keyOf (title, unit, _) = (title, unit)

-- | Convert a posting side to its concrete algebra hat.
sideHat :: PostSide -> Hat
sideHat PHat = Hat
sideHat PNot = Not

-- | Give a scalar its Not-minus-Hat sign without aggregating it.
signedScalar :: PostSide -> Double -> Double
signedScalar PNot value = value
signedScalar PHat value = negate value

-- | Decode the exact readout's Not-minus-Hat sign convention.
signedValue :: Ordering -> Double -> Double
signedValue GT amount = amount
signedValue LT amount = negate amount
signedValue EQ _ = 0

-- | Read an exact net from all journal components.
exactAt :: TestPart -> Journal.Journal Int Double TestBase -> Either String Double
exactAt = exactAtWith Just

-- | Read an exact net using an explicit component selection.
exactAtWith :: (TestPart -> Maybe TestPart)
            -> TestPart -> Journal.Journal Int Double TestBase
            -> Either String Double
exactAtWith select part source = case Exact.balanceMapByExact select source of
    Left failure -> Left (show failure)
    Right balances -> Right $ uncurry signedValue $
        Map.findWithDefault (EQ, 0) part balances

-- | Compute n*u*sum(abs input), u=2^-53, without rounding the test bound.
errorBound :: [Double] -> Rational
errorBound values = fromIntegral (length values) / (2 ^ (53 :: Int))
                  * sum (map toRational values)

-- | Compare one sequential index value with the E1 bound.
nearExact :: [Double] -> Double -> Double -> Bool
nearExact values actual expected =
    not (isNaN actual || isInfinite actual)
    && abs (toRational actual - toRational expected) <= errorBound values

-- | IX-1: bounded integer postings have exact signed addition per base.
propIntegerNet :: Property
propIntegerNet = forAll genIntegerCase $ \sample@(LedgerCase records _) ->
    forAll genPart $ \part ->
        forAll (chooseInt (0, 3)) $ \note ->
            forAll (listOf $ (,,) <$> elements [PHat, PNot]
                                  <*> (fromIntegral <$> chooseInt (0, 1000))
                                  <*> genPart) $ \entries ->
                let ledger = build sample
                    next = post note (makePosting entries) ledger
                    delta = sum [signedScalar side value
                                | (side, value, item) <- entries, item == part]
                    bounded = sum [value | (_, _, value, _) <- scalars sample]
                            + sum [value | (_, value, _) <- entries] < 2 ^ (53 :: Int)
                in bounded ==> counterexample (show (records, part, entries)) $
                    netAt next part == netAt ledger part + Signed delta

-- | IX-2: the sequential index stays within E1 of the exact journal net.
propExactNet :: Property
propExactNet = forAll genFractionalCase $ \sample ->
    let ledger = build sample
        parts = Set.toList $ Set.fromList [part | (_, _, _, part) <- scalars sample]
        agrees part = case exactAt part (journal ledger) of
            Left failure -> counterexample failure False
            Right expected ->
                let values = [value | (_, _, value, item) <- scalars sample, item == part]
                in counterexample (show (part, values, netAt ledger part, expected)) $
                    nearExact values (getSigned (netAt ledger part)) expected
    in conjoin (map agrees parts)

-- | IX-3: components partition every posted base and preserve net values.
propComponents :: Property
propComponents = forAll genIntegerCase $ \sample ->
    let ledger = build sample
        parts = Set.fromList [part | (_, _, _, part) <- scalars sample]
        keys = Set.fromList (map keyOf (Set.toList parts))
        disjoint = all (\left -> all (\right ->
            left == right || Set.null (Set.intersection
                (Set.fromList (HashMap.keys (component ledger left)))
                (Set.fromList (HashMap.keys (component ledger right))))) (Set.toList keys))
            (Set.toList keys)
        values = all (\key -> all (\part ->
            HashMap.lookup part (component ledger key) == Just (netAt ledger part))
            [part | part <- Set.toList parts, keyOf part == key]) (Set.toList keys)
        actual = Set.unions [Set.fromList (HashMap.keys (component ledger key))
                            | key <- Set.toList keys]
    in counterexample (show sample) $
        actual == parts && disjoint && values

-- | Observe each note's posting multiset, ignoring notes with no entries.
sameJournal :: Journal.Journal Int Double TestBase
            -> Journal.Journal Int Double TestBase -> Bool
sameJournal left right = observations left == observations right
  where
    observations = HashMap.filter (not . null) . HashMap.map toASCList . Journal.toMap

-- | Sequential flow accumulation deliberately uses old + new.
expectedFlow :: LedgerCase
             -> Int
             -> TestKey
             -> PostSide
             -> TestBase
             -> Map.Map TestPart Double
expectedFlow sample note key side query = foldl add Map.empty (scalars sample)
  where
    add totals (entryNote, entrySide, value, part)
        | entryNote == note && entrySide == side && keyOf part == key
          && ignoreWildcard part (base query) == part =
            Map.alter (Just . maybe value (+ value)) part totals
        | otherwise = totals

-- | The two gross sides are accumulated separately in posting order.
expectedSides :: LedgerCase -> TestKey -> Map.Map TestPart (Double, Double)
expectedSides sample key = foldl add Map.empty (scalars sample)
  where
    add totals (_, side, value, part)
        | keyOf part == key = Map.alter (Just . advance . maybe (0, 0) id) part totals
        | otherwise = totals
      where
        advance (notTotal, hatTotal) = case side of
            PNot -> (notTotal + value, hatTotal)
            PHat -> (notTotal, hatTotal + value)

-- | IX-4: note/side flow has the same scalar addition order as foldEntries.
propFlows :: Property
propFlows = forAll genFractionalCase $ \sample@(LedgerCase _ query) ->
    let ledger = build sample
        keys = Set.toList $ Set.fromList [keyOf part | (_, _, _, part) <- scalars sample]
        check note key side =
            let expected = expectedFlow sample note key side query
                actual = flowIn ledger note key side query
            in counterexample (show (note, key, side, query, expected, actual))
                (actual == Map.toAscList expected)
        checkSides key =
            let expected = expectedSides sample key
                actual = Map.fromList (HashMap.toList (sidesIn ledger key))
            in counterexample (show (key, expected, actual)) (actual == expected)
        cleared = clearFlows ledger
        clearCheck = all (\note -> all (\key -> all (\side ->
            null (flowIn cleared note key side query)) [PHat, PNot]) keys) [0..3]
        reposted = post 1 (makePosting [(PNot, 0.3, (Cash, Yen, Amount))]) cleared
        fresh = expectedFlow (LedgerCase [(1, [(PNot, 0.3, (Cash, Yen, Amount))])] query)
                             1 (Cash, Yen) PNot query
    in conjoin [conjoin [check note key side | note <- [0..3], key <- keys,
                                              side <- [PHat, PNot]],
                conjoin (map checkSides keys),
                property clearCheck,
                property (sameJournal (journal ledger) (journal cleared)),
                property (all (\part -> netAt ledger part == netAt cleared part)
                              [part | (_, _, _, part) <- scalars sample]),
                property (all (\key -> sidesIn ledger key == sidesIn cleared key) keys),
                property (all (\key -> component ledger key == component cleared key) keys),
                property (all (\group -> componentsOf ledger group
                                       == componentsOf cleared group)
                              [Cash, Products, Sales, wildcard]),
                property (flowIn reposted 1 (Cash, Yen) PNot query
                          == Map.toAscList fresh)]

-- | IX-8b: query index agrees with exact projection within E1 per base.
propQuery :: Property
propQuery = forAll genFractionalCase $ \sample@(LedgerCase _ query) ->
    let ledger = build sample
        keys = keyOf (base query) :
            Set.toList (Set.fromList [keyOf part | (_, _, _, part) <- scalars sample])
    in forAll (elements keys) $ \key ->
        let selected = [part | (_, _, _, part) <- scalars sample,
                               keyOf part == key,
                               ignoreWildcard part (base query) == part]
            projected = Journal.projWithBase
                [merge HatNot (base query)] (journal ledger)
            actualList = queryIn ledger key query
            actual = Map.fromList actualList
            expectedParts = Set.fromList selected
            selectPart part
                | keyOf part == key = Just part
                | otherwise = Nothing
            check part = case exactAtWith selectPart part projected of
                Left failure -> counterexample failure False
                Right expected ->
                    let values = [value | (_, _, value, item) <- scalars sample,
                                          item == part]
                    in counterexample (show (part, actual, expected)) $
                        maybe False (\observed ->
                            nearExact values (getSigned observed) expected)
                            (Map.lookup part actual)
        in counterexample (show (sample, key)) $
            actualList == Map.toAscList actual
            && Set.fromList (Map.keys actual) == expectedParts
            .&&. conjoin (map check (Set.toList expectedParts))

-- | The journal contains the multiset of raw scalar entries under each note.
propJournal :: Property
propJournal = forAll genFractionalCase $ \sample@(LedgerCase records _) ->
    let observed = journal (build sample)
        oracle = foldMap (\(note, entries) -> fromList (map raw entries) Journal..| note)
                         records
        raw (side, value, part) =
            value .@ merge (sideHat side) part
    in counterexample (show sample) (sameJournal observed oracle)

-- | IX-10: only posted components are listed, sorted within their group.
propGroup :: Property
propGroup = forAll genIntegerCase $ \sample ->
    let ledger = build sample
        keys = Set.toList $ Set.fromList [keyOf part | (_, _, _, part) <- scalars sample]
        groups = [Cash, Products, Sales, wildcard]
    in counterexample (show sample) $ all (\group ->
        componentsOf ledger group == sort [key | key <- keys, fst key == group]) groups

-- | Report a fixed assertion failure through the executable's exit status.
assertTest :: String -> Bool -> IO ()
assertTest label success = unless success $ do
    putStrLn ("[FAIL] ledger: " ++ label)
    exitFailure

-- | Run 200 generated cases and fail the executable on a counterexample.
quickProperty :: Testable property => String -> property -> IO ()
quickProperty label proposition = do
    result <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = False } proposition
    unless (isSuccess result) $ do
        putStrLn ("[FAIL] ledger: " ++ label ++ ": " ++ output result)
        exitFailure
    putStrLn ("[PASS] ledger: " ++ label)

-- | Fixed cases pin wildcard direction, empty and zero-only postings, and clear.
testRegressions :: IO ()
testRegressions = do
    let wild = (Cash, Yen, wildcard)
        concrete = (Cash, Yen, Amount)
        key = (Cash, Yen)
        query side part = side :< part
        empty = emptyLedger :: TestLedger
        ledger = post 1 (makePosting [(PNot, 2, wild), (PHat, 2, concrete)]) empty
        zeroOnly = post 1 (makePosting [(PNot, 0, concrete)]) empty
    assertTest "stored wildcard is a value" $
        queryIn ledger key (query HatNot concrete) == [(concrete, Signed (-2))]
    assertTest "query wildcard selects stored wildcard" $
        Map.fromList (queryIn ledger key (query HatNot wild))
            == Map.fromList [(wild, Signed 2), (concrete, Signed (-2))]
    assertTest "query hat is ignored" $
        all (\side -> queryIn ledger key (query side wild)
                   == queryIn ledger key (query HatNot wild)) [Hat, Not]
    assertTest "flow stored wildcard is a value" $
        null (flowIn ledger 1 key PNot (query HatNot concrete))
        && flowIn ledger 1 key PNot (query HatNot wild) == [(wild, 2)]
    assertTest "flow query hat is ignored" $
        all (\side -> flowIn ledger 1 key PNot (query side wild)
                   == flowIn ledger 1 key PNot (query HatNot wild)) [Hat, Not]
    assertTest "absent and empty components" $
        HashMap.null (component empty key) && null (componentsOf empty Cash)
        && null (queryIn empty key (query HatNot wild))
        && netAt empty concrete == Signed 0
        && netAt ledger (Products, Yen, Amount) == Signed 0
        && HashMap.null (sidesIn ledger (Products, Yen))
        && null (flowIn ledger 99 key PNot (query HatNot wild))
    assertTest "zero-only posting is not indexed" $
        HashMap.null (component zeroOnly key) && null (componentsOf zeroOnly Cash)
        && null (flowIn zeroOnly 1 key PNot (query HatNot concrete))
    let cancelled = post 1 (makePosting [(PNot, 0.1, concrete),
                                          (PHat, 0.1, concrete)]) empty
    assertTest "cancelled component remains listed" $
        componentsOf cancelled Cash == [key]
        && HashMap.member concrete (component cancelled key)
    let ordered = post 1 (makePosting [(PNot, 1e16, concrete),
                                        (PNot, 1, concrete),
                                        (PNot, 1, concrete)]) empty
        observed = Map.fromList (flowIn ordered 1 key PNot (query HatNot concrete))
        oracle = foldEntries (\sumSoFar value _ -> sumSoFar + value) 0
                    (toAlg (makePosting [(PNot, 1e16, concrete),
                                         (PNot, 1, concrete),
                                         (PNot, 1, concrete)]))
    assertTest "flow uses foldEntries scalar order" (observed == Map.singleton concrete oracle)
    let mixed = makePosting [(PNot, 1e16, concrete), (PNot, 1, concrete),
                             (PHat, 1e16, concrete), (PHat, 0.3, concrete)]
        initial = post 1 (makePosting [(PNot, 0.2, concrete)]) empty
        accumulated = post 1 mixed initial
        netStep total value postingBase = case hat postingBase of
            Not -> total + value
            Hat -> total - value
            HatNot -> error "netStep: Posting concrete-side invariant violated"
        sequentialNet = foldEntries netStep 0.2 (toAlg mixed)
    assertTest "net uses each scalar in foldEntries order" $
        getSigned (netAt accumulated concrete) == sequentialNet
    let large = 2 ^ (53 :: Int)
        rounded = post 3 (makePosting [(PHat, large, concrete)]) $
            post 2 (makePosting [(PNot, 1, concrete)]) $
            post 1 (makePosting [(PNot, large, concrete)]) empty
    assertTest "sequential net differs from exact cancellation within E1" $
        getSigned (netAt rounded concrete) == 0
        && exactAt concrete (journal rounded) == Right 1
        && nearExact [large, 1, large] (getSigned (netAt rounded concrete)) 1

-- | Forcing a populated Liner also forces the ledger indexes and key list.
testNFData :: IO ()
testNFData = do
    let forcePosting :: [(PostSide, Double, (Tiny, Tiny, Tiny))]
                     -> Posting ForceBase
        forcePosting = foldMap (\(side, value, part) ->
            entry side (checked value) part)
        ledger = post 1 (forcePosting
            [(PNot, 2, (First, First, Second)),
             (PHat, 1, (First, First, First)),
             (PNot, 3, (Second, First, First))]) emptyLedger
            :: Ledger Int ForceBase
    forced <- evaluate (force ledger)
    assertTest "NFData Liner fixture" $
        length (componentsOf forced First) == 1
        && length (componentsOf forced Second) == 1

-- | Run every ledger property and fixed regression.
runTests :: IO ()
runTests = do
    quickProperty "IX-1 integer net" propIntegerNet
    quickProperty "IX-2 exact net E1" propExactNet
    quickProperty "IX-3 component partition" propComponents
    quickProperty "IX-4 flows and clear" propFlows
    quickProperty "IX-8b query exact E1" propQuery
    quickProperty "IX-10 component listing" propGroup
    quickProperty "journal posting multiset" propJournal
    testRegressions
    testNFData
