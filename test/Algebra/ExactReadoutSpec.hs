{-# LANGUAGE ScopedTypeVariables #-}

-- | Posting readouts against Rational oracles. These tests cover the rounding
-- boundaries of the algebra and journal contracts, including legacy tolerance.
module Algebra.ExactReadoutSpec (runTests) where

import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Accounting.Exchange
import Prelude hiding (map)
import Control.Monad (forM_, unless)
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import GHC.Float (castDoubleToWord64)
import Test.QuickCheck hiding (label)

import ExchangeAlgebra.Algebra
import ExchangeAlgebra.Algebra.Value (MoneyDecimal, MoneyDouble)
import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Algebra.Internal as Internal
import qualified ExchangeAlgebra.Journal as Journal

-- | Complete base: an account title and an independent owner coordinate.
type Base = HatBase (AccountTitles, T.Text)

-- | Synthetic posting before construction, also used by the independent oracle.
type Row = (Double, Base)

-- | Exact signed residuals by complete base, before any rounding.
residuals :: [Row] -> Map.Map (AccountTitles, T.Text) Rational
residuals rows = Map.fromListWith (+)
    [(base b, signed value b) | (value, b) <- rows, value /= 0]
  where
    signed value b
        | isHat b   = negate (toRational value)
        | otherwise = toRational value

-- | Construct without aggregating, using the public entry boundary.
ledger :: HatVal n => [(n, Base)] -> Alg n Base
ledger rows = EA.fromList [value .@ b | (value, b) <- rows]

-- | Stop at the first failed fixed observation.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual = unless (expected == actual) $
    fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

-- | Include cent amounts, large integers, and widely separated binary exponents.
genRows :: Gen [Row]
genRows = resize 24 $ listOf $ do
    value <- frequency
        [ (5, (\n -> fromIntegral (n :: Int) / 100) <$> chooseInt (1, 100000))
        , (3, elements [0.1, 0.2, 0.3, 1, 2^53, 1e12, 0.5])
        , (2, elements [encodeFloat 1 (-1074), encodeFloat 1 (-1022), 1e100, 1e-100])
        ]
    side <- elements [Not, Hat]
    title <- elements [Cash, Sales]
    owner <- elements (List.map T.pack ["1", "2", "3", "4"])
    pure (value, side :< (title, owner))

-- | The key deliberately groups distinct complete bases and both accounting sides.
keyOf :: (AccountTitles, T.Text) -> Maybe Int
keyOf (_, owner) = Just (T.length owner)

-- | Round each output exactly once, following the distinct grouping boundaries.
readoutProperty :: Property
readoutProperty = forAll genRows $ \rows -> forAll (shuffle rows) $ \permuted ->
    let algebra = ledger rows
        reordered = ledger permuted
        nets = residuals rows
        gross = sum [toRational value | (value, _) <- rows]
        signedKeys = Map.fromListWith (+)
            [(T.length owner, value) | ((_, owner), value) <- Map.toList nets]
        pairs = Map.fromListWith (\(a,b) (c,d) -> (a+c,b+d))
            [(T.length owner, (max 0 value, max 0 (negate value)))
            | ((_, owner), value) <- Map.toList nets, value /= 0]
        roundedPairs = fmap (\(a,b) -> (fromRational a, fromRational b)) pairs
        surviving = Map.map fromRational (Map.filter (/= 0) nets)
        sides = Map.fromListWith (+)
            [((hat b, base b), toRational value) | (value,b) <- rows]
        compressed = Map.fromList
            [((hat b, base b), value) | (value,b) <- entries (compress algebra)]
        accounting = sum
            [case whichSide b of
                Credit -> toRational value
                Debit  -> negate (toRational value)
                Side   -> 0
            | (value,b) <- rows]
        expectedSide
            | accounting > 0 = Credit
            | accounting < 0 = Debit
            | otherwise      = Side
        query = [HatNot :< (AccountTitle, T.empty)]
        selected = sum (List.map abs (Map.elems nets))
        plus = sum [value | ((side,_),value) <- Map.toList sides, side == Not]
        minus = sum [value | ((side,_),value) <- Map.toList sides, side == Hat]
        callback _ value = value .@ (Not :< (Cash,T.pack "1"))
        posted = EA.postFromNetBy (keyOf . base) callback algebra
        postedExpected = sum
            [toRational (fromRational value :: Double)
            | value <- Map.elems (Map.fromListWith (+)
                [(T.length owner, abs value) | ((_,owner),value) <- Map.toList nets
                , value /= 0])]
    in conjoin
        [ counterexample "norm" $ norm algebra === fromRational gross
        , counterexample "bar" $ EA.balanceMapBy Just (bar algebra) === surviving
        , counterexample "compress" $ Map.filter (/=0) compressed === fmap fromRational sides
        , counterexample "balanceMapBy" $ EA.balanceMapBy keyOf algebra === fmap fromRational signedKeys
        , counterexample "netPairMapBy" $ EA.netPairMapBy keyOf algebra === roundedPairs
        , counterexample "balance" $ balance algebra === (accounting == 0)
        , counterexample "diffRL" $ diffRL algebra === (expectedSide, fromRational (abs accounting))
        , counterexample "projection" $ EA.projNetNorm query algebra === fromRational selected
        , counterexample "balanceBy" $
            EA.balanceBy [Not :< (AccountTitle,T.empty)] [Hat :< (AccountTitle,T.empty)] algebra
                === fromRational (plus - minus)
        , counterexample "postFromNetBy" $ norm posted === fromRational postedExpected
        , counterexample "permutation" $ observations algebra === observations reordered
        ]
  where
    entries = EA.foldEntries (\out value b -> (value,b):out) []
    observations algebra =
        (castDoubleToWord64 (norm algebra), EA.balanceMapBy Just (bar algebra),
         List.sort (entries (compress algebra)), EA.balanceMapBy keyOf algebra,
         EA.netPairMapBy keyOf algebra, (balance algebra, diffRL algebra),
         (EA.projNetNorm [HatNot :< (AccountTitle,T.empty)] algebra,
          EA.balanceBy [Not :< (AccountTitle,T.empty)] [Hat :< (AccountTitle,T.empty)] algebra),
         EA.balanceMapBy Just (EA.postFromNetBy (keyOf . base)
            (\_ value -> value .@ (Not :< (Cash,T.pack "1"))) algebra))

-- | Note-local cancellation and global rounding are distinct from gathering notes.
journalProperty :: Property
journalProperty = forAll genRows $ \rows ->
    forAll (shuffle (zip (cycle ["a", "b", "c"]) rows)) $ \permuted ->
    let annotated = zip (cycle ["a", "b", "c"]) rows
        journal = Journal.fromList [value .@ b Journal..| note | (note,(value,b)) <- annotated]
        reordered = Journal.fromList [value .@ b Journal..| note | (note,(value,b)) <- permuted]
        byNote = Map.fromListWith (++) [(note, [row]) | (note,row) <- annotated]
        magnitude = sum [sum (List.map abs (Map.elems (residuals rs))) | rs <- Map.elems byNote]
        chosen = sum [sum (List.map abs (Map.elems (residuals rs)))
                     | (note,rs) <- Map.toList byNote, note /= "c"]
        query = [HatNot :< (AccountTitle,T.empty)]
        callback key value = value .@ (Not :< (Cash,T.pack (show key))) Journal..| "callback"
        output = Journal.postFromNetBy (keyOf . base) callback journal
        gross = sum [toRational value | (value,_) <- rows]
        nets = residuals rows
        accounting = sum
            [case whichSide b of
                Credit -> toRational value
                Debit  -> negate (toRational value)
                Side   -> 0
            | (value,b) <- rows]
        expectedSide
            | accounting > 0 = Credit
            | accounting < 0 = Debit
            | otherwise      = Side
        posted = Map.fromList
            [((Cash,T.pack (show key)), fromRational value)
            | (key,value) <- Map.toList (Map.fromListWith (+)
                [(T.length owner, abs value) | ((_,owner),value) <- Map.toList nets
                , value /= 0])]
        compressed = fmap (fmap fromRational . Map.fromListWith (+) . List.map
            (\(value,b) -> ((hat b,base b), toRational value))) byNote
        observed = EA.foldEntries (\out value b ->
            Map.insert (base b) (if isHat b then negate value else value) out) Map.empty
    in conjoin
        [ norm journal === fromRational gross
        , observed (Journal.toAlg (bar journal))
            === fmap fromRational (Map.filter (/=0) nets)
        , balance journal === (accounting == 0)
        , diffRL journal === (expectedSide, fromRational (abs accounting))
        , Journal.projWithBaseNetNorm query journal === fromRational magnitude
        , Journal.projWithNoteBaseNetNorm ["a","b","a"] query journal === fromRational chosen
        , observed (Journal.toAlg output) === posted
        , counterexample "journal permutation" $ observations journal === observations reordered
        , fmap (EA.foldEntries (\out value b -> Map.insert (hat b,base b) value out) Map.empty)
            (Map.fromList (HashMap.toList (Journal.toMap (compress journal)))) === compressed
        ]

  where
    observations journal =
        (norm journal, EA.balanceMapBy Just (Journal.toAlg (bar journal)),
         fmap (EA.foldEntries (\out value b -> Map.insert (hat b,base b) value out) Map.empty)
            (Journal.toMap (compress journal)),
         (balance journal,diffRL journal),
         Journal.projWithBaseNetNorm [HatNot :< (AccountTitle,T.empty)] journal,
         Journal.projWithNoteBaseNetNorm ["a","b","a"] [HatNot :< (AccountTitle,T.empty)] journal,
         Journal.toMap (Journal.postFromNetBy (keyOf . base)
            (\key value -> value .@ (Not :< (Cash,T.pack (show key))) Journal..| "callback") journal))

-- | Compare classifications and exact finite residuals, then the stored sequences.
pairOrderProperty :: Property
pairOrderProperty = forAll genPair $ \(hats,nots) ->
    forAll genPair $ \(otherHats,otherNots) ->
    let oracle hs ns
            | any isNaN (hs ++ ns) || (any isInfinite hs && any isInfinite ns) = (3 :: Int, 0)
            | any isInfinite hs = (2, 0)
            | any isInfinite ns = (0, 0)
            | otherwise = (1, sum (List.map toRational hs) - sum (List.map toRational ns))
        first = Internal.Pair (Seq.fromList hats) (Seq.fromList nots)
        second = Internal.Pair (Seq.fromList otherHats) (Seq.fromList otherNots)
        expected = compare (oracle hats nots) (oracle otherHats otherNots)
                <> compare (hats,nots) (otherHats,otherNots)
    in compare first second === expected
  where
    genPair = do
        rows <- genRows
        let hats = [value | (value,b) <- rows, isHat b]
            nots = [value | (value,b) <- rows, not (isHat b)]
        elements [(hats,nots), (1/0:hats,nots), (hats,1/0:nots),
                  (1/0:hats,1/0:nots), (0/0:hats,nots)]

-- | Fixed binary64 boundaries and the six migration examples.
fixedCases :: IO ()
fixedCases = do
    let t = 2^(53 :: Int) :: Double
        maximumFinite = encodeFloat (2^(53 :: Int)-1) 971 :: Double
        tiny = encodeFloat 1 (-1074) :: Double
        a = Not :< (Cash,T.pack "1")
        ah = Hat :< (Cash,T.pack "1")
        b = Not :< (Cash,T.pack "2")
        bh = Hat :< (Cash,T.pack "2")
        allBases = [HatNot :< (AccountTitle,T.empty)]
        cents = ledger [(0.1,a),(0.2,a),(0.3,ah)] :: Alg Double Base
        centsOracle = toRational (0.1 :: Double) + toRational (0.2 :: Double)
                    - toRational (0.3 :: Double)
    check "example 1: binary decimal residual" (fromRational centsOracle) (norm (bar cents))
    check "example 2: genuine small residual" (0.5 :: Double)
        (norm (bar (ledger [(1e12,a),(0.5,a),(1e12,ah)])))
    let x = ledger [(t,a),(1,a)]
        y = ledger [(1,b)]
    check "example 3: one gross rounding" (t+2) (norm (x .+ y))
    check "example 3: separately rounded norms" t (norm x + norm y)
    check "example 4: unrounded projection" (t+2) (EA.projNetNorm allBases (x .+ y))
    check "example 4: rounded bar first" t (norm (bar (x .+ y)))
    let lost = ledger [(t,a),(1,a),(t,ah)]
    check "example 5: original net" 1 (norm (bar lost))
    check "example 5: compress loses net" 0 (norm (bar (compress lost)))
    -- Example 6 observes the acceptance boundary through balance. The separate
    -- validation layer is intentionally outside this work item's write scope.
    check "example 6: rounded equality is insufficient" False (balance lost)
    check "example 6: legacy tolerance" True (balanceWithin defaultTolerance lost)
    forM_ [ [(maximumFinite,a),(maximumFinite,a),(maximumFinite,ah)]
          , [(tiny,a),(tiny,a),(tiny,ah)]
          , [(1e100,a),(1,a),(1e100,ah)]
          ] $ \rows -> do
        let expected = fromRational (sum (Map.elems (residuals rows)))
        forM_ (List.permutations rows) $ \order ->
            check "boundary permutations" expected (norm (bar (ledger order)))
    check "positive zero" 0 (castDoubleToWord64 (norm ((-0) `Internal.Single` a)))
    let grouped = ledger [(t,a),(1,a),(t,bh)]
        exactN = toRational t + 1
        exactH = toRational t
    check "unrounded key identity" 1 (exactN - exactH)
    check "Double signed key" (Map.singleton () 1) (EA.balanceMapBy (const (Just ())) grouped)
    check "Double rounded pair" (Map.singleton () (t,t)) (EA.netPairMapBy (const (Just ())) grouped)
    let decimal = ledger [(2^53,a),(1,a),(2^53,bh)] :: Alg MoneyDecimal Base
    check "MoneyDecimal signed oracle" (Map.singleton () (fromRational (exactN-exactH)))
        (EA.balanceMapBy (const (Just ())) decimal)
    check "MoneyDecimal pair-difference oracle" (Map.singleton () (fromRational (exactN-exactH)))
        (fmap (uncurry (-)) (EA.netPairMapBy (const (Just ())) decimal))
    let first = x .+ y
        second = ledger [(t,a),(1,b)]
        post _ value = value .@ a
    check "same rounded bar" (bar first) (bar second)
    check "callback first" (t+2) (norm (EA.postFromNetBy (const (Just ())) post first))
    check "callback second" t (norm (EA.postFromNetBy (const (Just ())) post second))
    check "Rational first callback oracle"
        (fromRational (sum (List.map abs (Map.elems (residuals [(t,a),(1,a),(1,b)])))))
        (norm (EA.postFromNetBy (const (Just ())) post first))
    check "Rational second callback oracle"
        (fromRational (sum (List.map abs (Map.elems (residuals [(t,a),(1,b)])))))
        (norm (EA.postFromNetBy (const (Just ())) post second))
    let decimalFirst = ledger [(2^53,a),(1,a),(1,b)] :: Alg MoneyDecimal Base
        decimalSecond = ledger [(2^53,a),(1,b)] :: Alg MoneyDecimal Base
    check "MoneyDecimal first callback" (2^53+2)
        (norm (EA.postFromNetBy (const (Just ())) (\_ value -> value .@ a) decimalFirst))
    check "MoneyDecimal second callback" (2^53+1)
        (norm (EA.postFromNetBy (const (Just ())) (\_ value -> value .@ a) decimalSecond))
    let money = ledger [(0.1,a),(0.2,a),(0.3,ah)] :: Alg MoneyDouble Base
    check "MoneyDouble rounds like Double" (fromRational centsOracle) (norm (bar money))
    let infinity = 1/0 :: Double
        special = EA.fromList [infinity `Internal.Single` a, 1 `Internal.Single` a,
                               infinity `Internal.Single` ah]
        specialValues = EA.vals (bar special)
    check "nonfinite bar retains postings" 3 (length specialValues)
    check "nonfinite balance" False (balance special)
    check "nonfinite norm" True (isInfinite (norm special))
    case diffRL special of
        (Side, value) -> check "NaN diffRL" True (isNaN value)
        result -> fail ("unexpected nonfinite direction: " ++ show result)
    case Map.lookup () (EA.netPairMapBy (const (Just ())) special) of
        Just (notValue, hatValue) -> check "NaN pair" True
            (isNaN notValue && isNaN hatValue)
        Nothing -> fail "missing NaN key"
    check "nonfinite callback bypass" 3
        (length (EA.vals (EA.postFromNetBy (const (Just ()))
            (\_ _ -> error "nonfinite callback invoked") special)))
    let pair :: [Double] -> [Double] -> Internal.Pair Double
        pair hats nots = Internal.Pair (Seq.fromList hats) (Seq.fromList nots)
    check "Pair exact net before structural tie" GT
        (compare (pair [1,t] []) (pair [t] []))
    check "Pair equal net uses structure" (compare ([1,2] :: [Double]) [3])
        (compare (pair [1,2] []) (pair [3] []))
    check "Pair finite overflow below infinity" LT
        (compare (pair [maximumFinite,maximumFinite] []) (pair [infinity] []))
    check "Pair negative infinity below finite" LT
        (compare (pair [] [infinity]) (pair [] [maximumFinite,maximumFinite]))
    check "Pair NaN above positive infinity" GT
        (compare (pair [infinity] [infinity]) (pair [infinity] []))
    let classify value
            | isNaN value = "NaN"
            | value == infinity = "+Infinity"
            | value == negate infinity = "-Infinity"
            | otherwise = show value
    forM_ [(infinity,a,Debit,"+Infinity",("+Infinity","0.0")),
           (infinity,ah,Credit,"-Infinity",("0.0","+Infinity")),
           (0/0,a,Side,"NaN",("NaN","NaN"))] $
        \(value,postingBase,direction,signed,pairValues) -> do
            let input = value `Internal.Single` postingBase
                (actualDirection,actualDifference) = diffRL input
            check "nonfinite diffRL direction" direction actualDirection
            check "nonfinite diffRL magnitude" (if isNaN value then "NaN" else "+Infinity")
                (classify actualDifference)
            check "nonfinite net pair side" (Map.singleton () pairValues)
                (fmap (\(n,h) -> (classify n,classify h)) (EA.netPairMapBy (const (Just ())) input))
            check "nonfinite signed key" (Map.singleton () signed)
                (fmap classify (EA.balanceMapBy (const (Just ())) input))
            check "nonfinite projection" (classify value) (classify (EA.projNetNorm allBases input))
            check "nonfinite compress" [classify value] (List.map classify (EA.vals (compress input)))
    check "compress rounds nonfinite sides independently" ["+Infinity","+Infinity"]
        (List.map classify (EA.vals (compress special)))
    check "opposite infinities signed key" (Map.singleton () "NaN")
        (fmap classify (EA.balanceMapBy (const (Just ())) special))
    check "opposite infinities projection" "NaN" (classify (EA.projNetNorm allBases special))
    let specialJournal = (infinity `Internal.Single` a Journal..| "first")
                      .+ (1 .@ a Journal..| "second")
                      .+ (infinity `Internal.Single` ah Journal..| "third")
                      .+ (2 .@ b Journal..| "first")
        journalOutput = Journal.postFromNetBy (const (Just ()))
            (\_ value -> value .@ b Journal..| "callback") specialJournal
    check "Journal preserves nonfinite bases and original notes"
        (Map.fromList [("first",[("+Infinity",a)]), ("second",[("1.0",a)]),
                       ("third",[("+Infinity",ah)]), ("callback",[("2.0",b)])])
        (Map.fromList [(note, List.sort (EA.foldEntries (\out value postingBase ->
            (classify value,postingBase):out) [] alg))
            | (note,alg) <- HashMap.toList (Journal.toMap journalOutput)])
    -- Golden values obtained from the Hackage 0.5.3.0 package, not the new bar.
    forM_ [(cents, [], True, (Side,0)),
           (ledger [(1e12,a),(0.5,a),(1e12,ah)], [], True, (Side,0)),
           (ledger [(12,a),(3,ah)], [(Not,9)], False, (Debit,9)),
           (ledger [(3,a),(12,ah)], [(Hat,9)], False, (Credit,9)),
           (ledger [(1e10,a),(0.1,a),(0.2,a),(1e10,ah),(0.3,ah)], [], True, (Side,0))]
        $ \(input,expected,expectedBalance,expectedDifference) -> do
        let observe = EA.foldEntries (\rows value postingBase -> (hat postingBase,value):rows) []
        check "W1 Alg golden" expected (observe (barWithin defaultTolerance input))
        check "W1 Journal golden" expected
            (observe (Journal.toAlg (barWithin defaultTolerance (input Journal..| ("golden" :: String)))))
        check "W1 Alg balance golden" expectedBalance (balanceWithin defaultTolerance input)
        check "W1 Alg difference golden" expectedDifference (diffRLWithin defaultTolerance input)
        check "W1 Journal balance golden" expectedBalance
            (balanceWithin defaultTolerance (input Journal..| ("golden" :: String)))
        check "W1 Journal difference golden" expectedDifference
            (diffRLWithin defaultTolerance (input Journal..| ("golden" :: String)))

-- | Run the requested properties and fixed migration cases without tolerances.
runTests :: IO ()
runTests = do
    fixedCases
    forM_ [readoutProperty, journalProperty, pairOrderProperty] $ \propertyToRun -> do
        result <- quickCheckWithResult stdArgs {maxSuccess = 200} propertyToRun
        unless (isSuccess result) (fail "exact readout property failed")
