{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- | Compare compact pairs with a two-sequence reference, including invalid raw values.
module Algebra.PairSpec (runTests) where

import qualified ExchangeAlgebra.Algebra.Internal as E
import qualified ExchangeAlgebra.Algebra.Internal as PairImport (Pair(..))
import ExchangeAlgebra.Algebra.Base
import ExchangeAlgebra.Algebra.Value
import qualified ExchangeAlgebra.Algebra.Transfer as T
import qualified Data.Sequence as S
import qualified Data.HashMap.Strict as H
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Binary as B
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as BL
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_)
import Test.QuickCheck
import Support (assertEqual, quickProp)

type Base = HatBase CountUnit
type Algebra = E.Alg Double Base

-- | Distinguish the base-level predicate from the Hat predicate used by legacy map.
newtype CustomBase = CustomBase Base
    deriving (Eq, Ord, Show, Hashable)

instance Element CustomBase where
    wildcard = CustomBase wildcard

instance BaseClass CustomBase

instance HatBaseClass CustomBase where
    type BasePart CustomBase = CountUnit
    base (CustomBase b) = base b
    hat (CustomBase b) = hat b
    merge h bp = CustomBase (merge h bp)
    toHat (CustomBase b) = CustomBase (toHat b)
    toNot (CustomBase b) = CustomBase (toNot b)
    revHat (CustomBase b) = CustomBase (revHat b)
    isHat (CustomBase b) = not (isHat b)
    isNot (CustomBase b) = not (isNot b)

-- | The previous pair representation; no compact constructors or production helpers.
data RefPair = RefPair (S.Seq Double) (S.Seq Double)

-- | Preserve the observable singleton versus map distinction.
data Ref = Empty | One Double Base | Many (H.HashMap CountUnit RefPair)

-- | Concatenate both sides in left-to-right order.
append :: RefPair -> RefPair -> RefPair
append (RefPair hs ns) (RefPair ks ms) = RefPair (hs S.>< ks) (ns S.>< ms)

-- | Unchecked reference posting, for representation and invalid-value tests.
posting :: Double -> Base -> RefPair
posting v b
    | isHat b = RefPair (S.singleton v) S.empty
    | otherwise = RefPair S.empty (S.singleton v)

-- | Old insertion order: a new posting precedes the existing side.
insertRef :: Base -> Double -> Ref -> Ref
insertRef _ v x | isZeroValue v = x
insertRef b v Empty = One v b
insertRef b v (One w c) = insertRef c w (Many (H.singleton (base b) (posting v b)))
insertRef b v (Many m) = Many (H.insertWith append (base b) (posting v b) m)

-- | Old pairwise union, including its asymmetric singleton insertion path.
unionRef :: Ref -> Ref -> Ref
unionRef Empty x = x
unionRef x Empty = x
unionRef (One v b) (One w c)
    | isZeroValue v = if isZeroValue w then Empty else One w c
    | isZeroValue w = One v b
    | otherwise = insertRef c w (One v b)
unionRef x (One v b) = insertRef b v x
unionRef (One v b) x = insertRef b v x
unionRef (Many m) (Many n) = Many (H.unionWith append m n)

-- | Construct only the expected representation, after computing it independently.
materialize :: Ref -> Algebra
materialize Empty = E.Zero
materialize (One v b) = E.Single v b
materialize (Many m) = E.linerFromMap (H.map pair m)
  where
    pair :: RefPair -> E.Pair Double
    pair (RefPair hs ns) = E.Pair hs ns

-- | Collapse a singleton map exactly where the old implementation did.
rebuild :: H.HashMap CountUnit RefPair -> Ref
rebuild m
    | H.null m = Empty
    | otherwise = case H.toList m of
        [(b, RefPair hs ns)] -> case (F.toList hs, F.toList ns) of
            ([v], []) -> One v (Hat :< b)
            ([], [v]) -> One v (Not :< b)
            _ -> Many m
        _ -> Many m

-- | Empty-map reduction used by filter, bar, and compress; retain nonempty maps.
keepMap :: H.HashMap CountUnit RefPair -> Ref
keepMap m = if H.null m then Empty else Many m

-- | Sequence traversal used by foldEntries and mapping, before zero filtering.
entries :: Ref -> [(Double, Base)]
entries Empty = []
entries (One v b) = [(v, b)]
entries (Many m) = concatMap perBase (H.toList m)
  where
    perBase :: (CountUnit, RefPair) -> [(Double, Base)]
    perBase (bp, RefPair hs ns) =
        [(v, Hat :< bp) | v <- F.toList hs] ++ [(v, Not :< bp) | v <- F.toList ns]

-- | Old toList reverses each side, visits Not before Hat, then prepends each base.
listRef :: Ref -> [Algebra]
listRef Empty = []
listRef (One v b) = [E.Single v b]
listRef (Many m) = H.foldlWithKey' step [] m
  where
    step :: [Algebra] -> CountUnit -> RefPair -> [Algebra]
    step xs bp (RefPair hs ns) =
        F.foldl' (add Hat bp) (F.foldl' (add Not bp) xs ns) hs
    add :: Hat -> CountUnit -> [Algebra] -> Double -> [Algebra]
    add h bp xs v = if isZeroValue v then xs else E.Single v (h :< bp) : xs

-- | Encode the previous tag, count, and two-sequence layout directly.
wireRef :: Ref -> BL.ByteString
wireRef = P.runPut . putRef
  where
    putRef :: Ref -> P.Put
    putRef Empty = B.put (0 :: Int)
    putRef (One v b) = B.put (1 :: Int) >> B.put v >> B.put b
    putRef (Many m) = do
        B.put (2 :: Int)
        B.put (H.size m)
        forM_ (H.toList m) $ \(bp, RefPair hs ns) -> do
            B.put bp
            B.put (S.length hs)
            mapM_ B.put hs
            B.put (S.length ns)
            mapM_ B.put ns

-- | A predicate restriction neither aggregates nor collapses singleton maps.
filterRef :: (Double -> Base -> Bool) -> Ref -> Ref
filterRef _ Empty = Empty
filterRef f x@(One v b) = if f v b then x else Empty
filterRef f (Many m) = keepMap (H.mapMaybeWithKey step m)
  where
    step :: CountUnit -> RefPair -> Maybe RefPair
    step bp (RefPair hs ns) = nonempty
        (S.filter (\v -> f v (Hat :< bp)) hs)
        (S.filter (\v -> f v (Not :< bp)) ns)

-- | Drop only empty pairs, retaining the original order otherwise.
nonempty :: S.Seq Double -> S.Seq Double -> Maybe RefPair
nonempty hs ns
    | S.null hs && S.null ns = Nothing
    | otherwise = Just (RefPair hs ns)

-- | Exact old side states, computed from sequences rather than compact pairs.
states :: RefPair -> (Accum Double, Accum Double)
states (RefPair hs ns) = (total hs, total ns)
  where
    total :: S.Seq Double -> Accum Double
    total = F.foldl' (flip addAccum) emptyAccum

-- | Old exact bar, including negative, nonfinite, and zero raw inputs.
barRef :: Ref -> Ref
barRef Empty = Empty
barRef x@(One v _) = if isZeroValue v then Empty else x
barRef (Many m) = keepMap (H.mapMaybe step m)
  where
    step :: RefPair -> Maybe RefPair
    step pair = case signAccum residual of
        Just EQ -> Nothing
        Just LT -> settle Hat (negateAccum residual)
        _ -> settle Not residual
      where
        (hs, ns) = states pair
        residual = mergeAccum ns (negateAccum hs)
        nonfinite = signAccum (mergeAccum residual (negateAccum residual)) /= Just EQ
        settle :: Hat -> Accum Double -> Maybe RefPair
        settle side magnitude
            | isErrorValue v && nonfinite = Just pair
            | isZeroValue v = Nothing
            | otherwise = Just (posting v (side :< Yen))
          where
            v = roundAccum magnitude

-- | Old compress rounds each side independently, even for invalid raw values.
compressRef :: Ref -> Ref
compressRef Empty = Empty
compressRef x@(One v _) = if isZeroValue v then Empty else x
compressRef (Many m) = keepMap (H.mapMaybe step m)
  where
    step :: RefPair -> Maybe RefPair
    step pair = let (hs, ns) = states pair in nonempty (side hs) (side ns)
    side :: Accum Double -> S.Seq Double
    side state = let v = roundAccum state in if isZeroValue v then S.empty else S.singleton v

-- | Old tolerance bar sums sequentially, with its existing singleton shortcut.
withinRef :: Ref -> Ref
withinRef Empty = Empty
withinRef x@(One _ _) = x
withinRef (Many m) = keepMap (H.mapMaybe step m)
  where
    step :: RefPair -> Maybe RefPair
    step p@(RefPair hs ns)
        | not (isErrorValue h || isErrorValue n)
            && abs (h - n) <= absTol tolerance + relTol tolerance * max (abs h) (abs n) = Nothing
        | S.length hs == 1 && S.null ns && h > n = Just p
        | S.null hs && S.length ns == 1 && n > h = Just p
        | compare h n == GT = Just (RefPair (S.singleton (h - n)) S.empty)
        | otherwise = Just (RefPair S.empty (S.singleton (n - h)))
      where
        h = F.foldl' (+) 0 hs
        n = F.foldl' (+) 0 ns
        tolerance = defaultTolerance

-- | Apply a side-preserving value transform without changing the representation.
scaleRef :: Double -> Ref -> Ref
scaleRef _ Empty = Empty
scaleRef 0 _ = Empty
scaleRef k (One v b) = if isZeroValue (k * v) then Empty else One (k * v) b
scaleRef k (Many m) = filterRef (\v _ -> not (isZeroValue v)) $ Many
    (H.map (\(RefPair hs ns) -> RefPair (fmap (k *) hs) (fmap (k *) ns)) m)

-- | Swap sides without aggregating or changing within-side order.
hatRef :: Ref -> Ref
hatRef Empty = Empty
hatRef (One v b) = One v (revHat b)
hatRef (Many m) = Many (H.map (\(RefPair hs ns) -> RefPair ns hs) m)

-- | Concrete or wildcard selection on the CountUnit axis.
projRef :: [Base] -> Ref -> Ref
projRef qs x = case x of
    Empty -> Empty
    One v b -> if any (`matches` b) qs then One v b else Empty
    Many m -> rebuild (H.mapMaybeWithKey choose m)
  where
    matches :: Base -> Base -> Bool
    matches (h :< bp) (k :< cp) = (h == HatNot || h == k) && (bp == CountUnit || bp == cp)
    choose :: CountUnit -> RefPair -> Maybe RefPair
    choose bp (RefPair hs ns) = nonempty
        (if any (`matches` (Hat :< bp)) qs then hs else S.empty)
        (if any (`matches` (Not :< bp)) qs then ns else S.empty)

-- | Independent ordered list interpretation of typed posting mapping.
mapRef :: (Double -> Base -> (Double, Base)) -> Ref -> Ref
mapRef _ Empty = Empty
mapRef f (One v b) = let (w, c) = f v b in if isZeroValue w then Empty else One w c
mapRef f x = rebuild (foldr add H.empty (map (uncurry f) (entries x)))
  where
    add :: (Double, Base) -> H.HashMap CountUnit RefPair -> H.HashMap CountUnit RefPair
    add (v, b) m
        | isZeroValue v = m
        | otherwise = H.insertWith append (base b) (posting v b) m

-- | Legacy map reverses unchanged entries on each side, before merging collisions.
legacyMapRef :: (Double -> Base -> (Double, Base)) -> Ref -> Ref
legacyMapRef f x@(One _ _) = mapRef f x
legacyMapRef _ Empty = Empty
legacyMapRef f (Many m) = rebuild (foldr add H.empty (concatMap perBase (H.toList m)))
  where
    perBase :: (CountUnit, RefPair) -> [(Double, Base)]
    perBase (bp, RefPair hs ns) = changedH ++ changedN ++ sameH ++ sameN
      where
        (changedH, sameH) = side (Hat :< bp) hs
        (changedN, sameN) = side (Not :< bp) ns
    side :: Base -> S.Seq Double -> ([(Double, Base)], [(Double, Base)])
    side b vs = (filter ((/= b) . snd) mapped, reverse (filter ((== b) . snd) mapped))
      where
        mapped = filter (not . isZeroValue . fst) [f v b | v <- F.toList vs]
    add :: (Double, Base) -> H.HashMap CountUnit RefPair -> H.HashMap CountUnit RefPair
    add (v, b) = H.insertWith append (base b) (posting v b)

-- | The old pair ordering: exact signed classification, then lexicographic sequences.
compareRef :: RefPair -> RefPair -> Ordering
compareRef x@(RefPair hs ns) y@(RefPair ks ms) =
    compare (key x) (key y) <> compare (hs, ns) (ks, ms)
  where
    key :: RefPair -> (Int, Rational)
    key pair = case exactAccum residual of
        Just value -> (1, value)
        Nothing -> case signAccum residual of
            Just LT -> (0, 0)
            Just GT -> (2, 0)
            _ -> (3, 0)
      where
        (h, n) = states pair
        residual = negateAccum (mergeAccum n (negateAccum h))

-- | Inspect every pair produced by the implementation.
normalized :: Algebra -> Bool
normalized (E.Liner m _ _ _ _ _) = all valid (H.elems m)
  where
    valid :: E.Pair Double -> Bool
    valid (E.HatOne _) = True
    valid (E.NotOne _) = True
    valid (E.PairSeq hs ns) = (S.length hs, S.length ns) `notElem` [(1, 0), (0, 1)]
normalized _ = True

-- | Compare observable order, shape, text, and bytes; string comparison handles NaN.
check :: String -> Ref -> Algebra -> IO ()
check label ref actual = do
    assertEqual (label ++ ": bytes") (wireRef ref) (B.encode actual)
    assertEqual (label ++ ": toList") (show (listRef ref)) (show (E.toList actual))
    assertEqual (label ++ ": vals") (show (map fst (entries ref))) (show (E.vals actual))
    let expectedBases = case ref of
            Many _ -> reverse (map snd (entries ref))
            _ -> map snd (entries ref)
        expectedShow = case L.sort (listRef ref) of
            [] -> "0"
            xs -> L.intercalate " .+ " (map show xs)
    assertEqual (label ++ ": bases") expectedBases (E.bases actual)
    assertEqual (label ++ ": show") expectedShow (show actual)
    assertEqual (label ++ ": canonical") True (normalized actual)
    assertEqual (label ++ ": foldEntries")
        (show (filter (not . isZeroValue . fst) (entries ref)))
        (show (E.foldEntries (\xs v b -> xs ++ [(v, b)]) [] actual))

-- | Exercise all value operations against the old two-sequence interpretation.
checkOperations :: Ref -> IO ()
checkOperations ref = do
    let x = materialize ref
        normRef = roundAccum (foldl (\a (v, _) -> addAccum v a) emptyAccum (entries ref))
        predicate :: Double -> Base -> Bool
        predicate v b = isHat b || v > 2
    check "input" ref x
    check "bar" (barRef ref) (E.bar x)
    check "minus" (barRef ref) ((E..-) x)
    check "compress" (compressRef ref) (E.compress x)
    check "barWithin" (withinRef ref) (E.barWithin defaultTolerance x)
    check "hat" (hatRef ref) ((E..^) x)
    check "scalar" (scaleRef 2 ref) (2 E..* x)
    check "filter" (filterRef predicate ref)
        (E.filter (\(v E.:@ b) -> predicate v b) x)
    assertEqual "norm" (show (normRef :: Double)) (show (E.norm x))
    forM_ [[Hat :< Yen], [HatNot :< CountUnit], [Not :< Dollar, HatNot :< Yen]] $ \qs ->
        check "proj" (projRef qs ref) (E.proj qs x)

-- | Each generated ledger contains singleton bases of both sides and a repeated base.
genMixedPostings :: Gen [(Double, Base)]
genMixedPostings = do
    h <- choose (1, 1000)
    n <- choose (1, 1000)
    hs <- chooseInt (2, 20) >>= \count -> vectorOf count (choose (1, 1000))
    ns <- chooseInt (2, 20) >>= \count -> vectorOf count (choose (1, 1000))
    shuffle $ [(h, Hat :< Yen), (n, Not :< Dollar)]
        ++ [(v, Hat :< Euro) | v <- hs] ++ [(v, Not :< Euro) | v <- ns]

-- | Decode independently written length/value streams, including an empty map entry.
binaryTests :: IO ()
binaryTests = do
    quickProp "Pair: mixed shapes round-trip through Binary" $
        forAll genMixedPostings $ \ps -> ioProperty $ do
            let ref = foldl unionRef Empty [One v b | (v, b) <- ps]
                actual = E.fromList [v E..@ b | (v, b) <- ps]
                decoded = B.decode (B.encode actual) :: Algebra
                mixed = case actual of
                    E.Liner m _ _ _ _ _ -> case (H.lookup Yen m, H.lookup Dollar m,
                                                 H.lookup Euro m) of
                        (Just (E.HatOne _), Just (E.NotOne _), Just (E.PairSeq hs ns)) ->
                            S.length hs >= 2 && S.length ns >= 2
                        _ -> False
                    _ -> False
            assertEqual "generator includes all three shapes on every trial" True mixed
            assertEqual "Alg Binary round-trip equality" actual decoded
            check "mixed Binary reference" ref decoded
            pure True
    forM_ [([2], [3]), ([2, 5], []), ([], [3, 7]), ([], [])] $ \(hs, ns) -> do
        let pair = RefPair (S.fromList hs) (S.fromList ns)
            bytes = wireRef (Many (H.singleton Yen pair))
            expected = if null hs && null ns then Empty else Many (H.singleton Yen pair)
            decoded = B.decode bytes :: Algebra
        assertEqual "handwritten Alg bytes decode to reference" (materialize expected) decoded
        check ("handwritten Binary lengths " ++ show (length hs, length ns)) expected decoded

-- | Legacy map must not substitute a custom base predicate for the Hat predicate.
customMapTests :: IO ()
customMapTests =
    forM_ [(S.singleton 3, S.empty), (S.empty, S.singleton 5),
           (S.fromList [3, 7], S.fromList [5, 11])] $ \(hs, ns) ->
        forM_ [\v b -> (v, b), \v b -> (v, hat b :< Dollar),
               \v b -> (v, revHat b), \v _ -> (v, Hat :< Dollar),
               \_ b -> (0, b)] $ \rewrite -> do
            let ref = Many (H.singleton Yen (RefPair hs ns))
                input = E.linerFromMap (H.singleton Yen (E.Pair hs ns))
                    :: E.Alg Double CustomBase
                callback :: E.Alg Double CustomBase -> E.Alg Double CustomBase
                callback (v E.:@ CustomBase b) =
                    let (w, c) = rewrite v b in E.Single w (CustomBase c)
                callback E.Zero = E.Zero
                callback _ = error "customMapTests: expected one posting"
                unwrap :: E.Alg Double CustomBase -> Algebra
                unwrap E.Zero = E.Zero
                unwrap (v E.:@ CustomBase b) = E.Single v b
                unwrap (E.Liner m _ _ _ _ _) = E.linerFromMap m
            check "custom HatBaseClass legacy map" (legacyMapRef rewrite ref)
                (unwrap (E.map callback input))
            check "custom HatBaseClass dropping map" Empty (unwrap (E.map (const E.Zero) input))

-- | Constructors, selectors, and record updates work for all three shapes.
patternTests :: IO ()
patternTests = do
    forM_ [([], []), ([1], []), ([], [2]), ([1, 2], [3]), ([1], [2])] $ \(hs, ns) -> do
        let p = PairImport.Pair (S.fromList hs) (S.fromList ns) :: E.Pair Double
            E.Pair actualH actualN = p
            q = p {E._hatSide = S.singleton 7}
            r = p {E._notSide = S.empty}
        assertEqual "pattern match hat" hs (F.toList actualH)
        assertEqual "pattern match not" ns (F.toList actualN)
        assertEqual "selector hat" hs (F.toList (E._hatSide p))
        assertEqual "selector not" ns (F.toList (E._notSide p))
        assertEqual "record update hat" ([7], ns)
            (F.toList (E._hatSide q), F.toList (E._notSide q))
        assertEqual "record update not" (hs, [])
            (F.toList (E._hatSide r), F.toList (E._notSide r))
        assertEqual "Pair decoder canonical" True $ case (B.decode (B.encode p) :: E.Pair Double) of
            E.PairSeq h n -> (S.length h, S.length n) `notElem` [(1, 0), (0, 1)]
            E.HatOne _ -> True
            E.NotOne _ -> True
    let lazyValue = error "lazy payload" :: Double
        lazyPairs = [E.Pair (S.singleton lazyValue) S.empty,
                     E.Pair S.empty (S.singleton lazyValue),
                     E.Pair (S.singleton lazyValue) (S.singleton lazyValue)]
    forM_ lazyPairs $ \pair -> do
        result <- try (evaluate pair) :: IO (Either SomeException (E.Pair Double))
        assertEqual "pair payload remains lazy" True (either (const False) (const True) result)
        _ <- evaluate (S.length (PairImport._hatSide pair))
        _ <- evaluate (pair {PairImport._notSide = S.singleton lazyValue})
        pure ()
    let oldNegativeCount = P.runPut (B.put (-1 :: Int) >> B.put (1 :: Int) >> B.put (2 :: Double))
        decoded = B.decode oldNegativeCount :: E.Pair Double
    assertEqual "Pair decoder preserves negative-count behavior" True (E.NotOne 2 == decoded)


-- | A generated input includes both singleton bases and repeated sides.
runTests :: IO ()
runTests = do
    patternTests
    binaryTests
    customMapTests
    quickProp "Pair: arbitrary posting lists preserve old order and wire layout" $
        forAll (listOf ((,) <$> choose (0, 1000) <*> elements
            [Hat :< Yen, Not :< Yen, Hat :< Dollar, Not :< Euro, Not :< CountUnit])) $ \ps ->
            ioProperty $ do
                let ref = foldl unionRef Empty [One v b | (v, b) <- ps]
                    actual = E.fromList [E.Single v b | (v, b) <- ps]
                check "fromList" ref actual
                checkOperations ref
                let mapped :: Double -> Base -> (Double, Base)
                    mapped v b = (v + 1, if base b == Dollar then Not :< Yen else b)
                check "mapPosting" (mapRef mapped ref) (E.mapPosting mapped actual)
                check "legacy map" (legacyMapRef mapped ref)
                    (E.map (\a -> let (v, b) = mapped (E._val a) (E._hatBase a)
                                  in E.Single v b) actual)
                check "mapMaybePosting" (mapRef (\v b -> (if v > 2 then v else 0, b)) ref)
                    (E.mapMaybePosting (\v b -> if v > 2 then Just (v, b) else Nothing) actual)
                let bulk = rebuild $ foldl
                        (\m (v, b) -> if isZeroValue v then m
                            else H.insertWith append (base b) (posting v b) m) H.empty ps
                check "unionsMerge" bulk (E.unionsMerge [E.Single v b | (v, b) <- ps])
                check "append liners" (unionRef ref ref) (actual E..+ actual)
                forM_ (E.decBy (Just . isHat) actual) $ \part ->
                    assertEqual "decBy canonical" True (normalized part)
                assertEqual "Alg decoder canonical" True (normalized (B.decode (B.encode actual)))
                pure True
    forM_ [0, -0, -1, 1e-12, 1, 0/0, 1/0, -1/0, 1e308] $ \v ->
        forM_ [RefPair (S.singleton v) S.empty, RefPair S.empty (S.singleton v),
               RefPair (S.fromList [v, 2]) (S.singleton 3)] $ \pair -> do
            checkOperations (Many (H.singleton Yen pair))
            checkOperations (One v (Hat :< Yen))
    let pairShapes = [(S.empty, S.empty)] ++
            [(S.singleton v, S.empty) | v <- [0, -1, 1, 0/0, 1/0]] ++
            [(S.empty, S.singleton v) | v <- [0, -1, 1, 0/0, 1/0]] ++
            [(S.fromList [1, 2], S.singleton 3), (S.singleton (0/0), S.singleton 1)]
    forM_ pairShapes $ \(hs, ns) -> forM_ pairShapes $ \(ks, ms) ->
        assertEqual "Pair Ord" (compareRef (RefPair hs ns) (RefPair ks ms))
            (compare (E.Pair hs ns) (E.Pair ks ms))
    forM_ [-1, 0/0, 1/0, -1/0] $ \v -> do
        let x = materialize (Many (H.singleton Yen (RefPair (S.singleton v) S.empty)))
        rejected <- try (evaluate (E.mapPosting (\w b -> (w, b)) x))
            :: IO (Either SomeException Algebra)
        assertEqual "mapPosting rejects invalid raw values" True
            (either (const True) (const False) rejected)
    let rules = T.mkTransferRules
            [T.TransferRule (Not :< Yen) (Not :< Euro) T.Relabel,
             T.TransferRule (Hat :< Dollar) (Hat :< Euro) T.Relabel]
        ref = Many (H.fromList [(Yen, RefPair S.empty (S.fromList [1, 2])),
                               (Dollar, RefPair (S.singleton 3) S.empty)])
        moved = foldl unionRef Empty
            [unionRef (One v (revHat b)) (One v (hat b :< Euro)) | (v, b) <- entries ref]
    case rules of
        Left err -> error (show err)
        Right rs -> case T.transferEntries rs (materialize ref) of
            Left err -> error (show err)
            Right result -> check "transferEntries" moved result
