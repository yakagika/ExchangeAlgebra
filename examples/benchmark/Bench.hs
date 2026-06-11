{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Core micro-benchmarks for the exchangealgebra library.
--
-- This is the benchmark harness referenced by the performance roadmap
-- (ROAD_MAP "ベンチマーク基盤の整備"). It measures the algebraic core operations
-- that the Phase 3 optimization work items act on (construction, bar, projection),
-- at a few input sizes so scaling is visible.
--
-- Run:
--   stack bench exchangealgebra-examples:bench-core
--   stack bench exchangealgebra-examples:bench-core \
--     --benchmark-arguments '--output examples/benchmark/result/report.html'
--
-- Each benchmark drives a scalar-producing pipeline (ending in 'norm' or
-- 'projWithBaseNorm') so 'whnf' forces the whole computation, and inputs are
-- constructed inside 'env' so their cost is excluded from the timed region.
--
-- == HatBase key-hashing study (2026-06-08)
--
-- The MoneyDouble re-profile (sim2fast) showed HashMap *key hashing* of the
-- @BasePart b@ key — for @HatBase2@ a 4-tuple @(AccountTitles,Company,Company,
-- CountUnit)@ — is the new dominant cost (~33%). The @hash/*@ and @container/*@
-- groups below isolate that cost and compare key representations WITHOUT
-- touching the library, to find the highest-ROI lever before any change:
--
--   * V0 tuple    — baseline (current behaviour: 4 chained mixes per key)
--   * V1 packed   — same Eq as the tuple, but Hashable packs to one Word64 (1 mix)
--   * V2 word     — key IS a Word64 (the "intern" ceiling, as a value)
--   * V3 hashed   — 'Data.Hashable.Hashed' (cache the hash in the key)
--   * V4 intmap   — Data.IntMap keyed by the packed Int (no hashing at all)
--   * V5 ordmap   — Data.Map keyed by the tuple (Ord compare instead of hashing)
--
-- Layer A (@hash/*@) measures pure hashing; Layer B (@container/*@) measures the
-- build / lookup / union operations where that hashing actually bites.
module Main (main) where

import           Criterion.Main
import           Control.DeepSeq          (NFData (..))
import           Data.Bits                ((.&.), (.|.), shiftL)
import qualified Data.HashMap.Strict      as HM
import           Data.Hashable            (Hashable (..), Hashed, hashed)
import qualified Data.IntMap.Strict       as IM
import           Data.List                (foldl')
import qualified Data.Map.Strict          as M
import           Data.Word                (Word64)

import           ExchangeAlgebra.Journal  -- constructors / operators: :@ :< .+ .| Hat Not Cash ...
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Journal  as EJ

type A = EA.Alg Double (HatBase AccountTitles)
type J = EJ.Journal Int Double (HatBase AccountTitles)

-- NFData (Journal n v b) comes from the library (ExchangeAlgebra.Journal)
-- since the Simulate.Lite work; no bench-local orphan needed.

-- Bench-local NFData for the enum axes so the 4-tuple keys can be realized in
-- 'env'. These are nullary-constructor enums, so 'seq' reaches normal form.
instance NFData AccountTitles where rnf x = x `seq` ()
instance NFData CountUnit    where rnf x = x `seq` ()

-- Rotate over a handful of asset/revenue titles and both Hat/Not sides.
bases4 :: [AccountTitles]
bases4 = [Cash, Deposits, Sales, Products]

mkAlgs :: Int -> [A]
mkAlgs n =
    [ val :@ hb
    | i <- [1 .. n]
    , let val = fromIntegral (i `mod` 7 + 1) :: Double
          hb  = (if even i then Hat else Not) :< (bases4 !! (i `mod` 4))
    ]

mkJournals :: Int -> [J]
mkJournals n =
    [ (val :@ hb) .| (i `mod` 50)
    | i <- [1 .. n]
    , let val = fromIntegral (i `mod` 7 + 1) :: Double
          hb  = (if even i then Hat else Not) :< (bases4 !! (i `mod` 4))
    ]

sizes :: [Int]
sizes = [1000, 10000]

projKey :: [HatBase AccountTitles]
projKey = [Hat :< Cash]

------------------------------------------------------------------
-- * Journal append (audit R5 / ROAD_MAP P1b)
--
-- A/B for addJournal/appendMap, which folds rhs postings into an
-- accumulator Journal (the inner loop of Lite's @sigma msgs id@ and of
-- every @<>@ on a ledger). Three scenarios isolate the levers:
--
--   append-base-only  : each rhs journal is a @fromMap@ product (base
--                       non-empty, delta empty). Stresses the
--                       materializeMap/toMap short-circuit (delta null).
--   append-same-note  : all N postings share ONE note key, so every fold
--                       step hits the existing-key branch — the
--                       idempotent axis-index re-insert that P1b removes.
--   append-distinct-note : each posting gets a distinct note key, so every
--                       step is a genuinely new index insert (control).
--
-- Inputs are prebuilt single-posting journals in 'env'; only the fold is
-- timed. 'norm' forces the whole accumulator structure.

appendN :: Int

appendN = 2000

-- One posting per element. 'fromMap' so the rhs is base-only (Lite shape).
mkBaseOnly :: Int -> [J]
mkBaseOnly n =
    [ EJ.fromMap (HM.singleton (i `mod` 50) (val :@ hb))
    | i <- [1 .. n]
    , let val = fromIntegral (i `mod` 7 + 1) :: Double
          hb  = (if even i then Hat else Not) :< (bases4 !! (i `mod` 4))
    ]

-- All postings share the single note key 0 (existing-key append path).
mkSameNote :: Int -> [J]
mkSameNote n =
    [ (val :@ hb) .| (0 :: Int)
    | i <- [1 .. n]
    , let val = fromIntegral (i `mod` 7 + 1) :: Double
          hb  = (if even i then Hat else Not) :< (bases4 !! (i `mod` 4))
    ]

-- Each posting gets a distinct note key (new-key append path).
mkDistinctNote :: Int -> [J]
mkDistinctNote n =
    [ (val :@ hb) .| i
    | i <- [1 .. n]
    , let val = fromIntegral (i `mod` 7 + 1) :: Double
          hb  = (if even i then Hat else Not) :< (bases4 !! (i `mod` 4))
    ]

-- Fold the prebuilt journals with (.+) = addJournal, forcing the result.
foldAppend :: [J] -> Double
foldAppend = norm . foldl' (.+) (mempty :: J)

------------------------------------------------------------------
-- * Quotient decomposition study (dec/*, Phase 1 feat/quotient-decomposition)
--
-- A/B for the dec_κ family: per-key reporting over K company keys, comparing
--   (a) the naive loop — one wildcard balanceBy query per key (K queries),
--   (b) balanceMapBy   — one-pass per-key net (P1c),
--   (c) decBy + norm   — one-pass redundancy-preserving partition,
--   (d) postFromNetBy  — fused classify→net→post.
-- The input alg is prebuilt in 'env' so only the query side is timed.
------------------------------------------------------------------

-- bench-local orphans, same shape as examples/basic/simulateEx2.hs
instance Element Int where
    wildcard = -1
instance BaseClass Int where

type CB = HatBase (AccountTitles, Int)
type A2 = EA.Alg Double CB

-- nc companies x 12 postings each (mixed Hat/Not over 4 titles)
mkDecAlg :: Int -> A2
mkDecAlg nc = EA.fromList
    [ val .@ hb
    | c <- [1 .. nc]
    , i <- [1 .. (12 :: Int)]
    , let val = fromIntegral (i `mod` 7 + 1) :: Double
          hb  = (if even i then Hat else Not) :< (bases4 !! (i `mod` 4), c)
    ]

decNs :: [Int]
decNs = [200, 1000]

------------------------------------------------------------------
-- * HatBase key-hashing study
------------------------------------------------------------------

-- | The real Alg HashMap key for @HatBase2@: @BasePart (HatBase a) = a@, here the
-- 4-tuple. AccountTitles/CountUnit hash via @fromEnum@; Company is an Int with
-- wildcard @-1@. The tuple Hashable chains a mix per component (4 mixes/key).
type Key4 = (AccountTitles, Int, Int, CountUnit)

-- | V1: identical Eq to the tuple, but the hash packs the 4 axes into one Word64
-- and mixes once. Measures "make the hash cheaper, same key type" (lever L1).
newtype PK = PK Key4 deriving (Eq)
instance NFData PK where rnf (PK k) = rnf k
instance Hashable PK where
    {-# INLINE hashWithSalt #-}
    hashWithSalt s (PK k) = hashWithSalt s (packKey k)

-- | V2: the key *is* a Word64 (the interning ceiling, as a value — lever L3 upper
-- bound without the merge-time id reconciliation machinery).
newtype KW = KW Word64 deriving (Eq, NFData, Hashable)

-- | Pack @(account, company1, company2, unit)@ into a Word64 and mix once.
-- Injective for the representable ranges (account/unit < 256, company in
-- [-1, 2^24-2]); out-of-range only collides (HashMap falls back to Eq), it is
-- never incorrect. The @+1@ offset makes the wildcard company @-1@ encode as 0,
-- so no negative shows up in the field.
packKey :: Key4 -> Word64
packKey (a, c1, c2, u) =
        (fromIntegral (fromEnum a) .&. 0xFF)
    .|. ((fromIntegral (fromEnum u) .&. 0xFF) `shiftL` 8)
    .|. (encComp c1 `shiftL` 16)
    .|. (encComp c2 `shiftL` 40)
  where
    encComp :: Int -> Word64
    encComp x = fromIntegral (x + 1) .&. 0xFFFFFF
{-# INLINE packKey #-}

-- | @m@ companies → @m*m@ distinct keys, mirroring the all-pairs trade network.
mkKeys :: Int -> [Key4]
mkKeys m =
    [ (acct, c1, c2, unit)
    | c1 <- [1 .. m]
    , c2 <- [1 .. m]
    , let acct = bases4 !! ((c1 + c2) `mod` 4)
          unit = if even (c1 * c2) then Amount else Yen
    ]

-- Container builders (one per key representation). insertWith (+) so colliding
-- keys combine, exactly like the Pair-merge in the real Liner.
buildHM :: [Key4] -> HM.HashMap Key4 Double
buildHM = foldl' (\m k -> HM.insertWith (+) k 1 m) HM.empty

buildPK :: [Key4] -> HM.HashMap PK Double
buildPK = foldl' (\m k -> HM.insertWith (+) (PK k) 1 m) HM.empty

buildKW :: [Key4] -> HM.HashMap KW Double
buildKW = foldl' (\m k -> HM.insertWith (+) (KW (packKey k)) 1 m) HM.empty

buildHashed :: [Key4] -> HM.HashMap (Hashed Key4) Double
buildHashed = foldl' (\m k -> HM.insertWith (+) (hashed k) 1 m) HM.empty

buildIM :: [Key4] -> IM.IntMap Double
buildIM = foldl' (\m k -> IM.insertWith (+) (fromIntegral (packKey k)) 1 m) IM.empty

buildM :: [Key4] -> M.Map Key4 Double
buildM = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty

-- Pure hashing fold (Layer A): hash every key, sum the salts.
hashFold :: Hashable a => [a] -> Int
hashFold = foldl' (\acc k -> acc + hashWithSalt 0 k) 0
{-# INLINE hashFold #-}

-- Companies per size: m → m*m keys. 40²=1600, 120²=14400 distinct bases.
hashMs :: [Int]
hashMs = [40, 120]

label :: Int -> String
label m = "N=" ++ show (m * m)

main :: IO ()
main = defaultMain
    [ bgroup "Alg/fromList"
        [ env (pure (mkAlgs n)) $ \xs ->
            bench (show n) $ whnf (norm . EA.fromList) xs
        | n <- sizes ]
    , bgroup "Alg/sigma"
        [ env (pure (mkAlgs n)) $ \xs ->
            bench (show n) $ whnf (\ys -> norm (EA.sigma ys id)) xs
        | n <- sizes ]
    , bgroup "Alg/unionsMerge"
        [ env (pure (mkAlgs n)) $ \xs ->
            bench (show n) $ whnf (norm . EA.unionsMerge) xs
        | n <- sizes ]
    , bgroup "Alg/bar"
        [ env (pure (mkAlgs n)) $ \xs ->
            bench (show n) $ whnf (norm . bar . EA.fromList) xs
        | n <- sizes ]
    , bgroup "Alg/proj"
        [ env (pure (mkAlgs n)) $ \xs ->
            bench (show n) $ whnf (norm . EA.proj projKey . EA.fromList) xs
        | n <- sizes ]
    -- fromList is a strict left fold (L.foldl' (.+) mempty); ~15x at N=10000,
    -- wider at larger N (the old lazy right fold was super-linear to force).
    , bgroup "Journal/fromList+projWithBaseNorm"
        [ env (pure (mkJournals n)) $ \js ->
            bench (show n) $ whnf (EJ.projWithBaseNorm projKey . EJ.fromList) js
        | n <- sizes ]

    ------------------------------------------------------------------
    -- Journal append (audit R5 / ROAD_MAP P1b)
    ------------------------------------------------------------------
    , bgroup "Journal/append-base-only"
        [ env (pure (mkBaseOnly appendN)) $ \js ->
            bench (show appendN) $ whnf foldAppend js ]
    , bgroup "Journal/append-same-note"
        [ env (pure (mkSameNote appendN)) $ \js ->
            bench (show appendN) $ whnf foldAppend js ]
    , bgroup "Journal/append-distinct-note"
        [ env (pure (mkDistinctNote appendN)) $ \js ->
            bench (show appendN) $ whnf foldAppend js ]

    ------------------------------------------------------------------
    -- Quotient decomposition study (dec/*)
    ------------------------------------------------------------------
    , bgroup "dec/naive-balanceBy-per-key"
        [ env (pure (mkDecAlg nc)) $ \x ->
            bench ("K=" ++ show nc) $ whnf
                (\a -> foldl'
                    (\acc c -> acc + EA.balanceBy [Not :< ((.#), c)] [Hat :< ((.#), c)] a)
                    0 [1 .. nc])
                x
        | nc <- decNs ]
    , bgroup "dec/balanceMapBy"
        [ env (pure (mkDecAlg nc)) $ \x ->
            bench ("K=" ++ show nc) $ whnf
                (\a -> M.foldl' (+) 0 (EA.balanceMapBy (\(_, c) -> Just c) a))
                x
        | nc <- decNs ]
    , bgroup "dec/decBy+norm"
        [ env (pure (mkDecAlg nc)) $ \x ->
            bench ("K=" ++ show nc) $ whnf
                (\a -> M.foldl' (\acc al -> acc + norm al) 0
                           (EA.decBy (\(_ :< (_, c)) -> Just c) a))
                x
        | nc <- decNs ]
    , bgroup "dec/postFromNetBy"
        [ env (pure (mkDecAlg nc)) $ \x ->
            bench ("K=" ++ show nc) $ whnf
                (\a -> norm (EA.postFromNetBy
                    (\b -> case b of (Hat :< (t, c)) -> Just (t, c); _ -> Nothing)
                    (\(t, c) v -> v .@ (Not :< (t, c)))
                    a))
                x
        | nc <- decNs ]

    ------------------------------------------------------------------
    -- Layer A: pure key hashing (HatBase key-hashing study)
    ------------------------------------------------------------------
    , bgroup "hash/tuple"
        [ env (pure (mkKeys m)) $ \ks -> bench (label m) $ whnf hashFold ks
        | m <- hashMs ]
    , bgroup "hash/packed"
        [ env (pure (fmap PK (mkKeys m))) $ \ks -> bench (label m) $ whnf hashFold ks
        | m <- hashMs ]
    , bgroup "hash/word"
        [ env (pure (fmap (KW . packKey) (mkKeys m))) $ \ks -> bench (label m) $ whnf hashFold ks
        | m <- hashMs ]
    , bgroup "hash/hashed"
        [ env (pure (fmap hashed (mkKeys m))) $ \ks -> bench (label m) $ whnf hashFold ks
        | m <- hashMs ]
    , bgroup "hash/int"
        [ env (pure (fmap (fromIntegral . packKey) (mkKeys m) :: [Int])) $ \ks ->
            bench (label m) $ whnf hashFold ks
        | m <- hashMs ]

    ------------------------------------------------------------------
    -- Layer B1: container build (insertWith over all keys)
    ------------------------------------------------------------------
    , bgroup "container-build/tuple"
        [ env (pure (mkKeys m)) $ \ks -> bench (label m) $ nf buildHM ks | m <- hashMs ]
    , bgroup "container-build/packed"
        [ env (pure (mkKeys m)) $ \ks -> bench (label m) $ nf buildPK ks | m <- hashMs ]
    , bgroup "container-build/word"
        [ env (pure (mkKeys m)) $ \ks -> bench (label m) $ nf buildKW ks | m <- hashMs ]
    , bgroup "container-build/hashed"
        [ env (pure (mkKeys m)) $ \ks -> bench (label m) $ nf buildHashed ks | m <- hashMs ]
    , bgroup "container-build/intmap"
        [ env (pure (mkKeys m)) $ \ks -> bench (label m) $ nf buildIM ks | m <- hashMs ]
    , bgroup "container-build/ordmap"
        [ env (pure (mkKeys m)) $ \ks -> bench (label m) $ nf buildM ks | m <- hashMs ]

    ------------------------------------------------------------------
    -- Layer B2: lookup (map prebuilt in env; sum lookups of every key)
    ------------------------------------------------------------------
    , bgroup "container-lookup/tuple"
        [ env (pure (let ks = mkKeys m in (ks, buildHM ks))) $ \ ~(ks, mp) ->
            bench (label m) $ whnf (\m' -> foldl' (\a k -> a + HM.lookupDefault 0 k m') 0 ks) mp
        | m <- hashMs ]
    , bgroup "container-lookup/packed"
        [ env (pure (let ks = mkKeys m in (fmap PK ks, buildPK ks))) $ \ ~(ks, mp) ->
            bench (label m) $ whnf (\m' -> foldl' (\a k -> a + HM.lookupDefault 0 k m') 0 ks) mp
        | m <- hashMs ]
    , bgroup "container-lookup/word"
        [ env (pure (let ks = mkKeys m in (fmap (KW . packKey) ks, buildKW ks))) $ \ ~(ks, mp) ->
            bench (label m) $ whnf (\m' -> foldl' (\a k -> a + HM.lookupDefault 0 k m') 0 ks) mp
        | m <- hashMs ]
    , bgroup "container-lookup/intmap"
        [ env (pure (let ks = mkKeys m in (fmap (fromIntegral . packKey) ks :: [Int], buildIM ks))) $ \ ~(ks, mp) ->
            bench (label m) $ whnf (\m' -> foldl' (\a k -> a + IM.findWithDefault 0 k m') 0 ks) mp
        | m <- hashMs ]
    , bgroup "container-lookup/ordmap"
        [ env (pure (let ks = mkKeys m in (ks, buildM ks))) $ \ ~(ks, mp) ->
            bench (label m) $ whnf (\m' -> foldl' (\a k -> a + M.findWithDefault 0 k m') 0 ks) mp
        | m <- hashMs ]

    ------------------------------------------------------------------
    -- Layer B3: union with full key overlap (re-hashing on merge)
    ------------------------------------------------------------------
    , bgroup "container-union/tuple"
        [ env (pure (let ks = mkKeys m in (buildHM ks, buildHM ks))) $ \ ~(a, b) ->
            bench (label m) $ whnf (\(x, y) -> HM.size (HM.unionWith (+) x y)) (a, b)
        | m <- hashMs ]
    , bgroup "container-union/packed"
        [ env (pure (let ks = mkKeys m in (buildPK ks, buildPK ks))) $ \ ~(a, b) ->
            bench (label m) $ whnf (\(x, y) -> HM.size (HM.unionWith (+) x y)) (a, b)
        | m <- hashMs ]
    , bgroup "container-union/word"
        [ env (pure (let ks = mkKeys m in (buildKW ks, buildKW ks))) $ \ ~(a, b) ->
            bench (label m) $ whnf (\(x, y) -> HM.size (HM.unionWith (+) x y)) (a, b)
        | m <- hashMs ]
    , bgroup "container-union/hashed"
        [ env (pure (let ks = mkKeys m in (buildHashed ks, buildHashed ks))) $ \ ~(a, b) ->
            bench (label m) $ whnf (\(x, y) -> HM.size (HM.unionWith (+) x y)) (a, b)
        | m <- hashMs ]
    , bgroup "container-union/intmap"
        [ env (pure (let ks = mkKeys m in (buildIM ks, buildIM ks))) $ \ ~(a, b) ->
            bench (label m) $ whnf (\(x, y) -> IM.size (IM.unionWith (+) x y)) (a, b)
        | m <- hashMs ]
    ]
