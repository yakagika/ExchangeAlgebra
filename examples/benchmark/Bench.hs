{-# LANGUAGE OverloadedStrings #-}
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
module Main (main) where

import           Criterion.Main
import           Control.DeepSeq          (NFData (..))

import           ExchangeAlgebra.Journal  -- constructors / operators: :@ :< .+ .| Hat Not Cash ...
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Journal  as EJ

type A = EA.Alg Double (HatBase AccountTitles)
type J = EJ.Journal Int Double (HatBase AccountTitles)

-- A shallow NFData for Journal, only so 'env' can realize benchmark inputs.
-- (Library Alg already has an NFData instance.) Bench-local orphan.
instance NFData (EJ.Journal n v b) where
    rnf j = j `seq` ()

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
    , bgroup "Journal/fromList+projWithBaseNorm"
        [ env (pure (mkJournals n)) $ \js ->
            bench (show n) $ whnf (EJ.projWithBaseNorm projKey . EJ.fromList) js
        | n <- sizes ]
    ]
