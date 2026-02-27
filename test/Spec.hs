module Main (main) where

import           ExchangeAlgebraJournal
import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Journal as EJ
import qualified Data.List as L
import           System.Exit (exitFailure)

type TestAlg = EA.Alg Double (HatBase CountUnit)
type TestJournal = EJ.Journal String Double (HatBase CountUnit)

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

main :: IO ()
main = do
    testProjMultiPatternOnePass
    testProjNormFastPath
    testProjWithBaseNorm
    testProjWithNoteNorm
