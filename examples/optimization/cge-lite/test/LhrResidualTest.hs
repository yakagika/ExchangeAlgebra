{- |
  LhrResidualTest -- step ③b-1 diagnostic + sentinel for the LHR reduced
  instrument/residual system (general-equilibrium:phase1-cge-reproduction task
  1e, work item 3).

  Before building the EA journal, this settles /numerically/ what the
  codex\/Fable cross-check flagged as unknown:

    * which candidate residual families are true residuals (move under an
      instrument perturbation) versus structural identities (≡ 0 by
      construction — a degenerate Jacobian row);
    * whether the activity zero-profit residual (RActProfit) is degenerate
      under the current PVADEF-derived PVA (the cross-check's P0 prediction);
    * which row is the Walras-dependent one to drop.

  The test asserts base residuals ≈ 0 and prints the sensitivity matrix
  |dR/d(instrument)| for inspection.  swazilan only (the forward pass is exact
  there; margins/home are工程4).
-}
module Main where

import           Data.List       (intercalate)
import qualified Data.Map.Strict as M
import           Numeric         (showFFloat)

import qualified LhrCalibration as L
import           LhrWiring
import           TestHarness

main :: IO ()
main = do
    inpTxt <- readFile "optimization/cge-lite/lhr/swazilan-inputs.csv"
    case L.parseInputs inpTxt >>= L.calibrate of
        Left msg -> runChecks "LhrResidualTest" [bad "swazilan calibrate" msg]
        Right cal -> do
            let ins0   = baseInstruments cal
                res0   = residuals cal ins0
                coords = instrCoords cal
                rkeys  = M.keys res0
                jac    = [ (rk, [ (c, sens cal ins0 res0 rk c) | c <- coords ])
                         | rk <- rkeys ]
            putStrLn "=== base residuals (should all be ~0) ==="
            mapM_ (\(rk, v) -> putStrLn ("  " ++ pad 16 (showRK rk) ++ " = " ++ sci v))
                  (M.toList res0)
            putStrLn ""
            putStrLn "=== sensitivity |dR/d(instrument)| per residual row ==="
            putStrLn ("  instruments: " ++ intercalate " " (map showIC coords))
            mapM_ (\(rk, row) -> do
                      let mx = maximum (0 : map (abs . snd) row)
                          tag = if mx < 1e-7 then "  <-- DEGENERATE (identity row)" else ""
                      putStrLn ("  " ++ pad 16 (showRK rk)
                                ++ " max=" ++ sci mx ++ tag)
                      putStrLn ("      [" ++ intercalate ", "
                                  [ showIC c ++ "=" ++ sci v | (c, v) <- row ] ++ "]"))
                  jac
            putStrLn ""
            -- Walras-drop analysis: the reduced Newton system needs a square,
            -- non-singular Jacobian.  Work in the economically-scaled Jacobian
            -- dR/d(log instrument) = coordBase * dR/d(instrument), then for each
            -- candidate dropped residual form the 6x6 and report |det| and the
            -- min |pivot| (a singularity/conditioning proxy).  The dependent
            -- (Walras) row is the drop that leaves the best-conditioned system.
            let scaledRow rk = [ coordBase ins0 c * sens cal ins0 res0 rk c | c <- coords ]
                allRows = [ (rk, scaledRow rk) | rk <- rkeys ]
                dropCandidates = [ rk | rk <- rkeys, isEconomic rk ]
            putStrLn "=== Walras-drop analysis (scaled 6x6, drop one economic row) ==="
            mapM_ (\drk -> do
                      let mat = [ r | (rk, r) <- allRows, rk /= drk ]
                          (d, mp) = detMinPivot mat
                      putStrLn ("  drop " ++ pad 12 (showRK drk)
                                ++ " |det|=" ++ sci d ++ "  min|pivot|=" ++ sci mp
                                ++ (if mp < 1e-6 then "  <-- SINGULAR" else "")))
                  dropCandidates
            putStrLn ""
            let actMax = maximum (0 : [ abs (sens cal ins0 res0 rk c)
                                      | rk@(RActProfit _) <- rkeys, c <- coords ])
                dropPivot drk = snd (detMinPivot [ r | (rk, r) <- allRows, rk /= drk ])
                facPivot = maximum (0 : [ dropPivot rk | rk@(RFacEquil _) <- rkeys ])
                savPivot = dropPivot RSavInv
            runChecks "LhrResidualTest" $
                [ approx 1e-6 ("base " ++ showRK rk) 0.0 v | (rk, v) <- M.toList res0 ]
                ++ [ require "ACTPROFIT non-degenerate under PVA dual cost"
                             (actMax > 100)
                             ("ACTPROFIT max sensitivity " ++ sci actMax ++ " <= 100 (degenerate)")
                   , require "FACEQUIL is Walras-essential (dropping it => singular)"
                             (facPivot < 1e-3)
                             ("dropping FACEQUIL min|pivot| " ++ sci facPivot ++ " not < 1e-3")
                   , require "SAVINVBAL is the Walras row (dropping it => non-singular 6x6)"
                             (savPivot > 0.1)
                             ("dropping SAVINVBAL min|pivot| " ++ sci savPivot ++ " not > 0.1")
                   ]

isEconomic :: ResidualKey -> Bool
isEconomic RCpi           = False
isEconomic (RActProfit _) = False
isEconomic _              = True

-- | Gaussian elimination with partial pivoting: |determinant| and the
-- smallest pivot magnitude encountered (0 => singular).
detMinPivot :: [[Double]] -> (Double, Double)
detMinPivot m0 = go m0 1.0 (1/0)
  where
    go [] acc mp = (abs acc, mp)
    go rows acc mp =
        let n = length (head rows)
            piv = maximumBy (abs . head) rows
            others = deleteFirst piv rows
            p = head piv
        in if abs p < 1e-12
              then (0.0, 0.0)
              else let elim r = zipWith (\x y -> y - (head r / p) * x) piv r
                       reduced = [ tail (elim r) | r <- others ]
                   in go reduced (acc * p) (min mp (abs p))
    maximumBy f (x:xs) = foldl (\a b -> if f b > f a then b else a) x xs
    maximumBy _ []     = error "maximumBy: empty"
    deleteFirst y (x:xs) | x == y    = xs
                         | otherwise = x : deleteFirst y xs
    deleteFirst _ []                 = []

-- | Forward-difference sensitivity of residual @rk@ to instrument @c@.
sens :: L.LhrCalibration -> Instruments -> M.Map ResidualKey Double
     -> ResidualKey -> InstrCoord -> Double
sens cal ins0 res0 rk c =
    let e    = 1e-6 * max 1.0 (abs (coordBase ins0 c))
        res1 = residuals cal (perturb e c ins0)
    in (M.findWithDefault 0.0 rk res1 - M.findWithDefault 0.0 rk res0) / e

showRK :: ResidualKey -> String
showRK (RComEquil (L.Ac c)) = "COMEQUIL(" ++ c ++ ")"
showRK (RFacEquil (L.Ac f)) = "FACEQUIL(" ++ f ++ ")"
showRK RCurAcc              = "CURACCBAL"
showRK RSavInv              = "SAVINVBAL"
showRK (RYiDef (L.Ac i))    = "YIDEF(" ++ i ++ ")"
showRK (RActProfit (L.Ac a)) = "ACTPROFIT(" ++ a ++ ")"
showRK RCpi                 = "CPIDEF"

showIC :: InstrCoord -> String
showIC (CPDS (L.Ac c)) = "PDS." ++ c
showIC (CWF (L.Ac f))  = "WF." ++ f
showIC CEXR            = "EXR"
showIC (CQA (L.Ac a))  = "QA." ++ a
showIC CIADJ           = "IADJ"
showIC (CYI (L.Ac i))  = "YI." ++ i

sci :: Double -> String
sci v = showFFloat (Just 4) v ""

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '
