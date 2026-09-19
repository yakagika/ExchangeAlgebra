{- |
  LhrShockTest -- comparative-static sentinel for the LHR standard CGE AS-ABM
  (general-equilibrium:phase1-cge-reproduction task 1e, work item 5).

  For each default-closure @sim100.gms@ shock, the reduced 49-coordinate EA
  auctioneer starts from the unshocked calibration instruments and must find
  the same all-variable equilibrium as the independent 335-variable Python
  oracle.  The test also verifies the RQ1 accounting claim at the shocked
  equilibrium: realized income equals notional instrument income in every
  ledger residual.  A pre-solve ledger guard ensures that each shock actually
  creates an imbalance for the auctioneer to remove.
-}
module Main where

import           Data.List        (intercalate)
import qualified Data.Map.Strict  as M
import           System.Directory (createDirectoryIfMissing)
import           Text.Read        (readMaybe)

import qualified LhrCalibration as L
import           LhrLedger      (ledgerResiduals)
import           LhrShock
import           LhrWiring
import           Solver         (ConvergenceTol (..), SentinelLog (..))
import           TestHarness

type SolMap = M.Map (String, [String]) Double

-- | (dataset name, normalized calibration input).
datasets :: [(String, FilePath)]
datasets =
    [ ("zimbabwe", "optimization/cge-lite/lhr/zimbabwe-inputs.csv") ]

-- | Forward-pass variables legitimately absent from the oracle dump: CPI is
-- the fixed numeraire (CPIbar), not a free variable in its solution vector.
extraAllowed :: M.Map (String, [String]) ()
extraAllowed = M.fromList [(("CPI", []), ())]

resultDir :: FilePath
resultDir = "optimization/cge-lite/result/lhr-shock"

main :: IO ()
main = do
    checks <- fmap concat (mapM datasetChecks datasets)
    runChecks "LhrShockTest" checks

datasetChecks :: (String, FilePath) -> IO [Check]
datasetChecks (name, inputPath) = do
    inpTxt <- readFile inputPath
    case L.parseInputs inpTxt >>= L.calibrate of
        Left msg -> pure [bad (name ++ " parse/calibrate") msg]
        Right cal0 -> do
            let ins0 = baseInstruments cal0
            writeSolution (dumpPath name "BASE") (forwardSolution cal0 ins0)
            shockResults <- mapM (shockChecks name cal0 ins0) allShocks
            pure (ok (name ++ " parse/calibrate") : concat shockResults)

shockChecks :: String -> L.LhrCalibration -> Instruments -> Shock -> IO [Check]
shockChecks name cal0 ins0 shock = do
    fixtureTxt <- readFile (fixturePath name shock)
    let cal' = applyShock shock cal0
        tol = ConvergenceTol { tolNorm = 1e-10, tolMaxIter = 200 }
        (sol, slog) = solveReduced cal' ins0 tol
        fwd = forwardSolution cal' sol
        tag = name ++ " " ++ shockName shock
    putStrLn $ tag ++ " solve: converged=" ++ show (slConverged slog)
             ++ " K=" ++ show (slIterations slog)
             ++ " ||z||=" ++ show (slResidualNorm slog)
             ++ " cond=" ++ show (slConditionProxy slog)
    writeSolution (dumpPath name (shockName shock)) fwd
    let common =
            [ require (tag ++ " solve converged") (slConverged slog)
                      "auctioneer did not converge"
            , nonTrivialCheck tag cal' ins0 sol
            ]
            ++ ledgerSolutionChecks tag cal' sol
            ++ [ledgerGuardCheck tag cal' ins0]
    pure $ case parseSolution fixtureTxt of
        Left msg -> common ++ [bad (tag ++ " solution csv parse") msg]
        Right expected ->
            common
            ++ [ok (tag ++ " solution csv parse")]
            ++ valueChecks tag expected fwd
            ++ [coverageCheck tag expected fwd]

-- | At least one searched coordinate must leave its unshocked base value.
nonTrivialCheck :: String -> L.LhrCalibration -> Instruments -> Instruments -> Check
nonTrivialCheck tag cal ins0 sol =
    require (tag ++ " non-trivial instrument move") (not (null moved))
            "no coordinate moved from the unshocked base by relative 1e-6"
  where
    moved =
        [ showIC c
        | c <- instrCoords cal
        , let b = coordBase ins0 c
              a = coordBase sol c
        , abs (a - b) > 1e-6 * max 1.0 (abs b) ]

-- | RQ1 at the new equilibrium: every realized/notional gap is zero.
ledgerSolutionChecks :: String -> L.LhrCalibration -> Instruments -> [Check]
ledgerSolutionChecks tag cal sol =
    [ approx 1e-6 (tag ++ " ledger@sol " ++ showRK rk) 0.0 v
    | (rk, v) <- M.toList (ledgerResiduals cal sol) ]

-- | Before adjustment the shock must be visible as an accounting imbalance.
ledgerGuardCheck :: String -> L.LhrCalibration -> Instruments -> Check
ledgerGuardCheck tag cal ins0 =
    require (tag ++ " ledger@unshocked guard")
            (any ((> 1e-6) . abs) (M.elems (ledgerResiduals cal ins0)))
            "all shocked-calibration ledger residuals are zero at unshocked instruments"

-- | Every Python-oracle variable must agree within the cross-solver band.
valueChecks :: String -> SolMap -> SolMap -> [Check]
valueChecks tag expected actual =
    [ case M.lookup key actual of
        Just a  -> approx (1e-6 * max 1.0 (abs e)) (lbl tag key) e a
        Nothing -> bad (lbl tag key) "forward pass did not produce this variable"
    | (key, e) <- M.toList expected ]

-- | The EA forward pass must not invent variables beyond the fixed numeraire.
coverageCheck :: String -> SolMap -> SolMap -> Check
coverageCheck tag expected actual =
    require (tag ++ " forward-pass coverage (no stray variables)")
            (null stray)
            ("forward pass produced variables absent from the dump: "
             ++ intercalate ", " (take 20 (map showKey stray)))
  where
    stray = [ key | key <- M.keys actual
                  , not (M.member key expected), not (M.member key extraAllowed) ]

fixturePath :: String -> Shock -> FilePath
fixturePath name shock =
    "optimization/cge-lite/lhr/" ++ name ++ "-" ++ shockName shock ++ "-solution.csv"

dumpPath :: String -> String -> FilePath
dumpPath name experiment = resultDir ++ "/" ++ name ++ "-" ++ experiment
                                ++ "-ea-solution.csv"

-- | Stable CSV dump for the GE-side MACROTAB comparison.
writeSolution :: FilePath -> SolMap -> IO ()
writeSolution path sol = do
    createDirectoryIfMissing True resultDir
    writeFile path $ unlines
        ("variable,index,value"
         : [ var ++ "," ++ intercalate "." ix ++ "," ++ show value
           | ((var, ix), value) <- M.toAscList sol ])

lbl :: String -> (String, [String]) -> String
lbl tag key = tag ++ " " ++ showKey key

showKey :: (String, [String]) -> String
showKey (nm, []) = nm
showKey (nm, ix) = nm ++ "(" ++ intercalate "." ix ++ ")"

showRK :: ResidualKey -> String
showRK (RComEquil (L.Ac c))  = "COMEQUIL(" ++ c ++ ")"
showRK (RFacEquil (L.Ac f))  = "FACEQUIL(" ++ f ++ ")"
showRK RCurAcc               = "CURACCBAL"
showRK RSavInv               = "SAVINVBAL"
showRK (RYiDef (L.Ac i))     = "YIDEF(" ++ i ++ ")"
showRK (RActProfit (L.Ac a)) = "ACTPROFIT(" ++ a ++ ")"
showRK (RPqDef (L.Ac c))     = "PQDEF(" ++ c ++ ")"
showRK (ROutAggFoc (L.Ac a) (L.Ac c)) = "OUTAGGFOC(" ++ a ++ "," ++ c ++ ")"
showRK RCpi                  = "CPIDEF"

showIC :: InstrCoord -> String
showIC (CPDS (L.Ac c)) = "PDS." ++ c
showIC (CWF (L.Ac f))  = "WF." ++ f
showIC CEXR            = "EXR"
showIC (CQA (L.Ac a))  = "QA." ++ a
showIC CIADJ           = "IADJ"
showIC (CYI (L.Ac i))  = "YI." ++ i
showIC (CPQ (L.Ac c))  = "PQ." ++ c
showIC (CPXAC (L.Ac a) (L.Ac c)) = "PXAC." ++ a ++ "." ++ c

------------------------------------------------------------------
-- * Solution CSV parsing (variable,index,value)
------------------------------------------------------------------

parseSolution :: String -> Either String SolMap
parseSolution txt =
    case rows of
        [] -> Left "empty solution.csv"
        hdr : body
            | splitComma hdr /= ["variable", "index", "value"] ->
                Left ("unexpected solution.csv header: " ++ hdr)
            | otherwise -> foldl step (Right M.empty) (zip [2 :: Int ..] body)
  where
    rows = filter (not . null) (map stripCR (lines txt))
    step (Left msg) _ = Left msg
    step (Right acc) (lineNo, ln) =
        case splitComma ln of
            [var, ix, val] -> do
                value <- readDouble ("line " ++ show lineNo ++ " " ++ var) val
                Right (M.insert (var, splitIndex ix) value acc)
            _ -> Left ("line " ++ show lineNo ++ ": expected three CSV columns")

readDouble :: String -> String -> Either String Double
readDouble ctx s =
    case readMaybe s of
        Just value -> Right value
        Nothing    -> Left (ctx ++ ": bad Double " ++ s)

splitComma :: String -> [String]
splitComma = splitOn ','

splitIndex :: String -> [String]
splitIndex "" = []
splitIndex s  = splitOn '.' s

splitOn :: Char -> String -> [String]
splitOn ch s =
    case break (== ch) s of
        (a, [])     -> [a]
        (a, _ : xs) -> a : splitOn ch xs

stripCR :: String -> String
stripCR s =
    case reverse s of
        '\r' : xs -> reverse xs
        _         -> s
