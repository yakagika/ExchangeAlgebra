{- |
  LhrModelTest -- sentinel for the LHR standard-CGE per-agent AS-ABM
  responses (general-equilibrium:phase1-cge-reproduction task 1e, work item
  3, step ②).

  Each layer-1 response ('LhrModel.activityPlan' \/ 'householdPlan' \/
  'commodityPlan' \/ 'instIncomeMap') is fed the /calibrated base signal/ read
  straight from the Python ground-truth solution dump
  (@lhr\/<data>-solution.csv@) and its quantity output is matched to the same
  dump at 1e-9.  No solve is involved: this isolates each behavioural equation
  so a transcription error surfaces on exactly the agent that carries it.

  Both bundled datasets run through the same set-agnostic checks.  @swazilan@
  is the singleton rung (one instance of every equation type); @test@ adds
  multiple activities/commodities/factors/institutions, home consumption, and
  the degenerate CET/Armington branches, so the responses are exercised in the
  general case too.  Gross activity output is checked through the
  home-netting identity @theta*QA = QXAC(net) + sum_h QHA@ (COMPRDFN).
-}
module Main where

import           Data.List       (intercalate)
import           Data.Maybe      (fromMaybe)
import qualified Data.Map.Strict as M
import           Text.Read       (readMaybe)

import qualified LhrCalibration as L
import           LhrModel
import           TestHarness

-- | (variable name, dotted index components) -> value.
type SolMap = M.Map (String, [String]) Double

datasets :: [String]
datasets = ["swazilan", "test"]

main :: IO ()
main = do
    checks <- fmap concat (mapM datasetChecks datasets)
    runChecks "LhrModelTest" checks

datasetChecks :: String -> IO [Check]
datasetChecks name = do
    inpTxt <- readFile ("optimization/cge-lite/lhr/" ++ name ++ "-inputs.csv")
    solTxt <- readFile ("optimization/cge-lite/lhr/" ++ name ++ "-solution.csv")
    let parsed = do
            inp <- L.parseInputs inpTxt
            L.calibrate inp
        solE = parseSolution solTxt
    pure $ case (parsed, solE) of
        (Left msg, _)  -> [bad (name ++ " parse/calibrate") msg]
        (_, Left msg)  -> [bad (name ++ " solution csv parse") msg]
        (Right cal, Right sol) ->
            ok (name ++ " parse/calibrate")
            : modelChecks name cal sol

------------------------------------------------------------------
-- * Response checks (set-agnostic)
------------------------------------------------------------------

modelChecks :: String -> L.LhrCalibration -> SolMap -> [Check]
modelChecks name cal sol =
       activityChecks name cal sol ps
    ++ householdChecks name cal sol ps
    ++ commodityChecks name cal sol ps
    ++ instChecks name cal sol ps
  where
    ps = priceSignalFrom sol

-- | Prices read straight from the solution dump.  Absent variables leave a
-- map empty; a response only looks up keys its equations reference.
priceSignalFrom :: SolMap -> PriceSignal
priceSignalFrom sol = PriceSignal
    { psPA    = m1 "PA"
    , psPINTA = m1 "PINTA"
    , psPDS   = m1 "PDS"
    , psPDD   = m1 "PDD"
    , psPM    = m1 "PM"
    , psPE    = m1 "PE"
    , psPQ    = m1 "PQ"
    , psPXAC  = m2 "PXAC"
    , psWF    = m1 "WF"
    , psEXR   = fromMaybe 1.0 (M.lookup ("EXR", []) sol)
    }
  where
    m1 nm = M.fromList [ (L.Ac k, v) | ((n, [k]), v) <- M.toList sol, n == nm ]
    m2 nm = M.fromList [ ((L.Ac a, L.Ac b), v) | ((n, [a, b]), v) <- M.toList sol, n == nm ]

activityChecks :: String -> L.LhrCalibration -> SolMap -> PriceSignal -> [Check]
activityChecks name cal sol ps =
    [ chk name sol "PVA" [aS] (apPVA ap)   | (a, aS, ap) <- acts ]
    ++ [ chk name sol "QVA" [aS] (apQVA ap)   | (_, aS, ap) <- acts ]
    ++ [ chk name sol "QINTA" [aS] (apQINTA ap) | (_, aS, ap) <- acts ]
    ++ [ chk name sol "QF" [fS, aS] (look (apQF ap) fS)
       | ([fS, aS], _) <- rowsOf "QF" sol, (a, aS', ap) <- acts, aS' == aS ]
    ++ [ chk name sol "QINT" [cS, aS] (look (apQINT ap) cS)
       | ([cS, aS], _) <- rowsOf "QINT" sol, (_, aS', ap) <- acts, aS' == aS ]
    -- Gross output identity (COMPRDFN): theta*QA = QXAC(net) + sum_h QHA.
    ++ [ require (lbl name "QXACgross" [aS, cS])
                 (approxEq grossExpected (look (apQXACgross ap) cS))
                 ("gross theta*QA = " ++ show (look (apQXACgross ap) cS)
                  ++ ", QXAC(net)+sum QHA = " ++ show grossExpected)
       | ([aS, cS], vnet) <- rowsOf "QXAC" sol
       , (_, aS', ap) <- acts, aS' == aS
       , let grossExpected = vnet + qhaSum sol aS cS ]
  where
    acts = [ (a, acN a, activityPlan (L.calParams cal) (L.calBase cal)
                                     (L.calSets cal) a (scaleQA sol a) ps)
           | a <- L.setA (L.calSets cal) ]

householdChecks :: String -> L.LhrCalibration -> SolMap -> PriceSignal -> [Check]
householdChecks name cal sol ps =
    [ chk name sol "QH" [cS, hS] (look (hpQH hp) cS)
    | ([cS, hS], _) <- rowsOf "QH" sol, (_, hS', hp) <- hhs, hS' == hS ]
    ++ [ chk name sol "QHA" [aS, cS, hS] (look2 (hpQHA hp) aS cS)
       | ([aS, cS, hS], _) <- rowsOf "QHA" sol, (_, hS', hp) <- hhs, hS' == hS ]
  where
    hhs = [ (h, acN h, householdPlan (L.calParams cal) (L.calSets cal)
                                     h (scale1 sol "EH" h) ps)
          | h <- L.setH (L.calSets cal) ]

commodityChecks :: String -> L.LhrCalibration -> SolMap -> PriceSignal -> [Check]
commodityChecks name cal sol ps =
       [ chk name sol "QX" [cS] (cpQX cp) | (_, cS, cp) <- coms, present "QX" [cS] ]
    ++ [ chk name sol "QD" [cS] (cpQD cp) | (_, cS, cp) <- coms, present "QD" [cS] ]
    ++ [ chk name sol "QE" [cS] (cpQE cp) | (_, cS, cp) <- coms, present "QE" [cS] ]
    -- QM/QQ are the supply-side Armington response only where the commodity
    -- has a domestic anchor (c in CD).  For an import-only commodity
    -- (CM and not CD) QM = QQ is demand-determined and cleared by the
    -- auctioneer's COMEQUIL residual (invariant (vii)); it carries no
    -- price-responsive content, so it is verified in step ③, not here.
    ++ [ chk name sol "QM" [cS] (cpQM cp)
       | (c, cS, cp) <- coms, present "QM" [cS], c `elem` cds ]
    ++ [ chk name sol "QQ" [cS] (cpQQ cp)
       | (c, cS, cp) <- coms, present "QQ" [cS], c `elem` cds ]
  where
    present nm ix = M.member (nm, ix) sol
    cds = L.setCd (L.calSets cal)
    coms = [ (c, acN c, commodityPlan (L.calParams cal) (L.calSets cal)
                                      c (qxacNetFor sol c) ps)
           | c <- L.setC (L.calSets cal) ]

instChecks :: String -> L.LhrCalibration -> SolMap -> PriceSignal -> [Check]
instChecks name cal sol ps =
    [ chk name sol "YIF" [iS, fS] (look (iiYIF ii) fS)
    | ([iS, fS], _) <- rowsOf "YIF" sol, (_, iS', ii) <- insts, iS' == iS ]
    ++ [ chk name sol "YI" [iS] (iiYIrecv ii)
       | (i, iS, ii) <- insts, i `elem` L.setInsdng (L.calSets cal) ]
    ++ [ chk name sol "EH" [iS] (fromMaybe (0/0) (iiEH ii))
       | (i, iS, ii) <- insts, i `elem` L.setH (L.calSets cal) ]
  where
    incSig = IncomeSignal
        (M.fromList [ (L.Ac k, v) | ((n, [k]), v) <- M.toList sol, n == "YF" ])
        (M.fromList [ (L.Ac k, v) | ((n, [k]), v) <- M.toList sol, n == "YI" ])
    transSig = TransferSignal
        (M.fromList [ ((L.Ac a, L.Ac b), v)
                    | ((n, [a, b]), v) <- M.toList sol, n == "TRII" ])
    insts = [ (i, acN i, instIncomeMap (L.calParams cal) (L.calBase cal)
                                       (L.calSets cal) i incSig transSig ps)
            | i <- L.setInsd (L.calSets cal) ]

------------------------------------------------------------------
-- * Lookup helpers
------------------------------------------------------------------

-- | Named 1e-9 check of a response value against the solution dump.
chk :: String -> SolMap -> String -> [String] -> Double -> Check
chk name sol var ix actual =
    case M.lookup (var, ix) sol of
        Just e  -> approx (1e-9 * max 1.0 (abs e)) (lbl name var ix) e actual
        Nothing -> bad (lbl name var ix) "solution dump has no such variable"

lbl :: String -> String -> [String] -> String
lbl name var ix = name ++ " " ++ var ++ "(" ++ intercalate "." ix ++ ")"

approxEq :: Double -> Double -> Bool
approxEq e a = abs (e - a) <= 1e-9 * max 1.0 (abs e)

rowsOf :: String -> SolMap -> [([String], Double)]
rowsOf nm sol = [ (ix, v) | ((n, ix), v) <- M.toList sol, n == nm ]

look :: M.Map L.Ac Double -> String -> Double
look m k = M.findWithDefault 0.0 (L.Ac k) m

look2 :: M.Map (L.Ac, L.Ac) Double -> String -> String -> Double
look2 m a b = M.findWithDefault 0.0 (L.Ac a, L.Ac b) m

scaleQA :: SolMap -> L.Ac -> Double
scaleQA sol a = M.findWithDefault 0.0 ("QA", [acN a]) sol

scale1 :: SolMap -> String -> L.Ac -> Double
scale1 sol var k = M.findWithDefault 0.0 (var, [acN k]) sol

qxacNetFor :: SolMap -> L.Ac -> M.Map L.Ac Double
qxacNetFor sol c =
    M.fromList [ (L.Ac aS, v) | ([aS, cS], v) <- rowsOf "QXAC" sol, L.Ac cS == c ]

qhaSum :: SolMap -> String -> String -> Double
qhaSum sol aS cS =
    sum [ v | ([a, c, _], v) <- rowsOf "QHA" sol, a == aS, c == cS ]

acN :: L.Ac -> String
acN (L.Ac s) = s

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
                v <- readDouble ("line " ++ show lineNo ++ " " ++ var) val
                Right (M.insert (var, splitIndex ix) v acc)
            _ -> Left ("line " ++ show lineNo ++ ": expected three CSV columns")

readDouble :: String -> String -> Either String Double
readDouble ctx s =
    case readMaybe s of
        Just v  -> Right v
        Nothing -> Left (ctx ++ ": bad Double " ++ s)

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
