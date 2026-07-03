{- |
  LhrCalibration -- Lofgren-Harris-Robinson standard CGE calibration,
  transcribed from @plans/lhr-spec/lhr_resolve.py@ (GE plan
  @general-equilibrium:phase1-cge-reproduction@, task 1e work item 2).

  This module starts at the normalized fixture boundary emitted by
  @build_inputs()@ and ports only the calibration block (mod100 sections 2
  and 4).  The equation system is deliberately out of scope.
-}
module LhrCalibration
    ( Ac (..)
    , TaxparRule (..)
    , LhrInputs (..)
    , LhrSets (..)
    , LhrParams (..)
    , LhrBase (..)
    , LhrCalibration (..)
    , parseInputs
    , calibrate
    , toTable
    , setsTable
    ) where

import           Control.Monad   (foldM)
import           Data.List       (foldl', isPrefixOf, sortBy)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Text.Read       (readMaybe)

-- | Runtime SAM account / set member.  LHR datasets define their own
-- account universes, so these are fixture strings rather than an ADT.
newtype Ac = Ac String deriving (Eq, Ord, Show)

data TaxparRule
    = SamRow Ac
    | Zero
    deriving (Eq, Show)

data LhrInputs = LhrInputs
    { rawSetsAll          :: !(M.Map String [Ac])
    , rawAc               :: ![Ac]
    , rawA                :: ![Ac]
    , rawC                :: ![Ac]
    , rawF                :: ![Ac]
    , rawIns              :: ![Ac]
    , rawInsd             :: ![Ac]
    , rawInsdng           :: ![Ac]
    , rawH                :: ![Ac]
    , rawEn               :: ![Ac]
    , rawCtd              :: ![Ac]
    , rawCte              :: ![Ac]
    , rawCtm              :: ![Ac]
    , rawSam              :: !(M.Map (Ac, Ac) Double)
    , rawTradelas         :: !(M.Map (Ac, String) Double)
    , rawTradelasCondRowC :: !(M.Map String Double)
    , rawTradelasCondCRow :: !(M.Map String Double)
    , rawProdelas         :: !(M.Map Ac Double)
    , rawLeselas1         :: !(M.Map (Ac, Ac) Double)
    , rawLeselas2Default  :: !Double
    , rawFrisch           :: !(M.Map Ac Double)
    , rawQfbase           :: !(M.Map (Ac, Ac) Double)
    , rawQfsbase          :: !(M.Map Ac Double)
    , rawShrhome          :: !(M.Map (Ac, Ac, Ac) Double)
    , rawTaxparRules      :: ![((String, String), TaxparRule)]
    , rawElasacDefault    :: !(Maybe Double)
    , rawCinvYes          :: !Bool
    , rawCinvNegNo        :: !Bool
    } deriving (Eq, Show)

data LhrSets = LhrSets
    { setAc     :: ![Ac]
    , setAcnt   :: ![Ac]
    , setA      :: ![Ac]
    , setC      :: ![Ac]
    , setF      :: ![Ac]
    , setIns    :: ![Ac]
    , setInsd   :: ![Ac]
    , setInsdng :: ![Ac]
    , setH      :: ![Ac]
    , setCd     :: ![Ac]
    , setCdn    :: ![Ac]
    , setCe     :: ![Ac]
    , setCen    :: ![Ac]
    , setCm     :: ![Ac]
    , setCmn    :: ![Ac]
    , setCx     :: ![Ac]
    , setCt     :: ![Ac]
    , setCtd    :: ![Ac]
    , setCte    :: ![Ac]
    , setCtm    :: ![Ac]
    , setCinv   :: ![Ac]
    , setAleo   :: ![Ac]
    , setCdm    :: ![Ac]
    } deriving (Eq, Show)

data LhrParams = LhrParams
    { paramTm       :: !(M.Map Ac Double)
    , paramTe       :: !(M.Map Ac Double)
    , paramTq       :: !(M.Map Ac Double)
    , paramTa       :: !(M.Map Ac Double)
    , paramTva      :: !(M.Map Ac Double)
    , paramTf       :: !(M.Map Ac Double)
    , paramIcm      :: !(M.Map (Ac, Ac) Double)
    , paramIce      :: !(M.Map (Ac, Ac) Double)
    , paramIcd      :: !(M.Map (Ac, Ac) Double)
    , paramIca      :: !(M.Map (Ac, Ac) Double)
    , paramTheta    :: !(M.Map (Ac, Ac) Double)
    , paramIva      :: !(M.Map Ac Double)
    , paramInta     :: !(M.Map Ac Double)
    , paramCwts     :: !(M.Map Ac Double)
    , paramDwts     :: !(M.Map Ac Double)
    , paramAlphava  :: !(M.Map Ac Double)
    , paramDeltava  :: !(M.Map (Ac, Ac) Double)
    , paramRhova    :: !(M.Map Ac Double)
    , paramAlphaac  :: !(M.Map Ac Double)
    , paramDeltaac  :: !(M.Map (Ac, Ac) Double)
    , paramRhoac    :: !(M.Map Ac Double)
    , paramAlphat   :: !(M.Map Ac Double)
    , paramDeltat   :: !(M.Map Ac Double)
    , paramRhot     :: !(M.Map Ac Double)
    , paramAlphaq   :: !(M.Map Ac Double)
    , paramDeltaq   :: !(M.Map Ac Double)
    , paramRhoq     :: !(M.Map Ac Double)
    , paramShif     :: !(M.Map (Ac, Ac) Double)
    , paramShii     :: !(M.Map (Ac, Ac) Double)
    , paramTrnsfr   :: !(M.Map (Ac, Ac) Double)
    , paramBetam    :: !(M.Map (Ac, Ac) Double)
    , paramBetah    :: !(M.Map (Ac, Ac, Ac) Double)
    , paramGammam   :: !(M.Map (Ac, Ac) Double)
    , paramGammah   :: !(M.Map (Ac, Ac, Ac) Double)
    , paramQbarinv  :: !(M.Map Ac Double)
    , paramQbarg    :: !(M.Map Ac Double)
    , paramQdst     :: !(M.Map Ac Double)
    , paramMpsbar   :: !(M.Map Ac Double)
    , paramTinsbar  :: !(M.Map Ac Double)
    , paramMps01    :: !(M.Map Ac Double)
    , paramTins01   :: !(M.Map Ac Double)
    , paramFrisch   :: !(M.Map Ac Double)
    } deriving (Eq, Show)

data LhrBase = LhrBase
    { baseCpi0    :: !Double
    , baseDpi0    :: !Double
    , baseExr0    :: !Double
    , baseEg0     :: !Double
    , baseFsav0   :: !Double
    , baseGadj0   :: !Double
    , baseGsav0   :: !Double
    , baseGovshr0 :: !Double
    , baseIadj0   :: !Double
    , baseInvshr0 :: !Double
    , baseTabs0   :: !Double
    , baseEh0     :: !(M.Map Ac Double)
    , baseMps0    :: !(M.Map Ac Double)
    , baseTins0   :: !(M.Map Ac Double)
    , basePa0     :: !(M.Map Ac Double)
    , basePdd0    :: !(M.Map Ac Double)
    , basePds0    :: !(M.Map Ac Double)
    , basePe0     :: !(M.Map Ac Double)
    , basePinta0  :: !(M.Map Ac Double)
    , basePm0     :: !(M.Map Ac Double)
    , basePq0     :: !(M.Map Ac Double)
    , basePva0    :: !(M.Map Ac Double)
    , basePwe0    :: !(M.Map Ac Double)
    , basePwm0    :: !(M.Map Ac Double)
    , basePx0     :: !(M.Map Ac Double)
    , basePxac0   :: !(M.Map (Ac, Ac) Double)
    , baseQa0     :: !(M.Map Ac Double)
    , baseQd0     :: !(M.Map Ac Double)
    , baseQe0     :: !(M.Map Ac Double)
    , baseQf0     :: !(M.Map (Ac, Ac) Double)
    , baseQfs0    :: !(M.Map Ac Double)
    , baseQg0     :: !(M.Map Ac Double)
    , baseQh0     :: !(M.Map (Ac, Ac) Double)
    , baseQha0    :: !(M.Map (Ac, Ac, Ac) Double)
    , baseQint0   :: !(M.Map (Ac, Ac) Double)
    , baseQinta0  :: !(M.Map Ac Double)
    , baseQinv0   :: !(M.Map Ac Double)
    , baseQm0     :: !(M.Map Ac Double)
    , baseQq0     :: !(M.Map Ac Double)
    , baseQt0     :: !(M.Map Ac Double)
    , baseQva0    :: !(M.Map Ac Double)
    , baseQx0     :: !(M.Map Ac Double)
    , baseQxac0   :: !(M.Map (Ac, Ac) Double)
    , baseTrii0   :: !(M.Map (Ac, Ac) Double)
    , baseWf0     :: !(M.Map Ac Double)
    , baseWfdist0 :: !(M.Map (Ac, Ac) Double)
    , baseYf0     :: !(M.Map Ac Double)
    , baseYg0     :: !Double
    , baseYi0     :: !(M.Map Ac Double)
    , baseYif0    :: !(M.Map (Ac, Ac) Double)
    } deriving (Eq, Show)

data LhrCalibration = LhrCalibration
    { calSets    :: !LhrSets
    , calParams  :: !LhrParams
    , calBase    :: !LhrBase
    , calSam     :: !(M.Map (Ac, Ac) Double)
    , calTaxpar  :: !(M.Map (String, Ac) Double)
    , calShrhome :: !(M.Map (Ac, Ac, Ac) Double)
    } deriving (Eq, Show)

------------------------------------------------------------------
-- * CSV input boundary
------------------------------------------------------------------

data InputAcc = InputAcc
    { iaSets              :: !(M.Map String [(Int, Ac)])
    , iaSam               :: !(M.Map (Ac, Ac) Double)
    , iaTradelas          :: !(M.Map (Ac, String) Double)
    , iaTradelasCondRowC  :: !(M.Map String Double)
    , iaTradelasCondCRow  :: !(M.Map String Double)
    , iaProdelas          :: !(M.Map Ac Double)
    , iaLeselas1          :: !(M.Map (Ac, Ac) Double)
    , iaLeselas2Default   :: !(Maybe Double)
    , iaFrisch            :: !(M.Map Ac Double)
    , iaQfbase            :: !(M.Map (Ac, Ac) Double)
    , iaQfsbase           :: !(M.Map Ac Double)
    , iaShrhome           :: !(M.Map (Ac, Ac, Ac) Double)
    , iaTaxparRules       :: ![((String, String), TaxparRule)]
    , iaElasacDefault     :: !(Maybe Double)
    , iaCinvYes           :: !(Maybe Bool)
    , iaCinvNegNo         :: !(Maybe Bool)
    } deriving (Eq, Show)

emptyInputAcc :: InputAcc
emptyInputAcc = InputAcc
    { iaSets = M.empty
    , iaSam = M.empty
    , iaTradelas = M.empty
    , iaTradelasCondRowC = M.empty
    , iaTradelasCondCRow = M.empty
    , iaProdelas = M.empty
    , iaLeselas1 = M.empty
    , iaLeselas2Default = Nothing
    , iaFrisch = M.empty
    , iaQfbase = M.empty
    , iaQfsbase = M.empty
    , iaShrhome = M.empty
    , iaTaxparRules = []
    , iaElasacDefault = Nothing
    , iaCinvYes = Nothing
    , iaCinvNegNo = Nothing
    }

parseInputs :: String -> Either String LhrInputs
parseInputs txt = do
    let ls0 = filter (not . null) (map stripCR (lines txt))
    case ls0 of
        [] -> Left "empty inputs.csv"
        hdr : body
            | splitComma hdr /= ["kind", "index", "value"] ->
                Left ("unexpected inputs.csv header: " ++ hdr)
            | otherwise -> do
                rows <- mapM parseRow (zip [2 :: Int ..] body)
                acc <- foldM ingest emptyInputAcc rows
                mkInputs acc

data CsvRow = CsvRow !Int !String ![String] !String deriving (Eq, Show)

parseRow :: (Int, String) -> Either String CsvRow
parseRow (n, ln) =
    case splitComma ln of
        [kind, ix, val] -> Right (CsvRow n kind (splitIndex ix) val)
        _ -> Left ("line " ++ show n ++ ": expected three CSV columns")

ingest :: InputAcc -> CsvRow -> Either String InputAcc
ingest acc (CsvRow n kind ix val)
    | "SET." `isPrefixOf` kind = do
        let name = drop 4 kind
        case ix of
            [member]
                | not (null name) -> do
                    pos <- readInt ("line " ++ show n ++ " " ++ kind) val
                    Right acc { iaSets = M.insertWith (++) name [(pos, Ac member)] (iaSets acc) }
            _ -> Left ("line " ++ show n ++ ": SET row needs one index")
    | kind == "SAM" = case ix of
        [r, c] -> insertDouble n kind val (\v -> acc { iaSam = M.insert (Ac r, Ac c) v (iaSam acc) })
        _ -> Left ("line " ++ show n ++ ": SAM row needs two indexes")
    | kind == "TRADELAS" = case ix of
        ["__COND_ROWC__", k] -> insertDouble n kind val (\v -> acc { iaTradelasCondRowC = M.insert k v (iaTradelasCondRowC acc) })
        ["__COND_CROW__", k] -> insertDouble n kind val (\v -> acc { iaTradelasCondCRow = M.insert k v (iaTradelasCondCRow acc) })
        [c, k]               -> insertDouble n kind val (\v -> acc { iaTradelas = M.insert (Ac c, k) v (iaTradelas acc) })
        _ -> Left ("line " ++ show n ++ ": TRADELAS row needs two indexes")
    | kind == "PRODELAS" = case ix of
        [a] -> insertDouble n kind val (\v -> acc { iaProdelas = M.insert (Ac a) v (iaProdelas acc) })
        _ -> Left ("line " ++ show n ++ ": PRODELAS row needs one index")
    | kind == "LESELAS1" = case ix of
        [c, h] -> insertDouble n kind val (\v -> acc { iaLeselas1 = M.insert (Ac c, Ac h) v (iaLeselas1 acc) })
        _ -> Left ("line " ++ show n ++ ": LESELAS1 row needs two indexes")
    | kind == "FRISCH" = case ix of
        [h] -> insertDouble n kind val (\v -> acc { iaFrisch = M.insert (Ac h) v (iaFrisch acc) })
        _ -> Left ("line " ++ show n ++ ": FRISCH row needs one index")
    | kind == "QFBASE" = case ix of
        [f, a] -> insertDouble n kind val (\v -> acc { iaQfbase = M.insert (Ac f, Ac a) v (iaQfbase acc) })
        _ -> Left ("line " ++ show n ++ ": QFBASE row needs two indexes")
    | kind == "QFSBASE" = case ix of
        [f] -> insertDouble n kind val (\v -> acc { iaQfsbase = M.insert (Ac f) v (iaQfsbase acc) })
        _ -> Left ("line " ++ show n ++ ": QFSBASE row needs one index")
    | kind == "SHRHOME" = case ix of
        [a, c, h] -> insertDouble n kind val (\v -> acc { iaShrhome = M.insert (Ac a, Ac c, Ac h) v (iaShrhome acc) })
        _ -> Left ("line " ++ show n ++ ": SHRHOME row needs three indexes")
    | kind == "LESELAS2_DEFAULT" = case ix of
        [] -> insertDouble n kind val (\v -> acc { iaLeselas2Default = Just v })
        _  -> Left ("line " ++ show n ++ ": LESELAS2_DEFAULT must be scalar")
    | kind == "ELASAC_DEFAULT" = case ix of
        [] -> insertDouble n kind val (\v -> acc { iaElasacDefault = Just v })
        _  -> Left ("line " ++ show n ++ ": ELASAC_DEFAULT must be scalar")
    | kind == "TAXPAR_RULE" = case ix of
        [tx, over] -> do
            rule <- parseTaxparRule n val
            Right acc { iaTaxparRules = iaTaxparRules acc ++ [((tx, over), rule)] }
        _ -> Left ("line " ++ show n ++ ": TAXPAR_RULE row needs two indexes")
    | kind == "CINV_YES" = case ix of
        [] -> do b <- readBool ("line " ++ show n ++ " CINV_YES") val
                 Right acc { iaCinvYes = Just b }
        _  -> Left ("line " ++ show n ++ ": CINV_YES must be scalar")
    | kind == "CINV_NEG_NO" = case ix of
        [] -> do b <- readBool ("line " ++ show n ++ " CINV_NEG_NO") val
                 Right acc { iaCinvNegNo = Just b }
        _  -> Left ("line " ++ show n ++ ": CINV_NEG_NO must be scalar")
    | otherwise = Left ("line " ++ show n ++ ": unknown kind " ++ kind)

mkInputs :: InputAcc -> Either String LhrInputs
mkInputs acc = do
    ac      <- reqSet "AC"
    a       <- reqSet "A"
    c       <- reqSet "C"
    f       <- reqSet "F"
    ins     <- reqSet "INS"
    insd    <- reqSet "INSD"
    insdng  <- reqSet "INSDNG"
    h       <- reqSet "H"
    let optSet name = orderedSet (M.findWithDefault [] name (iaSets acc))
        allSets = M.map orderedSet (iaSets acc)
    Right LhrInputs
        { rawSetsAll = allSets
        , rawAc = ac
        , rawA = a
        , rawC = c
        , rawF = f
        , rawIns = ins
        , rawInsd = insd
        , rawInsdng = insdng
        , rawH = h
        , rawEn = optSet "EN"
        , rawCtd = optSet "CTD"
        , rawCte = optSet "CTE"
        , rawCtm = optSet "CTM"
        , rawSam = iaSam acc
        , rawTradelas = iaTradelas acc
        , rawTradelasCondRowC = iaTradelasCondRowC acc
        , rawTradelasCondCRow = iaTradelasCondCRow acc
        , rawProdelas = iaProdelas acc
        , rawLeselas1 = iaLeselas1 acc
        , rawLeselas2Default = fromMaybe 0.0 (iaLeselas2Default acc)
        , rawFrisch = iaFrisch acc
        , rawQfbase = iaQfbase acc
        , rawQfsbase = iaQfsbase acc
        , rawShrhome = iaShrhome acc
        , rawTaxparRules = iaTaxparRules acc
        , rawElasacDefault = iaElasacDefault acc
        , rawCinvYes = fromMaybe False (iaCinvYes acc)
        , rawCinvNegNo = fromMaybe False (iaCinvNegNo acc)
        }
  where
    reqSet name =
        case M.lookup name (iaSets acc) of
            Nothing -> Left ("dataset error: set " ++ name ++ " missing")
            Just xs -> Right (orderedSet xs)

orderedSet :: [(Int, Ac)] -> [Ac]
orderedSet = map snd . sortBy (\(a, _) (b, _) -> compare a b)

insertDouble :: Int -> String -> String -> (Double -> InputAcc) -> Either String InputAcc
insertDouble n kind val f = do
    v <- readDouble ("line " ++ show n ++ " " ++ kind) val
    Right (f v)

parseTaxparRule :: Int -> String -> Either String TaxparRule
parseTaxparRule _ "ZERO" = Right Zero
parseTaxparRule n val =
    case splitOn '.' val of
        ["SAMROW", acct] -> Right (SamRow (Ac acct))
        _ -> Left ("line " ++ show n ++ ": bad TAXPAR_RULE value " ++ val)

readDouble :: String -> String -> Either String Double
readDouble ctx s =
    case readMaybe s of
        Just v  -> Right v
        Nothing -> Left (ctx ++ ": bad Double " ++ s)

readInt :: String -> String -> Either String Int
readInt ctx s =
    case readMaybe s of
        Just v  -> Right v
        Nothing -> Left (ctx ++ ": bad Int " ++ s)

readBool :: String -> String -> Either String Bool
readBool ctx s =
    case (readMaybe s :: Maybe Int) of
        Just 0 -> Right False
        Just 1 -> Right True
        _      -> Left (ctx ++ ": expected 0 or 1")

splitComma :: String -> [String]
splitComma = splitOn ','

splitIndex :: String -> [String]
splitIndex "" = []
splitIndex s  = splitOn '.' s

splitOn :: Char -> String -> [String]
splitOn ch s =
    case break (== ch) s of
        (a, [])      -> [a]
        (a, _ : xs)  -> a : splitOn ch xs

stripCR :: String -> String
stripCR s =
    case reverse s of
        '\r' : xs -> reverse xs
        _         -> s

------------------------------------------------------------------
-- * Calibration
------------------------------------------------------------------

calibrate :: LhrInputs -> Either String LhrCalibration
calibrate inp =
    case taxparResult of
        Left msg -> Left msg
        Right taxpar0
            | worst > 1e-6 * max 1.0 maxcell ->
                Left ("SAM unbalanced beyond rounding: max |col-row| = " ++ show worst)
            | not (null lesFailures) ->
                Left (head lesFailures)
            | otherwise ->
                Right LhrCalibration
                    { calSets = sets'
                    , calParams = params'
                    , calBase = base'
                    , calSam = samBalanced
                    , calTaxpar = taxpar0
                    , calShrhome = shrhome'
                    }
          where
            -- lhr_resolve.py: ACNT = [a for a in AC if a != "TOTAL"].
            acnt = [a | a <- ac, a /= total]

            tax tx e = M.findWithDefault 0.0 (tx, e) taxpar0

            -- mod100 section 2: sectors with only exports get exports snapped.
            samExportsAdjusted = foldl' snapExports sam0 cSet
            snapExports m c =
                let dom = sumL [sam m a c | a <- aSet]
                        - (sam m c row - tax "EXPTAX" c
                           - sumL [sam m t c | t <- cte])
                    snapped = sumL [sam m a c | a <- aSet]
                            + tax "EXPTAX" c
                            + sumL [sam m t c | t <- cte]
                in if abs dom < 1e-6 then setSam c row snapped m else m

            -- Derived commodity sets.
            cd = [c | c <- cSet
                    , sumL [sam samExportsAdjusted a c | a <- aSet]
                      - (sam samExportsAdjusted c row - tax "EXPTAX" c
                         - sumL [sam samExportsAdjusted t c | t <- cte]) > 1e-10]
            cdn = [c | c <- cSet, c `notElem` cd]
            ce = [c | c <- cSet, sam samExportsAdjusted c row /= 0.0]
            cen = [c | c <- cSet, c `notElem` ce]
            cm = [c | c <- cSet, sam samExportsAdjusted row c /= 0.0]
            cmn = [c | c <- cSet, c `notElem` cm]
            cx = [c | c <- cSet, sumL [sam samExportsAdjusted a c | a <- aSet] /= 0.0]
            ct = [c | c <- cSet
                    , (sumL [sam samExportsAdjusted c t | t <- ctd]
                       + sumL [sam samExportsAdjusted c t | t <- cte]
                       + sumL [sam samExportsAdjusted c t | t <- ctm]) /= 0.0]
            -- v1.00: Leontief top nest forced for all activities.
            aleo = aSet

            -- SAM adjustments (netting; verbatim order).
            samNetIns = foldl' netIns samExportsAdjusted insd
            netIns m i = setSam row i 0.0 (setSam i row (sam m i row - sam m row i) m)

            samNetFactors = foldl' netFactor samNetIns fSet
            netFactor m f = setSam f row 0.0 (setSam row f (sam m row f - sam m f row) m)

            samNetGov = foldl' netGov samNetFactors insdng
            netGov m i = setSam gov i 0.0 (setSam i gov (sam m i gov - sam m gov i) m)

            samNoDiagonal = foldl' (\m a -> setSam a a 0.0 m) samNetGov acnt

            -- Recompute totals.
            samBalanced = foldl' setTotals samNoDiagonal acnt
            setTotals m a =
                let m1 = setSam total a (sumL [sam m x a | x <- acnt]) m
                in setSam a total (sumL [sam m1 a x | x <- acnt]) m1

            -- Balance check.
            maxcell0 = maxList [abs (sam samBalanced r c) | r <- acnt, c <- acnt]
            maxcell = if maxcell0 == 0.0 then 1.0 else maxcell0
            worst = maxList [abs (sam samBalanced total a - sam samBalanced a total) | a <- acnt]

            -- CINV (dat file section 2 rules).
            cinv0 = if rawCinvYes inp then [c | c <- cSet, sam samBalanced c si /= 0.0] else []
            cinv = if rawCinvNegNo inp then [c | c <- cinv0, sam samBalanced c si >= 0.0] else cinv0

            -- SHRHOME default (mod100 section 2): output value shares.
            shrhome0 = rawShrhome inp
            shrhome'
                | any (\(a, h) -> sam samBalanced a h /= 0.0) [(a, h) | a <- aSet, h <- hSet]
                  && M.null shrhome0 =
                    M.fromList
                        [ ((a, c, h), sam samBalanced a c / sumL [sam samBalanced a cp | cp <- cSet])
                        | a <- aSet, h <- hSet
                        , sam samBalanced a h /= 0.0
                        , c <- cSet
                        , sam samBalanced a c /= 0.0
                        ]
                | otherwise = shrhome0

            shr a c h = M.findWithDefault 0.0 (a, c, h) shrhome'

            -- Elasticities with dataset defaults / conditions.
            tradelas0 = M.fromList
                [ ((c, k), tradelasValue c k)
                | c <- cSet, k <- ["SIGMAQ", "SIGMAT"] ]
            tradelasValue c k =
                let v0 = M.findWithDefault 0.0 (c, k) (rawTradelas inp)
                    v1 = case M.lookup k (rawTradelasCondRowC inp) of
                        Just v | sam samBalanced row c /= 0.0 -> v
                        _ -> v0
                    v2 = case M.lookup k (rawTradelasCondCRow inp) of
                        Just v | sam samBalanced c row /= 0.0 -> v
                        _ -> v1
                in v2

            -- Superfluous-elasticity elimination.
            tradelas = foldl' eliminateTradelas tradelas0 cSet
            eliminateTradelas m c =
                let m1 = if (c `elem` cen) || (c `elem` ce && c `elem` cdn)
                         then M.insert (c, "SIGMAT") 0.0 m else m
                in if (c `elem` cmn) || (c `elem` cm && c `elem` cdn)
                   then M.insert (c, "SIGMAQ") 0.0 m1 else m1

            -- PRODELAS(a).
            prodelas = M.fromList [(a, M.findWithDefault 0.0 a (rawProdelas inp)) | a <- aSet]

            -- ELASAC(c).
            elasac0 = M.fromList [(c, fromMaybe 0.0 (rawElasacDefault inp)) | c <- cSet]
            elasac = foldl' zeroElasac elasac0 cSet
            zeroElasac m c =
                if sumL [sam samBalanced a c | a <- aSet] == 0.0
                then M.insert c 0.0 m
                else m

            -- LESELAS1(c,h).
            leselas1 = M.fromList
                [ ((c, h), if sam samBalanced c h == 0.0
                           then 0.0
                           else M.findWithDefault 0.0 (c, h) (rawLeselas1 inp))
                | c <- cSet, h <- hSet ]

            -- LESELAS2(a,c,h).
            leselas2 = M.fromList
                [ ((a, c, h), if shr a c h /= 0.0 then rawLeselas2Default inp else 0.0)
                | a <- aSet, c <- cSet, h <- hSet ]

            -- FRISCH(h).
            frisch = rawFrisch inp

            -- QF2BASE (mod100 section 2, three branches).
            qf2base = M.fromList
                [ ((f, a), qf2 f a)
                | f <- fSet, a <- aSet
                , sam samBalanced f a /= 0.0 ]
            qf2 f a
                | not (truth2 (rawQfbase inp) (f, a)) && truth1 (rawQfsbase inp) f =
                    (rawQfsbase inp M.! f) * sam samBalanced f a
                    / sumL [sam samBalanced f ap | ap <- aSet]
                | not (truth2 (rawQfbase inp) (f, a)) && not (truth1 (rawQfsbase inp) f) =
                    sam samBalanced f a
                | otherwise = rawQfbase inp M.! (f, a)

            -- ---------- mod100 section 4: parameter definitions ----------
            sets' = LhrSets
                { setAc = ac, setAcnt = acnt, setA = aSet, setC = cSet
                , setF = fSet, setIns = ins, setInsd = insd, setInsdng = insdng
                , setH = hSet, setCd = cd, setCdn = cdn, setCe = ce, setCen = cen
                , setCm = cm, setCmn = cmn, setCx = cx, setCt = ct
                , setCtd = ctd, setCte = cte, setCtm = ctm, setCinv = cinv
                , setAleo = aleo, setCdm = cdm
                }

            -- PSUP(c) = 1.
            psup = M.fromList [(c, 1.0) | c <- cSet]
            -- PE0(c) = PSUP(c).
            pe0 = M.fromList [(c, psup M.! c) | c <- ce]
            -- PX0(c) = PSUP(c).
            px0 = M.fromList [(c, psup M.! c) | c <- cx]
            -- PDS0(c) = PSUP(c).
            pds0 = M.fromList [(c, psup M.! c) | c <- cd]
            -- PXAC0(a,c) = PSUP(c).
            pxac0 = M.fromList [((a, c), psup M.! c) | a <- aSet, c <- cSet, sam samBalanced a c /= 0.0]
            -- PA0(a) = 1.
            pa0 = M.fromList [(a, 1.0) | a <- aSet]
            -- EXR0 = 1.
            exr0 = 1.0

            -- QA0(a) = SAM("TOTAL",a) / PA0(a).
            qa0 = M.fromList [(a, sam samBalanced total a / (pa0 M.! a)) | a <- aSet]
            -- QVA0(a) = sum(f, SAM(f,a)) + TAXPAR("VATAX",a).
            qva0 = M.fromList [(a, sumL [sam samBalanced f a | f <- fSet] + tax "VATAX" a) | a <- aSet]
            -- PVA0(a) = QVA0(a) / QVA0(a).
            pva0 = M.fromList [(a, (qva0 M.! a) / (qva0 M.! a)) | a <- aSet]
            -- iva(a) = QVA0(a) / QA0(a).
            iva = M.fromList [(a, (qva0 M.! a) / (qa0 M.! a)) | a <- aSet]
            -- QXAC0(a,c) = SAM(a,c) / PXAC0(a,c).
            qxac0 = M.fromList [((a, c), sam samBalanced a c / (pxac0 M.! (a, c))) | (a, c) <- M.keys pxac0]
            -- QHA0(a,c,h) = SHRHOME(a,c,h) * SAM(a,h) / PXAC0(a,c).
            qha0 = M.fromList
                [ ((a, c, h), shr a c h * sam samBalanced a h / (pxac0 M.! (a, c)))
                | a <- aSet, c <- cSet, h <- hSet, shr a c h /= 0.0 ]

            -- QX0(c) = sum(a, SAM(a,c)) / PX0(c).
            qx0 = M.fromList [(c, sumL [sam samBalanced a c | a <- aSet] / (px0 M.! c)) | c <- cx]
            -- QE0(c) = (SAM(c,"ROW") - TAXPAR("EXPTAX",c) - sum(cte, SAM(cte,c))) / PE0(c).
            qe0 = M.fromList [(c, (sam samBalanced c row - tax "EXPTAX" c - sumL [sam samBalanced t c | t <- cte]) / (pe0 M.! c)) | c <- ce]
            -- PWE0(c) = (SAM(c,"ROW") / EXR0) / QE0(c).
            pwe0 = M.fromList [(c, (sam samBalanced c row / exr0) / (qe0 M.! c)) | c <- ce, qe0 M.! c /= 0.0]
            -- te(c) = TAXPAR("EXPTAX",c) / SAM(c,"ROW").
            te = M.fromList [(c, if sam samBalanced c row /= 0.0 then tax "EXPTAX" c / sam samBalanced c row else 0.0) | c <- cSet]

            -- QD0(c) = QX0(c) - QE0(c).
            qd0 = M.fromList [(c, M.findWithDefault 0.0 c qx0 - M.findWithDefault 0.0 c qe0) | c <- cd]
            -- PDD0(c) = (PDS0(c)*QD0(c) + sum(ctd, SAM(ctd,c))) / QD0(c).
            pdd0 = M.fromList [(c, ((pds0 M.! c) * (qd0 M.! c) + sumL [sam samBalanced t c | t <- ctd]) / (qd0 M.! c)) | c <- cd, qd0 M.! c /= 0.0]
            -- PM0(c) = PDD0(c); if QD0(c) = 0 then PM0(c) = 1.
            pm0 = M.fromList [(c, if M.findWithDefault 0.0 c qd0 == 0.0 then 1.0 else M.findWithDefault 0.0 c pdd0) | c <- cSet]
            -- QM0(c) = (SAM("ROW",c) + TAXPAR("IMPTAX",c) + sum(ctm, SAM(ctm,c))) / PM0(c).
            qm0 = M.fromList [(c, (sam samBalanced row c + tax "IMPTAX" c + sumL [sam samBalanced t c | t <- ctm]) / (pm0 M.! c)) | c <- cm]
            -- PWM0(c) = (SAM("ROW",c) / EXR0) / QM0(c).
            pwm0 = M.fromList [(c, (sam samBalanced row c / exr0) / (qm0 M.! c)) | c <- cm, qm0 M.! c /= 0.0]
            -- tm(c) = TAXPAR("IMPTAX",c) / SAM("ROW",c).
            tm = M.fromList [(c, if sam samBalanced row c /= 0.0 then tax "IMPTAX" c / sam samBalanced row c else 0.0) | c <- cSet]

            -- CDM(c) = CD(c) or CM(c).
            cdm = [c | c <- cSet, c `elem` cd || c `elem` cm]
            -- QQ0(c) = QD0(c) + QM0(c).
            qq0 = M.fromList [(c, M.findWithDefault 0.0 c qd0 + M.findWithDefault 0.0 c qm0) | c <- cdm]
            -- PQ0(c) = (SAM(c,"TOTAL") - SAM(c,"ROW")) / QQ0(c).
            pq0 = M.fromList [(c, (sam samBalanced c total - sam samBalanced c row) / (qq0 M.! c)) | c <- cdm, qq0 M.! c /= 0.0]
            -- tq(c) = TAXPAR("COMTAX",c) / (PQ0(c)*QQ0(c)).
            tq = M.fromList [(c, if truth1 qq0 c then tax "COMTAX" c / ((pq0 M.! c) * (qq0 M.! c)) else 0.0) | c <- cSet]

            -- SHCTD(c) = sum(ctd, SAM(c,ctd) / SAM("TOTAL",ctd)).
            shctd = M.fromList [(c, sumL [sam samBalanced c t / sam samBalanced total t | t <- ctd]) | c <- ct]
            -- SHCTM(c) = sum(ctm, SAM(c,ctm) / SAM("TOTAL",ctm)).
            shctm = M.fromList [(c, sumL [sam samBalanced c t / sam samBalanced total t | t <- ctm]) | c <- ct]
            -- SHCTE(c) = sum(cte, SAM(c,cte) / SAM("TOTAL",cte)).
            shcte = M.fromList [(c, sumL [sam samBalanced c t / sam samBalanced total t | t <- cte]) | c <- ct]

            -- icd(t,c).
            icd = M.fromList [((t, c), ((shctd M.! t) * sumL [sam samBalanced x c | x <- ctd] / (pq0 M.! t)) / (qd0 M.! c)) | t <- ct, c <- cd, truth1 qd0 c]
            -- icm(t,c).
            icm = M.fromList [((t, c), ((shctm M.! t) * sumL [sam samBalanced x c | x <- ctm] / (pq0 M.! t)) / (qm0 M.! c)) | t <- ct, c <- cm, truth1 qm0 c]
            -- ice(t,c).
            ice = M.fromList [((t, c), ((shcte M.! t) * sumL [sam samBalanced x c | x <- cte] / (pq0 M.! t)) / (qe0 M.! c)) | t <- ct, c <- ce, truth1 qe0 c]

            -- tva(a) = TAXPAR("VATAX",a) / (PVA0(a)*QVA0(a)).
            tva = M.fromList [(a, tax "VATAX" a / ((pva0 M.! a) * (qva0 M.! a))) | a <- aSet]
            -- ta(a) = TAXPAR("ACTTAX",a) / SAM(a,"TOTAL").
            ta = M.fromList [(a, tax "ACTTAX" a / sam samBalanced a total) | a <- aSet]

            -- theta(a,c).
            theta = M.fromList
                [ ((a, c), ((sam samBalanced a c + sumL [shr a c h * sam samBalanced a h | h <- hSet])
                            / (pxac0 M.! (a, c))) / (qa0 M.! a))
                | (a, c) <- M.keys pxac0 ]

            -- QINTA0(a) = sum(c, SAM(c,a) / PQ0(c)).
            qinta0 = M.fromList [(a, sumL [sam samBalanced c a / (pq0 M.! c) | c <- cSet, truth1 pq0 c]) | a <- aSet]
            -- ica(c,a).
            ica = M.fromList
                [ ((c, a), (sam samBalanced c a / (pq0 M.! c)) / (qinta0 M.! a))
                | c <- cSet, a <- aSet
                , qinta0 M.! a /= 0.0, truth1 pq0 c, sam samBalanced c a /= 0.0 ]
            -- inta(a) = QINTA0(a) / QA0(a).
            inta = M.fromList [(a, (qinta0 M.! a) / (qa0 M.! a)) | a <- aSet]
            -- PINTA0(a) = sum(c, ica(c,a)*PQ0(c)).
            pinta0 = M.fromList [(a, sumL [M.findWithDefault 0.0 (c, a) ica * M.findWithDefault 0.0 c pq0 | c <- cSet]) | a <- aSet]

            -- cwts(c).
            cwts = M.fromList
                [ (c, sumL [sam samBalanced c h | h <- hSet]
                      / sumL [sam samBalanced cp h | cp <- cSet, h <- hSet])
                | c <- cSet ]
            -- dwts denominator.
            dwtsDen = sumL
                [ sumL [sam samBalanced a cp | a <- aSet]
                  - (sam samBalanced cp row - sumL [sam samBalanced t cp | t <- cte])
                | cp <- cSet ]
            -- dwts(c).
            dwts = M.fromList
                [ (c, (sumL [sam samBalanced a c | a <- aSet]
                       - (sam samBalanced c row - sumL [sam samBalanced t c | t <- cte]))
                      / dwtsDen)
                | c <- cSet ]
            -- CPI0 = sum(c, cwts(c)*PQ0(c)).
            cpi0 = sumL [(cwts M.! c) * M.findWithDefault 0.0 c pq0 | c <- cSet]
            -- DPI0 = sum(cd, dwts(cd)*PDS0(cd)).
            dpi0 = sumL [(dwts M.! c) * (pds0 M.! c) | c <- cd]

            -- rhoq(c).
            rhoq = M.fromList [(c, (1.0 / (tradelas M.! (c, "SIGMAQ"))) - 1.0) | c <- cSet, c `elem` cm && c `elem` cd]
            -- rhot(c).
            rhot = M.fromList [(c, (1.0 / (tradelas M.! (c, "SIGMAT"))) + 1.0) | c <- cSet, c `elem` ce && c `elem` cd]
            -- rhova(a).
            rhova = M.fromList [(a, (1.0 / (prodelas M.! a)) - 1.0) | a <- aSet]

            -- RHOAC(c).
            rhoac = M.fromList [(c, 1.0 / (elasac M.! c) - 1.0) | c <- cSet, elasac M.! c /= 0.0]
            -- deltaac(a,c).
            deltaac = M.fromList
                [ ((a, c), ((pxac0 M.! (a, c)) * (qxac0 M.! (a, c)) ** (1.0 / (elasac M.! c))) / deltaacDen c)
                | c <- cSet, elasac M.! c /= 0.0
                , a <- aSet, M.member (a, c) pxac0 ]
            deltaacDen c =
                sumL [ (pxac0 M.! (ap, c)) * (qxac0 M.! (ap, c)) ** (1.0 / (elasac M.! c))
                     | ap <- aSet, M.member (ap, c) pxac0 ]
            -- alphaac(c).
            alphaac = M.fromList
                [ (c, (qx0 M.! c)
                      / (sumL [(deltaac M.! (a, c)) * (qxac0 M.! (a, c)) ** (-(rhoac M.! c))
                              | a <- aSet, M.member (a, c) deltaac]
                         ) ** (-(1.0 / (rhoac M.! c))))
                | c <- cSet, any (\a -> M.member (a, c) deltaac) aSet ]

            -- QF0(f,a) = QF2BASE(f,a).
            qf0 = qf2base
            -- QFS0(f) = sum(a, QF0(f,a)).
            qfs0 = M.fromList [(f, sumL [M.findWithDefault 0.0 (f, a) qf0 | a <- aSet]) | f <- fSet]
            -- WFA(f,a) = SAM(f,a) / QF0(f,a).
            wfa = M.fromList [((f, a), sam samBalanced f a / (qf0 M.! (f, a))) | (f, a) <- M.keys qf0]
            -- WF0(f) = sum(a, SAM(f,a)) / sum(a, QF0(f,a)).
            wf0 = M.fromList [(f, sumL [sam samBalanced f a | a <- aSet] / sumL [M.findWithDefault 0.0 (f, a) qf0 | a <- aSet]) | f <- fSet]
            -- wfdist0(f,a) = WFA(f,a) / WF0(f).
            wfdist0 = M.fromList [((f, a), (wfa M.! (f, a)) / (wf0 M.! f)) | (f, a) <- M.keys wfa]

            -- deltava(f,a).
            deltava = M.fromList
                [ ((f, a), ((wfdist0 M.! (f, a)) * (wf0 M.! f) * (qf0 M.! (f, a)) ** (1.0 + (rhova M.! a))) / deltavaDen a)
                | a <- aSet, f <- fSet, M.member (f, a) qf0 ]
            deltavaDen a =
                sumL [ (wfdist0 M.! (fp, a)) * (wf0 M.! fp) * (qf0 M.! (fp, a)) ** (1.0 + (rhova M.! a))
                     | fp <- fSet, M.member (fp, a) qf0 ]
            -- alphava(a).
            alphava = M.fromList
                [ (a, (qva0 M.! a)
                      / (sumL [(deltava M.! (f, a)) * (qf0 M.! (f, a)) ** (-(rhova M.! a))
                              | f <- fSet, M.member (f, a) qf0]
                         ) ** (-(1.0 / (rhova M.! a))))
                | a <- aSet ]

            -- QINT0(c,a).
            qint0 = M.fromList
                [ ((c, a), sam samBalanced c a / (pq0 M.! c))
                | c <- cSet, a <- aSet, truth1 pq0 c, sam samBalanced c a /= 0.0 ]
            -- QT0(c).
            qt0 = M.fromList
                [ (c, (sumL [sam samBalanced c t | t <- ctd]
                       + sumL [sam samBalanced c t | t <- cte]
                       + sumL [sam samBalanced c t | t <- ctm]) / (pq0 M.! c))
                | c <- ct ]

            -- deltat(c).
            deltat = M.fromList
                [ (c, 1.0 / (1.0 + (pds0 M.! c) / (pe0 M.! c)
                              * ((qe0 M.! c) / (qd0 M.! c)) ** ((rhot M.! c) - 1.0)))
                | c <- cSet, c `elem` ce && c `elem` cd ]
            -- alphat(c).
            alphat = M.fromList
                [ (c, (qx0 M.! c)
                      / ((deltat M.! c) * (qe0 M.! c) ** (rhot M.! c)
                         + (1.0 - (deltat M.! c)) * (qd0 M.! c) ** (rhot M.! c)
                        ) ** (1.0 / (rhot M.! c)))
                | c <- M.keys deltat ]

            -- predelta(c).
            predelta = M.fromList
                [ (c, ((pm0 M.! c) / (pdd0 M.! c)) * ((qm0 M.! c) / (qd0 M.! c)) ** (1.0 + (rhoq M.! c)))
                | c <- cSet, c `elem` cm && c `elem` cd ]
            -- deltaq(c).
            deltaq = M.fromList [(c, (predelta M.! c) / (1.0 + (predelta M.! c))) | c <- M.keys predelta]
            -- alphaq(c).
            alphaq = M.fromList
                [ (c, (qq0 M.! c)
                      / ((deltaq M.! c) * (qm0 M.! c) ** (-(rhoq M.! c))
                         + (1.0 - (deltaq M.! c)) * (qd0 M.! c) ** (-(rhoq M.! c))
                        ) ** (-(1.0 / (rhoq M.! c))))
                | c <- M.keys deltaq ]

            -- Institution block.
            -- YI0(i) = SAM("TOTAL",i).
            yi0 = M.fromList [(i, sam samBalanced total i) | i <- insdng]
            -- YF0(f) = sum(a, SAM(f,a)).
            yf0 = M.fromList [(f, sumL [sam samBalanced f a | a <- aSet]) | f <- fSet]
            -- YIF0(i,f) = SAM(i,f).
            yif0 = M.fromList [((i, f), sam samBalanced i f) | i <- insd, f <- fSet, sam samBalanced i f /= 0.0]
            -- trnsfr("ROW",f).
            trnsfrRow = M.fromList [((row, f), sam samBalanced row f / exr0) | f <- fSet]
            -- trnsfr(i,"ROW") and trnsfr(i,"GOV").
            trnsfrInst = M.fromList
                ([ ((i, row), sam samBalanced i row / exr0) | i <- insd ]
                 ++ [ ((i, gov), sam samBalanced i gov / cpi0) | i <- insd ])
            trnsfr = M.union trnsfrRow trnsfrInst
            -- tf(f) = TAXPAR("FACTAX",f) / SAM("TOTAL",f).
            tf = M.fromList [(f, tax "FACTAX" f / sam samBalanced total f) | f <- fSet]
            -- shif(i,f).
            shif = M.fromList
                [ ((i, f), sam samBalanced i f
                            / (sam samBalanced f total - tax "FACTAX" f - sam samBalanced row f))
                | i <- insd, f <- fSet, sam samBalanced i f /= 0.0 ]
            -- TRII0(i,ip).
            trii0 = M.fromList [((i, ip), sam samBalanced i ip) | i <- insdng, ip <- insdng, sam samBalanced i ip /= 0.0]
            -- shii(i,ip).
            shii = M.fromList
                [ ((i, ip), sam samBalanced i ip
                             / (sam samBalanced total ip - tax "INSTAX" ip - sam samBalanced si ip))
                | i <- insdng, ip <- insdng, sam samBalanced i ip /= 0.0 ]
            -- MPS0(i).
            mps0 = M.fromList [(i, sam samBalanced si i / (sam samBalanced total i - tax "INSTAX" i)) | i <- insdng]
            -- mpsbar(i) = MPS0(i).
            mpsbar = mps0
            -- TINS0(i).
            tins0 = M.fromList [(i, tax "INSTAX" i / sam samBalanced total i) | i <- insdng]
            -- tinsbar(i) = TINS0(i).
            tinsbar = tins0
            -- mps01(i) = 1.
            mps01 = M.fromList [(i, 1.0) | i <- insdng]
            -- tins01(i) = 1.
            tins01 = M.fromList [(i, 1.0) | i <- insdng]

            -- EH0(h).
            eh0 = M.fromList [(h, sumL [sam samBalanced c h | c <- cSet] + sumL [sam samBalanced a h | a <- aSet]) | h <- hSet]
            -- QH0(c,h).
            qh0 = M.fromList [((c, h), sam samBalanced c h / (pq0 M.! c)) | c <- cSet, h <- hSet, truth1 pq0 c, sam samBalanced c h /= 0.0]
            -- YG0 = SAM("TOTAL","GOV").
            yg0 = sam samBalanced total gov
            -- EG0 = SAM("TOTAL","GOV") - SAM("S-I","GOV").
            eg0 = sam samBalanced total gov - sam samBalanced si gov
            -- QG0(c).
            qg0 = M.fromList [ (c, sam samBalanced c gov / (pq0 M.! c)) | c <- cSet, truth1 pq0 c, sam samBalanced c gov /= 0.0 ]
            -- qbarg(c) = QG0(c).
            qbarg = M.fromList [(c, M.findWithDefault 0.0 c qg0) | c <- cSet]
            -- GADJ0 = 1.
            gadj0 = 1.0
            -- GSAV0 = SAM("S-I","GOV").
            gsav0 = sam samBalanced si gov

            -- LES calibration.
            -- BUDSHR(c,h).
            budshr = M.fromList
                [ ((c, h), sam samBalanced c h / ehDen h)
                | c <- cSet, h <- hSet ]
            -- BUDSHR2(a,c,h).
            budshr2 = M.fromList
                [ ((a, c, h), sam samBalanced a h * shr a c h / ehDen h)
                | a <- aSet, c <- cSet, h <- hSet ]
            ehDen h = sumL [sam samBalanced cp h | cp <- cSet]
                    + sumL [sam samBalanced ap h | ap <- aSet]
            -- ELASCHK(h).
            elaschk = M.fromList
                [ (h, sumL [(budshr M.! (c, h)) * (leselas1 M.! (c, h)) | c <- cSet]
                      + sumL [(budshr2 M.! (a, c, h)) * (leselas2 M.! (a, c, h)) | a <- aSet, c <- cSet])
                | h <- hSet ]
            -- LESELAS1(c,h) = LESELAS1(c,h) / ELASCHK(h).
            leselas1Adj = M.fromList [((c, h), (leselas1 M.! (c, h)) / (elaschk M.! h)) | c <- cSet, h <- hSet]
            -- LESELAS2(a,c,h) = LESELAS2(a,c,h) / ELASCHK(h).
            leselas2Adj = M.fromList [((a, c, h), (leselas2 M.! (a, c, h)) / (elaschk M.! h)) | a <- aSet, c <- cSet, h <- hSet]

            -- betam(c,h).
            betam = M.fromList [((c, h), (budshr M.! (c, h)) * (leselas1Adj M.! (c, h))) | c <- cSet, h <- hSet]
            -- betah(a,c,h).
            betah = M.fromList [((a, c, h), (budshr2 M.! (a, c, h)) * (leselas2Adj M.! (a, c, h))) | a <- aSet, c <- cSet, h <- hSet]
            -- gammam(c,h).
            gammam = M.fromList
                [ ((c, h), (ehDen h / (pq0 M.! c)) * ((budshr M.! (c, h)) + (betam M.! (c, h)) / (frisch M.! h)))
                | c <- cSet, h <- hSet, budshr M.! (c, h) /= 0.0 ]
            -- gammah(a,c,h).
            gammah = M.fromList
                [ ((a, c, h), (ehDen h / (pxac0 M.! (a, c))) * ((budshr2 M.! (a, c, h)) + (betah M.! (a, c, h)) / (frisch M.! h)))
                | a <- aSet, c <- cSet, h <- hSet, budshr2 M.! (a, c, h) /= 0.0 ]

            -- LESCHK (mod100: Frisch consistency).
            lesFailures =
                [ "LESCHK failed for " ++ acName h ++ ": " ++ show (frisch M.! h) ++ " vs " ++ show (frisch2 h)
                | h <- hSet, abs ((frisch M.! h) - frisch2 h) > 1e-8 ]
            frisch2 h = -(eh0 M.! h) / ((eh0 M.! h) - supernum h)
            supernum h =
                sumL [M.findWithDefault 0.0 (a, c, h) gammah * M.findWithDefault 0.0 (a, c) pxac0 | a <- aSet, c <- cSet]
                + sumL [M.findWithDefault 0.0 (c, h) gammam * M.findWithDefault 0.0 c pq0 | c <- cSet]

            -- System-constraint block.
            -- qbarinv(c).
            qbarinv = M.fromList [(c, sam samBalanced c si / (pq0 M.! c)) | c <- cinv]
            -- QINV0(c) = qbarinv(c).
            qinv0 = qbarinv
            -- IADJ0 = 1.
            iadj0 = 1.0
            -- qdst(c).
            qdst = M.fromList
                [ (c, ((if c `notElem` cinv then sam samBalanced c si else 0.0)
                       + sam samBalanced c dstk) / (pq0 M.! c))
                | c <- cSet, truth1 pq0 c ]
            -- FSAV0 = SAM("S-I","ROW") / EXR0.
            fsav0 = sam samBalanced si row / exr0
            -- TABS0.
            tabs0 = sumL [sam samBalanced c h | c <- cSet, h <- hSet]
                  + sumL [sam samBalanced a h | a <- aSet, h <- hSet]
                  + sumL [sam samBalanced c gov | c <- cSet]
                  + sumL [sam samBalanced c si | c <- cSet]
                  + sumL [sam samBalanced c dstk | c <- cSet]
            -- INVSHR0 = SAM("TOTAL","S-I") / TABS0.
            invshr0 = sam samBalanced total si / tabs0
            -- GOVSHR0 = sum(c, SAM(c,"GOV")) / TABS0.
            govshr0 = sumL [sam samBalanced c gov | c <- cSet] / tabs0

            params' = LhrParams
                { paramTm = tm, paramTe = te, paramTq = tq
                , paramTa = ta, paramTva = tva, paramTf = tf
                , paramIcm = icm, paramIce = ice, paramIcd = icd
                , paramIca = ica, paramTheta = theta
                , paramIva = iva, paramInta = inta
                , paramCwts = cwts, paramDwts = dwts
                , paramAlphava = alphava, paramDeltava = deltava, paramRhova = rhova
                , paramAlphaac = alphaac, paramDeltaac = deltaac, paramRhoac = rhoac
                , paramAlphat = alphat, paramDeltat = deltat, paramRhot = rhot
                , paramAlphaq = alphaq, paramDeltaq = deltaq, paramRhoq = rhoq
                , paramShif = shif, paramShii = shii, paramTrnsfr = trnsfr
                , paramBetam = betam, paramBetah = betah
                , paramGammam = gammam, paramGammah = gammah
                , paramQbarinv = qbarinv, paramQbarg = qbarg, paramQdst = qdst
                , paramMpsbar = mpsbar, paramTinsbar = tinsbar
                , paramMps01 = mps01, paramTins01 = tins01
                , paramFrisch = frisch
                }

            base' = LhrBase
                { baseCpi0 = cpi0, baseDpi0 = dpi0, baseExr0 = exr0
                , baseEg0 = eg0, baseEh0 = eh0, baseFsav0 = fsav0
                , baseGadj0 = gadj0, baseGsav0 = gsav0
                , baseGovshr0 = govshr0, baseIadj0 = iadj0
                , baseInvshr0 = invshr0, baseMps0 = mps0
                , baseTins0 = tins0, basePa0 = pa0
                , basePdd0 = pdd0, basePds0 = pds0, basePe0 = pe0
                , basePinta0 = pinta0, basePm0 = pm0, basePq0 = pq0
                , basePva0 = pva0, basePwe0 = pwe0, basePwm0 = pwm0
                , basePx0 = px0, basePxac0 = pxac0
                , baseQa0 = qa0, baseQd0 = qd0, baseQe0 = qe0
                , baseQf0 = qf0, baseQfs0 = qfs0, baseQg0 = qg0
                , baseQh0 = qh0, baseQha0 = qha0, baseQint0 = qint0
                , baseQinta0 = qinta0, baseQinv0 = qinv0
                , baseQm0 = qm0, baseQq0 = qq0, baseQt0 = qt0
                , baseQva0 = qva0, baseQx0 = qx0, baseQxac0 = qxac0
                , baseTabs0 = tabs0, baseTrii0 = trii0
                , baseWf0 = wf0, baseWfdist0 = wfdist0
                , baseYf0 = yf0, baseYg0 = yg0
                , baseYi0 = yi0, baseYif0 = yif0
                }
  where
    ac = rawAc inp
    aSet = rawA inp
    cSet = rawC inp
    fSet = rawF inp
    ins = rawIns inp
    insd = rawInsd inp
    insdng = rawInsdng inp
    hSet = rawH inp
    ctd = rawCtd inp
    cte = rawCte inp
    ctm = rawCtm inp

    row = Ac "ROW"
    gov = Ac "GOV"
    total = Ac "TOTAL"
    si = Ac "S-I"
    dstk = Ac "DSTK"

    sam0 = rawSam inp

    taxparResult = foldM addTaxparRule M.empty (rawTaxparRules inp)
    addTaxparRule acc ((tx, over), rule) =
        case lookup over [("INSD", insd), ("F", fSet), ("C", cSet), ("A", aSet)] of
            Nothing -> Left ("dataset error: TAXPAR_RULE over unknown set " ++ over)
            Just domain -> Right $
                foldl' (\m e -> M.insert (tx, e) (ruleValue rule e) m) acc domain
    ruleValue (SamRow acct) e = sam sam0 acct e
    ruleValue Zero _ = 0.0

sumL :: [Double] -> Double
sumL = foldl' (+) 0.0

sam :: M.Map (Ac, Ac) Double -> Ac -> Ac -> Double
sam m r c = M.findWithDefault 0.0 (r, c) m

setSam :: Ac -> Ac -> Double -> M.Map (Ac, Ac) Double -> M.Map (Ac, Ac) Double
setSam r c v m
    | v == 0.0  = M.delete (r, c) m
    | otherwise = M.insert (r, c) v m

truth1 :: Ord k => M.Map k Double -> k -> Bool
truth1 m k =
    case M.lookup k m of
        Just v  -> v /= 0.0
        Nothing -> False

truth2 :: Ord k => M.Map k Double -> k -> Bool
truth2 = truth1

maxList :: [Double] -> Double
maxList []     = 0.0
maxList (x:xs) = foldl' max x xs

acName :: Ac -> String
acName (Ac s) = s

------------------------------------------------------------------
-- * Flattening for fixture comparison
------------------------------------------------------------------

toTable :: LhrCalibration -> M.Map (String, [String]) Double
toTable cal = M.unions
    [ one "tm"       (paramTm p)
    , one "te"       (paramTe p)
    , one "tq"       (paramTq p)
    , one "ta"       (paramTa p)
    , one "tva"      (paramTva p)
    , one "tf"       (paramTf p)
    , two "icm"      (paramIcm p)
    , two "ice"      (paramIce p)
    , two "icd"      (paramIcd p)
    , two "ica"      (paramIca p)
    , two "theta"    (paramTheta p)
    , one "iva"      (paramIva p)
    , one "inta"     (paramInta p)
    , one "cwts"     (paramCwts p)
    , one "dwts"     (paramDwts p)
    , one "alphava"  (paramAlphava p)
    , two "deltava"  (paramDeltava p)
    , one "rhova"    (paramRhova p)
    , one "alphaac"  (paramAlphaac p)
    , two "deltaac"  (paramDeltaac p)
    , one "RHOAC"    (paramRhoac p)
    , one "alphat"   (paramAlphat p)
    , one "deltat"   (paramDeltat p)
    , one "rhot"     (paramRhot p)
    , one "alphaq"   (paramAlphaq p)
    , one "deltaq"   (paramDeltaq p)
    , one "rhoq"     (paramRhoq p)
    , two "shif"     (paramShif p)
    , two "shii"     (paramShii p)
    , two "trnsfr"   (paramTrnsfr p)
    , two "betam"    (paramBetam p)
    , three "betah"  (paramBetah p)
    , two "gammam"   (paramGammam p)
    , three "gammah" (paramGammah p)
    , one "qbarinv"  (paramQbarinv p)
    , one "qbarg"    (paramQbarg p)
    , one "qdst"     (paramQdst p)
    , one "mpsbar"   (paramMpsbar p)
    , one "tinsbar"  (paramTinsbar p)
    , one "mps01"    (paramMps01 p)
    , one "tins01"   (paramTins01 p)
    , one "frisch"   (paramFrisch p)
    , scalar "CPI0"    (baseCpi0 b)
    , scalar "DPI0"    (baseDpi0 b)
    , scalar "EXR0"    (baseExr0 b)
    , scalar "EG0"     (baseEg0 b)
    , one "EH0"        (baseEh0 b)
    , scalar "FSAV0"   (baseFsav0 b)
    , scalar "GADJ0"   (baseGadj0 b)
    , scalar "GSAV0"   (baseGsav0 b)
    , scalar "GOVSHR0" (baseGovshr0 b)
    , scalar "IADJ0"   (baseIadj0 b)
    , scalar "INVSHR0" (baseInvshr0 b)
    , one "MPS0"       (baseMps0 b)
    , one "TINS0"      (baseTins0 b)
    , one "PA0"        (basePa0 b)
    , one "PDD0"       (basePdd0 b)
    , one "PDS0"       (basePds0 b)
    , one "PE0"        (basePe0 b)
    , one "PINTA0"     (basePinta0 b)
    , one "PM0"        (basePm0 b)
    , one "PQ0"        (basePq0 b)
    , one "PVA0"       (basePva0 b)
    , one "PWE0"       (basePwe0 b)
    , one "PWM0"       (basePwm0 b)
    , one "PX0"        (basePx0 b)
    , two "PXAC0"      (basePxac0 b)
    , one "QA0"        (baseQa0 b)
    , one "QD0"        (baseQd0 b)
    , one "QE0"        (baseQe0 b)
    , two "QF0"        (baseQf0 b)
    , one "QFS0"       (baseQfs0 b)
    , one "QG0"        (baseQg0 b)
    , two "QH0"        (baseQh0 b)
    , three "QHA0"     (baseQha0 b)
    , two "QINT0"      (baseQint0 b)
    , one "QINTA0"     (baseQinta0 b)
    , one "QINV0"      (baseQinv0 b)
    , one "QM0"        (baseQm0 b)
    , one "QQ0"        (baseQq0 b)
    , one "QT0"        (baseQt0 b)
    , one "QVA0"       (baseQva0 b)
    , one "QX0"        (baseQx0 b)
    , two "QXAC0"      (baseQxac0 b)
    , scalar "TABS0"   (baseTabs0 b)
    , two "TRII0"      (baseTrii0 b)
    , one "WF0"        (baseWf0 b)
    , two "wfdist0"    (baseWfdist0 b)
    , one "YF0"        (baseYf0 b)
    , scalar "YG0"     (baseYg0 b)
    , one "YI0"        (baseYi0 b)
    , two "YIF0"       (baseYif0 b)
    , two "SAM"        (calSam cal)
    , taxRows "TAXPAR" (calTaxpar cal)
    , three "shrhome"  (calShrhome cal)
    ]
  where
    p = calParams cal
    b = calBase cal

    scalar name v = M.singleton (name, []) v
    one name m = M.fromList [((name, [acName k]), v) | (k, v) <- M.toList m]
    two name m = M.fromList [((name, [acName k1, acName k2]), v) | ((k1, k2), v) <- M.toList m]
    three name m = M.fromList [((name, [acName k1, acName k2, acName k3]), v) | ((k1, k2, k3), v) <- M.toList m]
    taxRows name m = M.fromList [((name, [tx, acName k]), v) | ((tx, k), v) <- M.toList m]

setsTable :: LhrSets -> [(String, Ac, Int)]
setsTable s = concat
    [ rows "AC"     (setAc s)
    , rows "ACNT"   (setAcnt s)
    , rows "A"      (setA s)
    , rows "C"      (setC s)
    , rows "F"      (setF s)
    , rows "INS"    (setIns s)
    , rows "INSD"   (setInsd s)
    , rows "INSDNG" (setInsdng s)
    , rows "H"      (setH s)
    , rows "CD"     (setCd s)
    , rows "CDN"    (setCdn s)
    , rows "CE"     (setCe s)
    , rows "CEN"    (setCen s)
    , rows "CM"     (setCm s)
    , rows "CMN"    (setCmn s)
    , rows "CX"     (setCx s)
    , rows "CT"     (setCt s)
    , rows "CTD"    (setCtd s)
    , rows "CTE"    (setCte s)
    , rows "CTM"    (setCtm s)
    , rows "CINV"   (setCinv s)
    , rows "ALEO"   (setAleo s)
    , rows "CDM"    (setCdm s)
    ]
  where
    rows name xs = [(name, x, i) | (i, x) <- zip [0..] xs]
