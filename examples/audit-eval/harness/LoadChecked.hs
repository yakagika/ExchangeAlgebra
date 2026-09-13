{-# LANGUAGE OverloadedStrings #-}

{- |
  LoadChecked.hs - checked-loader gate for audit-eval arm A-prime.

  Reads a single JSON object

      {"postings":[{"txid":"t1","side":"debit","account":"Cash","amount":1000}],
       "sources":[{"id":"t1","amount":1000}]}

  from stdin (or from a file given as the first CLI argument), validates the
  postings with ExchangeAlgebra.Convert.Checked, optionally reconciles source
  transaction amounts, and prints a verdict JSON object on stdout.

  The flat posting-array interface groups postings by txid in first-seen order.
  Repeated txids are intentionally merged into the same entry before journal
  construction, so checkedJournal's DuplicateTxId case cannot arise through
  this path. If a model double-counts a source transaction by repeating its
  postings, the surfaced error is reconcileSources AmountMismatch.

  Numeric parsing follows oracle/Oracle.hs's dependency-light subset. Integer
  number tokens (no '.', 'e', or 'E') are converted through Integer/fromInteger
  exactly. Decimal/scientific tokens are parsed through Double and converted by
  realToFrac; this keeps parity with the oracle but has the usual binary FP
  boundary limitation.

  Gate failures are data, not infrastructure errors: malformed postings,
  checked-entry errors, and source mismatches print {"ok":false,...} and exit
  0. Only unparseable JSON itself exits nonzero, matching oracle/Oracle.hs.
-}

import           Data.Char (isDigit, isSpace, ord, chr, isHexDigit, digitToInt)
import           Control.Monad (foldM)
import           Data.Decimal (DecimalRaw(..))
import           Data.Ratio (numerator, denominator)
import           Numeric (showHex)
import qualified Text.ParserCombinators.ReadP as RP
import           ExchangeAlgebra.Convert (parseAccountTitle)
import qualified ExchangeAlgebra.Bookkeeping as BK
import qualified ExchangeAlgebra.Algebra.Transfer as EAT
import           ExchangeAlgebra.Algebra.Base.Account.Registry
import           ExchangeAlgebra.Algebra.Base.Account.Types
import           Data.List (foldl', intercalate, nub, sort, intersect, isInfixOf)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           ExchangeAlgebra hiding (map, filter)
import           ExchangeAlgebra.Assist
                     ( explainJournalErrors
                     , explainSourceErrors
                     )
import           ExchangeAlgebra.Convert.Checked
                     ( EntryError(..)
                     , JournalError(..)
                     , SourceError(..)
                     , ProcessingContext(..)
                     , checkedEntryTextIn
                     , checkedEntryText
                     , reconcileSources
                     )
import           ExchangeAlgebra.Journal (Journal, (.|), toAlg)
import           EmitCanonical (postingsJSON, postingsWithTxidsJSON)

------------------------------------------------------------------
-- Minimal JSON parser (same subset as oracle/Oracle.hs)
------------------------------------------------------------------

data J = JStr String | JNum String | JArr [J] | JObj [(String, J)]
       | JBool Bool | JNull
       deriving (Show, Eq)

type P a = String -> Maybe (a, String)

skipWs :: String -> String
skipWs = dropWhile isSpace

pValue :: P J
pValue s = case skipWs s of
    ('"':rest) -> pStringBody rest >>= \(str, r) -> Just (JStr str, r)
    ('[':rest) -> pArray rest
    ('{':rest) -> pObject rest
    ('t':_) -> pLit "true" (JBool True) (skipWs s)
    ('f':_) -> pLit "false" (JBool False) (skipWs s)
    ('n':_) -> pLit "null" JNull (skipWs s)
    rest -> pNumber rest

pLit :: String -> J -> P J
pLit lit v s
    | take (length lit) s == lit = Just (v, drop (length lit) s)
    | otherwise = Nothing

pStringBody :: P String
pStringBody s = go s ""
  where
    go ('\\':c:rest) acc = go rest (unesc c : acc)
    go ('"':rest) acc = Just (reverse acc, rest)
    go (c:rest) acc = go rest (c : acc)
    go [] _ = Nothing
    unesc 'n' = '\n'
    unesc 't' = '\t'
    unesc 'r' = '\r'
    unesc c = c

pNumber :: P J
pNumber s =
    let (tok, rest) = span isNumChar s
    in if null tok || not (validNumber tok)
       then Nothing
       else Just (JNum tok, rest)
  where
    isNumChar c = isDigit c || c `elem` ("+-.eE" :: String)
    validNumber tok = case reads tok :: [(Double, String)] of
        [(_, "")] -> True
        _ -> False

pArray :: P J
pArray s = case skipWs s of
    (']':rest) -> Just (JArr [], rest)
    _ -> go s []
  where
    go s' acc = do
        (v, r1) <- pValue s'
        case skipWs r1 of
            (',':r2) -> go r2 (v : acc)
            (']':r2) -> Just (JArr (reverse (v : acc)), r2)
            _ -> Nothing

pObject :: P J
pObject s = case skipWs s of
    ('}':rest) -> Just (JObj [], rest)
    _ -> go s []
  where
    go s' acc = case skipWs s' of
        ('"':r0) -> do
            (k, r1) <- pStringBody r0
            case skipWs r1 of
                (':':r2) -> do
                    (v, r3) <- pValue r2
                    case skipWs r3 of
                        (',':r4) -> go r4 ((k, v) : acc)
                        ('}':r4) -> Just (JObj (reverse ((k, v) : acc)), r4)
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing

parseJSON :: String -> Maybe J
parseJSON s = case pValue s of
    Just (v, rest) | all isSpace rest -> Just v
    _ -> Nothing

------------------------------------------------------------------
-- Extraction and gate errors
------------------------------------------------------------------

type Amount = MoneyDecimal
type MinBase = HatBase AccountTitles
type MinEntry = Alg MoneyDecimal MinBase
type MinJournal = Journal String MoneyDecimal MinBase

data RawPosting = RawPosting
    { rpIndex :: Int
    , rpTxid :: Maybe String
    , rpEntry :: Maybe String
    , rpSide :: String
    , rpAccount :: String
    , rpAmount :: Amount
    } deriving (Show)

data Posting = Posting
    { pTxid :: String
    , pSide :: String
    , pAccount :: String
    , pAmount :: Amount
    } deriving (Show)

data Source = Source
    { sId :: String
    , sAmount :: Amount
    } deriving (Show)

data InputError = InputError
    { ieKind :: String
    , ieRawName :: String
    , ieMessage :: String
    } deriving (Show)

data EntryBlock = EntryBlock String (NonEmpty (EntryError Amount))

parseAmount :: Int -> String -> Either InputError Amount
parseAmount idx tok
    | integerToken tok =
        case reads tok :: [(Integer, String)] of
            [(i, "")]
                | i <= 0 -> Left (nonPositive idx tok)
                | otherwise -> Right (fromInteger i)
            _ -> Left (malformed idx "amount is not a JSON number")
    | otherwise =
        case reads tok :: [(Double, String)] of
            [(d, "")]
                | d <= 0 -> Left (nonPositive idx tok)
                | otherwise -> Right (realToFrac d)
            _ -> Left (malformed idx "amount is not a JSON number")
  where
    integerToken = not . any (`elem` (".eE" :: String))
    nonPositive i raw = InputError
        "malformed_posting"
        "NonPositiveAmount"
        ("malformed_posting index " ++ show i ++ ": NonPositiveAmount "
            ++ show i ++ " amount " ++ raw)
    malformed i msg = InputError
        "malformed_posting"
        "malformed_posting"
        ("malformed_posting index " ++ show i ++ ": " ++ msg)

parseSourceAmount :: Int -> String -> Either InputError Amount
parseSourceAmount idx tok =
    case parseAmount idx tok of
        Right amount -> Right amount
        Left err -> Left err
            { ieKind = "malformed_source"
            , ieRawName = if ieRawName err == "NonPositiveAmount"
                          then "NonPositiveSourceAmount"
                          else "malformed_source"
            , ieMessage = "malformed_source index " ++ show idx ++ ": invalid amount"
            }

lookupField :: String -> [(String, J)] -> Maybe J
lookupField = lookup

stringField :: String -> [(String, J)] -> Either String (Maybe String)
stringField key kvs = case lookupField key kvs of
    Nothing -> Right Nothing
    Just (JStr s) -> Right (Just s)
    Just _ -> Left (key ++ " must be a string")

requiredString :: Int -> String -> [(String, J)] -> Either InputError String
requiredString idx key kvs = case lookupField key kvs of
    Just (JStr s) -> Right s
    Nothing -> Left (malformedPosting idx ("missing " ++ key))
    Just _ -> Left (malformedPosting idx (key ++ " must be a string"))

requiredNumber :: Int -> String -> [(String, J)] -> Either InputError String
requiredNumber idx key kvs = case lookupField key kvs of
    Just (JNum tok) -> Right tok
    Nothing -> Left (malformedPosting idx ("missing " ++ key))
    Just _ -> Left (malformedPosting idx (key ++ " must be a number"))

malformedPosting :: Int -> String -> InputError
malformedPosting idx msg = InputError
    "malformed_posting"
    "malformed_posting"
    ("malformed_posting index " ++ show idx ++ ": " ++ msg)

missingTxid :: Int -> InputError
missingTxid idx = InputError
    "missing_txid"
    "missing_txid"
    ("missing_txid index " ++ show idx ++ ": posting must include txid")

parseRawPosting :: Int -> J -> Either InputError RawPosting
parseRawPosting idx (JObj kvs) = do
    txid <- either (Left . malformedPosting idx) Right (stringField "txid" kvs)
    entry <- either (Left . malformedPosting idx) Right (stringField "entry" kvs)
    side <- requiredString idx "side" kvs
    account <- requiredString idx "account" kvs
    amountTok <- requiredNumber idx "amount" kvs
    amount <- parseAmount idx amountTok
    Right RawPosting
        { rpIndex = idx
        , rpTxid = txid
        , rpEntry = entry
        , rpSide = side
        , rpAccount = account
        , rpAmount = amount
        }
parseRawPosting idx _ = Left (malformedPosting idx "posting must be an object")

parseSource :: Int -> J -> Either InputError Source
parseSource idx (JObj kvs) = do
    sid <- case lookupField "id" kvs of
        Just (JStr s) -> Right s
        Nothing -> Left sourceMalformed
        Just _ -> Left sourceMalformed
    amountTok <- case lookupField "amount" kvs of
        Just (JNum tok) -> Right tok
        Nothing -> Left sourceMalformed
        Just _ -> Left sourceMalformed
    amount <- parseSourceAmount idx amountTok
    Right (Source sid amount)
  where
    sourceMalformed = InputError
        "malformed_source"
        "malformed_source"
        ("malformed_source index " ++ show idx ++ ": source must have string id and numeric amount")
parseSource idx _ = Left (InputError
    "malformed_source"
    "malformed_source"
    ("malformed_source index " ++ show idx ++ ": source must be an object"))

partitionEither :: [Either a b] -> ([a], [b])
partitionEither = foldr step ([], [])
  where
    step (Left e) (es, xs) = (e : es, xs)
    step (Right x) (es, xs) = (es, x : xs)

extractInput :: J -> ([InputError], [Posting], [Source])
extractInput (JObj kvs) =
    let (sourceErrors, sources) = case lookupField "sources" kvs of
            Nothing -> ([], [])
            Just (JArr xs) -> partitionEither (zipWith parseSource [0..] xs)
            Just _ -> ([InputError "malformed_source" "malformed_source"
                        "malformed_source: sources must be an array"], [])
        (postingErrors, raws) = case lookupField "postings" kvs of
            Just (JArr xs) -> partitionEither (zipWith parseRawPosting [0..] xs)
            _ -> ([InputError "malformed_posting" "malformed_posting"
                   "malformed_posting: postings must be an array"], [])
        (keyErrors, postings) = assignKeys (not (null sources)) raws
    in (sourceErrors ++ postingErrors ++ keyErrors, postings, sources)
extractInput _ =
    ([InputError "malformed_input" "malformed_input"
      "malformed_input: top-level JSON value must be an object"], [], [])

assignKeys :: Bool -> [RawPosting] -> ([InputError], [Posting])
assignKeys sourcesPresent raws
    | sourcesPresent =
        let errs = [ missingTxid (rpIndex p) | p <- raws, rpTxid p == Nothing ]
            postings = [ toPosting p txid | p <- raws, Just txid <- [rpTxid p] ]
        in (errs, postings)
    | all (maybe True (const False) . rawKey) raws =
        ([], [ toPosting p "tx1" | p <- raws ])
    | otherwise =
        let errs = [ missingTxid (rpIndex p) | p <- raws, rawKey p == Nothing ]
            postings = [ toPosting p key | p <- raws, Just key <- [rawKey p] ]
        in (errs, postings)
  where
    rawKey p = case rpTxid p of
        Just txid -> Just txid
        Nothing -> rpEntry p
    toPosting p txid = Posting txid (rpSide p) (rpAccount p) (rpAmount p)

------------------------------------------------------------------
-- Checked loader
------------------------------------------------------------------

groupPostings :: [Posting] -> [(String, [Posting])]
groupPostings = foldl' add []
  where
    add [] p = [(pTxid p, [p])]
    add ((txid, ps):rest) p
        | txid == pTxid p = (txid, ps ++ [p]) : rest
        | otherwise = (txid, ps) : add rest p

checkEntryGroup :: (String, [Posting]) -> Either EntryBlock (String, MinEntry)
checkEntryGroup (txid, ps) =
    case checkedEntryText rows of
        Left errs -> Left (EntryBlock txid errs)
        Right alg -> Right (txid, alg)
  where
    rows =
        [ (T.pack (pSide p), T.pack (pAccount p), pAmount p)
        | p <- ps
        ]

buildJournal :: [(String, MinEntry)] -> MinJournal
buildJournal entries =
    foldl' (.+) mempty [ alg .| txid | (txid, alg) <- entries ]

runGate :: [Posting] -> [Source] -> Either ([EntryBlock], [SourceError String Amount]) MinJournal
runGate postings sources =
    let (entryBlocks, goodEntries) = partitionEither (map checkEntryGroup (groupPostings postings))
    in case entryBlocks of
        _:_ -> Left (entryBlocks, [])
        [] ->
            let journal = buildJournal goodEntries
                sourceRows = [ (sId s, sAmount s) | s <- sources ]
                sourceErrors =
                    if null sourceRows then [] else reconcileSources sourceRows journal
            in if null sourceErrors
               then Right journal
               else Left ([], sourceErrors)

------------------------------------------------------------------
-- Verdict rendering
------------------------------------------------------------------

jstr :: String -> String
jstr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc '\r' = "\\r"
    esc c = [c]

jarr :: [String] -> String
jarr xs = "[" ++ intercalate "," xs ++ "]"

jobj :: [(String, String)] -> String
jobj kvs = "{" ++ intercalate "," [ jstr k ++ ":" ++ v | (k, v) <- kvs ] ++ "}"

entryBlockJSON :: EntryBlock -> String
entryBlockJSON (EntryBlock txid errs) =
    jobj
        [ ("txid", jstr txid)
        , ("errors", jarr (map (jstr . show) (NE.toList errs)))
        ]

entryBlockToJournalError :: EntryBlock -> JournalError String Amount
entryBlockToJournalError (EntryBlock txid errs) = EntryErrors txid errs

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty [] = Nothing
nonEmpty (x:xs) = Just (x :| xs)

entryErrorName :: EntryError v -> String
entryErrorName (EntryParse _ _) = "EntryParse"
entryErrorName (NonPositiveAmount _ _ _) = "NonPositiveAmount"
entryErrorName (WildcardAccount _) = "WildcardAccount"
entryErrorName (WildcardSide _) = "WildcardSide"
entryErrorName (PostingNotAllowed _ _ _ _) = "PostingNotAllowed"
entryErrorName EmptyEntry = "EmptyEntry"
entryErrorName (Imbalanced _ _) = "Imbalanced"

sourceErrorName :: SourceError n v -> String
sourceErrorName (MissingSource _) = "MissingSource"
sourceErrorName (UnknownSource _) = "UnknownSource"
sourceErrorName (AmountMismatch _ _ _) = "AmountMismatch"

rawSummary :: [InputError] -> [EntryBlock] -> [SourceError String Amount] -> String
rawSummary inputErrs entryBlocks sourceErrs =
    intercalate "; " . filter (not . null) $
        [ inputSummary
        , entrySummary
        , sourceSummary
        ]
  where
    inputSummary
        | null inputErrs = ""
        | otherwise = "input: " ++ intercalate ", " (map ieRawName inputErrs)
    entrySummary =
        intercalate "; "
            [ txid ++ ": " ++ intercalate ", " (map entryErrorName (NE.toList errs))
            | EntryBlock txid errs <- entryBlocks
            ]
    sourceSummary
        | null sourceErrs = ""
        | otherwise = "sources: " ++ intercalate ", " (map sourceErrorName sourceErrs)

richSummary :: [InputError] -> [EntryBlock] -> [SourceError String Amount] -> String
richSummary inputErrs entryBlocks sourceErrs =
    intercalate "\n" . filter (not . null) $
        [ intercalate "\n" (map ieMessage inputErrs)
        , entryRich
        , sourceRich
        ]
  where
    entryRich = case nonEmpty (map entryBlockToJournalError entryBlocks) of
        Nothing -> ""
        Just errs -> T.unpack (explainJournalErrors errs)
    sourceRich
        | null sourceErrs = ""
        | otherwise = T.unpack (explainSourceErrors sourceErrs)

failureVerdict :: [InputError] -> [EntryBlock] -> [SourceError String Amount] -> String
failureVerdict inputErrs entryBlocks sourceErrs =
    jobj
        [ ("ok", "false")
        , ("entry_errors", jarr (map entryBlockJSON entryBlocks))
        , ("source_errors", jarr (map (jstr . show) sourceErrs))
        , ("input_errors", jarr (map (jstr . ieMessage) inputErrs))
        , ("raw", jstr (rawSummary inputErrs entryBlocks sourceErrs))
        , ("rich", jstr (richSummary inputErrs entryBlocks sourceErrs))
        ]

successVerdict :: MinJournal -> String
successVerdict journal =
    "{\"ok\":true,\"journal\":" ++ postingsJSON (toAlg journal) ++ "}"

------------------------------------------------------------------
-- Main
------------------------------------------------------------------

main :: IO ()
main = do
    originalArgs <- getArgs
    let (contract, args) = case originalArgs of
            ("--contract":version:rest) -> (version,rest)
            _ -> ("v2",originalArgs)
    input <- case args of
        (path:_) -> readFile path
        [] -> getContents

    case (if contract == "v3" then parseJSON3 else parseJSON) input of
        Nothing -> do
            hPutStrLn stderr "LoadChecked: input is not parseable JSON"
            putStrLn "{\"ok\":false,\"error\":\"unparseable input\"}"
            exitFailure
        Just value | contract == "v3" -> putStrLn $ either failureVerdict3 id (runV3 value)
        Just _ | contract /= "v2" -> putStrLn (failureVerdict [InputError "invalid_contract" "invalid_contract" "invalid_contract"] [] [])
        Just value -> do
            let (inputErrs, postings, sources) = extractInput value
            if not (null inputErrs)
                then putStrLn (failureVerdict inputErrs [] [])
                else case runGate postings sources of
                    Right journal -> putStrLn (successVerdict journal)
                    Left (entryBlocks, sourceErrs) ->
                        putStrLn (failureVerdict [] entryBlocks sourceErrs)

------------------------------------------------------------------
-- v3: exact, closed named-call endpoint. v2 above remains frozen.
------------------------------------------------------------------

data CatalogCall
    = Cogs Amount Amount | DepIndirect Amount | DepDirect Amount AccountTitles
    | Allowance Amount Amount | AllowanceRate Amount | AllowanceReset Amount Amount
    | Prepaid Amount AccountTitles | Unearned Amount AccountTitles
    | AccruedRevenueCall Amount AccountTitles | AccruedExpenseCall Amount AccountTitles
    | ReverseEntry String | ConsumptionTax Amount Amount | CorporateInterim Amount
    | CorporateSettlement Amount Amount | EquityEarnings Amount | EquityDividend Amount
    | EquityEntries Amount Amount | EquityBalance
    | PriorError Amount Amount AccountTitles AccountTitles | FinalStock
    | StraightLine AccountTitles Amount Bool
    | Consolidate [(String, [String])] [String]
    deriving (Show)

data Call = Call { callIndex :: Int, callName :: String, callTxid :: Maybe String,
                   callBody :: CatalogCall } deriving (Show)

type V3 a = Either InputError a

v3error :: String -> String -> V3 a
v3error reason detail = Left (InputError reason reason (reason ++ ": " ++ detail))

ensure :: String -> String -> Bool -> V3 ()
ensure reason detail yes = if yes then Right () else v3error reason detail

object3 :: String -> J -> V3 [(String, J)]
object3 _ (JObj xs) = do
    ensure "forbidden_field" "duplicate object key" (length (nub (map fst xs)) == length xs)
    pure xs
object3 reason _ = v3error reason "expected object"

keys3 :: String -> [String] -> [String] -> [(String, J)] -> V3 ()
keys3 reason required allowed xs = do
    ensure reason "unknown or missing property" (all (`elem` allowed) (map fst xs) && all (`elem` map fst xs) required)

str3 :: String -> String -> [(String,J)] -> V3 String
str3 reason key xs = case lookup key xs of
    Just (JStr s) | not (null s) -> pure s
    _ -> v3error reason (key ++ " must be a nonempty string")

array3 :: String -> String -> [(String,J)] -> V3 [J]
array3 reason key xs = case lookup key xs of
    Just (JArr ys) -> pure ys
    _ -> v3error reason (key ++ " must be an array")

-- Parse number tokens without a floating point intermediate. Decimal supports
-- at most 255 fractional places; refuse values it cannot represent exactly.
exactToken :: String -> Maybe Amount
exactToken tok = do
    let (sgn, rest) = case tok of '-' : xs -> (-1, xs); _ -> (1, tok)
        (mantissa, exponentPart) = break (`elem` ("eE" :: String)) rest
        (whole, fractionPart) = break (== '.') mantissa
        fraction = case fractionPart of [] -> ""; _:xs -> xs
        digits xs = not (null xs) && all (\c -> c >= '0' && c <= '9') xs
    if not (digits whole) || (length whole > 1 && take 1 whole == "0")
        || (not (null fractionPart) && not (digits fraction)) then Nothing else pure ()
    exponentValue <- case exponentPart of
        [] -> Just 0
        _:xs -> let ys = case xs of '+':zs -> zs; _ -> xs
                in case reads ys :: [(Integer,String)] of
                    [(n,"")] | abs n <= 10000 -> Just n
                    _ -> Nothing
    let places = toInteger (length fraction) - exponentValue
        coefficient = sgn * read (whole ++ fraction)
    if places > 255 || places < -10000 then Nothing
    else Just (MoneyDecimal (Decimal (fromInteger (max 0 places))
               (coefficient * 10 ^ max 0 (-places))))

amount3 :: String -> Bool -> String -> [(String,J)] -> V3 Amount
amount3 reason positive key xs = case lookup key xs of
    Just (JNum s) -> case exactToken s of
        Just a | if positive then a > 0 else a >= 0 -> pure a
        _ -> v3error reason (key ++ " must be an exactly representable nonnegative decimal")
    _ -> v3error reason (key ++ " must be numeric")

integer3 :: String -> [(String,J)] -> V3 Integer
integer3 key xs = do
    a <- amount3 "invalid_call_params" True key xs
    let r = toRational a
    ensure "invalid_call_params" (key ++ " must be integral") (denominator r == 1)
    pure (numerator r)

exactQuotient :: Rational -> V3 Amount
exactQuotient r = go 0 (numerator r) (denominator r)
  where
    go places n d
        | d == 1 = pure (MoneyDecimal (Decimal places n))
        | places == 255 = v3error "invalid_call_params" "inexact_decimal_quotient"
        | d `mod` 2 == 0 = go (places+1) (n*5) (d `div` 2)
        | d `mod` 5 == 0 = go (places+1) (n*2) (d `div` 5)
        | otherwise = v3error "invalid_call_params" "inexact_decimal_quotient"

account3 :: String -> V3 AccountTitles
account3 s = case parseAccountTitle (T.pack s) of
    Right a | show a == s && a `elem` concreteAccountTitles -> pure a
    _ -> v3error "invalid_call_params" ("unknown account " ++ s)

roleAccount :: AccountDivision -> String -> [(String,J)] -> V3 AccountTitles
roleAccount role key xs = do
    a <- str3 "invalid_call_params" key xs >>= account3
    ensure "invalid_call_params" (key ++ " account role")
        (fmap asemDivisionSemantics (accountSemantics a) == Just (StatementDivision role))
    pure a

parseCall3 :: Int -> J -> V3 Call
parseCall3 idx value = do
    xs <- object3 "invalid_call_params" value
    keys3 "invalid_call_params" ["name","params"] ["name","params","txid"] xs
    name <- str3 "invalid_call_params" "name" xs
    txid <- case lookup "txid" xs of Nothing -> pure Nothing; _ -> Just <$> str3 "invalid_call_params" "txid" xs
    ps <- maybe (v3error "invalid_call_params" "missing params") (object3 "invalid_call_params") (lookup "params" xs)
    body <- parseBody name ps
    pure (Call idx name txid body)
  where
    parseBody name ps =
      let keys ks = keys3 "invalid_call_params" ks ks ps
          a = amount3 "invalid_call_params" False
          pos = amount3 "invalid_call_params" True
          one ctor key positive = keys [key] >> ctor <$> (if positive then pos else a) key ps
          two ctor x y = keys [x,y] >> ctor <$> a x ps <*> a y ps
          role ctor key division = keys ["amount",key] >> ctor <$> pos "amount" ps <*> roleAccount division key ps
      in case name of
        "cogsAdjustmentEntries" -> two Cogs "beginningInventory" "endingInventory"
        "depreciationIndirectEntry" -> one DepIndirect "amount" True
        "depreciationDirectEntry" -> role DepDirect "asset" Assets
        "allowanceReplenishmentEntry"
            | lookup "rate_basis_points" ps /= Nothing -> do
                keys ["rate_basis_points"]
                rate <- a "rate_basis_points" ps
                ensure "invalid_call_params" "rate exceeds 10000" (rate <= 10000)
                pure (AllowanceRate rate)
            | otherwise -> two Allowance "estimate" "current"
        "allowanceResetEntries" -> two AllowanceReset "estimate" "current"
        "prepaidExpenseEntry"
            | lookup "payment_total" ps /= Nothing -> do
                keys ["payment_total","coverage_months","next_period_months","expenseAccount"]
                total <- pos "payment_total" ps
                coverage <- integer3 "coverage_months" ps
                months <- integer3 "next_period_months" ps
                ensure "invalid_call_params" "months exceed coverage" (months <= coverage)
                amount <- exactQuotient (toRational total * fromInteger months / fromInteger coverage)
                Prepaid amount <$> roleAccount Cost "expenseAccount" ps
            | otherwise -> role Prepaid "expenseAccount" Cost
        "unearnedRevenueEntry" -> role Unearned "revenueAccount" Revenue
        "accruedRevenueEntry" -> role AccruedRevenueCall "revenueAccount" Revenue
        "accruedExpenseEntry"
            | lookup "principal" ps /= Nothing -> do
                keys ["principal","annual_rate_basis_points","accrued_months","months_per_year","expenseAccount"]
                principal <- pos "principal" ps
                rate <- a "annual_rate_basis_points" ps
                months <- integer3 "accrued_months" ps
                year <- integer3 "months_per_year" ps
                ensure "invalid_call_params" "months exceed year" (months <= year)
                amount <- exactQuotient (toRational principal * toRational rate * fromInteger months / (10000 * fromInteger year))
                AccruedExpenseCall amount <$> roleAccount Cost "expenseAccount" ps
            | otherwise -> role AccruedExpenseCall "expenseAccount" Cost
        "reversingEntry" -> keys ["sourceTxid"] >> ReverseEntry <$> str3 "invalid_call_params" "sourceTxid" ps
        "consumptionTaxSettlementEntry" -> do
            keys ["paid","received"]
            paid <- a "paid" ps; received <- a "received" ps
            ensure "invalid_call_params" "received below paid" (received >= paid)
            pure (ConsumptionTax paid received)
        "corporateTaxInterimEntry" -> one CorporateInterim "amount" True
        "corporateTaxSettlementEntries" -> do
            keys ["total","interim"]
            total <- a "total" ps; interim <- a "interim" ps
            ensure "invalid_call_params" "interim exceeds total" (interim <= total)
            pure (CorporateSettlement total interim)
        "equityMethodEarningsEntry" -> one EquityEarnings "share" False
        "equityMethodDividendEntry" -> one EquityDividend "dividend" False
        "equityMethodEntries" -> two EquityEntries "share" "dividend"
        "equityMethodBalance" -> keys [] >> pure EquityBalance
        "priorPeriodErrorCorrection" -> do
            keys ["current","prior","expenseAccount","assetAccount"]
            PriorError <$> a "current" ps <*> a "prior" ps <*> roleAccount Cost "expenseAccount" ps <*> roleAccount Assets "assetAccount" ps
        "finalStockTransfer" -> keys [] >> pure FinalStock
        "straightLineDepreciation" -> do
            keys3 "invalid_call_params" ["asset","cost","salvage","years","period"] ["asset","cost","salvage","years","period","method"] ps
            asset <- roleAccount Assets "asset" ps
            cost <- pos "cost" ps; salvage <- a "salvage" ps
            years <- integer3 "years" ps; period <- integer3 "period" ps
            ensure "invalid_call_params" "salvage or period exceeds bound" (salvage <= cost && period <= years)
            method <- case lookup "method" ps of Nothing -> pure "indirect"; _ -> str3 "invalid_call_params" "method" ps
            ensure "invalid_call_params" "unknown method" (method `elem` ["indirect","direct"])
            annual <- exactQuotient ((toRational cost - toRational salvage) / fromInteger years)
            pure (StraightLine asset annual (method == "direct"))
        "consolidateInternalTransactions" -> do
            keys ["entities","eliminationTxids"]
            entities <- array3 "invalid_call_params" "entities" ps >>= mapM entity
            eliminations <- txids "eliminationTxids" ps
            ensure "invalid_call_params" "at least two distinct entities required"
                (length entities >= 2 && length (nub (map fst entities)) == length entities)
            pure (Consolidate entities eliminations)
        _ -> v3error "unknown_catalog_call" name
    txids key ps = do
        values <- array3 "invalid_call_params" key ps
        ids <- mapM (\v -> case v of JStr s | not (null s) -> pure s; _ -> v3error "invalid_call_params" key) values
        ensure "invalid_call_params" "empty or repeated txid references" (not (null ids) && nub ids == ids)
        pure ids
    entity value = do
        ps <- object3 "invalid_call_params" value
        keys3 "invalid_call_params" ["entity","txids"] ["entity","txids"] ps
        (,) <$> str3 "invalid_call_params" "entity" ps <*> txids "txids" ps

stage3 :: CatalogCall -> Int
stage3 c = case c of
    CorporateInterim _ -> 0
    EquityDividend _ -> 0
    ReverseEntry _ -> 0
    EquityEarnings _ -> 2
    EquityEntries _ _ -> 2
    Consolidate _ _ -> 2
    FinalStock -> 3
    EquityBalance -> 4
    _ -> 1

context3 :: Int -> ProcessingContext
context3 0 = OrdinaryJournal
context3 1 = ClosingProcess
context3 2 = ConsolidationWorksheet
context3 _ = EngineComputation

references3 :: CatalogCall -> [(String, Int)]
references3 (ReverseEntry tid) = [(tid,0)]
references3 (Consolidate es ids) = [(t,0) | (_,ts) <- es, t <- ts] ++ [(t,2) | t <- ids]
references3 _ = []

protected3 :: AccountTitles -> Bool
protected3 a = a `elem` [RetainedEarnings, EarnedSurplus, LegalRetainedEarnings, GeneralReserve]
    || case accountSemantics a of
        Just sem -> asemPostingCapability sem `elem` [EngineGeneratedOnly,NotPostable]
                 || any (`elem` asemRoles sem) [ClosingDevice, PeriodResult, ReportingSubtotal]
                 || "Translation" `isInfixOf` show a
        Nothing -> True

parsePosting3 :: Int -> J -> V3 Posting
parsePosting3 idx value = do
    xs <- object3 "malformed_posting" value
    keys3 "forbidden_field" ["txid","side","account","amount"] ["txid","side","account","amount"] xs
    tid <- str3 "missing_txid" "txid" xs
    side <- str3 "malformed_posting" "side" xs
    ensure "malformed_posting" "invalid side" (side `elem` ["debit","credit"])
    account <- str3 "malformed_posting" "account" xs
    _ <- account3 account
    amount <- amount3 "malformed_posting" True "amount" xs
    idx `seq` pure (Posting tid side account amount)

checkGroup3 :: ProcessingContext -> (String,[Posting]) -> V3 (String,MinEntry)
checkGroup3 context (tid, ps) = case checkedEntryTextIn context
    [(T.pack (pSide p), T.pack (pAccount p), pAmount p) | p <- ps] of
        Right alg -> pure (tid,alg)
        Left errors -> v3error (entryErrorName (NE.head errors)) (tid ++ " " ++ show errors)

-- Declared output sets, independent of the actual builder result.
allowed3 :: CatalogCall -> MinEntry -> [AccountTitles]
allowed3 c ledger = case c of
    Cogs _ _ -> [Purchases,MerchandiseInventory]
    DepIndirect _ -> [Depreciation,AccumulatedDepreciation]
    DepDirect _ a -> [Depreciation,a]
    Allowance _ _ -> allowanceAccounts
    AllowanceRate _ -> allowanceAccounts
    AllowanceReset _ _ -> allowanceAccounts
    Prepaid _ a -> [PrepaidExpenses,a]
    Unearned _ a -> [UnearnedRevenue,a]
    AccruedRevenueCall _ a -> [AccruedRevenue,a]
    AccruedExpenseCall _ a -> [AccruedExpenses,a]
    ReverseEntry _ -> map (getAccountTitle . _hatBase) (toList ledger)
    ConsumptionTax _ _ -> [ConsumptionTaxPaid,ConsumptionTaxReceived,AccruedConsumptionTax]
    CorporateInterim _ -> [PrepaidCorporateIncomeTaxes,Cash]
    CorporateSettlement _ _ -> [CorporateIncomeTaxes,PrepaidCorporateIncomeTaxes,AccruedCorporateIncomeTaxes]
    EquityEarnings _ -> [InvestmentInAssociate,EquityInEarningsOfInvestee]
    EquityDividend _ -> [Cash,InvestmentInAssociate]
    EquityEntries _ _ -> [Cash,InvestmentInAssociate,EquityInEarningsOfInvestee]
    EquityBalance -> []
    PriorError _ _ e a -> [RetainedEarnings,e,a]
    FinalStock -> RetainedEarnings : [a | a <- concreteAccountTitles,
        Just spec <- [accountSpec a], asClosing spec == CloseByDivision,
        asDivision spec `elem` [Cost,Revenue]]
    StraightLine a _ direct -> [Depreciation,if direct then a else AccumulatedDepreciation]
    Consolidate _ _ -> []
  where allowanceAccounts = [AllowanceForDoubtfulAccounts,ProvisionForDoubtfulAccounts,ReversalOfAllowanceForDoubtfulAccounts]

execute3 :: [(String,MinEntry)] -> MinEntry -> CatalogCall -> V3 (MinEntry, Maybe Amount)
execute3 entries ledger c = case c of
    Cogs a b -> generated (BK.cogsAdjustmentEntries mk a b)
    DepIndirect a -> generated (BK.depreciationIndirectEntry mk a)
    DepDirect a b -> generated (BK.depreciationDirectEntry mk a b)
    Allowance a b -> generated (BK.allowanceReplenishmentEntry mk a b)
    AllowanceRate rate -> do
        receivables <- balance AccountsReceivable
        current <- balance AllowanceForDoubtfulAccounts
        estimate <- exactQuotient (toRational receivables * toRational rate / 10000)
        generated (BK.allowanceReplenishmentEntry mk estimate current)
    AllowanceReset a b -> generated (BK.allowanceResetEntries mk a b)
    Prepaid a b -> generated (BK.prepaidExpenseEntry mk a b)
    Unearned a b -> generated (BK.unearnedRevenueEntry mk a b)
    AccruedRevenueCall a b -> generated (BK.accruedRevenueEntry mk a b)
    AccruedExpenseCall a b -> generated (BK.accruedExpenseEntry mk a b)
    ReverseEntry tid -> maybe (v3error "unresolved_txid_reference" tid) (generated . BK.reversingEntry) (lookup tid entries)
    ConsumptionTax a b -> generated (BK.consumptionTaxSettlementEntry mk a b)
    CorporateInterim a -> generated (BK.corporateTaxInterimEntry mk a)
    CorporateSettlement a b -> generated (BK.corporateTaxSettlementEntries mk a b)
    EquityEarnings a -> generated (BK.equityMethodEarningsEntry mk a)
    EquityDividend a -> generated (BK.equityMethodDividendEntry mk a)
    EquityEntries a b -> generated (BK.equityMethodEntries mk a b)
    EquityBalance -> pure (mempty,Just (BK.equityMethodBalance ledger))
    PriorError a b e asset -> generated (BK.priorPeriodErrorCorrection mk a b e asset)
    FinalStock -> generated (bar (EAT.finalStockTransfer ledger .+ BK.reversingEntry (bar ledger)))
    StraightLine asset amount direct -> generated (if direct then BK.depreciationDirectEntry mk amount asset else BK.depreciationIndirectEntry mk amount)
    Consolidate es ids -> do
        -- All references and every worksheet txid have already been certified.
        let selected = foldl' (.+) mempty [a | (t,a) <- entries, t `elem` (concatMap snd es ++ ids)]
            consolidated = bar selected
        ensure "Imbalanced" "consolidation recipe" (norm (decL consolidated) == norm (decR consolidated))
        -- Source and elimination postings already occur once in the journal.
        pure (mempty,Nothing)
  where
    mk = (:<)
    balance account = do
        let net = bar (projByAccountTitle account ledger)
        ensure "invalid_call_params" ("abnormal balance " ++ show account)
            (all ((== whichSide (Not :< account)) . whichSide . _hatBase) (toList net))
        pure (norm net)
    generated a = pure (a,Nothing)

signature3 :: MinEntry -> [(String,String,Amount)]
signature3 a = sort [(if whichSide (_hatBase x) == Debit then "debit" else "credit",
                    show (getAccountTitle (_hatBase x)), _val x) | x <- toList a]

runV3 :: J -> V3 String
runV3 value = do
    xs <- object3 "malformed_input" value
    keys3 "forbidden_field" ["postings","calls"] ["postings","calls","sources","opening","task","decision","findings","conditional"] xs
    postings <- array3 "malformed_posting" "postings" xs >>= sequence . zipWith parsePosting3 [0..]
    calls <- array3 "invalid_call_params" "calls" xs >>= sequence . zipWith parseCall3 [0..]
    extras <- validateExtras3 xs
    ensure "malformed_input" "empty answer" (not (null postings && null calls) || any nonemptyAnswer extras)
    (opening, openingId) <- case lookup "opening" xs of
        Nothing -> pure ([],Nothing)
        Just v -> do
            os <- object3 "malformed_input" v
            keys3 "forbidden_field" ["txid","rows"] ["txid","rows"] os
            tid <- str3 "malformed_input" "txid" os
            rows <- array3 "malformed_input" "rows" os
            ensure "malformed_input" "empty opening" (not (null rows))
            ps <- sequence [do rs <- object3 "malformed_posting" row
                               keys3 "forbidden_field" ["side","account","amount"] ["side","account","amount"] rs
                               parsePosting3 i (JObj (("txid",JStr tid):rs)) | (i,row) <- zip [0..] rows]
            checked <- checkGroup3 EngineComputation (tid,ps)
            pure ([checked],Just tid)
    ensure "opening_preloaded_by_harness" "opening_preloaded_by_harness" (all (\p -> Just (pTxid p) /= openingId) postings)
    task <- case lookup "task" xs of
        Nothing -> pure []
        Just v -> do
            ts <- object3 "malformed_input" v
            keys3 "forbidden_field" ["category"] ["category","closing_txid","ordinary_txids"] ts
            cat <- str3 "malformed_input" "category" ts
            ensure "malformed_input" "unknown category" (cat `elem` ["journalize","closing","statements","consolidation"])
            case lookup "closing_txid" ts of Nothing -> pure (); _ -> str3 "malformed_input" "closing_txid" ts >> pure ()
            case lookup "ordinary_txids" ts of
                Nothing -> pure ()
                Just (JArr ids) -> do
                    ensure "malformed_input" "invalid ordinary txid list"
                        (nub ids == ids && all (\v -> case v of JStr tid -> not (null tid); _ -> False) ids)
                _ -> v3error "malformed_input" "ordinary_txids must be an array"
            pure ts
    let groups = groupPostings postings
        rawIds = map fst groups
        refs = concatMap (references3 . callBody) calls
        stages = map (stage3 . callBody) calls
        taskStage = if lookup "category" task == Just (JStr "closing") then 1 else 0
        ordinaryIds = case lookup "ordinary_txids" task of
            Just (JArr ids) -> [tid | JStr tid <- ids]
            _ -> []
        rawStage tid = maybe (if tid `elem` ordinaryIds then 0 else taskStage) id (lookup tid refs)
        effectiveId call = case (callBody call,lookup "closing_txid" task) of
            (FinalStock,Just (JStr tid)) -> tid
            _ -> maybe ("call:" ++ show (callIndex call) ++ ":" ++ callName call) id (callTxid call)
    ensure "call_order" "stages must be monotone" (and (zipWith (<=) stages (drop 1 stages)))
    ensure "unresolved_txid_reference" "reference not in raw postings" (all ((`elem` rawIds) . fst) refs)
    ensure "invalid_call_params" "incompatible reference reuse" (length (nub (map fst refs)) == length refs)
    ensure "invalid_call_params" "repeated generated txid" (let ids = map effectiveId calls in nub ids == ids)
    ensure "opening_preloaded_by_harness" "call txid collides with opening" (all ((/= openingId) . Just . effectiveId) calls)
    mapM_ (\call -> case (callBody call,lookup "closing_txid" task,callTxid call) of
        (FinalStock,Just (JStr tid),Just supplied) -> ensure "closing_txid_mismatch" supplied (tid == supplied)
        _ -> pure ()) calls
    case lookup "closing_txid" task of
        Just (JStr tid) -> ensure "direct_posting_forbidden" "closing txid is reserved for finalStockTransfer" (tid `notElem` rawIds)
        _ -> pure ()
    mapM_ (\(_,ps) -> do
        accounts <- mapM (account3 . pAccount) ps
        let divisions = [asDivision spec | a <- accounts, Just spec <- [accountSpec a]]
        ensure "direct_posting_forbidden" "raw P/L-to-equity transfer"
            (not (Equity `elem` divisions && any (`elem` divisions) [Cost,Revenue]))) groups
    mapM_ (\p -> do a <- account3 (pAccount p)
                    ensure "direct_posting_forbidden" (pTxid p ++ " " ++ pAccount p) (not (protected3 a))) postings
    raw <- mapM (\g -> checkGroup3 (context3 (rawStage (fst g))) g) groups
    sources <- case lookup "sources" xs of
        Nothing -> pure []
        Just (JArr ss) -> mapM parseSource3 ss
        _ -> v3error "malformed_source" "sources must be an array"
    ensure "malformed_source" "duplicate source id" (length (nub (map fst sources)) == length sources)
    (entries, provenance) <- foldM (step raw rawStage effectiveId) (opening ++ raw,[]) calls
    let sourceErrors = if null sources then [] else reconcileSources sources
            (buildJournal [(tid,a) | (tid,a) <- entries, tid `elem` map fst sources])
    case sourceErrors of e:_ -> v3error (sourceErrorName e) (show sourceErrors); [] -> pure ()
    pure (jobj ([ ("ok","true"), ("journal",postingsWithTxidsJSON entries),
                  ("provenance",jarr provenance)] ++ [(k,render3 v) | (k,v) <- extras]))
  where
    parseSource3 v = do
        ps <- object3 "malformed_source" v
        keys3 "forbidden_field" ["id","amount"] ["id","amount"] ps
        (,) <$> str3 "malformed_source" "id" ps <*> amount3 "malformed_source" True "amount" ps
    nonemptyAnswer ("conditional",_) = True
    nonemptyAnswer (_,JObj ys) = not (null ys)
    nonemptyAnswer (_,JArr ys) = not (null ys)
    nonemptyAnswer _ = False
    step raw rawStage effectiveId (entries,provenance) call = do
        let body = callBody call
            ledger = foldl' (.+) mempty (map snd entries)
        (delta,projection) <- execute3 raw ledger body
        let allowed = allowed3 body (case body of ReverseEntry tid -> maybe mempty id (lookup tid raw); _ -> ledger)
        ensure "output_account_not_allowed" (callName call)
            (all ((`elem` allowed) . getAccountTitle . _hatBase) (toList delta))
        let rows = signature3 delta
        if null rows then pure () else do
            _ <- checkGroup3 (context3 (stage3 body)) (effectiveId call,[Posting (effectiveId call) s a v | (s,a,v) <- rows])
            mapM_ (\(tid,alg) -> if rawStage tid /= stage3 body then pure () else do
                let rs = signature3 alg
                    overlap = intersect rs rows
                ensure "duplicate_effect" tid (rs /= rows)
                ensure "possible_duplicate_effect" tid (null overlap)) raw
        ensure "invalid_call_params" "generated txid collides with raw" (effectiveId call `notElem` map fst raw)
        let consumed = case body of
                FinalStock -> map fst entries
                EquityBalance -> map fst entries
                AllowanceRate _ -> map fst entries
                _ -> map fst (references3 body)
            entityProvenance = case body of
                Consolidate es _ -> [("source_entities",jarr [jobj
                    [("entity",jsonString3 entity),("txids",jarr (map jsonString3 tids))]
                    | (entity,tids) <- es])]
                _ -> []
            prov = jobj ([ ("index",show (callIndex call)), ("name",jsonString3 (callName call)),
                          ("txid",jsonString3 (effectiveId call)), ("stage",show (stage3 body)),
                          ("context",jsonString3 (show (context3 (stage3 body)))),
                          ("consumed_txids",jarr (map jsonString3 consumed))] ++ entityProvenance ++
                          maybe [] (\a -> [("projection",show (toDecimal a))]) projection)
        pure (entries ++ [(effectiveId call,delta) | not (null rows)],provenance ++ [prov])

validateExtras3 :: [(String,J)] -> V3 [(String,J)]
validateExtras3 xs = mapM validate [(k,v) | (k,v) <- xs, k `elem` ["decision","findings","conditional"]]
  where
    validate pair@("decision",value) = do
        fields <- object3 "forbidden_field" value
        ensure "forbidden_field" "decision values must be strings" (all (\(_,v) -> case v of JStr _ -> True; _ -> False) fields)
        pure pair
    validate pair@("findings",JArr values) = mapM_ (strings ["type","locus","detail"]) values >> pure pair
    validate pair@("conditional",value) = strings ["condition","ifTrue","otherwise"] value >> pure pair
    validate _ = v3error "forbidden_field" "invalid nonposting component"
    strings keys value = do
        fields <- object3 "forbidden_field" value
        keys3 "forbidden_field" keys keys fields
        mapM_ (\key -> str3 "forbidden_field" key fields >> pure ()) keys

render3 :: J -> String
render3 (JStr s) = jsonString3 s
render3 (JNum s) = s
render3 (JBool b) = if b then "true" else "false"
render3 JNull = "null"
render3 (JArr vs) = "[" ++ intercalate "," (map render3 vs) ++ "]"
render3 (JObj xs) = "{" ++ intercalate "," [jsonString3 k ++ ":" ++ render3 v | (k,v) <- xs] ++ "}"

jsonString3 :: String -> String
jsonString3 s = '"' : concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c | ord c < 32 = "\\u" ++ replicate (4-length h) '0' ++ h where h = showHex (ord c) ""
    escape c = [c]

-- A separate strict JSON grammar keeps v2's permissive parser byte compatible.
parseJSON3 :: String -> Maybe J
parseJSON3 input = case RP.readP_to_S (ws *> val <* ws <* RP.eof) input of
    (value,""):_ -> Just value
    _ -> Nothing
  where
    ws = () <$ RP.munch (`elem` (" \n\r\t" :: String))
    lexeme p = p <* ws
    str = RP.between (RP.char '"') (RP.char '"') (concat <$> RP.many char)
    char = ((:[]) <$> RP.satisfy (\c -> c /= '"' && c /= '\\' && ord c >= 32)) RP.<++ do
        _ <- RP.char '\\'
        c <- RP.get
        case lookup c [('"','"'),('\\','\\'),('/','/'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t')] of
            Just decoded -> pure [decoded]
            Nothing | c == 'u' -> do
                n <- hex4
                if n >= 0xd800 && n <= 0xdbff then do
                    _ <- RP.string "\\u"
                    low <- hex4
                    if low >= 0xdc00 && low <= 0xdfff then pure [chr (0x10000 + (n-0xd800)*1024 + low-0xdc00)] else RP.pfail
                else if n >= 0xdc00 && n <= 0xdfff then RP.pfail else pure [chr n]
            _ -> RP.pfail
    hex4 = do
        digits <- sequence (replicate 4 (RP.satisfy isHexDigit))
        pure (foldl' (\n c -> n*16 + digitToInt c) 0 digits)
    val = lexeme $ (JStr <$> str) RP.<++
        RP.between (RP.char '[' *> ws) (RP.char ']') (JArr <$> RP.sepBy val (RP.char ',' *> ws)) RP.<++
        RP.between (RP.char '{' *> ws) (RP.char '}') (JObj <$> RP.sepBy pair (RP.char ',' *> ws)) RP.<++
        (RP.string "true" >> pure (JBool True)) RP.<++ (RP.string "false" >> pure (JBool False)) RP.<++
        (RP.string "null" >> pure JNull) RP.<++ do
            tok <- RP.munch1 (`elem` ("-+0123456789.eE" :: String))
            if validJSONNumber3 tok then pure (JNum tok) else RP.pfail
    pair = do k <- lexeme str; _ <- RP.char ':' *> ws; v <- val; pure (k,v)

-- The v3 encoder also escapes every JSON control character in diagnostics.
-- Keep failureVerdict itself untouched for frozen v2 replay.
failureVerdict3 :: InputError -> String
failureVerdict3 err = jobj
    [ ("ok","false"), ("entry_errors","[]"), ("source_errors","[]")
    , ("input_errors",jarr [jsonString3 (ieMessage err)])
    , ("raw",jsonString3 (rawSummary [err] [] []))
    , ("rich",jsonString3 (richSummary [err] [] [])) ]

-- Syntax validation is independent of MoneyDecimal's representable range.
-- A valid but unrepresentable token reaches amount3 and is a data rejection.
validJSONNumber3 :: String -> Bool
validJSONNumber3 token = case RP.readP_to_S (number <* RP.eof) token of
    [] -> False
    _ -> True
  where
    digit = RP.satisfy (\c -> c >= '0' && c <= '9')
    number = do
        _ <- RP.option "" (RP.string "-")
        _ <- RP.string "0" RP.<++ ((:) <$> RP.satisfy (\c -> c >= '1' && c <= '9') <*> RP.munch (\c -> c >= '0' && c <= '9'))
        _ <- RP.option "" (RP.char '.' *> RP.many1 digit)
        _ <- RP.option "" (do
            _ <- RP.satisfy (`elem` ("eE" :: String))
            sign <- RP.option "" ((:[]) <$> RP.satisfy (`elem` ("+-" :: String)))
            ds <- RP.many1 digit
            pure (sign ++ ds))
        pure ()
