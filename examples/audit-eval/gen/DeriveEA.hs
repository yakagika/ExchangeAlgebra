{- |
  DeriveEA.hs — EA-backed derived-value oracle for generated clean journals.

  Reads a canonical posting array
      [{"side":"debit","account":"Cash","amount":1000,"entry":"e1"}, ...]
  from stdin, or from the first CLI argument, builds a checked EA Journal, and
  emits {"derived":{...}} using the same flat key vocabulary as gen.pandas_oracle.

  This script is intentionally for clean journals only. Defective audit journals
  may contain hallucinated accounts, which the EA type layer must reject.
-}

import           Data.Char (isDigit, isSpace)
import           Data.Decimal (Decimal)
import           Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           ExchangeAlgebra hiding (filter, map)
import           ExchangeAlgebra.Convert (parseAccountTitle, parseSide)
import           ExchangeAlgebra.Convert.Checked (checkedJournal)
import qualified ExchangeAlgebra.Journal as EJ

------------------------------------------------------------------
-- Minimal JSON parser (canonical subset)
------------------------------------------------------------------

data J = JStr String | JNum Integer | JArr [J] | JObj [(String, J)]
       | JBool Bool | JNull
       deriving (Show)

type P a = String -> Maybe (a, String)

skipWs :: String -> String
skipWs = dropWhile isSpace

pValue :: P J
pValue s = case skipWs s of
    ('"':rest) -> pStringBody rest >>= \(str, r) -> Just (JStr str, r)
    ('[':rest) -> pArray rest
    ('{':rest) -> pObject rest
    ('t':_)    -> pLit "true" (JBool True) (skipWs s)
    ('f':_)    -> pLit "false" (JBool False) (skipWs s)
    ('n':_)    -> pLit "null" JNull (skipWs s)
    rest       -> pNumber rest

pLit :: String -> J -> P J
pLit lit v s | take (length lit) s == lit = Just (v, drop (length lit) s)
             | otherwise                  = Nothing

pStringBody :: P String
pStringBody s = go s ""
  where
    go ('\\':c:rest) acc = go rest (unesc c : acc)
    go ('"':rest)    acc = Just (reverse acc, rest)
    go (c:rest)      acc = go rest (c : acc)
    go []            _   = Nothing
    unesc 'n' = '\n'
    unesc 't' = '\t'
    unesc c   = c

pNumber :: P J
pNumber s =
    let (sign, rest0) = case s of
            ('-':rest) -> ("-", rest)
            _          -> ("", s)
        (digits, rest) = span isDigit rest0
    in if null digits
       then Nothing
       else Just (JNum (read (sign ++ digits)), rest)

pArray :: P J
pArray s = case skipWs s of
    (']':rest) -> Just (JArr [], rest)
    _          -> go s []
  where
    go s' acc = do
        (v, r1) <- pValue s'
        case skipWs r1 of
            (',':r2) -> go r2 (v : acc)
            (']':r2) -> Just (JArr (reverse (v : acc)), r2)
            _        -> Nothing

pObject :: P J
pObject s = case skipWs s of
    ('}':rest) -> Just (JObj [], rest)
    _          -> go s []
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
                        _        -> Nothing
                _ -> Nothing
        _ -> Nothing

parseJSON :: String -> Maybe J
parseJSON s = case pValue s of
    Just (v, rest) | all isSpace rest -> Just v
    _                                 -> Nothing

------------------------------------------------------------------
-- Posting extraction and checked journal construction
------------------------------------------------------------------

data Posting = Posting
    { pSide    :: String
    , pAccount :: String
    , pAmount  :: Integer
    , pEntry   :: Maybe String
    } deriving (Show)

fromJ :: J -> Maybe [Posting]
fromJ (JArr items) = mapM go items
  where
    go (JObj kvs) = do
        JStr side <- lookup "side" kvs
        JStr acct <- lookup "account" kvs
        JNum amt  <- lookup "amount" kvs
        let entry = case lookup "entry" kvs of
                Just (JStr e) -> Just e
                _             -> Nothing
        Just (Posting side acct amt entry)
    go _ = Nothing
fromJ _ = Nothing

type MinBase = HatBase AccountTitles
type MinJournal = EJ.Journal String MoneyDecimal MinBase

groupPostings :: [Posting] -> [(String, [Posting])]
groupPostings postings = foldl add [] postings
  where
    anyEntry = any (maybe False (const True) . pEntry) postings
    key p
        | anyEntry  = maybe "__missing_entry__" id (pEntry p)
        | otherwise = "entry"

    add [] p = [(key p, [p])]
    add ((k, ps):rest) p
        | k == key p  = (k, ps ++ [p]) : rest
        | otherwise   = (k, ps) : add rest p

parsePosting :: Posting -> Either String (Side, AccountTitles, MoneyDecimal)
parsePosting p = do
    side <- either (Left . show) Right (parseSide (T.pack (pSide p)))
    acct <- either (Left . show) Right (parseAccountTitle (T.pack (pAccount p)))
    pure (side, acct, fromInteger (pAmount p))

buildJournal :: [Posting] -> Either String MinJournal
buildJournal postings = do
    entries <- mapM parseGroup (groupPostings postings)
    case checkedJournal entries of
        Left err      -> Left (show err)
        Right journal -> Right journal
  where
    parseGroup (entryId, rows) = do
        parsed <- mapM parsePosting rows
        pure (entryId, parsed)

------------------------------------------------------------------
-- Derived values
------------------------------------------------------------------

type Totals = (Integer, Integer)

amountInteger :: MoneyDecimal -> Integer
amountInteger v = truncate (toDecimal v :: Decimal)

addPosting :: M.Map AccountTitles Totals -> Alg MoneyDecimal MinBase -> M.Map AccountTitles Totals
addPosting acc x =
    let b = _hatBase x
        title = getAccountTitle b
        amount = amountInteger (_val x)
        delta = if whichSide b == Debit then (amount, 0) else (0, amount)
    in M.insertWith combine title delta acc
  where
    combine (d1, c1) (d2, c2) = (d1 + d2, c1 + c2)

totalsByAccount :: MinJournal -> M.Map AccountTitles Totals
totalsByAccount journal = foldl addPosting M.empty (toList (EJ.toAlg journal))

normalBalance :: AccountTitles -> Totals -> Integer
normalBalance title (debits, credits)
    | whichSide (Not :< title) == Debit = debits - credits
    | otherwise                         = credits - debits

trialBalance :: Totals -> Integer
trialBalance (debits, credits) = debits - credits

-- Contra accounts (classifyAccountContra) contribute negatively to their
-- division's total: total_assets = gross assets - allowance - accumulated
-- depreciation. Mirrors gen/pandas_oracle.py `sum_category`.
sumDivision :: AccountDivision -> M.Map AccountTitles Totals -> Integer
sumDivision division totals =
    sum
        [ sign * normalBalance title dc
        | (title, dc) <- M.toList totals
        , classifyAccountDivision title == division
        , let sign = if classifyAccountContra title then -1 else 1
        ]

derivedPairs :: MinJournal -> [(String, Integer)]
derivedPairs journal =
    ledgerPairs ++ financialPairs
  where
    totals = totalsByAccount journal
    accounts = sortOn (show . fst) (M.toList totals)

    ledgerPairs = concat
        [ let name = show title
              balance = normalBalance title dc
              (debits, credits) = dc
          in [ ("ledger." ++ name ++ ".debits", debits)
             , ("ledger." ++ name ++ ".credits", credits)
             , ("ledger." ++ name ++ ".balance", balance)
             , ("trial_balance." ++ name, trialBalance dc)
             ]
        | (title, dc) <- accounts
        ]

    totalAssets = sumDivision Assets totals
    totalLiabilities = sumDivision Liability totals
    openingEquity = sumDivision Equity totals
    totalRevenue = sumDivision Revenue totals
    totalExpenses = sumDivision Cost totals
    netIncome = totalRevenue - totalExpenses
    totalEquity = openingEquity + netIncome
    balanceCheck = totalAssets - (totalLiabilities + totalEquity)

    financialPairs
        | M.null totals =
            [ ("financial_statements.total_assets", 0)
            , ("financial_statements.total_liabilities", 0)
            , ("financial_statements.total_equity", 0)
            , ("financial_statements.total_revenue", 0)
            , ("financial_statements.total_expenses", 0)
            , ("financial_statements.net_income", 0)
            , ("financial_statements.balance_check", 0)
            ]
        | otherwise =
            [ ("financial_statements.total_assets", totalAssets)
            , ("financial_statements.total_liabilities", totalLiabilities)
            , ("financial_statements.opening_equity", openingEquity)
            , ("financial_statements.total_equity", totalEquity)
            , ("financial_statements.total_revenue", totalRevenue)
            , ("financial_statements.total_expenses", totalExpenses)
            , ("financial_statements.net_income", netIncome)
            , ("financial_statements.balance_check", balanceCheck)
            ]

------------------------------------------------------------------
-- JSON rendering
------------------------------------------------------------------

jstr :: String -> String
jstr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc c    = [c]

renderDerived :: [(String, Integer)] -> String
renderDerived pairs =
    "{\"derived\":{"
    ++ intercalate "," [jstr key ++ ":" ++ show value | (key, value) <- pairs]
    ++ "}}"

------------------------------------------------------------------
-- Main
------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        (path:_) -> readFile path
        []       -> getContents

    case parseJSON input >>= fromJ of
        Nothing -> do
            hPutStrLn stderr "DeriveEA: input is not a canonical posting array"
            exitFailure
        Just postings ->
            case buildJournal postings of
                Left err -> do
                    hPutStrLn stderr ("DeriveEA: " ++ err)
                    exitFailure
                Right journal ->
                    putStrLn (renderDerived (derivedPairs journal))
