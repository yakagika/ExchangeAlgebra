{- |
  DeriveEA.hs — EA-backed derived-value oracle for generated tasks.

  Backward-compatible input is a canonical posting array. New task kinds use
  an object with mode "closing" or "consolidation". Closing adjustments are
  built with ExchangeAlgebra.Bookkeeping and closing balances are produced by
  ExchangeAlgebra.Algebra.Transfer. Consolidation keeps entity on the Journal
  note axis and derives eliminations with bar.
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
import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Algebra.Transfer as EAT
import qualified ExchangeAlgebra.Bookkeeping as BK
import           ExchangeAlgebra.Convert (parseAccountTitle, parseSide)
import           ExchangeAlgebra.Convert.Checked (checkedJournal)
import qualified ExchangeAlgebra.Journal as EJ

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

field :: String -> [(String, J)] -> Maybe J
field = lookup

integerField :: String -> [(String, J)] -> Maybe Integer
integerField key obj = do
    JNum value <- field key obj
    pure value

data Posting = Posting
    { pSide    :: String
    , pAccount :: String
    , pAmount  :: Integer
    , pEntry   :: Maybe String
    , pEntity  :: Maybe String
    } deriving (Show)

postingsFromJ :: J -> Maybe [Posting]
postingsFromJ (JArr items) = mapM go items
  where
    go (JObj kvs) = do
        JStr side <- lookup "side" kvs
        JStr acct <- lookup "account" kvs
        JNum amt  <- lookup "amount" kvs
        let stringAt key = case lookup key kvs of
                Just (JStr value) -> Just value
                _                 -> Nothing
        Just (Posting side acct amt (stringAt "entry") (stringAt "entity"))
    go _ = Nothing
postingsFromJ _ = Nothing

type MinBase = HatBase AccountTitles
type MinAlg = Alg MoneyDecimal MinBase
type MinJournal = EJ.Journal String MoneyDecimal MinBase
type EntityJournal = EJ.Journal (String, String) MoneyDecimal MinBase

groupPostingsBy :: Eq key => (Posting -> key) -> [Posting] -> [(key, [Posting])]
groupPostingsBy key = foldl add []
  where
    add [] p = [(key p, [p])]
    add ((k, ps):rest) p
        | k == key p = (k, ps ++ [p]) : rest
        | otherwise  = (k, ps) : add rest p

entryKey :: Posting -> String
entryKey = maybe "entry" id . pEntry

entityKey :: Posting -> String
entityKey = maybe "" id . pEntity

entityEntryKey :: Posting -> (String, String)
entityEntryKey posting = (entityKey posting, entryKey posting)

parsePosting :: Posting -> Either String (Side, AccountTitles, MoneyDecimal)
parsePosting p = do
    side <- either (Left . show) Right (parseSide (T.pack (pSide p)))
    acct <- either (Left . show) Right (parseAccountTitle (T.pack (pAccount p)))
    pure (side, acct, fromInteger (pAmount p))

buildJournalBy :: [Posting] -> Either String EntityJournal
buildJournalBy postings = do
    entries <- mapM parseGroup (groupPostingsBy entityEntryKey postings)
    case checkedJournal entries of
        Left err      -> Left (show err)
        Right journal -> Right journal
  where
    parseGroup (_, []) = Left "empty posting group"
    parseGroup ((entityId, entryId), rows) = do
        if null entityId
           then Left ("missing entity for entry " ++ entryId)
           else pure ()
        parsed <- mapM parsePosting rows
        pure ((entityId, entryId), parsed)

buildJournal :: [Posting] -> Either String MinJournal
buildJournal postings = do
    entries <- mapM parseGroup (groupPostingsBy entryKey postings)
    case checkedJournal entries of
        Left err      -> Left (show err)
        Right journal -> Right journal
  where
    parseGroup (entryId, rows) = do
        parsed <- mapM parsePosting rows
        pure (entryId, parsed)

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

totalsByAlg :: MinAlg -> M.Map AccountTitles Totals
totalsByAlg = foldl addPosting M.empty . toList

balancesByAlg :: MinAlg -> M.Map AccountTitles MinAlg
balancesByAlg = M.map bar . foldl addBalance M.empty . toList
  where
    addBalance acc x = M.insertWith (.+) (getAccountTitle (_hatBase x)) x acc

normalBalance :: AccountTitles -> Totals -> Integer
normalBalance title (debits, credits)
    | whichSide (Not :< title) == Debit = debits - credits
    | otherwise                         = credits - debits

trialBalance :: Totals -> Integer
trialBalance (debits, credits) = debits - credits

balanceSideAmount :: MinAlg -> (String, Integer)
balanceSideAmount balance =
    let net = bar balance
        amount = amountInteger (norm net)
    in case toList net of
        []    -> ("zero", 0)
        (x:_) ->
            ( if whichSide (_hatBase x) == Debit then "debit" else "credit"
            , amount
            )

sumDivision :: AccountDivision -> M.Map AccountTitles Totals -> Integer
sumDivision division totals =
    sum
        [ sign * normalBalance title dc
        | (title, dc) <- M.toList totals
        , classifyAccountDivision title == division
        , let sign = if classifyAccountContra title then -1 else 1
        ]

data DerivedValue = DerivedNum Integer | DerivedString String

ledgerPairsFor :: M.Map AccountTitles Totals -> M.Map AccountTitles MinAlg
               -> [(String, DerivedValue)]
ledgerPairsFor totals balances = concat
    [ let name = show title
          balance = normalBalance title dc
          (debits, credits) = dc
          (actualSide, actualAmount) = balanceSideAmount (balances M.! title)
      in [ ("ledger." ++ name ++ ".debits", DerivedNum debits)
         , ("ledger." ++ name ++ ".credits", DerivedNum credits)
         , ("ledger." ++ name ++ ".balance", DerivedNum balance)
         , ("ledger." ++ name ++ ".balance_side", DerivedString actualSide)
         , ("ledger." ++ name ++ ".balance_amount", DerivedNum actualAmount)
         ]
    | (title, dc) <- sortOn (show . fst) (M.toList totals)
    ]

trialPairsFor :: M.Map AccountTitles Totals -> M.Map AccountTitles MinAlg
              -> [(String, DerivedValue)]
trialPairsFor totals balances = concat
    [ let name = show title
          (actualSide, actualAmount) = balanceSideAmount (balances M.! title)
      in [ ("trial_balance." ++ name, DerivedNum (trialBalance dc))
         , ("trial_balance." ++ name ++ ".side", DerivedString actualSide)
         , ("trial_balance." ++ name ++ ".amount", DerivedNum actualAmount)
         ]
    | (title, dc) <- sortOn (show . fst) (M.toList totals)
    ]

financialPairsFor :: M.Map AccountTitles Totals -> [(String, DerivedValue)]
financialPairsFor totals
    | M.null totals =
        [ ("financial_statements.total_assets", DerivedNum 0)
        , ("financial_statements.total_liabilities", DerivedNum 0)
        , ("financial_statements.total_equity", DerivedNum 0)
        , ("financial_statements.total_revenue", DerivedNum 0)
        , ("financial_statements.total_expenses", DerivedNum 0)
        , ("financial_statements.net_income", DerivedNum 0)
        , ("financial_statements.balance_check", DerivedNum 0)
        ]
    | otherwise =
        [ ("financial_statements.total_assets", DerivedNum totalAssets)
        , ("financial_statements.total_liabilities", DerivedNum totalLiabilities)
        , ("financial_statements.opening_equity", DerivedNum openingEquity)
        , ("financial_statements.total_equity", DerivedNum totalEquity)
        , ("financial_statements.total_revenue", DerivedNum totalRevenue)
        , ("financial_statements.total_expenses", DerivedNum totalExpenses)
        , ("financial_statements.net_income", DerivedNum netIncome)
        , ("financial_statements.balance_check", DerivedNum balanceCheck)
        ]
  where
    totalAssets = sumDivision Assets totals
    totalLiabilities = sumDivision Liability totals
    openingEquity = sumDivision Equity totals
    totalRevenue = sumDivision Revenue totals
    totalExpenses = sumDivision Cost totals
    netIncome = totalRevenue - totalExpenses
    totalEquity = openingEquity + netIncome
    balanceCheck = totalAssets - (totalLiabilities + totalEquity)

derivedPairsAlg :: MinAlg -> [(String, DerivedValue)]
derivedPairsAlg alg =
    ledgerPairsFor totals balances
    ++ trialPairsFor totals balances
    ++ financialPairsFor totals
  where
    totals = totalsByAlg alg
    balances = balancesByAlg alg

financialValue :: String -> [(String, DerivedValue)] -> DerivedValue
financialValue key pairs = case lookup key pairs of
    Just value -> value
    Nothing    -> DerivedNum 0

closingDerivedPairs :: MinAlg -> MinAlg -> MinAlg -> [(String, DerivedValue)]
closingDerivedPairs adjusted fullLedger closed =
    ledgerPairsFor fullTotals fullBalances
    ++ trialPairsFor adjustedTotals adjustedBalances
    ++ [ (key, financialValue key source)
       | (key, source) <-
           [ ("financial_statements.total_assets", closedFinancial)
           , ("financial_statements.total_liabilities", closedFinancial)
           , ("financial_statements.total_equity", closedFinancial)
           , ("financial_statements.balance_check", closedFinancial)
           , ("financial_statements.opening_equity", adjustedFinancial)
           , ("financial_statements.total_revenue", adjustedFinancial)
           , ("financial_statements.total_expenses", adjustedFinancial)
           , ("financial_statements.net_income", adjustedFinancial)
           ]
       ]
  where
    fullTotals = totalsByAlg fullLedger
    fullBalances = balancesByAlg fullLedger
    adjustedTotals = totalsByAlg adjusted
    adjustedBalances = balancesByAlg adjusted
    adjustedFinancial = financialPairsFor adjustedTotals
    closedFinancial = financialPairsFor (totalsByAlg closed)

data ClosingAdjustments = ClosingAdjustments
    { caDepreciationCost          :: Integer
    , caDepreciationResidual      :: Integer
    , caDepreciationLife          :: Integer
    , caAccruedPrincipal          :: Integer
    , caAccruedRateBps            :: Integer
    , caAccruedMonths             :: Integer
    , caMonthsPerYear             :: Integer
    , caPrepaidPayment            :: Integer
    , caPrepaidCoverageMonths     :: Integer
    , caPrepaidNextPeriodMonths   :: Integer
    , caAllowanceRateBps          :: Integer
    , caBeginningInventory        :: Integer
    , caEndingInventory           :: Integer
    }

closingInput :: [(String, J)] -> Maybe ([Posting], ClosingAdjustments)
closingInput obj = do
    postingsJ <- field "postings" obj
    postings <- postingsFromJ postingsJ
    JObj adj <- field "adjustments" obj
    spec <- ClosingAdjustments
        <$> integerField "depreciation_cost" adj
        <*> integerField "depreciation_residual_value" adj
        <*> integerField "depreciation_useful_life_years" adj
        <*> integerField "accrued_expense_principal" adj
        <*> integerField "accrued_expense_annual_rate_basis_points" adj
        <*> integerField "accrued_expense_months" adj
        <*> integerField "months_per_year" adj
        <*> integerField "prepaid_payment_total" adj
        <*> integerField "prepaid_coverage_months" adj
        <*> integerField "prepaid_next_period_months" adj
        <*> integerField "allowance_rate_basis_points" adj
        <*> integerField "beginning_inventory" adj
        <*> integerField "ending_inventory" adj
    pure (postings, spec)

exactRatio :: String -> Integer -> Integer -> Either String Integer
exactRatio label numerator denominator
    | denominator <= 0 = Left (label ++ " denominator must be positive")
    | remainder /= 0 = Left (label ++ " must resolve to a whole-number amount")
    | otherwise = Right quotient
  where
    (quotient, remainder) = numerator `divMod` denominator

closingAdjustmentsAlg :: MinAlg -> ClosingAdjustments -> Either String MinAlg
closingAdjustmentsAlg base spec = do
    depreciation <- exactRatio
        "depreciation"
        (caDepreciationCost spec - caDepreciationResidual spec)
        (caDepreciationLife spec)
    accruedExpense <- exactRatio
        "accrued expense"
        (caAccruedPrincipal spec * caAccruedRateBps spec * caAccruedMonths spec)
        (10000 * caMonthsPerYear spec)
    prepaidExpense <- exactRatio
        "prepaid expense"
        (caPrepaidPayment spec * caPrepaidNextPeriodMonths spec)
        (caPrepaidCoverageMonths spec)
    allowanceEstimate <- exactRatio
        "allowance estimate"
        (receivables * caAllowanceRateBps spec)
        10000
    let allowanceDelta = allowanceEstimate - allowanceCurrent
    if allowanceDelta < 0
       then Left "allowance replenishment must be non-negative"
       else Right $
           BK.depreciationIndirectEntry mk (fromInteger depreciation)
        .+ BK.accruedExpenseEntry mk (fromInteger accruedExpense) InterestExpense
        .+ BK.prepaidExpenseEntry mk (fromInteger prepaidExpense) RentExpense
        .+ BK.allowanceReplenishmentEntry mk (fromInteger allowanceEstimate) (fromInteger allowanceCurrent)
        .+ BK.cogsAdjustmentEntries mk
             (fromInteger (caBeginningInventory spec))
             (fromInteger (caEndingInventory spec))
  where
    mk = (:<) :: BK.MkBase MinBase
    totals = totalsByAlg (bar base)
    balanceOf title = maybe 0 (normalBalance title) (M.lookup title totals)
    receivables = balanceOf AccountsReceivable
    allowanceCurrent = balanceOf AllowanceForDoubtfulAccounts

isNominalAlg :: MinAlg -> Bool
isNominalAlg x =
    let division = classifyAccountDivision (getAccountTitle (_hatBase x))
    in division == Cost || division == Revenue

deriveClosing :: [Posting] -> ClosingAdjustments -> Either String [(String, DerivedValue)]
deriveClosing postings spec = do
    journal <- buildJournal postings
    let base = EJ.toAlg journal
    adjustments <- closingAdjustmentsAlg base spec
    let adjusted = base .+ adjustments
        nominal = EA.filter isNominalAlg adjusted
        real = EA.filter (not . isNominalAlg) adjusted
        summary = EAT.incomeSummaryAccount nominal
        resultOnly = projByAccountTitle NetIncome summary .+ projByAccountTitle NetLoss summary
        closedNamed = real .+ EAT.netIncomeTransfer resultOnly
        closedFinal = EAT.finalStockTransfer adjusted
    if bar closedNamed /= bar closedFinal
       then Left "named closing transfers disagree with finalStockTransfer"
       else
           let closingEntry = bar (closedFinal .+ BK.reversingEntry (bar adjusted))
               fullLedger = adjusted .+ closingEntry
           in Right (closingDerivedPairs adjusted fullLedger closedFinal)

consolidationInput :: [(String, J)] -> Maybe ([Posting], [Posting])
consolidationInput obj = do
    postings <- field "postings" obj >>= postingsFromJ
    internal <- field "internal_postings" obj >>= postingsFromJ
    pure (postings, internal)

deriveConsolidation :: [Posting] -> [Posting] -> Either String [(String, DerivedValue)]
deriveConsolidation postings internal = do
    entityJournal <- buildJournalBy postings
    internalJournal <- buildJournalBy internal
    let internalAlg = EJ.toAlg internalJournal
        elimination = BK.reversingEntry internalAlg
        consolidated = EJ.toAlg entityJournal .+ elimination
    if bar (internalAlg .+ elimination) /= (Zero :: MinAlg)
       then Left "internal postings and bar-netted elimination do not cancel"
       else pure (derivedPairsAlg consolidated)

jstr :: String -> String
jstr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc c    = [c]

renderDerived :: [(String, DerivedValue)] -> String
renderDerived pairs =
    "{\"derived\":{" ++ intercalate "," [jstr key ++ ":" ++ renderValue value | (key, value) <- pairs] ++ "}}"
  where
    renderValue (DerivedNum value) = show value
    renderValue (DerivedString value) = jstr value

deriveRoot :: J -> Either String [(String, DerivedValue)]
deriveRoot array@(JArr _) = do
    postings <- maybe (Left "input is not a canonical posting array") Right (postingsFromJ array)
    journal <- buildJournal postings
    pure (derivedPairsAlg (EJ.toAlg journal))
deriveRoot (JObj obj) = case field "mode" obj of
    Just (JStr "closing") -> do
        (postings, spec) <- maybe (Left "invalid closing request") Right (closingInput obj)
        deriveClosing postings spec
    Just (JStr "consolidation") -> do
        (postings, internal) <- maybe (Left "invalid consolidation request") Right (consolidationInput obj)
        deriveConsolidation postings internal
    Just (JStr mode) -> Left ("unknown mode: " ++ mode)
    _                -> Left "object input requires a mode"
deriveRoot _ = Left "input must be a posting array or mode object"

main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        (path:_) -> readFile path
        []       -> getContents
    case maybe (Left "invalid JSON") deriveRoot (parseJSON input) of
        Left err -> do
            hPutStrLn stderr ("DeriveEA: " ++ err)
            exitFailure
        Right pairs -> putStrLn (renderDerived pairs)
