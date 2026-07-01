{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- The alias table intentionally references the deprecated 'Commutation'
-- constructor so that its legacy Japanese label ("通信費") is recognised and
-- flagged as ambiguous against 'CommunicationExpenses'; silence the warning.
{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Module      : ExchangeAlgebra.Convert
Description : Pure conversion between external (side, account-name, amount) data and
              exchange-algebra terms. No serialization dependency (Text only).

This module is the dependency-free core of the input-conversion layer: it turns
externally supplied postings---a debit\/credit 'Side', an account name as 'Text',
and an amount---into exchange-algebra 'Alg' terms, and back. The format glue
(JSON\/XML, i.e. @aeson@) deliberately lives outside the published library
(see @examples\/audit-eval\/runner@), so the algebra core stays serialization-free.

The mapping from a debit\/credit side to the @Hat@\/@Not@ marker is derived from
the library's own 'whichSide', so the credit\/debit semantics are never duplicated.
Unknown or wildcard account names are rejected (a /correct-by-construction/ guard
against hallucinated accounts). Account names that are genuinely ambiguous (one
Japanese label shared by several constructors, e.g. @準備預金@ for both the asset
and liability side of reserve deposits) are also rejected, with the candidate
accounts reported, so callers must disambiguate by canonical name.

A fixed-schema CSV reader (@side,account,amount@, optional @note@ column) is
provided in "ExchangeAlgebra.Convert.Csv".
-}
module ExchangeAlgebra.Convert
    ( ConvError(..)
    , concreteAccountTitles
    , normalizeTitle
    , parseAccountTitle
    , parseSide
    , markerForSide
    , postingFromSide
    , journalFromSides
    ) where

import           Data.Char (isAlphaNum, isSpace)
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra         ( Alg, HatVal, Redundant((.+)), (.@) )
import           ExchangeAlgebra.Algebra.Base    ( AccountTitles(..)
                                                 , HatBase(..)
                                                 , Hat(..)
                                                 , Side(..)
                                                 , whichSide )

-- $setup
-- The examples use 'Text' literals, so enable @OverloadedStrings@ in doctest.
-- >>> :set -XOverloadedStrings

-- | Conversion errors. Kept structural so callers can report precisely.
data ConvError = UnknownAccount Text                    -- ^ name matched no concrete account title
               | AmbiguousAccount Text [AccountTitles]  -- ^ name matched several accounts (disambiguate by canonical name)
               | UnknownSide    Text                    -- ^ side string was not debit\/credit
               | MalformedCsv   Text                    -- ^ CSV header\/row was structurally invalid (see "ExchangeAlgebra.Convert.Csv")
               | BadAmount      Text                    -- ^ amount field was not a valid non-negative number
    deriving (Eq, Show)

-- | All concrete account titles, /excluding/ the wildcard 'AccountTitle'.
--
-- The 'AccountTitles' type ends with a wildcard constructor whose account
-- division is undefined; external input must never resolve to it, so we expose
-- the safe, non-wildcard range explicitly rather than deriving 'Bounded' here
-- and risking the wildcard sneaking in. The upper bound is the last concrete
-- constructor before 'AccountTitle'.
--
-- >>> take 1 concreteAccountTitles
-- [Cash]
-- >>> Cash `elem` concreteAccountTitles && Sales `elem` concreteAccountTitles
-- True
-- >>> AccountsPayable `elem` concreteAccountTitles
-- True
-- >>> AccountTitle `elem` concreteAccountTitles
-- False
concreteAccountTitles :: [AccountTitles]
concreteAccountTitles = [Cash .. ReversalOfAllowanceForDoubtfulAccounts]

-- | Account-name lookup table: every key (canonical constructor names and the
-- Japanese\/abbreviation aliases below) is run through 'normalizeTitle', so matching is
-- case-, whitespace-, and symbol-insensitive. Keys are tagged so that a single
-- alias shared by several accounts is reported as 'AmbiguousAccount' rather than
-- silently resolving to whichever entry happens to come first.
--
-- The canonical English names (derived from 'show' over 'concreteAccountTitles')
-- are always unique, so they are never ambiguous and always resolve.
accountTable :: [(Text, [AccountTitles])]
accountTable = collapse [ (normalizeTitle k, a) | (k, a) <- entries ]
  where
    -- Group entries by normalised key, accumulating every account that key maps
    -- to (so genuine collisions become ambiguous, not first-wins).
    collapse kvs =
        [ (k, L.nub as)
        | k <- L.nub (L.map fst kvs)
        , let as = [ a | (k', a) <- kvs, k' == k ] ]

    entries :: [(Text, AccountTitles)]
    entries =  canonical ++ aliases

    -- Canonical: the constructor name. Always unique.
    canonical = [ (T.pack (show a), a) | a <- concreteAccountTitles ]

    -- Aliases: Japanese labels (from the Element.hs translation Haddock, which is
    -- the canonical bilingual reference) plus a few English abbreviations.
    -- Genuinely shared labels are listed for each account; lookup then returns
    -- 'AmbiguousAccount' for those, forcing the caller to use a canonical name.
    aliases =
      [ -- Assets (資産)
        ("現金",                 Cash)
      , ("普通預金",             Deposits)
      , ("当座預金",             CurrentDeposits)
      , ("有価証券",             Securities)
      , ("投資有価証券",         InvestmentSecurities)
      , ("長期国債",             LongTermNationalBonds)          -- ambiguous: holder vs issuer
      , ("長期国債",             LongTermNationalBondsPayable)   --   (use canonical name to disambiguate)
      , ("短期国債",             ShortTermNationalBonds)         -- ambiguous: holder vs issuer
      , ("短期国債",             ShortTermNationalBondsPayable)  --   (use canonical name to disambiguate)
      , ("商品",                 Products)
      , ("機械装置",             Machinery)
      , ("建物",                 Building)
      , ("車両運搬具",           Vehicle)
      , ("株式投資",             StockInvestment)
      , ("設備投資",             EquipmentInvestment)
      , ("長期貸付金",           LongTermLoansReceivable)
      , ("売掛金",               AccountsReceivable)
      , ("a/r",                  AccountsReceivable)
      , ("accounts receivable",  AccountsReceivable)
      , ("短期貸付金",           ShortTermLoansReceivable)
      , ("準備預金",             ReserveDepositReceivable)       -- ambiguous: asset vs liability side
      , ("準備預金",             ReserveDepositPayable)          --   (use canonical name to disambiguate)
      , ("金",                   Gold)
      , ("政府サービス",         GovernmentService)
        -- Equity (資本)
      , ("資本金",               CapitalStock)
      , ("繰越利益剰余金",       RetainedEarnings)
      , ("利益準備金",           LegalRetainedEarnings)
        -- Liability (負債)
      , ("長期借入金",           LongTermLoansPayable)
      , ("短期借入金",           ShortTermLoansPayable)
      , ("借入金",               LoansPayable)
      , ("減価償却引当金",       ReserveForDepreciation)
      , ("減価償却累計額",       AccumulatedDepreciation)        -- shared w/ ReserveForDepreciation legacy
      , ("受入預金",             DepositPayable)
      , ("発行銀行券",           CentralBankNotePayable)
        -- Expense (費用)
      , ("減価償却費",           Depreciation)
      , ("売上原価",             SalesCost)
      , ("cogs",                 SalesCost)
      , ("cost of sales",        SalesCost)
      , ("旅費交通費",           BusinessTrip)
      , ("通信費",               CommunicationExpenses)          -- ambiguous: legacy 'Commutation' is also "通信費"
      , ("通信費",               Commutation)                    --   (use canonical name to disambiguate)
      , ("水道光熱費",           UtilitiesExpense)
      , ("支払家賃",             RentExpense)
      , ("広告宣伝費",           AdvertisingExpense)
      , ("発送費",               DeliveryExpenses)
      , ("消耗品費",             SuppliesExpenses)
      , ("雑費",                 MiscellaneousExpenses)
      , ("給料",                 WageExpenditure)
      , ("支払利息",             InterestExpense)
      , ("租税公課",             TaxesExpense)
      , ("消費支出",             ConsumptionExpenditure)
      , ("補助金支出",           SubsidyExpense)
      , ("国庫納付金支出",       CentralBankPaymentExpense)
      , ("仕入",                 Purchases)
      , ("当期純利益",           NetIncome)
        -- Revenue (収益)
      , ("付加価値",             ValueAdded)
      , ("補助金収入",           SubsidyIncome)
      , ("国債利息収入",         NationalBondInterestEarned)
      , ("預金利息収入",         DepositInterestEarned)
      , ("売上総利益",           GrossProfit)
      , ("経常利益",             OrdinaryProfit)
      , ("受取利息",             InterestEarned)
      , ("受取手数料",           ReceiptFee)
      , ("受取家賃",             RentalIncome)
      , ("賃金収入",             WageEarned)
      , ("租税収入",             TaxesRevenue)
      , ("国庫納付金収入",       CentralBankPaymentIncome)
      , ("売上",                 Sales)
      , ("当期純損失",           NetLoss)
        -- Elementary bookkeeping additions: Assets (資産)
      , ("小口現金",             PettyCash)
      , ("受取手形",             NotesReceivable)
      , ("電子記録債権",         ElectronicallyRecordedReceivable)
      , ("クレジット売掛金",     CreditCardReceivable)
      , ("手形貸付金",           NotesLoansReceivable)
      , ("繰越商品",             MerchandiseInventory)
      , ("前払金",               AdvancesPaid)
      , ("前払費用",             PrepaidExpenses)
      , ("未収収益",             AccruedRevenue)
      , ("未収入金",             OtherReceivables)
      , ("立替金",               PaymentsOnBehalf)
      , ("仮払金",               SuspensePayments)
      , ("仮払消費税",           ConsumptionTaxPaid)
      , ("仮払法人税等",         PrepaidCorporateIncomeTaxes)
      , ("土地",                 Land)
      , ("備品",                 Fixtures)
      , ("特許権",               Patent)
      , ("商標権",               Trademark)
      , ("ソフトウェア",         Software)
      , ("現金過不足",           CashOverShort)
        -- Liability (負債)
      , ("買掛金",               AccountsPayable)
      , ("a/p",                  AccountsPayable)
      , ("accounts payable",     AccountsPayable)
      , ("支払手形",             NotesPayable)
      , ("電子記録債務",         ElectronicallyRecordedObligations)
      , ("手形借入金",           NotesLoansPayable)
      , ("当座借越",             BankOverdraft)
      , ("前受金",               AdvancesReceived)
      , ("前受収益",             UnearnedRevenue)
      , ("未払費用",             AccruedExpenses)
      , ("未払金",               OtherPayables)
      , ("預り金",               DepositsReceived)
      , ("仮受金",               SuspenseReceipts)
      , ("仮受消費税",           ConsumptionTaxReceived)
      , ("未払消費税",           AccruedConsumptionTax)
      , ("未払法人税等",         AccruedCorporateIncomeTaxes)
      , ("未払配当金",           UnpaidDividends)
      , ("貸倒引当金",           AllowanceForDoubtfulAccounts)
        -- Cost (費用)
      , ("貸倒引当金繰入",       ProvisionForDoubtfulAccounts)
      , ("貸倒損失",             BadDebtLoss)
      , ("固定資産売却損",       LossOnSalesOfFixedAssets)
      , ("手形売却損",           LossOnSalesOfNotesReceivable)
      , ("支払手数料",           PaymentFees)
      , ("雑損",                 MiscellaneousLoss)
      , ("法人税等",             CorporateIncomeTaxes)
        -- Revenue (収益)
      , ("固定資産売却益",       GainOnSalesOfFixedAssets)
      , ("償却債権取立益",       RecoveryOfBadDebts)
      , ("雑益",                 MiscellaneousIncome)
      , ("貸倒引当金戻入",       ReversalOfAllowanceForDoubtfulAccounts)
      ]

-- | Normalise an account name for matching: case-fold, drop punctuation\/symbols
-- (keep only alphanumerics and spaces), and collapse internal whitespace to
-- single spaces (also trimming). So @\"A\/R\"@ and @\"ar\"@ coincide, and
-- @\"Accounts  Receivable\"@ matches @\"accounts receivable\"@. CJK characters
-- are alphanumeric (Unicode @Lo@), so Japanese labels survive unchanged.
--
-- (Renamed from @norm@: that name collides with the core value-domain
-- homomorphism 'ExchangeAlgebra.Algebra.norm', which is an entirely unrelated
-- operation — the two must not be confusable in downstream imports.)
--
-- >>> normalizeTitle "  Accounts   Receivable "
-- "accounts receivable"
-- >>> normalizeTitle "A/R"
-- "ar"
normalizeTitle :: Text -> Text
normalizeTitle = T.unwords . T.words . T.filter (\c -> isAlphaNum c || isSpace c) . T.toLower

-- | Parse an account name into a concrete 'AccountTitles'. Unknown names and the
-- wildcard are rejected; ambiguous Japanese labels (shared by several accounts)
-- are rejected with the candidates reported.
--
-- >>> parseAccountTitle "Cash"
-- Right Cash
-- >>> parseAccountTitle "  accounts receivable "
-- Right AccountsReceivable
-- >>> parseAccountTitle "Accounts   Receivable"
-- Right AccountsReceivable
-- >>> parseAccountTitle "A/R"
-- Right AccountsReceivable
-- >>> parseAccountTitle "売掛金"
-- Right AccountsReceivable
--
-- A label shared by two accounts (here the asset and liability side of reserve
-- deposits) is rejected as ambiguous, listing the candidates so the caller can
-- pick a canonical name:
--
-- >>> case parseAccountTitle "準備預金" of { Left (AmbiguousAccount _ as) -> as; _ -> [] }
-- [ReserveDepositReceivable,ReserveDepositPayable]
-- >>> parseAccountTitle "Goodwill_X"
-- Left (UnknownAccount "Goodwill_X")
-- >>> parseAccountTitle "AccountTitle"
-- Left (UnknownAccount "AccountTitle")
parseAccountTitle :: Text -> Either ConvError AccountTitles
parseAccountTitle t =
    case lookup (normalizeTitle t) accountTable of
        Just [a] -> Right a
        Just as  -> Left (AmbiguousAccount t as)
        Nothing  -> Left (UnknownAccount t)

-- | Parse a side string (@"debit"@\/@"credit"@, case-insensitive) into 'Side'.
--
-- >>> parseSide "debit"
-- Right Debit
-- >>> parseSide "CREDIT"
-- Right Credit
-- >>> parseSide "left"
-- Left (UnknownSide "left")
parseSide :: Text -> Either ConvError Side
parseSide t = case normalizeTitle t of
    "debit"  -> Right Debit
    "credit" -> Right Credit
    _        -> Left (UnknownSide t)

-- | The @Hat@\/@Not@ marker that places @account@ on the requested debit\/credit
-- @side@. Derived from the library's 'whichSide' so the credit\/debit rule is not
-- duplicated: an account sits on its \"natural\" side under 'Not'; if the
-- requested side differs, the 'Hat' (reversal) marker is used.
--
-- >>> markerForSide Debit Cash
-- Not
-- >>> markerForSide Credit Cash
-- Hat
-- >>> markerForSide Credit Sales
-- Not
-- >>> markerForSide Debit Sales
-- Hat
markerForSide :: Side -> AccountTitles -> Hat
markerForSide side account
    | whichSide (Not :< account) == side = Not
    | otherwise                          = Hat

-- | Build a single posting term: @amount .\@ (marker :< account)@, with the
-- marker chosen so the posting lands on @side@. Built through the smart
-- constructor '(.@)' so the non-negative\/non-error value invariant is enforced.
--
-- >>> postingFromSide Debit Cash (1000 :: Double)
-- 1000.00:@Not:<Cash
-- >>> postingFromSide Credit Sales (1000 :: Double)
-- 1000.00:@Not:<Sales
postingFromSide :: (HatVal v)
                => Side -> AccountTitles -> v -> Alg v (HatBase AccountTitles)
postingFromSide side account amount =
    amount .@ (markerForSide side account :< account)

-- | Fold a list of @(side, account, amount)@ postings into one algebra term.
-- Balance (debit == credit) is /not/ enforced here; check it with the library's
-- balance functions after conversion.
--
-- >>> journalFromSides [(Debit, Cash, 1000), (Credit, Sales, 1000)] :: Alg Double (HatBase AccountTitles)
-- 1000.00:@Not:<Cash .+ 1000.00:@Not:<Sales
journalFromSides :: (HatVal v)
                 => [(Side, AccountTitles, v)] -> Alg v (HatBase AccountTitles)
journalFromSides = foldr (\(s, a, v) acc -> postingFromSide s a v .+ acc) mempty
