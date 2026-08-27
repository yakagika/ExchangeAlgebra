{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ExchangeAlgebra.Assist
Description : Assistance helpers for LLM-facing account selection and validation feedback.

This module provides a small deterministic assistance layer for generated
journal-entry workflows. It exposes account-title metadata from the canonical
account registry, with semantic descriptions for technical and derived titles,
plus one-line explanations for validation errors from
"ExchangeAlgebra.Convert.Checked".
-}
module ExchangeAlgebra.Assist
    ( AccountInfo(..)
    , describeAccount
    , allAccountInfos
    , suggestAccounts
    , explainEntryError
    , explainJournalErrors
    , explainSourceErrors
    ) where

import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Ord (Down(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra.Base
                     ( AccountRole
                     , AccountSemantics(..)
                     , AccountSpec(..)
                     , AccountTitles(..)
                     , DivisionSemantics
                     , HomeSideSemantics
                     , PostingCapability
                     , ReportingEligibility
                     , accountSemantics
                     , accountSpec
                     , concreteAccountTitles
                     )
import           ExchangeAlgebra.Convert
                     ( ConvError(..) )
import           ExchangeAlgebra.Convert.Checked
                     ( EntryError(..)
                     , JournalError(..)
                     , SourceError(..)
                     )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> import ExchangeAlgebra.Algebra.Base (AccountTitles(..))
-- >>> import ExchangeAlgebra.Convert (ConvError(..))
-- >>> import ExchangeAlgebra.Convert.Checked (EntryError(..), JournalError(..), SourceError(..))

-- | Account-title metadata for LLM-facing lookup.
data AccountInfo = AccountInfo
  { aiTitle                :: AccountTitles
  , aiRoles                :: [AccountRole]
  , aiPostingCapability    :: PostingCapability
  , aiDivisionSemantics    :: DivisionSemantics
  , aiHomeSideSemantics    :: HomeSideSemantics
  , aiReportingEligibility :: ReportingEligibility
  , aiNameEn               :: Text
  , aiNameJa               :: Text
  , aiDesc                 :: Text
  } deriving (Show, Eq)

-- | Describe a concrete account title.
--
-- The wildcard 'AccountTitle' is rejected because
-- 'classifyAccountDivision' is intentionally undefined for it.
--
-- >>> fmap ((== "現金") . aiNameJa) (describeAccount Cash)
-- Just True
-- >>> fmap aiDivisionSemantics (describeAccount Cash)
-- Just (StatementDivision Assets)
-- >>> fmap aiDivisionSemantics (describeAccount IncomeSummary)
-- Just (DirectionEncoding Assets)
-- >>> describeAccount AccountTitle
-- Nothing
describeAccount :: AccountTitles -> Maybe AccountInfo
describeAccount title = do
    spec <- accountSpec title
    semantics <- accountSemantics title
    pure (toInfo title spec semantics)

-- | All concrete account-title descriptions in 'Enum' order.
--
-- >>> length allAccountInfos
-- 232
-- >>> take 1 (map aiTitle allAccountInfos)
-- [Cash]
-- >>> aiTitle (last allAccountInfos)
-- AvailableForSaleSecurities
allAccountInfos :: [AccountInfo]
allAccountInfos = mapMaybe describeAccount concreteAccountTitles

toInfo :: AccountTitles -> AccountSpec -> AccountSemantics -> AccountInfo
toInfo title spec semantics = AccountInfo
    { aiTitle = title
    , aiRoles = asemRoles semantics
    , aiPostingCapability = asemPostingCapability semantics
    , aiDivisionSemantics = asemDivisionSemantics semantics
    , aiHomeSideSemantics = asemHomeSideSemantics semantics
    , aiReportingEligibility = asemReportingEligibility semantics
    , aiNameEn = asNameEn spec
    , aiNameJa = safeNameJa title spec
    , aiDesc = safeDescription title spec
    }

-- | LLM-facing names must not encode an internal direction as if it were an
-- expense or revenue classification.
safeNameJa :: AccountTitles -> AccountSpec -> Text
safeNameJa NetIncome _ = "当期純利益"
safeNameJa NetLoss _   = "当期純損失"
safeNameJa GrossProfit _ = "売上総利益"
safeNameJa OrdinaryProfit _ = "経常利益"
safeNameJa _ spec      = asNameJa spec

-- | Semantic descriptions for technical, derived, and contextual titles.
-- Ordinary statement accounts retain their canonical registry description.
safeDescription :: AccountTitles -> AccountSpec -> Text
safeDescription NetIncome _ =
    "Period result: net income (当期純利益). Engine-generated only; the legacy Cost value is an internal direction encoding, not an expense classification."
safeDescription NetLoss _ =
    "Period result: net loss (当期純損失). Engine-generated only; the legacy Revenue value is an internal direction encoding, not a revenue classification."
safeDescription GrossProfit _ =
    "Reporting subtotal: gross profit (売上総利益). Derived from trial-balance values and not available for direct posting."
safeDescription OrdinaryProfit _ =
    "Reporting subtotal: ordinary profit (経常利益). Derived from trial-balance values and not available for direct posting."
safeDescription IncomeSummary _ =
    "Closing device: income summary (損益). Available only during closing; the legacy Assets value is an internal direction encoding, not a balance-sheet classification."
safeDescription SuspensePayments _ =
    "Temporary account: suspense payments (仮払金). Its legacy Assets value is a bookkeeping control class; unresolved balances require review before presentation."
safeDescription SuspenseReceipts _ =
    "Temporary account: suspense receipts (仮受金). Its legacy Liability value is a bookkeeping control class; unresolved balances require review before presentation."
safeDescription CashOverShort _ =
    "Temporary account: cash over and short (現金過不足). It must be cleared at closing and is not presented in financial statements."
safeDescription SuspenseAccount _ =
    "Temporary account: suspense account (未決算). Its legacy Assets value is a bookkeeping control class; unresolved balances require review before presentation."
safeDescription BranchCurrentAccount _ =
    "Reciprocal account: branch current account (支店). It may remain in head-office books but is eliminated when head-office and branch balances are combined."
safeDescription HeadOfficeCurrentAccount _ =
    "Reciprocal account: head-office current account (本店). It may remain in branch books but is eliminated when head-office and branch balances are combined."
safeDescription NetIncomeAttributableToNCI _ =
    "Consolidation attribution result: profit attributable to non-controlling interests (非支配株主に帰属する当期純利益). Available only in consolidation worksheets; the legacy Cost value is an internal direction encoding."
safeDescription NetLossAttributableToNCI _ =
    "Consolidation attribution result: loss attributable to non-controlling interests (非支配株主に帰属する当期純損失). Available only in consolidation worksheets; the legacy Revenue value is an internal direction encoding."
safeDescription _ spec = asDescription spec

-- | Suggest account titles by deterministic substring matching.
--
-- The query is split with 'T.words'. Each token is matched case-insensitively
-- against the constructor name, English name, Japanese name, and full
-- description. Results are ranked by the number of matched tokens, with 'Enum'
-- order as the tie-breaker.
--
-- >>> map aiTitle (take 3 (suggestAccounts "cash"))
-- [Cash,PettyCash,CashOverShort]
-- >>> map aiTitle (take 3 (suggestAccounts "現金"))
-- [Cash,PettyCash,CashOverShort]
-- >>> suggestAccounts ""
-- []
-- >>> suggestAccounts "zzzznomatch"
-- []
suggestAccounts :: Text -> [AccountInfo]
suggestAccounts query
    | null tokens = []
    | otherwise =
        map snd
        . sortOn (\(rank, info) -> (Down rank, fromEnum (aiTitle info)))
        . filter ((> 0) . fst)
        $ [ (matchRank info, info) | info <- allAccountInfos ]
  where
    tokens = map T.toCaseFold (T.words query)

    matchRank info =
        length
            [ token
            | token <- tokens
            , any (T.isInfixOf token) (searchFields info)
            ]

    searchFields info = map T.toCaseFold
        [ T.pack (show (aiTitle info))
        , aiNameEn info
        , aiNameJa info
        , aiDesc info
        ]

-- | Explain one checked-entry validation error as one English line.
--
-- >>> explainEntryError (Imbalanced 1500 1400 :: EntryError Int)
-- "entry is not balanced: debit total 1500 /= credit total 1400"
-- >>> explainEntryError (NonPositiveAmount 2 Cash (0 :: Int))
-- "posting 2 (Cash): amount must be > 0, got 0"
-- >>> explainEntryError (EntryParse 0 (UnknownAccount "Supplies") :: EntryError Int)
-- "posting 0: account \"Supplies\" does not resolve to a ledger account"
explainEntryError :: (Show v) => EntryError v -> Text
explainEntryError (EntryParse i err) =
    postingOnly i <> ": " <> explainConvError err
explainEntryError (NonPositiveAmount i account amount) =
    postingAccount i account <> ": amount must be > 0, got " <> showText amount
explainEntryError (WildcardAccount i) =
    postingOnly i <> ": wildcard AccountTitle is not a ledger account"
explainEntryError (WildcardSide i) =
    postingOnly i <> ": wildcard Side is not debit or credit"
explainEntryError EmptyEntry =
    "entry has no postings"
explainEntryError (Imbalanced debitTotal creditTotal) =
    "entry is not balanced: debit total "
    <> showText debitTotal
    <> " /= credit total "
    <> showText creditTotal

-- | Explain checked-journal errors, one line per error.
--
-- >>> explainJournalErrors (EntryErrors "tx1" (Imbalanced 1500 1400 :| []) :| [] :: NonEmpty (JournalError String Int))
-- "transaction \"tx1\": entry is not balanced: debit total 1500 /= credit total 1400"
-- >>> explainJournalErrors (DuplicateTxId "tx1" :| [] :: NonEmpty (JournalError String Int))
-- "transaction \"tx1\" appears more than once"
explainJournalErrors :: (Show n, Show v) => NonEmpty (JournalError n v) -> Text
explainJournalErrors =
    T.intercalate "\n" . concatMap explainJournalError . NE.toList

-- | Explain source-coverage errors, one line per error.
--
-- >>> explainSourceErrors [MissingSource "tx7" :: SourceError String Int]
-- "source transaction \"tx7\" has no journal entry"
-- >>> explainSourceErrors [AmountMismatch "tx7" 1500 1400 :: SourceError String Int]
-- "source transaction \"tx7\" amount mismatch: expected 1500, journal entry has 1400"
explainSourceErrors :: (Show n, Show v) => [SourceError n v] -> Text
explainSourceErrors = T.intercalate "\n" . map explainSourceError

explainJournalError :: (Show n, Show v) => JournalError n v -> [Text]
explainJournalError (EntryErrors txid errs) =
    [ "transaction " <> showText txid <> ": " <> explainEntryError err
    | err <- NE.toList errs
    ]
explainJournalError (DuplicateTxId txid) =
    [ "transaction " <> showText txid <> " appears more than once" ]

explainSourceError :: (Show n, Show v) => SourceError n v -> Text
explainSourceError (MissingSource txid) =
    "source transaction " <> showText txid <> " has no journal entry"
explainSourceError (UnknownSource txid) =
    "journal entry " <> showText txid <> " has no source transaction"
explainSourceError (AmountMismatch txid expected actual) =
    "source transaction "
    <> showText txid
    <> " amount mismatch: expected "
    <> showText expected
    <> ", journal entry has "
    <> showText actual

explainConvError :: ConvError -> Text
explainConvError (UnknownAccount account) =
    "account " <> showText account <> " does not resolve to a ledger account"
explainConvError (AmbiguousAccount account candidates) =
    "account "
    <> showText account
    <> " is ambiguous; candidates: "
    <> T.intercalate ", " (map showText candidates)
explainConvError (UnknownSide sideText) =
    "side " <> showText sideText <> " is not debit or credit"
explainConvError (MalformedCsv message) =
    "malformed CSV input: " <> showText message
explainConvError (BadAmount message) =
    "amount field is invalid: " <> showText message

postingOnly :: Int -> Text
postingOnly i = "posting " <> showText i

postingAccount :: Int -> AccountTitles -> Text
postingAccount i account =
    postingOnly i <> " (" <> showText account <> ")"

showText :: (Show a) => a -> Text
showText = T.pack . show
