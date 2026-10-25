{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Input vocabulary at the IO input boundary. These package-private types
-- combine identifiers, registry rules, and the closed bookkeeping catalog for
-- the admission engine and public entry point. Read raw postings and the trusted
-- specification before calls and complete submissions.
module ExchangeAlgebra.IO.Input.Admission.Submission where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

import ExchangeAlgebra.Accounting.Account (AccountTitles)
import ExchangeAlgebra.Accounting.Transaction
    ( CallId
    , EntityId
    , EvidenceId
    , FactId
    , PeriodId
    , TxKey
    )
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.Accounting.Catalog (CatalogCall)
import ExchangeAlgebra.IO.Input.Admission.Registry.Definition (TxIdRegistry)

-- | Side of one submitted posting. 'DebitSide' is the left side (@decL@) and
-- 'CreditSide' the right side (@decR@). Unlike the registry's @Side@, this
-- type has no wildcard, so a submission cannot leave the side unspecified.
data PostingSide
    = DebitSide -- ^ Debit (left) side.
    | CreditSide -- ^ Credit (right) side.
    deriving (Eq, Ord, Show)

-- | Untrusted account text. Admission resolves it against the account
-- vocabulary; this alias does not introduce a distinct type.
type AccountName = Text

-- | Typed side, untrusted account text, and strictly positive monetary amount.
-- Admission parses account names and checks amounts and balance.
type RawPostings = [(PostingSide, AccountName, MoneyDecimal)]

-- | Trusted admission contract. Evidence amounts are /total debit amounts
-- per transaction/, not net balances or individual posting amounts.
-- Facts are checked in the role declared by their registry entries.
data AdmissionSpec = AdmissionSpec
    { admissionRegistry   :: TxIdRegistry
    , admissionEvidence   :: Map EvidenceId MoneyDecimal -- ^ Evidence debit totals.
      -- Each amount is compared only with the transaction's total debit amount.
      -- Evidence does not reconcile the credit breakdown, account titles, or
      -- counterparty. Supply these totals independently of submitted postings.
    , admissionFacts      :: Map FactId RawPostings -- ^ Trusted raw fact entries.
    , admissionVocabulary :: Set AccountTitles -- ^ Allowed account titles.
    }

-- | An invocation in one entity and period. Posting operations require a
-- generated key in that scope; queries require 'Nothing'. A posting operation
-- that produces no rows still fulfills its declared generated key.
data Call = Call
    { callId        :: CallId
    , callEntity    :: EntityId -- ^ Entity scope of the call.
    , callPeriod    :: PeriodId -- ^ Period scope of the call.
    , callGenerated :: Maybe TxKey -- ^ Generated key, when applicable.
    , callBody      :: CatalogCall -- ^ Closed catalog request.
    }
    deriving (Eq, Show)

-- | Entire untrusted submission. Lists preserve duplicate keys for rejection.
-- The trusted executor must pass the complete submission to @admit@.
data Submission = Submission
    { submissionPostings :: [(TxKey, RawPostings)]
    , submissionCalls    :: [Call] -- ^ Submitted catalog calls.
    }
    deriving (Eq, Show)
