{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Derive ledger views and reporting values in the IO input layer from admitted
-- data. This module uses trial-balance validation and presentation; callers of
-- the public admission API consume its readouts and cannot supply balances.
-- Read the source getters before the derivation path.
module ExchangeAlgebra.IO.Input.Admission.Derive
    ( -- * Read-only observations
      admittedJournal
    , admittedSnapshot
    , admittedAudit
    , admittedTrialBalance
    , admittedAdjustedTrialBalance
    , admittedFinancialStatements
    , admittedClosingStatements
      -- * Derivation
    , deriveLedger
    , deriveTrialBalance
    , presentAdmitted
    , renderAdmittedStatements
    ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import ExchangeAlgebra.IO.Input.Admission.Internal
import ExchangeAlgebra.IO.Input.Admission.Types
import qualified ExchangeAlgebra.Journal as Journal
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
import qualified ExchangeAlgebra.Accounting.TrialBalance.Validation as TrialBalance
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- * Read-only observations

-- | Observe the original journal. Its ordinary mutators produce unadmitted
-- journals and cannot reconstruct an t'Admitted' value.
admittedJournal :: Admitted -> AdmissionJournal
admittedJournal (Admitted journal _ _ _ _) = journal

-- | Select a snapshot created by admission. Empty generated transactions keep
-- their keys in this view even when the underlying Journal stores no postings.
admittedSnapshot :: Snapshot -> Admitted -> LedgerView
admittedSnapshot snapshot (Admitted journal metadata _ ordinary adjusted) =
    Map.mapWithKey (\key _ -> Journal.toAlg (Journal.projWithNote [key] selected)) included
  where
    selected = case snapshot of
        DuringPeriod -> ordinary
        Adjusted -> adjusted
        Closed -> journal
    included = Map.filter visible metadata
    visible (EntryMetadata _ stage _) = case snapshot of
        DuringPeriod -> stage <= OrdinaryStage
        Adjusted -> stage <= ConsolidationStage
        Closed -> True

-- | Observe library-established origins and successful query projections.
admittedAudit :: Admitted -> [CallAudit]
admittedAudit (Admitted _ _ audit _ _) = audit

-- | Read the final validated trial-balance element without granting admission
-- authority to any transformed result. Net balances use the existing
-- @accountBalances@ readout; original postings remain available in the ledger.
admittedTrialBalance :: AdmittedTrialBalance -> Entry
admittedTrialBalance (AdmittedTrialBalance _ final _) =
    TrialBalance.validatedTrialBalance final

-- | Read the validated adjusted balance before closing removes revenue and
-- expense balances. The snapshot comes only from the admitted journal.
admittedAdjustedTrialBalance :: AdmittedTrialBalance -> Entry
admittedAdjustedTrialBalance (AdmittedTrialBalance _ _ adjusted) =
    TrialBalance.validatedTrialBalance adjusted

-- | Observe presentation of the adjusted snapshot, including nominal accounts.
admittedFinancialStatements :: AdmittedStatements -> Presentation.FinancialStatements MoneyDecimal
admittedFinancialStatements (AdmittedStatements _ adjusted _) = adjusted

-- | Observe presentation of the final snapshot, after closing when supplied.
admittedClosingStatements :: AdmittedStatements -> Presentation.FinancialStatements MoneyDecimal
admittedClosingStatements (AdmittedStatements _ _ final) = final

-- * Derivation

-- | Derive the complete per-transaction ledger, preserving all original
-- postings. Keys and account coordinates are compared exactly.
deriveLedger :: Admitted -> LedgerView
deriveLedger = admittedSnapshot Closed

-- | Accumulate failures from two independent snapshot validations.
combineResults
    :: Either (NonEmpty error) left
    -> Either (NonEmpty error) right
    -> Either (NonEmpty error) (left, right)
combineResults (Right left) (Right right) = Right (left, right)
combineResults (Left left) (Left right) = Left (left <> right)
combineResults (Left errors) _ = Left errors
combineResults _ (Left errors) = Left errors

-- | Validate adjusted and final snapshots using the existing strict policy.
-- The final snapshot is AfterClosing if the admitted registry entries include
-- a closing transaction, otherwise BeforeClosing. No external balance,
-- explanation, or reclassification can be injected into this path.
--
-- For multiple entities, derivation reads the combined admitted journal;
-- acceptance proves provenance and balance, not the economic correctness of
-- consolidation or the appropriateness of combining reporting periods.
deriveTrialBalance
    :: Admitted
    -> Either (NonEmpty (TrialBalance.TBFinding MoneyDecimal)) AdmittedTrialBalance
deriveTrialBalance admitted@(Admitted journal metadata _ _ adjusted) = do
    (finalBalance, adjustedBalance) <- combineResults
        (validate finalStage journal)
        (validate TrialBalance.BeforeClosing adjusted)
    Right (AdmittedTrialBalance admitted finalBalance adjustedBalance)
  where
    finalStage
        | any isClosing (Map.elems metadata) = TrialBalance.AfterClosing
        | otherwise = TrialBalance.BeforeClosing
    isClosing (EntryMetadata role _ _) = role == Closing
    validate stage source = TrialBalance.validateTrialBalance TrialBalance.strictTrialBalancePolicy
        (TrialBalance.TrialBalanceInput (Journal.toAlg source) stage Map.empty [] Set.empty)

-- | Present the validated adjusted and final snapshots. A reporting context
-- can control supported classifications, but cannot replace either balance.
presentAdmitted
    :: Presentation.ReportingContext MoneyDecimal
    -> AdmittedTrialBalance
    -> Either (NonEmpty (Presentation.PresentationIssue MoneyDecimal)) AdmittedStatements
presentAdmitted context accepted@(AdmittedTrialBalance _ final adjusted) = do
    (adjustedStatements, finalStatements) <- combineResults
        (Presentation.present context adjusted)
        (Presentation.present context final)
    Right (AdmittedStatements accepted adjustedStatements finalStatements)

-- | Escape one CSV cell by quoting it and doubling embedded quotes.
quoteCell :: Text -> Text
quoteCell cell = "\"" <> Text.replace "\"" "\"\"" cell <> "\""

-- | Render statement lines with their snapshot and exact decimal amount.
statementRows
    :: Text
    -> Presentation.FinancialStatements MoneyDecimal
    -> [[Text]]
statementRows snapshot statements =
    [[ snapshot
     , Text.pack (show (Presentation._lineAccount line))
     , Presentation._lineLabel line
     , Text.pack (show (Presentation._lineSection line))
     , Text.pack (show (Presentation._lineSide line))
     , Text.pack (show (Presentation._lineAmount line))
     ] | line <- Presentation._statementLines statements]

-- | Render UTF-8 CSV with a header and adjusted/final snapshot labels.
-- Every cell is quoted; newlines in labels are retained inside quoted cells.
-- Only successfully presented statements enter this rendering path.
renderAdmittedStatements :: AdmittedStatements -> ByteString
renderAdmittedStatements (AdmittedStatements _ adjusted final) = Text.encodeUtf8
    (Text.unlines (map (Text.intercalate "," . map quoteCell) rows))
  where
    rows = ["snapshot", "account", "label", "section", "side", "amount"]
        : statementRows "adjusted" adjusted ++ statementRows "final" final
