{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | One-line English explanations of admission failures for model-facing
-- notices. The typed failures remain the source of truth. Each text states
-- what happened, using identifiers as 'show' spells them, and does not say
-- how to repair the submission, which depends on the caller's context.
module ExchangeAlgebra.IO.Input.Admission.Explain
    ( explainAdmissionError
    , explainAdmissionFailure
    , explainAdmissionPhase
    , explainClosingDerivationError
    ) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text

import ExchangeAlgebra.Accounting.Catalog
    ( AmountLimit(..)
    , AmountMismatch(..)
    , CatalogExecutionProblem(..)
    , CatalogParameterProblem(..)
    , DebitCreditTotals(..)
    )
import ExchangeAlgebra.IO.Input.Admission.Diagnostic
import ExchangeAlgebra.IO.Input.Admission.Submission (Disclosure(..))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.IO.Input.Assist (explainEntryError)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import ExchangeAlgebra.IO.Input.Admission.Submission (fullDisclosure)

-- | Spell identifiers and diagnostic values as 'show' does, including escapes.
showText :: Show value => value -> Text
showText = Text.pack . show

-- | Explain a parameter rejection without prescribing a repair.
explainParameter :: CatalogParameterProblem -> Text
explainParameter problem = case problem of
    NonPositiveParameter parameter amount ->
        showText parameter <> " is not positive: " <> showText amount
    NegativeParameter parameter amount ->
        showText parameter <> " is negative: " <> showText amount
    AccountOutsideDivision parameter account division ->
        showText parameter <> " account " <> showText account
        <> " is outside division " <> showText division
    RateAboveLimit rate -> "rate " <> showText rate <> " exceeds 10000 basis points"
    ReceivedBelowPaid (AmountLimit paid received) ->
        "received tax " <> showText received <> " is below paid tax " <> showText paid
    InterimExceedsTotal (AmountLimit total interim) ->
        "interim tax " <> showText interim <> " exceeds total tax " <> showText total
    BlankReferenceKey key -> "reference key " <> showText key <> " has a blank coordinate"
    TooFewEntities count -> "consolidation has fewer than two entities: " <> showText count
    RepeatedEntity entity -> "entity " <> showText entity <> " is repeated"
    RepeatedReference key -> "reference " <> showText key <> " is repeated"

-- | Explain a catalog computation failure; its amounts are facts, not evidence.
explainExecution :: CatalogExecutionProblem -> Text
explainExecution problem = case problem of
    InexactQuotient dividend divisor ->
        "quotient of " <> showText dividend <> " divided by " <> showText divisor
        <> " is not an exact monetary decimal"
    ImbalancedConsolidation (DebitCreditTotals debit credit) ->
        "consolidation is not balanced: debit total " <> showText debit
        <> ", credit total " <> showText credit
    AbnormalBalance account -> "account " <> showText account <> " has an abnormal balance"
    ClosingFailed base -> "closing could not close the balance of " <> showText base

-- | Explain why a source reference is unavailable to a call.
explainReference :: ReferenceFailure -> Text
explainReference problem = case problem of
    ReferenceUnknown -> "key is absent from the registry"
    ReferenceAbsent -> "registered key has no admitted entry"
    ReferenceEntityMismatch entity -> "source belongs to another entity: " <> showText entity
    ReferencePeriodMismatch period -> "source belongs to another period: " <> showText period
    ReferenceRoleMismatch role -> "source has another accounting role: " <> showText role
    ReferenceProvenanceForbidden provenance ->
        "source provenance " <> showText provenance <> " is not authorized"
    ReferenceNotVisible source target ->
        "source stage " <> showText source <> " is not visible at " <> showText target
    ReferenceReused -> "reference was consumed more than once"

-- | Explain one admission error in one nonempty line, stating what happened
-- without prescribing a repair. Pass the specification's @admissionDisclosure@.
-- With evidence amounts withheld, a mismatch includes only the submitted debit
-- total, omitting both the expected amount and the difference. Invalid evidence
-- omits its amount. Facts and submitted amounts are never withheld.
-- Invalid entries reuse 'explainEntryError', separated by @"; "@.
-- The same disclosure and error always give the same text.
--
-- >>> import ExchangeAlgebra.Accounting.Transaction
-- >>> let key = TxKey (EntityId "A") (PeriodId "2026") (TxId "sale")
-- >>> let mismatch = EvidenceMismatch key (EvidenceId "receipt") (AmountMismatch 137 251)
-- >>> Text.isInfixOf "expected amount 137" (explainAdmissionError fullDisclosure mismatch)
-- True
-- >>> Text.isInfixOf "137" (explainAdmissionError (Disclosure False) mismatch)
-- False
-- >>> let withheld = explainAdmissionError (Disclosure False) mismatch
-- >>> Text.isInfixOf "submitted debit total 251" withheld
-- True
explainAdmissionError :: Disclosure -> AdmissionError -> Text
explainAdmissionError disclosure problem = case problem of
    UnknownTransaction key -> "Transaction " <> showText key <> " is not registered."
    SupplyNotAllowed key operation supplies ->
        "Transaction " <> showText key <> " uses an unauthorized supply route "
        <> showText operation <> "; allowed supplies: " <> showText supplies <> "."
    FactOverwrite key fact ->
        "Transaction " <> showText key <> " overwrites trusted fact " <> showText fact <> "."
    MissingRequiredTransaction key -> "Required transaction " <> showText key <> " is absent."
    DuplicateRawTransaction key -> "Raw transaction " <> showText key <> " occurs more than once."
    DuplicateGeneratedTransaction key ->
        "Generated transaction " <> showText key <> " occurs more than once."
    RawGeneratedCollision key -> "Raw and generated transactions share key " <> showText key <> "."
    DuplicateCallId identity -> "Call identity " <> showText identity <> " occurs more than once."
    InvalidCallIdentity identity ->
        "Call " <> showText identity <> " has a blank identity, entity, or period."
    MissingGeneratedKey identity -> "Generating call " <> showText identity <> " has no key."
    UnexpectedGeneratedKey identity key ->
        "Query " <> showText identity <> " has a generated key " <> showText key <> "."
    CallEntityMismatch identity key ->
        "Call " <> showText identity <> " and generated key " <> showText key
        <> " belong to different entities."
    CallPeriodMismatch identity key ->
        "Call " <> showText identity <> " and generated key " <> showText key
        <> " belong to different periods."
    UnknownCallScope identity entity period ->
        "Call " <> showText identity <> " has unregistered scope " <> showText entity
        <> ", " <> showText period <> "."
    MissingFact key fact ->
        "Transaction " <> showText key <> " requires missing fact " <> showText fact <> "."
    MissingEvidence key evidence ->
        "Transaction " <> showText key <> " requires missing evidence " <> showText evidence <> "."
    InvalidEvidence key evidence amount ->
        "Transaction " <> showText key <> " has nonpositive evidence " <> showText evidence
        <> invalidAmount amount <> "."
    EvidenceMismatch key evidence (AmountMismatch expected actual) ->
        "Transaction " <> showText key <> " differs from evidence " <> showText evidence
        <> ": " <> mismatchAmounts expected actual <> "."
    InvalidEntry key errors ->
        "Transaction " <> showText key <> ": "
        <> Text.intercalate "; " (map explainEntryError (NonEmpty.toList errors))
    AccountOutsideVocabulary key account ->
        "Transaction " <> showText key <> " uses account " <> showText account
        <> " outside the vocabulary."
    DirectPostingForbidden key account ->
        "Raw transaction " <> showText key <> " posts protected account " <> showText account <> "."
    RawProfitEquityTransfer key ->
        "Raw transaction " <> showText key <> " transfers profit to equity."
    GeneratedAccountForbidden identity account ->
        "Call " <> showText identity <> " generated unauthorized account "
        <> showText account <> "."
    InvalidCatalogParameters identity detail ->
        "Call " <> showText identity <> " has invalid parameters: "
        <> explainParameter detail <> "."
    CatalogExecutionFailure identity detail ->
        "Call " <> showText identity <> " failed: " <> explainExecution detail <> "."
    StageRegression identity previous current ->
        "Call " <> showText identity <> " regresses from " <> showText previous
        <> " to " <> showText current <> "."
    UnresolvedReference identity key role reason ->
        "Call " <> showText identity <> " could not resolve " <> showText key
        <> " for role " <> showText role <> ": " <> explainReference reason <> "."
    DuplicateEffect identity key ->
        "Call " <> showText identity <> " duplicates the raw effect of " <> showText key <> "."
  where
    invalidAmount :: MoneyDecimal -> Text
    invalidAmount amount
        | discloseEvidenceAmounts disclosure = ": amount " <> showText amount
        | otherwise = ": amount withheld"
    mismatchAmounts :: MoneyDecimal -> MoneyDecimal -> Text
    mismatchAmounts expected actual
        | discloseEvidenceAmounts disclosure =
            "expected amount " <> showText expected <> ", submitted debit total " <> showText actual
        | otherwise = "expected amount withheld, submitted debit total " <> showText actual

-- | Name a phase and the checks it performs, in one nonempty line.
--
-- >>> explainAdmissionPhase EvidencePhase
-- "EvidencePhase: compare debit totals with declared evidence."
explainAdmissionPhase :: AdmissionPhase -> Text
explainAdmissionPhase phase = showText phase <> ": " <> checks phase
  where
    checks :: AdmissionPhase -> Text
    checks CoveragePhase = "check identities, registry coverage, parameters, and call order."
    checks InputPhase = "check every fact and raw entry, including direct-posting restrictions."
    checks InputPolicyPhase = "check the account vocabulary of checked sources."
    checks ReferencePhase = "check unknown and repeated references before executing calls."
    checks ExecutionPhase =
        "resolve references, execute calls in order, and check generated entries "
        <> "until a call fails."
    checks EvidencePhase = "compare debit totals with declared evidence."

-- | Explain a failure with its phase on the first line and its errors in their
-- original order on subsequent lines, with no trailing newline. Each error uses
-- the same disclosure as 'explainAdmissionError'.
--
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> import ExchangeAlgebra.Accounting.Transaction (CallId(..))
-- >>> let problem = DuplicateCallId (CallId "repeat")
-- >>> let failure = AdmissionFailure CoveragePhase (problem :| [])
-- >>> let phaseText = explainAdmissionPhase CoveragePhase
-- >>> let errorText = explainAdmissionError fullDisclosure problem
-- >>> let expected = [phaseText, errorText]
-- >>> Text.lines (explainAdmissionFailure fullDisclosure failure) == expected
-- True
explainAdmissionFailure :: Disclosure -> AdmissionFailure -> Text
explainAdmissionFailure disclosure failure = Text.intercalate "\n"
    (explainAdmissionPhase (failurePhase failure)
        : map (explainAdmissionError disclosure) (NonEmpty.toList (failureErrors failure)))

-- | Explain a trusted-side closing failure in one nonempty line. These failures
-- carry no evidence amounts, so no disclosure applies.
--
-- >>> import ExchangeAlgebra.Accounting.Transaction (TxId(..))
-- >>> explainClosingDerivationError (BlankClosingTransaction (TxId ""))
-- "Closing transaction identity TxId \"\" is blank."
explainClosingDerivationError :: ClosingDerivationError -> Text
explainClosingDerivationError problem = case problem of
    BlankClosingTransaction identity ->
        "Closing transaction identity " <> showText identity <> " is blank."
    AlreadyClosed key -> "Transaction " <> showText key <> " already closes this entity and period."
    ClosingKeyCollision key -> "Closing key " <> showText key <> " is already admitted."
    EliminationNotClosable key ->
        "Elimination transaction " <> showText key <> " cannot be included in an entity's closing."
    ClosingNotDerivable key base ->
        "Closing transaction " <> showText key <> " could not close the balance of "
        <> showText base <> "."
