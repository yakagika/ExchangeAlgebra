-- | Translate public admission failures into repair instructions for the model.
-- This IO adapter retains structured details via Show after each instruction.
-- ModelRetry uses the resulting text as the next attempt's feedback.
module Feedback (notification) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified ExchangeAlgebra.IO.Input.Assist as Assist
import qualified ExchangeAlgebra.IO.Input.Admission as Admission

-- | Explain the repair location; trusted-side failures require operator action.
repair :: Admission.AdmissionError -> String
repair problem = case problem of
    Admission.UnknownTransaction _ -> "Use a registered txid from CALLS.md."
    Admission.SupplyNotAllowed _ _ _ -> "Use the registered raw/catalog supply route."
    Admission.FactOverwrite _ _ -> "Remove this transaction; the trusted side supplies it."
    Admission.MissingRequiredTransaction _ -> "Include this required txid in the full response."
    Admission.DuplicateRawTransaction _ -> "Keep one raw transaction with this txid."
    Admission.DuplicateGeneratedTransaction _ -> "Use each generated txid once."
    Admission.RawGeneratedCollision _ -> "Remove the raw entry for the catalog-generated txid."
    Admission.DuplicateCallId _ -> "Assign distinct call ids."
    Admission.InvalidCallIdentity _ -> "Use nonblank call id, entity, and period."
    Admission.MissingGeneratedKey _ -> "Supply the catalog call's generated txid."
    Admission.UnexpectedGeneratedKey _ _ -> "Remove the generated key from a query."
    Admission.CallEntityMismatch _ _ -> "Use entity roastery consistently."
    Admission.CallPeriodMismatch _ _ -> "Use period 2026-09 consistently."
    Admission.UnknownCallScope _ _ _ -> "Use the registered entity and period."
    Admission.MissingFact _ _ -> "Trusted configuration is incomplete; ask the operator."
    Admission.MissingEvidence _ _ -> "Trusted evidence is missing; ask the operator."
    Admission.InvalidEvidence _ _ _ -> "Trusted evidence is invalid; ask the operator."
    Admission.EvidenceMismatch _ _ _ ->
        "Match the independent debit total in CALLS.md; do not change the evidence."
    Admission.InvalidEntry _ problems -> "Repair this entry: " ++ unlines
        [Text.unpack (Assist.explainEntryError entryError) | entryError <- NonEmpty.toList problems]
    Admission.AccountOutsideVocabulary _ _ -> "Use only the declared account vocabulary."
    Admission.DirectPostingForbidden _ _ ->
        "Remove the protected raw account; use its catalog call."
    Admission.RawProfitEquityTransfer _ -> "Use FinalStock for the profit-to-equity closing."
    Admission.GeneratedAccountForbidden _ _ ->
        "Use catalog parameters whose generated accounts are in the vocabulary."
    Admission.InvalidCatalogParameters _ detail -> case detail of
        Admission.NonPositiveParameter _ _ -> "Use a strictly positive amount."
        Admission.NegativeParameter _ _ -> "Use a nonnegative amount."
        Admission.AccountOutsideDivision _ _ _ -> "Select an account in the required division."
        Admission.RateAboveLimit _ -> "Use a rate of at most 10000 basis points."
        Admission.ReceivedBelowPaid _ -> "Set received tax at least as high as paid tax."
        Admission.InterimExceedsTotal _ -> "Set interim tax no higher than total tax."
        Admission.BlankReferenceKey _ -> "Fill every coordinate of the referenced key."
        Admission.TooFewEntities _ -> "Supply at least two distinct entities."
        Admission.RepeatedEntity _ -> "Name each entity once."
        Admission.RepeatedReference _ -> "Reference each transaction once."
    Admission.CatalogExecutionFailure _ detail -> case detail of
        Admission.InexactQuotient _ _ -> "Use inputs whose quotient is an exact monetary decimal."
        Admission.ImbalancedConsolidation _ -> "Balance the selected consolidation entries."
        Admission.AbnormalBalance _ -> "Correct the account balance to its normal side."
        Admission.ClosingFailed _ -> "Closing could not close a balance; report this to the operator."
    Admission.StageRegression _ _ _ -> "Order calls by stage; put FinalStock last."
    Admission.UnresolvedReference _ _ _ _ ->
        "Use an available source with the required scope, role, provenance, and stage."
    Admission.DuplicateEffect _ _ -> "Remove the raw entry that repeats the catalog effect."

-- | Report all failures returned at the first failing admission stage.
-- EvidenceMismatch names the evidence's expected amount and the actual debit total.
notification :: Admission.AdmissionFailure -> String
notification failure = unlines $
    ("Rejected at " ++ show (Admission.failurePhase failure)
        ++ ". Resubmit the entire JSON object, not a patch.") :
    [repair problem ++ " Detail: " ++ show problem
    | problem <- NonEmpty.toList (Admission.failureErrors failure)]
