{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified ExchangeAlgebra.Journal.Core as Journal
import ExchangeAlgebra.Journal.Core ((.|))
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import ExchangeAlgebra

-- | A journal computed with the algebra, keyed by transaction name.
journal :: Journal.Journal Text.Text MoneyDecimal (HatBase AccountTitles)
journal = ((100 .@ Not :< Cash .+ 100 .@ Not :< Sales) .| "sale")
       .+ ((30 .@ Not :< RentExpense .+ 30 .@ Hat :< Cash) .| "rent")

-- | Submit the computed journal, admit it, and derive the closing entries.
main :: IO ()
main = do
    let entity = Admission.EntityId "shop"
        period = Admission.PeriodId "2026"
        key name = Admission.TxKey entity period (Admission.TxId name)
        evidence name = Admission.EvidenceId (name <> "-total")
        rule name = Admission.txRule Admission.Required
            [Admission.SupplySubmission Admission.Ordinary] (Just (evidence name))
    postings <- either (fail . show) pure (Admission.journalPostings key journal)
    registry <- either (fail . show) pure $
        Admission.txIdRegistry [(key name, rule name) | name <- ["sale", "rent"]]
    let spec = Admission.AdmissionSpec
            { Admission.admissionRegistry = registry
            , Admission.admissionEvidence =
                Map.fromList [(evidence "sale", 100), (evidence "rent", 30)]
            , Admission.admissionFacts = Map.empty
            , Admission.admissionVocabulary = Set.fromList
                [Cash, Sales, RentExpense, NetIncome, NetLoss, RetainedEarnings]
            , Admission.admissionDisclosure = Admission.fullDisclosure
            }
    accepted <- either (fail . show . Admission.failureErrors) pure $
        Admission.admit spec (Admission.Submission postings [])
    closed <- either (fail . show) pure $
        Admission.closeAdmitted (Admission.TxId "close") accepted
    print (length (Journal.toMap (Admission.admittedJournal closed)))
