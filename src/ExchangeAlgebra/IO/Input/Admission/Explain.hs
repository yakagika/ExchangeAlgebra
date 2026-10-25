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

import Data.Text (Text)

import ExchangeAlgebra.IO.Input.Admission.Diagnostic
    ( AdmissionError
    , AdmissionFailure
    , AdmissionPhase
    , ClosingDerivationError
    )
import ExchangeAlgebra.IO.Input.Admission.Submission (Disclosure)

-- | Explain one admission error in one nonempty line. Pass the
-- specification's @admissionDisclosure@: when it withholds evidence amounts,
-- the text omits the expected amount and the difference of an evidence
-- mismatch, keeping only the submitted debit total, and omits the amount of
-- an invalid evidence obligation. An
-- invalid entry is explained with "ExchangeAlgebra.IO.Input"'s
-- @explainEntryError@; fact and submitted amounts are never withheld.
-- The same disclosure and error always give the same text.
explainAdmissionError :: Disclosure -> AdmissionError -> Text
explainAdmissionError _ _ = undefined

-- | Explain a failure: the phase that stopped admission, then each error
-- under the same disclosure as 'explainAdmissionError', in order.
explainAdmissionFailure :: Disclosure -> AdmissionFailure -> Text
explainAdmissionFailure _ _ = undefined

-- | Name a phase and the checks it performs, in one nonempty line.
explainAdmissionPhase :: AdmissionPhase -> Text
explainAdmissionPhase _ = undefined

-- | Explain why closing could not be derived, in one nonempty line. Closing is
-- derived on the trusted side and these errors carry no evidence amounts, so
-- no disclosure applies.
explainClosingDerivationError :: ClosingDerivationError -> Text
explainClosingDerivationError _ = undefined
