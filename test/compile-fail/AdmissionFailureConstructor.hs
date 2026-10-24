module AdmissionFailureConstructor where

import ExchangeAlgebra.IO.Input.Admission

-- | The admission failure constructor must remain private.
failureStage :: AdmissionFailure -> AdmissionPhase
failureStage (AdmissionFailure phase _) = phase
