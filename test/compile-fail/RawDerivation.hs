module RawDerivation where

import ExchangeAlgebra.IO.Input.Admission

-- Public derivation requires the result of admission.
deriveRaw :: AdmissionJournal -> LedgerView
deriveRaw = deriveLedger
