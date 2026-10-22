module AdmissionNoteConstructor where

import ExchangeAlgebra.IO.Input.Admission

-- The transaction-note constructor must remain private.
forge :: TxKey -> AdmissionNote
forge key = AdmissionNote key Ordinary (SubmissionProvenance Nothing)
