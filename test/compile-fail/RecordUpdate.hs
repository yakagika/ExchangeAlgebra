module RecordUpdate where

import ExchangeAlgebra.IO.Input.Admission

-- The observation function must not be a record selector.
replaceJournal :: Admitted -> Admitted
replaceJournal accepted = accepted { admittedJournal = mempty }
