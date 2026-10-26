module PublicExplicitImportClient where

import ExchangeAlgebra.IO.Input.Admission
    ( Admitted
    , AdmissionFailure
    , admittedJournal
    , admit
    , checkEvidence
    )
import ExchangeAlgebra.Algebra.Base (HatBase, AccountTitles)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.Journal.Core (Journal)

-- | Explicit imports admit and observe an empty journal.
publicAdmission :: Either (AdmissionFailure String (HatBase AccountTitles))
                         (Admitted String (HatBase AccountTitles))
publicAdmission = admit (mempty :: Journal String MoneyDecimal (HatBase AccountTitles))

-- | Observation and evidence checking need only the public API.
publicEvidence :: Admitted String (HatBase AccountTitles) -> Bool
publicEvidence accepted = checkEvidence accepted id (admittedJournal accepted) == Right ()
