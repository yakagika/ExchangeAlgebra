module AdmissionFailureConstructor where

import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra.Base (HatBase)
import ExchangeAlgebra.Accounting.Account (AccountTitles)
import ExchangeAlgebra.Journal.Core (Journal)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

failureItems :: AdmissionFailure n b -> ()
failureItems (AdmissionFailure _) = ()
