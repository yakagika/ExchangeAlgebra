module AdmissionFailureConstructor where

import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra.Base (HatBase, AccountTitles)
import ExchangeAlgebra.Journal.Core (Journal)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

failureItems :: AdmissionFailure n b -> ()
failureItems (AdmissionFailure _) = ()
