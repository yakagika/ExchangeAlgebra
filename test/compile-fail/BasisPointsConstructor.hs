module BasisPointsConstructor where

import qualified ExchangeAlgebra.IO.Input.Admission as Admission

-- | The checked rate constructor must remain private.
unvalidatedRate :: Admission.BasisPoints
unvalidatedRate = Admission.BasisPoints 1
