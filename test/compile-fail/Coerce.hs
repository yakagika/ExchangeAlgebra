module Coerce where

import Data.Coerce (coerce)
import ExchangeAlgebra.IO.Input.Admission

-- A raw journal has no representational coercion to an admitted value.
forge :: AdmissionJournal -> Admitted
forge = coerce
