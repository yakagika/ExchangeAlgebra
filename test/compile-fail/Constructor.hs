module Constructor where

import ExchangeAlgebra.IO.Input.Admission

-- This constructor must not be available to a package client.
forge :: Admitted
forge = Admitted mempty mempty [] mempty mempty
