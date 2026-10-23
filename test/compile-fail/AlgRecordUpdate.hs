module AlgRecordUpdate where

import ExchangeAlgebra.Algebra

-- Selectors remain public, but record updates must be rejected.
posting :: Alg Double (HatBase CountUnit)
posting = (1 .@ (Not :< Yen)) { _val = 2 }
