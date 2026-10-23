module AlgConstructor where

import ExchangeAlgebra.Algebra

-- Construction through the public pattern must be rejected.
posting :: Alg Double (HatBase CountUnit)
posting = 1 :@ (Not :< Yen)
