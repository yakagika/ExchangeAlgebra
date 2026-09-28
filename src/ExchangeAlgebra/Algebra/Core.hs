{- | Generic exchange algebra operations.

Choose an operation by the question you need to answer:

@
Question                         Operation
Build a posting or sequence      .@, .+
Cancel opposing values           bar
Select bases                     proj, projNetNorm
Measure values                   norm, balanceBy
@

Import "ExchangeAlgebra.Accounting.Account" alongside this module when a
base uses account titles. The old "ExchangeAlgebra.Algebra" path also exposes
accounting operations during the transition, and
"ExchangeAlgebra.Algebra.Internal" exposes the raw representation.
-}
module ExchangeAlgebra.Algebra.Core
    ( Redundant(..)
    , Alg(Zero, (:@), _val, _hatBase)
    , isZero
    , (.@)
    , (<@)
    , vals
    , bases
    , fromList
    , toList
    , foldEntries
    , sigma
    , sigma2When
    , sigmaFromMap
    , toASCList
    , map
    , mapPosting
    , mapMaybePosting
    , mapBasePart
    , extendBy
    , filter
    , proj
    , projNetNorm
    , balanceBy
    , balanceMapBy
    , netPairMapBy
    , foldEntriesToMap
    , decBy
    , postFromNetBy
    , unionsMerge
    ) where

import Prelude hiding (map, filter)
import ExchangeAlgebra.Algebra.Core.Representation
