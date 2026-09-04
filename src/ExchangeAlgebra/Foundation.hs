{- |
    Module     : ExchangeAlgebra.Foundation
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    The redundant algebra itself, without any accounting vocabulary: the
    names that realise Definitions 1-6 of Akagi (2026), Appendix A
    (elements, bases, hat bases, the redundant algebra with its Hat, Bar,
    scalar and norm operations, and the base-generic projections and
    substitutions).

    Everything here is re-exported item by item from
    "ExchangeAlgebra.Algebra.Base.Element", "ExchangeAlgebra.Algebra.Base"
    and "ExchangeAlgebra.Algebra.Internal"; nothing is defined in this
    module, so a value built through this umbrella is the same type as one
    built through "ExchangeAlgebra.Algebra". What this umbrella deliberately
    leaves out is the accounting layer: 'ExchangeAlgebra.Accounting.ExBaseClass',
    account titles, sides, and transfers live in "ExchangeAlgebra.Accounting",
    and the account registry never appears here.

    Two notes on the surface:

    * 'Hat' has three constructors. 'Hat' and 'Not' are the two posting states
      of the paper (decrease / increase). 'HatNot' is the query wildcard
      (@'wildcard' :: 'Hat'@) used by 'proj' and by transfer patterns; it is
      not a third posting state and must not be used to build postings.
    * The laws of Definition 6 hold on the ℘-observation (the per-base
      multiset read through 'bar'); see the Haddock of 'mapBasePart' and
      'extendBy' for the raw / ℘ distinction.

    >>> import ExchangeAlgebra.Foundation
    >>> let x = 10 .@ Not :< Yen .+ 4 .@ Hat :< Yen :: Alg Double (HatBase CountUnit)
    >>> norm x
    14.0
    >>> norm (bar x)
    6.0
-}

module ExchangeAlgebra.Foundation
    ( -- * Definition 1: elements
      Element(..)
    , AxisDecompose(..)
    , (.#)
    , Name
    , Subject
    , CountUnit(..)
      -- * Definition 2: bases
    , BaseClass(..)
      -- * Definition 3: hat bases
    , Hat(..)
    , HatBaseClass(..)
    , HatBase(..)
      -- * Definition 4: value domain
    , HatVal(..)
    , Nearly(..)
    , isNearlyNum
    , nearlyEqScaled
      -- * Definitions 4-6: the redundant algebra
    , Alg(Zero, (:@), _val, _hatBase)
    , Redundant(..)
    , isZero
    , (.@)
    , (<@)
      -- ** Observation
    , vals
    , bases
    , toList
    , toASCList
    , foldEntries
    , foldEntriesToMap
      -- ** Construction
    , fromList
    , sigma
    , sigma2When
    , sigmaFromMap
    , unionsMerge
      -- ** Substitution and relabelling
    , map
    , mapPosting
    , mapMaybePosting
    , mapBasePart
    , extendBy
      -- ** Base-generic projection and decomposition
    , filter
    , proj
    , decBy
    , postFromNetBy
    ) where

import           Prelude hiding (map, filter)

import           ExchangeAlgebra.Algebra.Base.Element
                     ( Element(..)
                     , AxisDecompose(..)
                     , (.#)
                     , Name
                     , Subject
                     , CountUnit(..)
                     )
import           ExchangeAlgebra.Algebra.Base
                     ( BaseClass(..)
                     , Hat(..)
                     , HatBaseClass(..)
                     , HatBase(..)
                     )
import           ExchangeAlgebra.Algebra.Internal
                     ( HatVal(..)
                     , Nearly(..)
                     , isNearlyNum
                     , nearlyEqScaled
                     , Alg(Zero, (:@), _val, _hatBase)
                     , Redundant(..)
                     , isZero
                     , (.@)
                     , (<@)
                     , vals
                     , bases
                     , toList
                     , toASCList
                     , foldEntries
                     , foldEntriesToMap
                     , fromList
                     , sigma
                     , sigma2When
                     , sigmaFromMap
                     , unionsMerge
                     , map
                     , mapPosting
                     , mapMaybePosting
                     , mapBasePart
                     , extendBy
                     , filter
                     , proj
                     , decBy
                     , postFromNetBy
                     )
