{-# LANGUAGE TypeOperators #-}

{- |
    Module     : ExchangeAlgebra.Algebra
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    'Alg' is abstract here: 'Zero' and '(:@)' can be constructed and matched,
    the multi-posting 'ExchangeAlgebra.Algebra.Internal.Liner' representation
    and its cache fields are reachable
    only through "ExchangeAlgebra.Algebra.Internal".

-}

module ExchangeAlgebra.Algebra
    ( module ExchangeAlgebra.Algebra.Base
    , Nearly(..)
    , isNearlyNum
    , nearlyEqScaled
    , Redundant(..)
    , Exchange(..)
    , HatVal(..)
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
    , projCredit
    , projDebit
    , projByAccountTitle
    , projNetNorm
    , projNorm
    , balanceBy
    , balanceMapBy
    , netPairMapBy
    , foldEntriesToMap
    , decBy
    , postFromNetBy
    , projCurrentAssets
    , projFixedAssets
    , projDeferredAssets
    , projCurrentLiability
    , projFixedLiability
    , projCapitalStock
    , projContraAssets
    , projContra
    , rounding
    , unionsMerge
    ) where

import Prelude hiding (map, filter)
import ExchangeAlgebra.Algebra.Internal
import ExchangeAlgebra.Algebra.Base
