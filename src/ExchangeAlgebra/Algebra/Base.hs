{- |
    Module     : ExchangeAlgebra.Algebra.Base
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

-}

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DeriveGeneric              #-}

module ExchangeAlgebra.Algebra.Base
    ( customError
    , BaseClass(..)
    , HatBaseClass(..)
    , Hat(..)
    , BaseForSingleHat(..)
    , HatBase(..)
    , Element(..)
    , AxisKey(..)
    , axisIsWildcard
    , AxisDecompose(..)
    , (.#)
    , Name
    , Subject
    , CountUnit(..)
    , Hashable(..)
    , Generic
    ) where

import ExchangeAlgebra.Algebra.Base.Representation
    ( customError
    , BaseClass(..)
    , HatBaseClass(..)
    , Hat(..)
    , BaseForSingleHat(..)
    , HatBase(..)
    )
import ExchangeAlgebra.Algebra.Element
    ( Element(..)
    , AxisKey(..)
    , axisIsWildcard
    , AxisDecompose(..)
    , (.#)
    , Name
    , Subject
    , CountUnit(..)
    , Hashable(..)
    , Generic
    )
