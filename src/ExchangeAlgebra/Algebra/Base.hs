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
    ( module ExchangeAlgebra.Algebra.Base
    , module ExchangeAlgebra.Algebra.Base.Account.Registry
    , module ExchangeAlgebra.Algebra.Base.Account.Types
    , module ExchangeAlgebra.Algebra.Base.Element) where

import ExchangeAlgebra.Algebra.Base.Representation as ExchangeAlgebra.Algebra.Base
    ( customError, BaseClass(..), HatBaseClass(..), Hat(..)
    , BaseForSingleHat(..), HatBase(..) )
import ExchangeAlgebra.Accounting.Account as ExchangeAlgebra.Algebra.Base
    ( PIMO(..), switchSide, defaultSide, classifyAccountDivision
    , pimoFromDivision, pimoFlip )
import ExchangeAlgebra.Algebra.Element as ExchangeAlgebra.Algebra.Base.Element
    hiding (matchesQuery)
import ExchangeAlgebra.Accounting.Account as ExchangeAlgebra.Algebra.Base.Element
    ( AccountTitles(..) )
import ExchangeAlgebra.Accounting.Account as ExchangeAlgebra.Algebra.Base.Account.Registry
    ( AccountSpec(..), AccountSemantics(..), accountAliases, accountSpec
    , accountSemantics, accountSpecMap, concreteAccountTitles
    , classifyAccountContra, accountDescriptions )
import ExchangeAlgebra.Accounting.Account as ExchangeAlgebra.Algebra.Base.Account.Types
    ( AccountDivision(..), Side(..), ClosingRule(..), FixedCurrent(..)
    , AccountRole(..), PostingCapability(..), DivisionSemantics(..)
    , HomeSideSemantics(..), ReportingEligibility(..) )

import ExchangeAlgebra.Accounting.Exchange as ExchangeAlgebra.Algebra.Base
    ( ExBaseClass(..)
    , AccountBase(..)
    )
