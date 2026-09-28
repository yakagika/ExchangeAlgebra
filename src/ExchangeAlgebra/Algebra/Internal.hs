{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}

{- |
    Module     : ExchangeAlgebra.Algebra.Internal
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com
    Description : Internal representation of 'Alg' (all constructors, cache
                  fields and rebuild helpers). Not covered by the PVP contract;
                  import "ExchangeAlgebra.Algebra" instead unless you are
                  writing a test or an engine that must see 'Liner'.

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

-}


module ExchangeAlgebra.Algebra.Internal
    ( module ExchangeAlgebra.Algebra.Base
    , Nearly(..)
    , isNearlyNum
    , nearlyEqScaled
    , Redundant(..)
    , Exchange(..)
    , HatVal(..)
    , Pair(..)
    , Alg(..)
    , linerFromMap
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
    , unionsMerge)where

import ExchangeAlgebra.Algebra.Core.Representation
import ExchangeAlgebra.Algebra.Base
import ExchangeAlgebra.Algebra.Value.Class
import ExchangeAlgebra.Accounting.Exchange
    ( Exchange(..)
    , projCredit
    , projDebit
    , projByAccountTitle
    , projCurrentAssets
    , projFixedAssets
    , projDeferredAssets
    , projCurrentLiability
    , projFixedLiability
    , projCapitalStock
    , projContraAssets
    , projContra
    )
import Prelude hiding (map, filter)
