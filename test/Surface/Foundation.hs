{-# LANGUAGE TypeFamilies #-}

-- | Compile-time lock for the public names exported by
-- "ExchangeAlgebra.Foundation".
module Surface.Foundation
    ( Element
          ( wildcard
          , haveWildcard
          , isWildcard
          , ignoreWildcard
          , equal
          , (.==)
          , (./=)
          , compareElement
          , (.<)
          , (.>)
          , (.<=)
          , (.>=)
          , maxElement
          , minElement
          )
    , AxisDecompose (toAxisKeys)
    , (.#)
    , Name
    , Subject
    , CountUnit
          ( Yen
          , Dollar
          , Euro
          , CNY
          , Amount
          , CountUnit
          )
    , BaseClass (compareBase)
    , Hat (Hat, Not, HatNot)
    , HatBaseClass
          ( BasePart
          , base
          , hat
          , merge
          , toHat
          , toNot
          , revHat
          , isHat
          , isNot
          , compareHatBase
          )
    , HatBase ((:<), _hat, _base)
    , HatVal (zeroValue, isZeroValue, isErrorValue, showValue)
    , Nearly (isNearly)
    , isNearlyNum
    , nearlyEqScaled
    , Alg (Zero, (:@), _val, _hatBase)
    , Redundant
          ( (.^)
          , (.-)
          , bar
          , compress
          , (.+)
          , (.*)
          , norm
          , (<+)
          )
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
    ) where

import           Prelude hiding (filter, map)

import           ExchangeAlgebra.Foundation
                     ( Element
                           ( wildcard
                           , haveWildcard
                           , isWildcard
                           , ignoreWildcard
                           , equal
                           , (.==)
                           , (./=)
                           , compareElement
                           , (.<)
                           , (.>)
                           , (.<=)
                           , (.>=)
                           , maxElement
                           , minElement
                           )
                     , AxisDecompose (toAxisKeys)
                     , (.#)
                     , Name
                     , Subject
                     , CountUnit
                           ( Yen
                           , Dollar
                           , Euro
                           , CNY
                           , Amount
                           , CountUnit
                           )
                     , BaseClass (compareBase)
                     , Hat (Hat, Not, HatNot)
                     , HatBaseClass
                           ( BasePart
                           , base
                           , hat
                           , merge
                           , toHat
                           , toNot
                           , revHat
                           , isHat
                           , isNot
                           , compareHatBase
                           )
                     , HatBase ((:<), _hat, _base)
                     , HatVal (zeroValue, isZeroValue, isErrorValue, showValue)
                     , Nearly (isNearly)
                     , isNearlyNum
                     , nearlyEqScaled
                     , Alg (Zero, (:@), _val, _hatBase)
                     , Redundant
                           ( (.^)
                           , (.-)
                           , bar
                           , compress
                           , (.+)
                           , (.*)
                           , norm
                           , (<+)
                           )
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
