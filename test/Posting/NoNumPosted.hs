{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Expressions that must fail when 'Posted' has no 'Num' instance.
module Posting.NoNumPosted
    ( literalPosted
    , addPosted
    , addPosting
    , genericPosted
    ) where

import ExchangeAlgebra.Algebra.Posting (Posted, Posting)
import GHC.Generics (from)

-- | Demand an integer literal at the protected posting type.
literalPosted :: Posted Double
literalPosted = 1

-- | Demand arithmetic at the protected posting type.
addPosted :: Posted Double -> Posted Double
addPosted value = value + 1

-- | Demand arithmetic at the protected posting container type.
addPosting :: Posting Double b -> Posting Double b
addPosting value = value + 1

-- | Demand a Generic representation of the protected posting value.
genericPosted :: Posted Double -> ()
genericPosted value = from value `seq` ()
