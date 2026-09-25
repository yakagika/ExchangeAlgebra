{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Expressions that must fail when 'Posted' has no 'Num' instance.
module Ledger.NoNumPosted (literalPosted, addPosted, addPosting, genericPosted) where

import ExchangeAlgebra.Ledger.Posting (Posted, Posting)
import GHC.Generics (from)

-- | Demand an integer literal at the protected posting type.
literalPosted :: Posted
literalPosted = 1

-- | Demand arithmetic at the protected posting type.
addPosted :: Posted -> Posted
addPosted value = value + 1

-- | Demand arithmetic at the protected posting container type.
addPosting :: Posting b -> Posting b
addPosting value = value + 1

-- | Demand a Generic representation of the protected posting value.
genericPosted :: Posted -> ()
genericPosted value = from value `seq` ()
