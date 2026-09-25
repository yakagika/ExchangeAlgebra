{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Expressions that must fail when 'Posted' has no 'Num' instance.
module Ledger.NoNumPosted (literalPosted, addPosted) where

import ExchangeAlgebra.Ledger.Posting (Posted)

-- | Demand an integer literal at the protected posting type.
literalPosted :: Posted
literalPosted = 1

-- | Demand arithmetic at the protected posting type.
addPosted :: Posted -> Posted
addPosted value = value + 1
