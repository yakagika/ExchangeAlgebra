{-|
Module      : ExchangeAlgebra.Algebra.Transfer.Closing
Description : Shared closing posting construction.

This internal Foundation module constructs a source reversal and destination
posting using the algebra. The transfer rules and Accounting settlement select
the destination side before calling 'closingPairBy'. Read 'closingPairBy' after
the source-side rules in the calling module.

The pair implements the posting construction used in Definition 9.
-}
module ExchangeAlgebra.Algebra.Transfer.Closing
    ( closingPairBy
    ) where

import ExchangeAlgebra.Algebra
    ( Alg
    , HatVal
    , HatBaseClass(..)
    , ExBaseClass(..)
    , AccountTitles
    , Redundant((.+))
    , (.@)
    )

-- | Reverse a closing balance and post it to the given destination account.
-- The first argument transforms the source to the target side: @id@ retains
-- Hat/Not, and 'revHat' reverses it. The caller must classify the source,
-- and the value must satisfy the non-negative,
-- finite posting contract of '.@'.
closingPairBy :: (HatVal v, ExBaseClass b)
              => (b -> b)
              -> AccountTitles
              -> v
              -> b
              -> Alg v b
closingPairBy targetSide targetAccount value source
    =  (value .@ revHat source)
    .+ (value .@ setAccountTitle (targetSide source) targetAccount)
