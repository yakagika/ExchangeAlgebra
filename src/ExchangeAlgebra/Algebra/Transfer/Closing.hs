{-|
Module      : ExchangeAlgebra.Algebra.Transfer.Closing
Description : Shared closing posting construction.

This internal algebra module constructs a source reversal with a destination
posting. 'ExchangeAlgebra.Algebra.Transfer.Rule.closingEntries' and ledger
settlement select the closing side before calling 'closingPairBy'.
-}
module ExchangeAlgebra.Algebra.Transfer.Closing
    ( closingPairBy
    ) where

import           ExchangeAlgebra.Algebra
                     ( Alg
                     , HatVal
                     , HatBaseClass(..)
                     , ExBaseClass(..)
                     , AccountTitles
                     , Redundant((.+))
                     , (.@)
                     )

-- | Reverse a closing balance and post it to the given destination account.
-- True retains the source Hat/Not for the target; False reverses it. The caller
-- must classify the source, and the value must satisfy the non-negative,
-- finite posting contract of '.@'.
closingPairBy :: (HatVal v, ExBaseClass b)
              => Bool
              -> AccountTitles
              -> v
              -> b
              -> Alg v b
closingPairBy keepSide targetAccount value source =
    (value .@ revHat source)
        .+ (value .@ setAccountTitle targetSource targetAccount)
  where
    targetSource
        | keepSide = source
        | otherwise = revHat source
