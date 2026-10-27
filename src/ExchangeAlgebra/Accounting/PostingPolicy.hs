-- | Policies for admitting postings to bookkeeping accounts.
module ExchangeAlgebra.Accounting.PostingPolicy {-# DEPRECATED "Use ExchangeAlgebra.Accounting.Account instead." #-} 
    ( ProcessingContext(..), postingAllowedIn, postingCapabilityFor ) where

import ExchangeAlgebra.Accounting.Account.PostingPolicy
