-- | Compatibility entry for checked postings. Import
-- "ExchangeAlgebra.Algebra.Posting" for new code.
module ExchangeAlgebra.Posting {-# DEPRECATED "Use ExchangeAlgebra.Algebra.Posting instead." #-}
    ( Posted, PostedError(..), postedUpperBound, posted, unPosted
    , PostSide(..), sideHat, Posting, entry, postingAlg
    ) where

import ExchangeAlgebra.Algebra.Posting
import ExchangeAlgebra.Algebra ()
