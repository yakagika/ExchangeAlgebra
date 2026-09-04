{- |
    Module     : ExchangeAlgebra.Algebra.Readout.Net
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Read-outs that leave the redundant algebra. Every function here first
    passes through the 'ExchangeAlgebra.Algebra.bar' quotient (per-base
    netting) and then returns plain values, so the audit trail kept by the
    redundant sequence is discarded on purpose:

    * 'projNetNorm' \/ 'projNorm' (alias): the netted norm of a projection.
    * 'balanceBy': a __signed__ difference of two netted norms, the only
      place in the package where a value can be negative.
    * 'balanceMapBy' \/ 'netPairMapBy': keyed netted balances, signed and as
      non-negative (increase, decrease) pairs respectively.
    * 'projWithBaseNetNorm' \/ 'projWithNoteBaseNetNorm': the 'ExchangeAlgebra.Journal.Journal'
      counterparts of 'projNetNorm'.

    The names are re-exported unchanged from "ExchangeAlgebra.Algebra.Internal"
    and "ExchangeAlgebra.Journal"; this module only gives the lossy read-outs a
    namespace of their own, separate from the core that never nets implicitly.
    A signed read-out is a projection out of the algebra, not a group
    completion of it.
-}

module ExchangeAlgebra.Algebra.Readout.Net
    ( -- * Netted norms of an algebra
      projNetNorm
    , projNorm
      -- * Signed and paired balances
    , balanceBy
    , balanceMapBy
    , netPairMapBy
      -- * Netted norms of a journal
    , projWithBaseNetNorm
    , projWithNoteBaseNetNorm
    ) where

import           ExchangeAlgebra.Algebra.Internal
                     ( projNetNorm
                     , projNorm
                     , balanceBy
                     , balanceMapBy
                     , netPairMapBy
                     )
import           ExchangeAlgebra.Journal
                     ( projWithBaseNetNorm
                     , projWithNoteBaseNetNorm
                     )
