-- | Compile-time lock for the public names exported by
-- "ExchangeAlgebra.Journal.Exact".
module Surface.Journal.Exact (
                             -- * Accumulators
                             ExactSum(..)
                             , ExactSumError(..)
                             , netAccum
                             , sumExact
                             -- * Journal readouts
                             , normExact
                             , barExact
                             , balanceMapByExact
                             , netPairMapByExact
                             , postFromNetByExact
                             -- * Projections
                             , projNetNormExact
                             , projWithBaseNetNormExact
                             , projWithNoteBaseNetNormExact
                             -- * Accounting readouts
                             , diffRLExact
                             , balanceExact
                             , accountBalancesExact
                             ) where

import ExchangeAlgebra.Journal.Exact (
                                     ExactSum(..)
                                     , ExactSumError(..)
                                     , netAccum
                                     , sumExact
                                     , normExact
                                     , barExact
                                     , balanceMapByExact
                                     , netPairMapByExact
                                     , postFromNetByExact
                                     , projNetNormExact
                                     , projWithBaseNetNormExact
                                     , projWithNoteBaseNetNormExact
                                     , diffRLExact
                                     , balanceExact
                                     , accountBalancesExact
                                     )
