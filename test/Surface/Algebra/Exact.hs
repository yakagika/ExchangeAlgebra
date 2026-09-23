-- | Compile-time lock for the public names exported by
-- "ExchangeAlgebra.Algebra.Exact".
module Surface.Algebra.Exact (
                             -- * Accumulators
                             ExactSum(..)
                             , ExactSumError(..)
                             , netAccum
                             , sumExact
                             -- * Algebra readouts
                             , normExact
                             , barExact
                             , projNetNormExact
                             , balanceMapByExact
                             , netPairMapByExact
                             , postFromNetByExact
                             -- * Accounting readouts
                             , diffRLExact
                             , balanceExact
                             , accountBalancesExact
                             ) where

import ExchangeAlgebra.Algebra.Exact (
                                     ExactSum(..)
                                     , ExactSumError(..)
                                     , netAccum
                                     , sumExact
                                     , normExact
                                     , barExact
                                     , projNetNormExact
                                     , balanceMapByExact
                                     , netPairMapByExact
                                     , postFromNetByExact
                                     , diffRLExact
                                     , balanceExact
                                     , accountBalancesExact
                                     )
