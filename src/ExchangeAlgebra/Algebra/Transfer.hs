{- |
Module      : ExchangeAlgebra.Algebra.Transfer
Description : Generic coordinate transfers and additional entries.

This Algebra-layer module re-exports generic transfer rules and coordinate
collapse from their shared representation. Start with 'mkTransferRules', then
apply 'transferEntries' to generate source cancellations and target postings
(Definition 9). Add the returned entries to the original ledger with @(.+)@;
the original postings remain. Only 'collapseNetEntries' applies @bar@.

For account-aware transfers, use "ExchangeAlgebra.Accounting.Transfer".
For closing entries, use "ExchangeAlgebra.Accounting.Closing".
For unit conversion, use @mapPosting@ from "ExchangeAlgebra.Algebra.Core".
-}
module ExchangeAlgebra.Algebra.Transfer ( TransferScale(Relabel)
                                        , TransferRule(..)
                                        , TransferRules
                                        , TransferRuleError(..)
                                        , TransferApplyError(..)
                                        , mkTransferRules
                                        , rulesToList
                                        , transferEntries
                                        , collapseEntries
                                        , collapseNetEntries
                                        ) where

import ExchangeAlgebra.Algebra.Transfer.Representation
