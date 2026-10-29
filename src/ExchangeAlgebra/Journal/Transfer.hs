{- |
Module      : ExchangeAlgebra.Journal.Transfer
Description : Carry entries across notes and read side totals.

This Journal-layer module re-exports carry operations from
"ExchangeAlgebra.Journal.Transfer.Rule". Start with 'carryEntries' to generate
additional entries that retain the original postings, or 'carryBefore' to
replace selected notes with a carry note. 'sideTotalsBy' accumulates each
side exactly and rounds its total once to 'Double'.

For note-aware accounting transfers, use @transferEntriesBy@ from
"ExchangeAlgebra.Accounting.Transfer". For closing, call
@Closing.closingEntries (toAlg journal)@ with a qualified import of
"ExchangeAlgebra.Accounting.Closing", then attach a note to the returned
entries with @(.|)@ from "ExchangeAlgebra.Journal.Core". Add those entries
to the original journal with @(.+)@.
-}
module ExchangeAlgebra.Journal.Transfer ( carryEntries
                                        , carryBefore
                                        , SideTotals(..)
                                        , sideTotal
                                        , CarryError(..)
                                        , sideTotalsBy
                                        ) where

import ExchangeAlgebra.Journal.Transfer.Rule
