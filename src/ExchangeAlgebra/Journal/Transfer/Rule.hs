{- |
Module      : ExchangeAlgebra.Journal.Transfer.Rule
Description : Note-preserving transfer entries and closing across notes.

Use a qualified import to distinguish these functions from the Algebra API:

> import qualified ExchangeAlgebra.Journal.Transfer.Rule as Transfer

Definition 9 and laws L1-L5 are documented in
"ExchangeAlgebra.Algebra.Transfer.Rule". Transfer lifts that operation over
notes; closing reads the combined ledger and returns an unannotated algebra
through 'Either'.
-}
module ExchangeAlgebra.Journal.Transfer.Rule
    ( transferEntries
    , closingEntries
    ) where

import qualified Data.HashMap.Strict as Map
import           ExchangeAlgebra.Algebra (Alg, HatVal, HatBaseClass, ExBaseClass)
import           ExchangeAlgebra.Algebra.Transfer.Rule (TransferRules, TransferApplyError)
import qualified ExchangeAlgebra.Algebra.Transfer.Rule as Rule
import           ExchangeAlgebra.Journal (Journal, Note, toMap, fromMap, toAlg)

-- | Generate additional entries under the same notes as their sources.
-- Each note is processed in the traversal order of 'toMap'. The first failure
-- aborts the whole operation; no partial Journal is returned. This order is
-- an implementation traversal, not a chronological ordering of notes.
-- Input posting values must be valid and finite. No @bar@ is applied.
transferEntries :: (Note n, HatVal v, HatBaseClass b)
                => TransferRules v b -> Journal n v b
                -> Either (TransferApplyError v b) (Journal n v b)
transferEntries rules journal =
    fromMap . Map.fromList <$> traverse apply (Map.toList (toMap journal))
  where
    apply (note, algebra) = do
        entries <- Rule.transferEntries rules algebra
        pure (note, entries)

-- | Read net balances across all notes and return unannotated closing entries.
-- The input must include only postings through the closing date; 'toAlg'
-- does not select a period. Earlier periods must already have their closing
-- entries included. Attach the settlement note with @(.|)@ at the call site.
-- Source sequences are folded per base by the Algebra operation, without
-- a tolerance or implicit @bar@; small floating-point residues can remain.
-- Finite, non-negative inputs normally yield 'Right'; 'Left' occurs only
-- when a side's sum exceeds the value type's range. The Algebra operation
-- checks totals and differences and reports the first failing base in
-- ascending order, with Hat/Not normalized to Not. No partial result is returned.
closingEntries :: (Note n, HatVal v, ExBaseClass b)
               => Journal n v b -> Either (TransferApplyError v b) (Alg v b)
closingEntries = Rule.closingEntries . toAlg
