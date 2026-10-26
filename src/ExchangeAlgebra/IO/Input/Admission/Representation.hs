{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Package-private representation of admitted journals. Only 'admit'
-- constructs 'Admitted'; the public module exports the type without its
-- constructor, and no instance rebuilds a value from its contents.
module ExchangeAlgebra.IO.Input.Admission.Representation where

import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.Journal.Core (Journal)

-- | A journal every transaction of which passed the checks of 'admit'.
data Admitted n b = Admitted (Journal n MoneyDecimal b)

-- | The admitted journal, unchanged: same notes, postings, and sequences.
-- Use it with the trial-balance, statement, and closing functions. A journal
-- built from it is not admitted again until it passes 'admit'.
admittedJournal :: Admitted n b -> Journal n MoneyDecimal b
admittedJournal (Admitted journal) = journal
