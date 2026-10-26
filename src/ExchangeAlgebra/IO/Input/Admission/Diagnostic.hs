{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Admission and evidence failures at the IO input boundary. This
-- package-private module defines the vocabulary; the public module
-- "ExchangeAlgebra.IO.Input.Admission" exports every type with its
-- constructors except 'AdmissionFailure'.
module ExchangeAlgebra.IO.Input.Admission.Diagnostic where

import Data.List.NonEmpty (NonEmpty)

import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | One accounting-integrity violation of one transaction. A transaction is
-- the element of one note @n@ of the submitted journal; @b@ is the base of the
-- offending posting. Every check needs only the transaction itself, not the
-- other transactions, opening balances, or the answer of an exercise.
data AdmissionError n b
    = NonPositiveAmount n b MoneyDecimal
      -- ^ A posting whose value is zero, negative, or not finite.
    | WildcardSide n b
      -- ^ A posting whose side is the wildcard 'HatNot'. A ledger posting is
      -- on 'Hat' or 'Not'; 'HatNot' is a query pattern.
    | NotPostableAccount n b
      -- ^ A posting on an account that no process may post, such as the
      -- wildcard 'AccountTitle' ('NotPostable').
    | Imbalanced n MoneyDecimal MoneyDecimal
      -- ^ The debit ('decL') and credit ('decR') totals of the transaction,
      -- in that order. Each total is the exact sum rounded once. Not reported
      -- for a transaction that has a 'WildcardSide' posting, whose side
      -- cannot be classified.
    deriving (Eq, Show)

-- | All violations found by one 'admit' run. The constructor is
-- package-private, so a value always holds at least one violation found by
-- 'admit'.
newtype AdmissionFailure n b = AdmissionFailure (NonEmpty (AdmissionError n b))
    deriving (Eq, Show)

-- | Violations in ascending note order; within a note, postings in ascending
-- base order, and 'Imbalanced' last.
failureErrors :: AdmissionFailure n b -> NonEmpty (AdmissionError n b)
failureErrors (AdmissionFailure errors) = errors

-- | Why one expected element did not match the selected journal. @m@ and @b@
-- are the note and base of the expected element.
data EvidenceMismatch m b
    = EvidenceMissing m b MoneyDecimal
      -- ^ No selected element matches the note and base. The value is the
      -- expected one.
    | EvidenceDiffers m b MoneyDecimal MoneyDecimal
      -- ^ Exactly one selected element matches the note and base, with a
      -- different value: expected, then found.
    | EvidenceAmbiguous m b [MoneyDecimal]
      -- ^ Two or more selected elements match the note and base; their values
      -- in ascending note and base order. Combine them into one element in
      -- the selection, by 'bar' for the two sides of one base, or by a
      -- transfer for different accounts.
    deriving (Eq, Show)
