{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : ExchangeAlgebra.IO.Input.EInvoice.Journal
Description : One balanced exchange algebra entry per checked invoice.

Role: maps a checked 'Invoice' to terms on the caller's base and builds the
entry. The caller's function chooses the base of every term, so the base may
have elements beyond the account (tax rate, counterparty, date).

Layer: IO (input). Depends on "ExchangeAlgebra.IO.Input.EInvoice.Document"
and on the public exchange algebra modules only.

Read order: 'InvoiceJournalSpec' and 'TermRole', then 'invoiceTerms' (the
terms with exact amounts, before a value type is chosen), then
'invoiceJournal'.

Algebra contract: every term's value is positive. The direction of a term is
its debit or credit side, which becomes the Hat or Not label through
@journalForSides@; a negative document amount gives a term with the absolute
value on the opposite side. Terms that share a base stay separate; nothing
here calls @bar@.
-}
module ExchangeAlgebra.IO.Input.EInvoice.Journal
    ( -- * Specification
      InvoiceJournalSpec(..)
    , BookSide(..)
    , TaxAccounting(..)
    , TermRole(..)
      -- * Terms with exact amounts
    , InvoiceTerm
    , termRole
    , termSide
    , termBase
    , termAmount
    , invoiceTerms
      -- * Entries
    , InvoiceJournalError(..)
    , invoiceJournal
    ) where

import Data.List.NonEmpty (NonEmpty)

import ExchangeAlgebra.Accounting.Account (Side(..))
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass, TitleOf)
import ExchangeAlgebra.Algebra.Base (HatBase)
import ExchangeAlgebra.Algebra.Core (Alg)
import ExchangeAlgebra.Algebra.Value (HatVal)
import ExchangeAlgebra.IO.Input (EntryErrorOf)
import ExchangeAlgebra.IO.Input.EInvoice.Document
    ( Invoice
    , InvoiceLine
    , TaxCategory
    , TaxSubtotal
    )

-- | Whose books the entry is for. The base function must match: for
-- 'SellerBook' it returns sales, consumption tax received, and accounts
-- receivable, not the buyer's accounts.
data BookSide
    = BuyerBook
      -- ^ positive line and tax amounts on the debit side, positive payable on
      -- the credit side
    | SellerBook
      -- ^ positive line and tax amounts on the credit side, positive payable on
      -- the debit side
    deriving (Eq, Show)

-- | Where a breakdown's tax amount goes. Source: the National Tax Agency
-- circular 「消費税法等の施行に伴う法人税の取扱いについて」 (直法2-1, 1989-03-01,
-- last amended 2023-12-27), items 1(7), 1(8), 2, and 3.
data TaxAccounting
    = TaxExclusiveAccounting
      -- ^ on the base the caller gives for 'TaxTerm' (consumption tax paid or
      -- received)
    | TaxInclusiveAccounting
      -- ^ on the base of the breakdown category's lines, as a separate term;
      -- rejected when a category with a nonzero rate has nonzero-amount lines
      -- on more than one base, because the invoice gives the tax amount per
      -- category only (specification 4.3.1)
    deriving (Eq, Show)

-- | What a term records. The caller's base function receives it.
data TermRole
    = LineTerm InvoiceLine      -- ^ the line amount (cost, asset, or sales)
    | TaxTerm TaxSubtotal       -- ^ the breakdown tax amount, under both 'TaxAccounting' values
    | SettlementTerm            -- ^ the payable amount (accounts payable or receivable)
    deriving (Eq, Show)

-- | How to turn an invoice into an entry. Under 'TaxInclusiveAccounting' the
-- base function is not called with 'TaxTerm'.
data InvoiceJournalSpec k = InvoiceJournalSpec
    { specBookSide      :: BookSide
    , specTaxAccounting :: TaxAccounting
    , specBase          :: Invoice -> TermRole -> k
    }

-- | One term before a value type is chosen: an exact, positive amount on a
-- side of a base. Only 'invoiceTerms' constructs it.
data InvoiceTerm k = InvoiceTerm
    { termRoleField   :: TermRole
    , termSideField   :: Side
    , termBaseField   :: k
    , termAmountField :: Rational
    }
    deriving (Eq, Show)

-- | Where the term's amount comes from.
termRole :: InvoiceTerm k -> TermRole
termRole = termRoleField

-- | 'Debit' or 'Credit'.
termSide :: InvoiceTerm k -> Side
termSide = termSideField

-- | The caller's base.
termBase :: InvoiceTerm k -> k
termBase = termBaseField

-- | The absolute value of the document amount; greater than zero.
termAmount :: InvoiceTerm k -> Rational
termAmount = termAmountField

-- | The terms of an invoice.
--
-- Failure classification: a checked operation. @Left@ lists the categories
-- that 'TaxInclusiveAccounting' cannot place.
--
-- Laws (exact equality; observation by the returned list and its accessors):
--
-- * Correspondence. Take the source amounts in this order: each line amount
--   in document order, each breakdown tax amount in document order, the
--   payable amount. The list has, in the same order, exactly one term for
--   each nonzero source amount and none for a zero one. That term's
--   'termRole' names its source ('LineTerm', 'TaxTerm', 'SettlementTerm'),
--   its 'termAmount' is the absolute value of the source amount, and its
--   'termBase' is @specBase invoice role@, except that under
--   'TaxInclusiveAccounting' a 'TaxTerm' has the base shared by the
--   category's nonzero-amount lines.
-- * Sides. For 'BuyerBook', a positive line or tax amount gives 'Debit' and a
--   positive payable gives 'Credit'; a negative source amount gives the other
--   side. With the same base function, 'SellerBook' gives the same list with
--   every side swapped.
-- * Balance. The amounts on 'Debit' and on 'Credit' have equal sums.
--   Precondition: the exact balance law of 'parseInvoice'.
-- * Accounting methods agree. The two 'TaxAccounting' values give lists of
--   the same length whose roles, sides, and amounts agree position by
--   position; only the bases of 'TaxTerm' terms may differ.
-- * Zero document. An invoice whose amounts are all zero gives @Right []@.
invoiceTerms
    :: Eq k
    => InvoiceJournalSpec k
    -> Invoice
    -> Either (NonEmpty TaxCategory) [InvoiceTerm k]
invoiceTerms _ _ = undefined

-- | Why 'invoiceJournal' rejects an invoice.
data InvoiceJournalError acct v
    = InclusiveTaxSplit (NonEmpty TaxCategory)
      -- ^ from 'invoiceTerms'
    | UnrepresentableAmount Int Rational
      -- ^ position in 'invoiceTerms' and the amount, which @v@ cannot hold exactly
    | EntryRejected (NonEmpty (EntryErrorOf acct v))
      -- ^ from @checkedEntryTextBy@ on the accounts of the bases; the row
      -- index is the position in 'invoiceTerms'. An all-zero invoice gives
      -- @EntryRejected (EmptyEntry :| [])@.
    deriving (Eq, Show)

-- | Build the entry of an invoice on the caller's base.
--
-- Each amount @q@ becomes @x :: v@ when @roundRational Nearest q == Just x@
-- and @exactAccum (addAccum x emptyAccum) == Just q@; otherwise the invoice
-- is rejected with 'UnrepresentableAmount' (for 'Double', 0.1 is rejected and
-- 1.5 is not), so the entry never holds an amount the invoice does not state.
-- The accounts of the bases (by @getAccountTitle@) are checked with
-- @checkedEntryTextBy@ in the ordinary journal context.
--
-- Failure classification: a checked operation.
--
-- Laws (observation by @toList@, @norm@, and @balance@):
--
-- * Agreement. On success the entry equals @journalForSides@ of the
--   'invoiceTerms' list with each amount converted as above; @toList@ of the
--   entry has one term per element of that list, also when two of them share
--   a base (@bar@ of the entry merges them, and also cancels opposite terms).
-- * Balance. @balance@ of the entry is 'True'; exact for every value type,
--   because the amounts are exact.
-- * Check agreement. The result is @Left (EntryRejected es)@ exactly when the
--   terms convert and @checkedEntryTextBy@ rejects them with accounts in
--   place of bases.
-- * Not applicable. When the caller's base has an element set that the
--   settlement does not have (such as the tax rate), the base function should
--   put the wildcard there, which in a ledger entry means not applicable; a
--   projection on a concrete value of that element set then omits the
--   settlement term.
invoiceJournal
    :: ( HatVal v
       , ExBaseClass (HatBase k)
       , TitleOf (HatBase k) ~ acct
       , ExBaseClass (HatBase acct)
       , TitleOf (HatBase acct) ~ acct
       )
    => InvoiceJournalSpec k
    -> Invoice
    -> Either (InvoiceJournalError acct v) (Alg v (HatBase k))
invoiceJournal _ _ = undefined
