{- |
Module      : ExchangeAlgebra.IO.Input.EInvoice
Description : Read JP PINT e-invoices into exchange algebra entries.

Role: the entry point of the package. Reading an invoice and building its
entry are separate steps, so the checked 'Invoice' can be inspected or used
with several base functions.

Layer: IO (input), outside the core package @exchangealgebra@, which this
package depends on through public modules only.

What a user writes: a base type with an @ExBaseClass (HatBase k)@ instance
whose account type has an @AccountVocabulary@ instance, and one function
@Invoice -> TermRole -> k@ for the side of the books in use. Then:

> case parseInvoice bytes of
>     Left errors   -> reportInvoiceErrors errors
>     Right invoice -> either reportJournalError useEntry (invoiceJournal spec invoice)
-}
module ExchangeAlgebra.IO.Input.EInvoice
    ( -- * Reading
      Invoice
    , InvoiceError(..)
    , parseInvoice
      -- * Invoice contents used by a base function
    , InvoiceLine(..)
    , TaxSubtotal(..)
    , TaxCategory(..)
    , TaxCategoryCode(..)
    , Percent(..)
    , Amount(..)
      -- * Building entries
    , InvoiceJournalSpec(..)
    , BookSide(..)
    , TaxAccounting(..)
    , TermRole(..)
    , InvoiceJournalError(..)
    , invoiceJournal
    ) where

import ExchangeAlgebra.IO.Input.EInvoice.Document
    ( Amount(..)
    , Invoice
    , InvoiceError(..)
    , InvoiceLine(..)
    , Percent(..)
    , TaxCategory(..)
    , TaxCategoryCode(..)
    , TaxSubtotal(..)
    , parseInvoice
    )
import ExchangeAlgebra.IO.Input.EInvoice.Journal
    ( BookSide(..)
    , InvoiceJournalError(..)
    , InvoiceJournalSpec(..)
    , TaxAccounting(..)
    , TermRole(..)
    , invoiceJournal
    )
