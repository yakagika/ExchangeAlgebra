# Changelog for exchangealgebra-einvoice

## Unreleased

### Added

- First version. `parseInvoice` reads one UBL 2.1 `Invoice` in the JP PINT 1.1
  billing profile and checks what a journal entry needs: the document totals,
  the tax categories and rates, the tax amount of each category, and the form of
  the seller registration number. It is not a full JP PINT validator. An invoice
  outside the version 0.1 scope (a currency other than JPY, document-level
  allowances or charges, prepaid or rounding amounts, tax-inclusive line prices,
  or a `BillingReference`) is rejected, never read in part. Structural errors
  stop reading; consistency errors are reported together.
- `invoiceTerms` lists the terms of one invoice for the buyer's or the seller's
  books, under tax-exclusive or tax-inclusive accounting. A function supplied by
  the caller builds the base of each term, so the base may carry the tax rate or
  the counterparty besides the account. A negative amount becomes a term on the
  opposite side, so every value is non-negative, and terms with the same base
  are kept as separate terms.
- `invoiceJournal` builds one balanced entry from those terms. It rejects an
  amount that the value type cannot hold exactly and an account that does not
  accept an ordinary journal posting.
