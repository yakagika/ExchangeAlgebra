{- |
Module      : ExchangeAlgebra.IO.Input.EInvoice.Document
Description : Checked invoice documents read from JP PINT UBL XML.

Role: the reading boundary for e-invoices. 'parseInvoice' reads one UBL 2.1
@Invoice@ in the JP PINT 1.1 profile and checks the conditions that a journal
entry needs; an 'Invoice' value exists only after these checks, so later
stages do not repeat them. It is not a full JP PINT validator: conformance to
every rule of the profile is the job of the official validation artifacts.

Layer: IO (input). Depends on xml-conduit and on no exchange algebra module.
"ExchangeAlgebra.IO.Input.EInvoice.Journal" turns an 'Invoice' into terms.

Read order: 'parseInvoice', then 'InvoiceError' for what is rejected, then the
accessors of 'Invoice'.

Scope of version 0.1 (an invoice outside it is rejected, never read in part):

* root element @Invoice@, @CustomizationID@ and @ProfileID@ of the JP PINT
  billing profile (aligned-ibrp-001-jp, -002-jp), @InvoiceTypeCode@ 380
  (aligned-ibrp-cl-02-jp);
* document currency JPY, so one @TaxTotal@ suffices (a foreign-currency
  qualified invoice needs a second @TaxTotal@ in JPY);
* no @TaxIncludedIndicator@ true, no document-level @AllowanceCharge@, and
  @AllowanceTotalAmount@, @ChargeTotalAmount@, @PrepaidAmount@ and
  @PayableRoundingAmount@ absent or zero (line-level allowances and charges are
  inside the line amount);
* no @BillingReference@. An invoice that refers to a preceding invoice either
  replaces it (a corrected invoice issued in full, JP PINT specification,
  Example 6) or adds to it (a negative invoice, specification 2.3.8), and the
  document has no element that tells which. Booking a replacement as an
  independent entry counts the purchase twice; reversing the preceding entry
  needs the preceding invoice, which one document does not contain. A negative
  invoice without a reference is read;
* the seller tax identifier (the @CompanyID@ of the @PartyTaxScheme@ whose
  scheme is @VAT@) present as @T@ and 13 ASCII digits. aligned-ibr-jp-01
  requires this form only for periods from 2023-10-01; version 0.1 requires it
  always. The form is checked, not the registration itself.

Amounts are signed: a negative invoice (specification 1.4, 1.5.2) keeps its
signs here. The sign becomes the debit or credit side in the journal stage, so
no negative value reaches an exchange algebra term.
-}
module ExchangeAlgebra.IO.Input.EInvoice.Document
    ( -- * Checked invoices
      Invoice
    , invoiceIdentifier
    , invoiceIssueDate
    , invoicePeriod
    , invoiceSeller
    , invoiceBuyer
    , invoiceLines
    , invoiceTaxSubtotals
    , invoiceTotals
      -- * Components
    , Seller(..)
    , Buyer(..)
    , Period(..)
    , InvoiceLine(..)
    , TaxSubtotal(..)
    , TaxCategory(..)
    , TaxCategoryCode(..)
    , MonetaryTotal(..)
    , Amount(..)
    , Percent(..)
    , RegistrationNumber
    , registrationNumberText
      -- * Reading UBL XML
    , InvoiceError(..)
    , ElementPath(..)
    , TotalRule(..)
    , parseInvoice
    ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Data.Time (Day)

-- | A signed monetary amount in JPY, exact as written.
newtype Amount = Amount Rational
    deriving (Eq, Ord, Show)

-- | A tax rate in percent, exact as written (for example 10, 8, or 1).
newtype Percent = Percent Rational
    deriving (Eq, Ord, Show)

-- | A qualified invoice issuer registration number: @T@ followed by 13 ASCII
-- digits. Only 'parseInvoice' constructs it.
newtype RegistrationNumber = RegistrationNumber Text
    deriving (Eq, Ord, Show)

-- | The registration number as written in the invoice.
registrationNumberText :: RegistrationNumber -> Text
registrationNumberText (RegistrationNumber number) = number

-- | Japanese tax category codes (codelist Aligned-TaxCategoryCodes, rule
-- aligned-ibrp-cl-01-jp). The code does not fix the rate: the rate is the
-- separate 'categoryPercent'.
data TaxCategoryCode
    = StandardRate      -- ^ @S@
    | LowerRate         -- ^ @AA@
    | Exempt            -- ^ @E@
    | FreeExportItem    -- ^ @G@
    | OutsideScope      -- ^ @O@
    deriving (Eq, Ord, Show)

-- | A tax category: the code and the rate. Every code except @O@ has a rate
-- (aligned-ibrp-048); @E@ and @G@ have rate 0 (aligned-ibrp-e-05, g-05); @O@
-- has none (aligned-ibrp-o-05). One tax breakdown exists per distinct category
-- (specification 4.3.1).
data TaxCategory = TaxCategory
    { categoryCode    :: TaxCategoryCode
    , categoryPercent :: Maybe Percent
    }
    deriving (Eq, Ord, Show)

-- | The seller. A qualified invoice always names the registration number.
data Seller = Seller
    { sellerName         :: Text
    , sellerRegistration :: RegistrationNumber
    }
    deriving (Eq, Show)

-- | The buyer.
newtype Buyer = Buyer
    { buyerName :: Text
    }
    deriving (Eq, Show)

-- | An invoice period or a line period. For a qualified invoice the period
-- carries the transaction date, which may differ from the issue date.
data Period = Period
    { periodStart :: Maybe Day
    , periodEnd   :: Maybe Day
    }
    deriving (Eq, Show)

-- | One invoice line.
data InvoiceLine = InvoiceLine
    { lineIdentifier  :: Text
    , lineItemName    :: Text
    , linePeriod      :: Maybe Period
    , lineAmount      :: Amount       -- ^ @LineExtensionAmount@, net of line allowances and charges
    , lineTaxCategory :: TaxCategory  -- ^ @Item/ClassifiedTaxCategory@
    }
    deriving (Eq, Show)

-- | One tax breakdown of the @TaxTotal@.
data TaxSubtotal = TaxSubtotal
    { subtotalCategory      :: TaxCategory
    , subtotalTaxableAmount :: Amount
    , subtotalTaxAmount     :: Amount
    }
    deriving (Eq, Show)

-- | Document totals (@LegalMonetaryTotal@ and the @TaxTotal@ amount).
data MonetaryTotal = MonetaryTotal
    { totalLineExtension :: Amount
    , totalTaxExclusive  :: Amount
    , totalTax           :: Amount
    , totalTaxInclusive  :: Amount
    , totalPayable       :: Amount
    }
    deriving (Eq, Show)

-- | A checked invoice. The constructor is not exported: every value comes
-- from 'parseInvoice' and satisfies the laws stated there.
data Invoice = Invoice
    { documentIdentifier   :: Text
    , documentIssueDate    :: Day
    , documentPeriod       :: Maybe Period
    , documentSeller       :: Seller
    , documentBuyer        :: Buyer
    , documentLines        :: NonEmpty InvoiceLine
    , documentTaxSubtotals :: NonEmpty TaxSubtotal
    , documentTotals       :: MonetaryTotal
    }
    deriving (Eq, Show)

-- | @cbc:ID@ of the invoice.
invoiceIdentifier :: Invoice -> Text
invoiceIdentifier = documentIdentifier

-- | @cbc:IssueDate@.
invoiceIssueDate :: Invoice -> Day
invoiceIssueDate = documentIssueDate

-- | @cac:InvoicePeriod@ of the document. Either it or a line period exists
-- (aligned-ibrp-052).
invoicePeriod :: Invoice -> Maybe Period
invoicePeriod = documentPeriod

-- | @cac:AccountingSupplierParty@.
invoiceSeller :: Invoice -> Seller
invoiceSeller = documentSeller

-- | @cac:AccountingCustomerParty@.
invoiceBuyer :: Invoice -> Buyer
invoiceBuyer = documentBuyer

-- | Invoice lines in document order.
invoiceLines :: Invoice -> NonEmpty InvoiceLine
invoiceLines = documentLines

-- | Tax breakdowns in document order, one per distinct 'TaxCategory'.
invoiceTaxSubtotals :: Invoice -> NonEmpty TaxSubtotal
invoiceTaxSubtotals = documentTaxSubtotals

-- | Document totals.
invoiceTotals :: Invoice -> MonetaryTotal
invoiceTotals = documentTotals

-- | Local names from the document root to an element, for error reports.
newtype ElementPath = ElementPath [Text]
    deriving (Eq, Show)

-- | Totals rules. The first five are those of PINT-UBL-validation without
-- allowances, charges, prepaid amount, or rounding amount, compared as those
-- rules compare; the last one is exact.
data TotalRule
    = LineExtensionRule  -- ^ ibr-co-10: line extension total = sum of line amounts
    | TaxExclusiveRule   -- ^ ibr-co-13: tax exclusive total = line extension total
    | TaxTotalRule       -- ^ ibr-co-14: tax total = sum of breakdown tax amounts
    | TaxInclusiveRule   -- ^ ibr-co-15: tax inclusive = tax exclusive + tax total
    | PayableRule        -- ^ ibr-co-16: payable = tax inclusive
    | ExactBalanceRule
      -- ^ sum of line amounts + sum of breakdown tax amounts = payable, unrounded
    deriving (Eq, Show)

-- | Why an invoice is rejected. Classification: external input rejected at
-- the reading boundary. Structural errors stop the reading; the consistency
-- errors from 'TotalMismatch' on are reported together.
data InvoiceError
    = MalformedXml Text                           -- ^ xml-conduit could not read the bytes
    | UnsupportedDocument Text                    -- ^ root other than UBL @Invoice@
    | UnsupportedSpecification ElementPath Text   -- ^ @CustomizationID@ or @ProfileID@
    | UnsupportedTypeCode Text                    -- ^ @InvoiceTypeCode@ other than 380
    | UnsupportedCurrency ElementPath Text        -- ^ currency code or @currencyID@ not JPY
    | MissingElement ElementPath
    | RepeatedElement ElementPath
    | UnsupportedElement ElementPath              -- ^ present but outside version 0.1
    | InvalidDecimal ElementPath Text
    | InvalidDate ElementPath Text
    | UnknownTaxCategory ElementPath Text
    | InvalidRegistrationNumber Text
    | DecimalTaxAmount ElementPath Text
      -- ^ a JPY tax amount written with a decimal point (aligned-ibr-jp-06)
    | TotalMismatch TotalRule Amount Amount       -- ^ stated, computed
    | TaxAmountOutOfRange TaxSubtotal             -- ^ aligned-ibrp-051-jp, specification 4.3.1
    | InvalidPercentForCategory TaxCategory       -- ^ aligned-ibrp-048, e-05, g-05, o-05
    | DuplicateSubtotal TaxCategory
    | LineCategoryWithoutSubtotal TaxCategory
    | SubtotalWithoutLines TaxCategory
    | TaxableAmountMismatch TaxCategory Amount Amount
      -- ^ stated taxable amount, sum of the category's line amounts (specification 4.3.1)
    deriving (Eq, Show)

-- | Read and check one invoice.
--
-- Input: the bytes of one XML document. Reading uses the xml-conduit defaults
-- except that internal entity declarations are ignored: UBL invoices declare
-- no entities, so a document that relies on one is rejected instead of
-- expanded; the five predefined XML entities are still decoded. The setting is
-- fixed so that every caller gets it. Elements are read along paths from the
-- root, never by descendant search, so a second @TaxTotal@ is seen and rejected
-- rather than mixed in.
--
-- Failure classification: external input rejected at the reading boundary.
--
-- Laws (each holds for every @Right invoice@ result; observation by the
-- accessors of 'Invoice'; exact equality on 'Rational' unless stated):
--
-- * Totals. @totalLineExtension@ is the sum of 'lineAmount' (ibr-co-10),
--   @totalTax@ the sum of 'subtotalTaxAmount' (ibr-co-14),
--   @totalTaxExclusive == totalLineExtension@,
--   @totalTaxInclusive == totalTaxExclusive + totalTax@ and
--   @totalPayable == totalTaxInclusive@, compared as the PINT-UBL-validation
--   rules compare.
-- * Exact balance. The sum of 'lineAmount' plus the sum of 'subtotalTaxAmount'
--   equals @totalPayable@ without rounding. The journal stage relies on this.
-- * Categories. The categories of the lines and of the breakdowns are the
--   same set, each breakdown category occurs once, and each breakdown's
--   'subtotalTaxableAmount' equals the sum of its lines' amounts
--   (specification 4.3.1 without document-level allowances or charges).
-- * Rates. 'categoryPercent' is present except for @O@, and is 0 for @E@ and
--   @G@.
-- * Tax amount. Each breakdown tax amount is written without a decimal point
--   (aligned-ibr-jp-06). For a category other than @O@ whose rate @p@ rounds,
--   half up as XPath @round@ does, to a nonzero integer:
--   @floor (t * p / 100) <= tax <= ceiling (t * p / 100)@ where @t@ is the
--   taxable amount (aligned-ibrp-051-jp). Otherwise the tax amount is zero.
-- * Registration. The seller's registration number matches @T[0-9]{13}@ in
--   ASCII.
parseInvoice
    :: LBS.ByteString
    -> Either (NonEmpty InvoiceError) Invoice
parseInvoice _ = undefined
