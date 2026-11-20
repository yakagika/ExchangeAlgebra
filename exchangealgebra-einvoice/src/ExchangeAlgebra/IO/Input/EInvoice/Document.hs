{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Monad (guard, unless, when)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (traverse_)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, isJust)
import           Data.Ratio ((%))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, fromGregorianValid)
import qualified Text.XML as XML
import           Text.XML.Stream.Parse (psIgnoreInternalEntityDeclarations)

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

-- | Keep the path with each element so nested checks report their location.
data Located = Located ElementPath XML.Element

-- | Result of reading one part of the XML document.
type ReadInvoice a = Either InvoiceError a

-- | Read the structure and values used by the consistency checks.
readInvoice :: XML.Element -> ReadInvoice (Invoice, Bool)
readInvoice root = do
    unless (name == XML.Name "Invoice" (Just invoiceNamespace) Nothing)
        (Left (UnsupportedDocument (XML.nameLocalName name)))
    checkSpecification "CustomizationID" customization input
    checkSpecification "ProfileID" profile input
    kind <- requiredText (cbc "InvoiceTypeCode") input
    unless (kind == "380") (Left (UnsupportedTypeCode kind))
    required (cbc "DocumentCurrencyCode") input >>= checkCurrencyCode
    optional (cbc "TaxCurrencyCode") input >>= traverse_ checkCurrencyCode
    rejectPresent (cac "AllowanceCharge") input
    rejectPresent (cac "BillingReference") input
    identifier   <- requiredText (cbc "ID") input
    issueDate    <- required (cbc "IssueDate") input >>= readDate
    period       <- optional (cac "InvoicePeriod") input >>= traverse readPeriod
    supplier     <- required (cac "AccountingSupplierParty") input >>= required (cac "Party")
    customer     <- required (cac "AccountingCustomerParty") input >>= required (cac "Party")
    seller       <- readSeller supplier
    buyer        <- Buyer <$> partyName customer
    linesInOrder <- requiredMany (cac "InvoiceLine") input >>= traverse readLine
    unless (any (isJust . linePeriod) linesInOrder || isJust period)
        (Left (MissingElement (childPath (cac "InvoicePeriod") input)))
    tax <- required (cac "TaxTotal") input
    optional (cbc "TaxIncludedIndicator") tax >>= traverse_ checkTaxIncluded
    taxAmount    <- required (cbc "TaxAmount") tax >>= readMoney
    subtotals    <- requiredMany (cac "TaxSubtotal") tax >>= traverse readSubtotal
    legal        <- required (cac "LegalMonetaryTotal") input
    hasAllowance <- checkZeroAmount legal "AllowanceTotalAmount"
    hasCharge    <- checkZeroAmount legal "ChargeTotalAmount"
    traverse_ (checkZeroAmount legal) ["PrepaidAmount", "PayableRoundingAmount"]
    totals <- MonetaryTotal
        <$> (required (cbc "LineExtensionAmount") legal >>= readMoney)
        <*> (required (cbc "TaxExclusiveAmount") legal >>= readMoney)
        <*> pure taxAmount
        <*> (required (cbc "TaxInclusiveAmount") legal >>= readMoney)
        <*> (required (cbc "PayableAmount") legal >>= readMoney)
    pure (Invoice identifier issueDate period seller buyer linesInOrder subtotals totals,
          hasAllowance || hasCharge)
  where
    name = (XML.elementName root) { XML.namePrefix = Nothing }
    input = Located (ElementPath ["Invoice"]) root

    customization :: Text -> Bool
    customization value = any (`T.isPrefixOf` value)
        ["urn:fdc:peppol:jp:billing:3.0", "urn:peppol:pint:billing-1@jp-1"]

    profile :: Text -> Bool
    profile value = any (`T.isInfixOf` value)
        ["urn:fdc:peppol.eu:2017:poacc:billing:01:1.0", "urn:peppol:bis:billing"]

-- | Check a profile identifier using the matching rule for its field.
checkSpecification :: Text -> (Text -> Bool) -> Located -> ReadInvoice ()
checkSpecification field matches input = do
    element <- required (cbc field) input
    let value = scalar element
    unless (matches value) (Left (UnsupportedSpecification (location element) value))

-- | Require a currency code element to contain JPY.
checkCurrencyCode :: Located -> ReadInvoice ()
checkCurrencyCode element =
    unless (scalar element == "JPY")
        (Left (UnsupportedCurrency (location element) (scalar element)))

-- | Accept the XML boolean spellings for a false tax-included indicator.
checkTaxIncluded :: Located -> ReadInvoice ()
checkTaxIncluded element =
    unless (scalar element `elem` ["false", "0"]) (Left (UnsupportedElement (location element)))

-- | Require a zero amount when this optional element is present. Report its presence.
checkZeroAmount :: Located -> Text -> ReadInvoice Bool
checkZeroAmount legal field = do
    element <- optional (cbc field) legal
    traverse_ check element
    pure (isJust element)
  where
    check :: Located -> ReadInvoice ()
    check element = do
        Amount value <- readMoney element
        unless (value == 0) (Left (UnsupportedElement (location element)))

-- | Read the legal name and the registration number from the VAT scheme.
readSeller :: Located -> ReadInvoice Seller
readSeller party = do
    name    <- partyName party
    schemes <- traverse vatScheme (children (cac "PartyTaxScheme") party)

    scheme     <- exactlyOne (childPath (cac "PartyTaxScheme") party) (catMaybes schemes)
    identifier <- required (cbc "CompanyID") scheme

    let value = scalar identifier
    case T.uncons value of
        Just ('T', digits) | T.length digits == 13 && T.all asciiDigit digits ->
            pure (Seller name (RegistrationNumber value))
        _ -> Left (InvalidRegistrationNumber value)

  where
    vatScheme :: Located -> ReadInvoice (Maybe Located)
    vatScheme scheme = do
        taxScheme <- required (cac "TaxScheme") scheme
        code      <- requiredText (cbc "ID") taxScheme
        pure (scheme <$ guard (T.toUpper code == "VAT"))

-- | Read the registered name of a supplier or customer.
partyName :: Located -> ReadInvoice Text
partyName party =
    required (cac "PartyLegalEntity") party >>= requiredText (cbc "RegistrationName")

-- | Read the optional start and end dates of a document or line period.
readPeriod :: Located -> ReadInvoice Period
readPeriod element = Period
    <$> (optional (cbc "StartDate") element >>= traverse readDate)
    <*> (optional (cbc "EndDate") element >>= traverse readDate)

-- | Read the line net amount and the item classification.
readLine :: Located -> ReadInvoice InvoiceLine
readLine element = do
    identifier <- requiredText (cbc "ID") element
    amount     <- required (cbc "LineExtensionAmount") element >>= readMoney
    period     <- optional (cac "InvoicePeriod") element >>= traverse readPeriod
    item       <- required (cac "Item") element
    name       <- requiredText (cbc "Name") item
    category   <- required (cac "ClassifiedTaxCategory") item >>= readCategory
    pure (InvoiceLine identifier name period amount category)

-- | Read a tax breakdown. Its tax amount must be written without a decimal point.
readSubtotal :: Located -> ReadInvoice TaxSubtotal
readSubtotal element = do
    taxable <- required (cbc "TaxableAmount") element >>= readMoney
    tax     <- required (cbc "TaxAmount") element
    checkMoneyCurrency tax
    let text = scalar tax
        checkInteger =
            when (T.any (== '.') text) (Left (DecimalTaxAmount (location tax) text))
    amount   <- Amount <$> readDecimalWith checkInteger tax
    category <- required (cac "TaxCategory") element >>= readCategory
    pure (TaxSubtotal category taxable amount)

-- | Read the category code and its optional rate in percent.
readCategory :: Located -> ReadInvoice TaxCategory
readCategory element = do
    identifier <- required (cbc "ID") element
    code <- case scalar identifier of
        "S"   -> Right StandardRate
        "AA"  -> Right LowerRate
        "E"   -> Right Exempt
        "G"   -> Right FreeExportItem
        "O"   -> Right OutsideScope
        other -> Left (UnknownTaxCategory (location identifier) other)
    percent <- optional (cbc "Percent") element >>= traverse (fmap Percent . readDecimal)
    pure (TaxCategory code percent)

-- | Read a signed amount declared in JPY.
readMoney :: Located -> ReadInvoice Amount
readMoney element = checkMoneyCurrency element >> (Amount <$> readDecimal element)

-- | Require a JPY currency attribute on the monetary element being read.
checkMoneyCurrency :: Located -> ReadInvoice ()
checkMoneyCurrency (Located path element) =
    case Map.lookup (XML.Name "currencyID" Nothing Nothing) (XML.elementAttributes element) of
        Just "JPY"  -> Right ()
        Just other -> Left (UnsupportedCurrency path other)
        Nothing    -> Left (MissingElement path)

-- | Read a calendar date in the JP PINT year-month-day form.
readDate :: Located -> ReadInvoice Day
readDate element =
    case T.splitOn "-" value of
        [year, month, day]
            | T.length year == 4, T.length month == 2, T.length day == 2
            , T.all asciiDigit year, T.all asciiDigit month, T.all asciiDigit day
            , Just date <- fromGregorianValid
                (digitsValue year) (fromInteger (digitsValue month)) (fromInteger (digitsValue day))
            -> Right date
        _ -> Left (InvalidDate (location element) value)
  where
    value = scalar element

-- | Read a signed XML Schema decimal as an exact rational.
readDecimal :: Located -> ReadInvoice Rational
readDecimal = readDecimalWith (pure ())

-- | Run a lexical check on a valid decimal before constructing its rational value.
readDecimalWith :: ReadInvoice () -> Located -> ReadInvoice Rational
readDecimalWith check element
    | [whole] <- parts, nonemptyDigits whole = check >> pure (valueOf whole "")
    | [whole, fractional] <- parts
    , not (T.null whole && T.null fractional)
    , T.all asciiDigit whole
    , T.all asciiDigit fractional = check >> pure (valueOf whole fractional)
    | otherwise = Left (InvalidDecimal (location element) input)
  where
    input = scalar element
    (sign, unsigned) = case T.uncons input of
        Just ('-', rest) -> (-1, rest)
        Just ('+', rest) -> (1, rest)
        _                -> (1, input)
    parts = T.splitOn "." unsigned
    nonemptyDigits :: Text -> Bool
    nonemptyDigits digits = not (T.null digits) && T.all asciiDigit digits

    valueOf :: Text -> Text -> Rational
    valueOf whole fractional =
        (sign * digitsValue (whole <> fractional)) % (10 ^ T.length fractional)

-- | Recognize the digits accepted by XML Schema decimal and JP PINT registration numbers.
asciiDigit :: Char -> Bool
asciiDigit character = character >= '0' && character <= '9'

-- | Convert validated ASCII digits to an integer. The empty string represents zero.
digitsValue :: Text -> Integer
digitsValue = T.foldl' (\value digit -> 10 * value + toInteger (fromEnum digit - fromEnum '0')) 0

-- | Namespace URI of the UBL invoice root.
invoiceNamespace :: Text
invoiceNamespace = "urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"

-- | Construct a name in the UBL basic components namespace.
cbc :: Text -> XML.Name
cbc local = XML.Name local
    (Just "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2") Nothing

-- | Construct a name in the UBL aggregate components namespace.
cac :: Text -> XML.Name
cac local = XML.Name local
    (Just "urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2") Nothing

-- | Return the local names from the document root to the current element.
location :: Located -> ElementPath
location (Located path _) = path

-- | Append a child local name to the path of its parent.
childPath :: XML.Name -> Located -> ElementPath
childPath name (Located (ElementPath path) _) = ElementPath (path ++ [XML.nameLocalName name])

-- | Select immediate children by local name and namespace URI.
children :: XML.Name -> Located -> [Located]
children name parent@(Located _ element) =
    [ Located (childPath name parent) child
    | XML.NodeElement child <- XML.elementNodes element
    , (XML.elementName child) { XML.namePrefix = Nothing } == name
    ]

-- | Read a child that must occur exactly once.
required :: XML.Name -> Located -> ReadInvoice Located
required name parent = exactlyOne (childPath name parent) (children name parent)

-- | Require one match and report missing or repeated matches at the supplied path.
exactlyOne :: ElementPath -> [a] -> ReadInvoice a
exactlyOne path matches = case matches of
    []      -> Left (MissingElement path)
    [value] -> Right value
    _       -> Left (RepeatedElement path)

-- | Read one or more children in document order.
requiredMany :: XML.Name -> Located -> ReadInvoice (NonEmpty Located)
requiredMany name parent =
    maybe (Left (MissingElement (childPath name parent))) Right (NE.nonEmpty (children name parent))

-- | Read a child that can occur once or be absent.
optional :: XML.Name -> Located -> ReadInvoice (Maybe Located)
optional name parent = case children name parent of
    []      -> Right Nothing
    [value] -> Right (Just value)
    _       -> Left (RepeatedElement (childPath name parent))

-- | Reject the presence of an element that version 0.1 excludes.
rejectPresent :: XML.Name -> Located -> ReadInvoice ()
rejectPresent name parent =
    unless (null (children name parent)) (Left (UnsupportedElement (childPath name parent)))

-- | Read the text of a required child element.
requiredText :: XML.Name -> Located -> ReadInvoice Text
requiredText name parent = scalar <$> required name parent

-- | Read direct text content with leading and trailing XML whitespace removed.
scalar :: Located -> Text
scalar (Located _ element) =
    T.dropAround (`elem` [' ', '\t', '\r', '\n'])
        (T.concat [content | XML.NodeContent content <- XML.elementNodes element])

-- | Collect all consistency errors after the structure and values have been read.
consistencyErrors :: Bool -> Invoice -> [InvoiceError]
consistencyErrors hasAdjustments invoice =
    totalsErrors hasAdjustments invoice
    ++ [ TaxAmountOutOfRange subtotal | subtotal <- subtotals, not (validTax subtotal) ]
    ++ [ InvalidPercentForCategory category
       | category <- Set.toList (Set.union lineCategories subtotalCategories)
       , not (validPercent category)
       ]
    ++ [ DuplicateSubtotal category | (category, count) <- Map.toList subtotalCounts, count > 1 ]
    ++ map LineCategoryWithoutSubtotal
        (Set.toList (Set.difference lineCategories subtotalCategories))
    ++ map SubtotalWithoutLines
        (Set.toList (Set.difference subtotalCategories lineCategories))
    ++ [ TaxableAmountMismatch category taxable (Amount expected)
       | TaxSubtotal category taxable _ <- subtotals
       , Just expected <- [Map.lookup category lineSums]
       , taxable /= Amount expected
       ]
  where
    subtotals = NE.toList (invoiceTaxSubtotals invoice)
    lineSums = Map.fromListWith (+)
        [(lineTaxCategory line, value) | line <- NE.toList (invoiceLines invoice)
                                       , let Amount value = lineAmount line]
    subtotalCounts = Map.fromListWith (+)
        [(subtotalCategory subtotal, 1 :: Int) | subtotal <- subtotals]
    lineCategories     = Map.keysSet lineSums
    subtotalCategories = Map.keysSet subtotalCounts

-- | Compare stated and computed totals using the five PINT rules and exact balance.
totalsErrors :: Bool -> Invoice -> [InvoiceError]
totalsErrors hasAdjustments invoice =
    [ TotalMismatch rule stated (Amount computed)
    | (rule, stated, computed) <-
        [ (LineExtensionRule, totalLineExtension totals, roundMoney linesTotal)
        , (TaxExclusiveRule, totalTaxExclusive totals, expectedExclusive)
        , (TaxTotalRule, totalTax totals, roundMoney taxTotal)
        , (TaxInclusiveRule, totalTaxInclusive totals, roundMoney (exclusive + statedTax))
        , (PayableRule, totalPayable totals, value (totalTaxInclusive totals))
        , (ExactBalanceRule, totalPayable totals, linesTotal + taxTotal)
        ]
    , stated /= Amount computed
    ]
  where
    totals = invoiceTotals invoice
    linesTotal = sum (fmap (value . lineAmount) (invoiceLines invoice))
    taxTotal = sum (fmap (value . subtotalTaxAmount) (invoiceTaxSubtotals invoice))
    extension = value (totalLineExtension totals)
    expectedExclusive
        | hasAdjustments = roundMoney extension
        | otherwise      = extension
    exclusive = value (totalTaxExclusive totals)
    statedTax = value (totalTax totals)

    value :: Amount -> Rational
    value (Amount amount) = amount

-- | XPath round chooses the integer toward positive infinity at a tie.
xpathRound :: Rational -> Integer
xpathRound amount = floor (amount + 1 % 2)

-- | Round a monetary total to two decimal places using XPath tie handling.
roundMoney :: Rational -> Rational
roundMoney amount = xpathRound (100 * amount) % 100

-- | Check whether the rate is present and allowed for the category.
validPercent :: TaxCategory -> Bool
validPercent (TaxCategory code rate) = case code of
    OutsideScope    -> rate == Nothing
    Exempt          -> rate == Just (Percent 0)
    FreeExportItem  -> rate == Just (Percent 0)
    StandardRate    -> isJust rate
    LowerRate       -> isJust rate

-- | Check the signed tax amount against the range for the category and taxable amount.
validTax :: TaxSubtotal -> Bool
validTax (TaxSubtotal (TaxCategory code rate) (Amount taxable) (Amount tax))
    | code /= OutsideScope, Just (Percent percent) <- rate, xpathRound percent /= 0 =
        let expected = taxable * percent / 100
        in fromInteger (floor expected) <= tax && tax <= fromInteger (ceiling expected)
    | otherwise = tax == 0

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
parseInvoice bytes = do
    document <- case XML.parseLBS settings bytes of
        Left err    -> Left (NE.singleton (MalformedXml (T.pack (show err))))
        Right value -> Right value
    (invoice, hasAdjustments) <-
        either (Left . NE.singleton) Right (readInvoice (XML.documentRoot document))
    case NE.nonEmpty (consistencyErrors hasAdjustments invoice) of
        Nothing     -> Right invoice
        Just errors -> Left errors
  where
    settings :: XML.ParseSettings
    settings = XML.def { psIgnoreInternalEntityDeclarations = True }
