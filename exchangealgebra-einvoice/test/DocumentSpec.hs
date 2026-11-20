{-# LANGUAGE OverloadedStrings #-}

module DocumentSpec (runTests) where

import qualified Data.List.NonEmpty as NE
import Data.Ratio (denominator, (%))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (forAll)

import ExchangeAlgebra.IO.Input.EInvoice.Document
import InvoiceFixture
import TestSupport

-- | Check document laws and examples at the XML reading boundary.
runTests :: IO ()
runTests = do
    checkProperty "parseInvoice / Totals" $ forAll genFixture $ \model ->
        withInvoice model $ \invoice ->
            let net = sum (map snd (items model))
                tax = fromInteger (sum [q | (_, _, q) <- breakdown model])
            in invoiceTotals invoice ==
                MonetaryTotal (Amount net) (Amount net) (Amount tax)
                              (Amount (net + tax)) (Amount (net + tax))
    checkProperty "parseInvoice / Exact balance" $ forAll genFixture $ \model ->
        withInvoice model $ \invoice ->
            let net = sum (fmap (amountValue . lineAmount) (invoiceLines invoice))
                tax = sum (fmap (amountValue . subtotalTaxAmount) (invoiceTaxSubtotals invoice))
            in Amount (net + tax) == totalPayable (invoiceTotals invoice)
    checkProperty "parseInvoice / Categories" $ forAll genFixture $ \model ->
        withInvoice model $ \invoice ->
            let rows = NE.toList (invoiceLines invoice)
                subtotals = NE.toList (invoiceTaxSubtotals invoice)
                observed = [(lineTaxCategory row, amountValue (lineAmount row)) | row <- rows]
                codes = map subtotalCategory subtotals
                sumByCategory code = sum [q | (other, q) <- observed, other == code]
                matchesSum subtotal = subtotalTaxableAmount subtotal
                    == Amount (sumByCategory (subtotalCategory subtotal))
            in observed == items model
                && Set.fromList (map fst observed) == Set.fromList codes
                && Set.size (Set.fromList codes) == length codes
                && all matchesSum subtotals
    checkProperty "parseInvoice / Rates" $ forAll genFixture $ \model ->
        withInvoice model $ \invoice ->
            let codes = map lineTaxCategory (NE.toList (invoiceLines invoice))
                     ++ map subtotalCategory (NE.toList (invoiceTaxSubtotals invoice))
            in all validRate codes
    checkProperty "parseInvoice / Tax amount" $ forAll genFixture $ \model ->
        withInvoice model $ \invoice ->
            let subtotals = NE.toList (invoiceTaxSubtotals invoice)
                observed = [(subtotalCategory row, amountValue (subtotalTaxableAmount row),
                             amountValue (subtotalTaxAmount row)) | row <- subtotals]
                expected = [(category, taxable, fromInteger tax)
                           | (category, taxable, tax) <- breakdown model]
            in observed == expected && all validTaxAmount subtotals
    checkProperty "parseInvoice / Registration" $ forAll genFixture $ \model ->
        withInvoice model $ \invoice ->
            let number = registrationNumberText (sellerRegistration (invoiceSeller invoice))
            in number == registration model && T.length number == 14
                && T.take 1 number == "T"
                && T.all (\c -> '0' <= c && c <= '9') (T.drop 1 number)
    rejectionExamples
    readingExamples

-- | Extract the signed amount in JPY.
amountValue :: Amount -> Rational
amountValue (Amount value) = value

-- | Check the rate rules against a parsed category.
validRate :: TaxCategory -> Bool
validRate (TaxCategory code rate) = case code of
    OutsideScope    -> rate == Nothing
    Exempt          -> rate == Just (Percent 0)
    FreeExportItem  -> rate == Just (Percent 0)
    _               -> rate /= Nothing

-- | Check the permitted range and integrality of a parsed tax amount.
validTaxAmount :: TaxSubtotal -> Bool
validTaxAmount (TaxSubtotal category (Amount taxable) (Amount tax)) =
    denominator tax == 1 && case categoryPercent category of
        Just (Percent rate)
            | (floor (rate + 1 % 2) :: Integer) /= 0 ->
                let expected = taxable * rate / 100
                in tax `elem` map fromInteger [floor expected, ceiling expected]
        _ -> tax == 0

-- | Exercise every structural and consistency error constructor.
rejectionExamples :: IO ()
rejectionExamples = do
    reject "MalformedXml" "<Invoice>" $ \err -> case err of
        MalformedXml _ -> True
        _ -> False
    reject "UnsupportedDocument" "<Invoice/>" (== UnsupportedDocument "Invoice")
    reject "UnsupportedSpecification" (change "urn:peppol:pint:billing-1@jp-1" "other") $
        (== UnsupportedSpecification (ElementPath ["Invoice", "CustomizationID"]) "other")
    reject "UnsupportedTypeCode" (change ">380<" ">381<") (== UnsupportedTypeCode "381")
    reject "UnsupportedCurrency" (change "currencyID=\"JPY\">100" "currencyID=\"USD\">100") $
        \err -> case err of
            UnsupportedCurrency _ "USD" -> True
            _ -> False
    reject "MissingElement" (change "<cbc:IssueDate>2026-01-02</cbc:IssueDate>" "") $
        (== MissingElement (ElementPath ["Invoice", "IssueDate"]))
    reject "RepeatedElement" (insertRoot (aggregate "TaxTotal" "")) $
        (== RepeatedElement (ElementPath ["Invoice", "TaxTotal"]))
    reject "UnsupportedElement / BillingReference" (insertRoot (aggregate "BillingReference" "")) $
        (== UnsupportedElement (ElementPath ["Invoice", "BillingReference"]))
    reject "InvalidDecimal" (change ">100<" ">1e2<") $ \err -> case err of
        InvalidDecimal _ "1e2" -> True
        _ -> False
    reject "InvalidDate" (change "2026-01-02" "2026-02-30") $
        (== InvalidDate (ElementPath ["Invoice", "IssueDate"]) "2026-02-30")
    reject "UnknownTaxCategory" (change "<cbc:ID>S</cbc:ID>" "<cbc:ID>Z</cbc:ID>") $
        \err -> case err of
            UnknownTaxCategory _ "Z" -> True
            _ -> False
    reject "InvalidRegistrationNumber" (change "T0000000000000" "T000000000000０") $
        (== InvalidRegistrationNumber "T000000000000０")
    reject "DecimalTaxAmount" (change "</cbc:TaxableAmount><cbc:TaxAmount currencyID=\"JPY\">10<"
                                    "</cbc:TaxableAmount><cbc:TaxAmount currencyID=\"JPY\">10.0<") $
        \err -> case err of
            DecimalTaxAmount _ "10.0" -> True
            _ -> False
    reject "TotalMismatch" (change ">110</cbc:PayableAmount>" ">109</cbc:PayableAmount>") $
        (== TotalMismatch PayableRule (Amount 109) (Amount 110))
    reject "TaxAmountOutOfRange" (xml basic { breakdown = [(standard, 100, 11)] }) $
        (== TaxAmountOutOfRange (TaxSubtotal standard (Amount 100) (Amount 11)))
    let missingRate = TaxCategory StandardRate Nothing
    reject "InvalidPercentForCategory" (xml (fixture [(missingRate, 100)])) $
        (== InvalidPercentForCategory missingRate)
    reject "DuplicateSubtotal" (xml basic { breakdown = replicate 2 (standard, 100, 10) }) $
        (== DuplicateSubtotal standard)
    let mismatchedCategories = xml basic { breakdown = [(exempt, 100, 0)] }
    reject "LineCategoryWithoutSubtotal" mismatchedCategories
        (== LineCategoryWithoutSubtotal standard)
    reject "SubtotalWithoutLines" mismatchedCategories (== SubtotalWithoutLines exempt)
    reject "TaxableAmountMismatch" (xml basic { breakdown = [(standard, 99, 10)] }) $
        (== TaxableAmountMismatch standard (Amount 99) (Amount 100))
    let inconsistent =
            T.replace ">100</cbc:PayableAmount>" ">109</cbc:PayableAmount>" mismatchedCategories
    assert "Consistency errors accumulate" (parseText inconsistent) $
        \result -> case result of
            Left errors -> TotalMismatch ExactBalanceRule (Amount 109) (Amount 100) `elem` errors
                        && LineCategoryWithoutSubtotal standard `elem` errors
                        && SubtotalWithoutLines exempt `elem` errors
            Right _ -> False
  where
    change before after = T.replace before after (xml basic)

-- | Exercise lexical, currency, and namespace boundaries.
readingExamples :: IO ()
readingExamples = do
    assert "XPath negative half rounds toward positive infinity"
        (parseFixture (fixture [(exempt, -41 % 40)])) $ \result -> case result of
            Left errors ->
                TotalMismatch LineExtensionRule (Amount (-41 % 40)) (Amount (-51 % 50))
                    `elem` errors
            Right _ -> False
    assert "Decimal signed values and XML whitespace"
        (parseText (T.replace ">100<" "> \n+100.\t<" (xml basic))) isAccepted
    assert "Line-level Price and ItemPriceExtension currencies are ignored"
        (parseText (T.replace "<cac:Item>" (price <> extension <> "<cac:Item>") (xml basic)))
        isAccepted
    assert "Document tax total may be written with a decimal point"
        (parseText (T.replace "<cac:TaxTotal><cbc:TaxAmount currencyID=\"JPY\">10<"
                              "<cac:TaxTotal><cbc:TaxAmount currencyID=\"JPY\">10.0<" (xml basic)))
        isAccepted
    assert "Explicit zero document adjustments preserve 0.1 payable"
        (fmap (totalPayable . invoiceTotals) (parseText zeroAdjustments))
        (== Right (Amount (1 % 10)))
    reject "UnsupportedCurrency / TaxCurrencyCode"
        (insertRoot "<cbc:TaxCurrencyCode>USD</cbc:TaxCurrencyCode>") $ \err -> case err of
            UnsupportedCurrency (ElementPath ["Invoice", "TaxCurrencyCode"]) "USD" -> True
            _ -> False
    assert "Exact XML namespace matches independent of prefix"
        (parseText (T.replace "cbc:" "field:" (T.replace "xmlns:cbc" "xmlns:field" (xml basic))))
        isAccepted
    let withEntity = parseText (T.replace ">Supplier<" ">A&amp;B<" (xml basic))
    assert "Predefined XML entity is decoded" (fmap (sellerName . invoiceSeller) withEntity)
        (== Right "A&B")
    reject "Internal XML entity declaration cannot supply text"
        ("<!DOCTYPE Invoice [<!ENTITY name \"Supplier\">]>"
            <> T.replace ">Supplier<" ">&name;<" (xml basic)) $ \err -> case err of
                MalformedXml _ -> True
                _ -> False
  where
    price = aggregate "Price" "<cbc:PriceAmount currencyID=\"USD\">100</cbc:PriceAmount>"
    extension = aggregate "ItemPriceExtension"
        "<cbc:Amount currencyID=\"EUR\">100</cbc:Amount>"
    zeroAdjustments = T.replace "</cac:LegalMonetaryTotal>"
        (foldMap zeroAmount adjustmentFields <> "</cac:LegalMonetaryTotal>")
        (xml (fixture [(exempt, 1 % 10)]))
    adjustmentFields =
        ["AllowanceTotalAmount", "ChargeTotalAmount", "PrepaidAmount", "PayableRoundingAmount"]
    zeroAmount :: Text -> Text
    zeroAmount field = "<cbc:" <> field <> " currencyID=\"JPY\">0.0</cbc:" <> field <> ">"
    isAccepted = either (const False) (const True)

-- | Insert an element as a direct child of the example invoice.
insertRoot :: Text -> Text
insertRoot fragment = T.replace "</Invoice>" (fragment <> "</Invoice>") (xml basic)

-- | Require the given error to appear in the result.
reject :: String -> Text -> (InvoiceError -> Bool) -> IO ()
reject label bytes expected =
    assert label (parseText bytes) (either (any expected) (const False))
