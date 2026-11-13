{-# LANGUAGE OverloadedStrings #-}

-- | Post the tax-rate summaries of a Japanese qualified invoice as a buyer.
-- This example models the amounts needed for posting, not every legal invoice field.
import Data.Text (Text)
import ExchangeAlgebra
import ExchangeAlgebra.IO.Input (journalFromSides)

-- | A buyer's entry in yen over the built-in account titles.
type Entry = Alg MoneyDecimal (HatBase AccountTitles)

-- | One invoice-wide tax-rate summary; both amounts are non-negative yen.
data TaxSummary = TaxSummary
    { netAmount :: MoneyDecimal -- ^ Sum of tax-exclusive prices for this rate.
    , taxAmount :: MoneyDecimal -- ^ Tax rounded once for this invoice and rate.
    }

-- | The issuer and the two tax-rate summaries used in this example.
-- Fixed fields ensure each rate occurs once, rather than once per product.
data Invoice = Invoice
    { issuer       :: Text       -- ^ Synthetic supplier name.
    , standardRate :: TaxSummary -- ^ The invoice's 10% summary.
    , reducedRate  :: TaxSummary -- ^ The invoice's reduced 8% summary.
    }

-- | Calculate tax from an invoice-wide net total and an exact decimal rate.
-- Inputs are non-negative; this example chooses to discard fractions of one yen.
-- Conversion to Rational only exposes the exact decimal product to Prelude.floor.
taxSummary :: MoneyDecimal -> MoneyDecimal -> TaxSummary
taxSummary rate net = TaxSummary
    { netAmount = net
    , taxAmount = fromInteger wholeYen
    }
  where
    wholeYen :: Integer
    wholeYen = floor $ toRational (net * rate)

-- | Post a purchase on credit using tax-exclusive accounting.
-- The invoice retains the rate breakdown; the entry explicitly totals purchases
-- and tax before constructing three postings. No existing postings are compressed.
invoiceToJournal :: Invoice -> Entry
invoiceToJournal invoice = journalFromSides
    [ (Debit, Purchases, net)
    , (Debit, ConsumptionTaxPaid, tax)
    , (Credit, AccountsPayable, net + tax)
    ]
  where
    net :: MoneyDecimal
    net = netAmount (standardRate invoice) + netAmount (reducedRate invoice)
    tax :: MoneyDecimal
    tax = taxAmount (standardRate invoice) + taxAmount (reducedRate invoice)

-- | Stop on an expectation failure, reporting both values.
-- Invariant: every manually specified expectation must match exactly.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual
    | expected == actual = pure ()
    | otherwise          = error
        ("InvoiceJournal invariant: " ++ label
            ++ ": expected " ++ show expected ++ ", got " ++ show actual)

-- | Check tax amounts, all three postings, and debit/credit totals, then report success.
verifyInvoice :: Invoice -> IO ()
verifyInvoice invoice = do
    -- 12345 * 0.10 = 1234.50 -> 1234; 6789 * 0.08 = 543.12 -> 543 yen.
    -- Net = 12345 + 6789 = 19134; tax = 1234 + 543 = 1777 yen.
    -- Debit = 19134 + 1777 = 20911; credit = 20911 yen.
    let entry :: Entry
        entry = invoiceToJournal invoice
        expected :: Entry
        expected = 19134 .@ Not :< Purchases
                .+  1777 .@ Not :< ConsumptionTaxPaid
                .+ 20911 .@ Not :< AccountsPayable
        debitTotal :: MoneyDecimal
        debitTotal = norm (decL entry)
        creditTotal :: MoneyDecimal
        creditTotal = norm (decR entry)
        standard = standardRate invoice
        reduced  = reducedRate invoice
    check "10% tax" 1234 (taxAmount standard)
    check "8% tax" 543 (taxAmount reduced)
    check "postings" expected entry
    check "debit total" 20911 debitTotal
    check "credit total" 20911 creditTotal
    check "debit equals credit" debitTotal creditTotal
    putStrLn
        ("InvoiceJournal: expected tax and balanced entry matched for " ++ show (issuer invoice))

-- | Build the two rate summaries before posting the synthetic invoice.
main :: IO ()
main = verifyInvoice Invoice
    { issuer       = "Example Supplier"
    , standardRate = taxSummary 0.10 12345
    , reducedRate  = taxSummary 0.08 6789
    }
