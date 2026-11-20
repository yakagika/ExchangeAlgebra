{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JournalSpec (runTests) where

import Data.Hashable (Hashable(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy(..))
import Data.Ratio ((%))
import qualified Data.Text as T
import Test.QuickCheck
    ( Property
    , conjoin
    , counterexample
    , forAll
    , property
    , (===)
    )

import ExchangeAlgebra.Accounting.Account
    ( AccountTitles(..)
    , PostingCapability(ClosingOnly)
    , ProcessingContext(..)
    , Side(..)
    )
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..), Exchange(balance), journalForSides)
import ExchangeAlgebra.Algebra.Base (Element(..), HatBase((:<)))
import ExchangeAlgebra.Algebra.Core (Alg, norm, toList)
import ExchangeAlgebra.Algebra.Value (HatVal, MoneyDecimal)
import ExchangeAlgebra.IO.Input (ConvErrorOf(..), EntryErrorOf(..), checkedEntryTextBy)
import ExchangeAlgebra.IO.Input.EInvoice.Document
import ExchangeAlgebra.IO.Input.EInvoice.Journal
import InvoiceFixture
import TestSupport

-- | Tax rate carried in the test base. A wildcard means no rate applies.
newtype Rate = Rate (Maybe Percent)
    deriving (Eq, Ord, Show)

instance Hashable Rate where
    hashWithSalt salt (Rate rate) = hashWithSalt salt (fmap (\(Percent value) -> value) rate)

instance Element Rate where
    wildcard = Rate Nothing

instance ExBaseClass (HatBase (AccountTitles, Rate)) where
    getAccountTitle (_ :< (account, _)) = account
    setAccountTitle (marker :< (_, rate)) account = marker :< (account, rate)

-- | The test base pairs an account with its tax rate.
type Key = (AccountTitles, Rate)

-- | The result of building one entry on the test base.
type Entry v = Either (InvoiceJournalError AccountTitles v) (Alg v (HatBase Key))

-- | Check the journal laws and the examples using parsed invoices.
runTests :: IO ()
runTests = do
    checkProperty "invoiceTerms / Correspondence" $ forAll genFixture $ \model ->
        withInvoice model $ \invoice -> withTerms buyer invoice $ \terms ->
            let expected =
                    [(role, specBase buyer invoice role, abs amount)
                    | (role, amount) <- sources invoice, amount /= 0]
            in [(termRole term, termBase term, termAmount term) | term <- terms] === expected
    checkProperty "invoiceTerms / Sides" $ forAll genFixture $ \model ->
        withInvoice model $ \invoice -> withTerms buyer invoice $ \purchase ->
            withTerms (buyer { specBookSide = SellerBook }) invoice $ \sale ->
                let expected = [positiveSide role amount
                               | (role, amount) <- sources invoice, amount /= 0]
                in conjoin
                    [ map termSide purchase === expected
                    , map observe sale === map (\t -> (termRole t, opposite (termSide t),
                                                       termBase t, termAmount t)) purchase
                    ]
    checkProperty "invoiceTerms / Balance" $ forAll genFixture $ \model ->
        withInvoice model $ \invoice -> withTerms buyer invoice $ \terms ->
            let total side = sum [termAmount term | term <- terms, termSide term == side]
            in total Debit === total Credit
    checkProperty "invoiceTerms / Accounting methods agree" $ forAll genFixture $ \model ->
        withInvoice model $ \invoice -> withTerms buyer invoice $ \exclusive ->
            withTerms inclusive invoice $ \included ->
                let expected term = case termRole term of
                        TaxTerm subtotal -> (Purchases, rateOf (subtotalCategory subtotal))
                        _                -> termBase term
                    withoutBase term = (termRole term, termSide term, termAmount term)
                in conjoin
                    [ map withoutBase included === map withoutBase exclusive
                    , map termBase included === map expected exclusive
                    ]
    checkProperty "invoiceTerms / Zero document" $ forAll genZeroFixture $ \model ->
        withInvoice model $ \invoice -> conjoin
            [ invoiceTerms buyer invoice === Right []
            , invoiceTerms inclusive invoice === Right []
            ]
    journalProperties "Double" (Proxy :: Proxy Double)
    journalProperties "MoneyDecimal" (Proxy :: Proxy MoneyDecimal)
    examples

-- | Check the journal laws for one value type.
journalProperties
    :: forall v. (HatVal v, Fractional v, Real v)
    => String
    -> Proxy v
    -> IO ()
journalProperties label _ = do
    checkProperty (law "Agreement") $ forAll genFixture $ \model ->
        withInvoice model $ \invoice -> conjoin
            [ withTerms spec invoice $ \terms ->
                case (testRows terms, invoiceJournal spec invoice :: Entry v) of
                    (Left (row, amount), actual) ->
                        actual === Left (UnrepresentableAmount row amount)
                    (Right [], actual) ->
                        actual === Left (EntryRejected (EmptyEntry :| []))
                    (Right rows, Right actual)   ->
                        let expected = journalForSides rows
                        in conjoin
                            [ toList actual === toList expected
                            , length (toList actual) === length terms
                            , norm actual === norm expected
                            ]
                    (Right _, Left errors)       -> counterexample (show errors) False
            | spec <- [buyer, inclusive, seller]
            ]
    checkProperty (law "Balance") $ forAll genFixture $ \model ->
        withInvoice model $ \invoice -> conjoin
            [ case invoiceJournal spec invoice :: Entry v of
                Right entry -> property (balance entry)
                Left errors -> case errors of
                    UnrepresentableAmount _ _          -> property True
                    EntryRejected (EmptyEntry :| [])   -> property True
                    _                                  -> counterexample (show errors) False
            | spec <- [buyer, inclusive, seller]
            ]
    checkProperty (law "Check agreement") $ forAll genFixture $ \model ->
        withInvoice model $ \invoice -> conjoin
            [ withTerms spec invoice $ \terms ->
                let actual = invoiceJournal spec invoice :: Entry v
                in case testRows terms of
                    Left (row, amount) -> actual === Left (UnrepresentableAmount row amount)
                    Right rows -> case directCheck rows of
                        Left errors -> actual === Left (EntryRejected errors)
                        Right _     -> case actual of
                            Left errors -> counterexample (show errors) False
                            Right _     -> property True
            | spec <- [buyer, inclusive, seller, buyer { specBase = invalidBase }]
            ]
  where
    law name = "invoiceJournal / " ++ name ++ " (" ++ label ++ ")"
    invalidBase invoice role = case role of
        SettlementTerm -> (IncomeSummary, wildcard)
        _              -> specBase buyer invoice role

-- | Construct expected rows using the round trip through Real and Fractional.
testRows
    :: (Fractional v, Real v)
    => [InvoiceTerm Key]
    -> Either (Int, Rational) [(Side, Key, v)]
testRows terms = traverse convert (zip [0 ..] terms)
  where
    convert (index, term)
        | toRational value == termAmount term = Right (termSide term, termBase term, value)
        | otherwise                           = Left (index, termAmount term)
      where
        value = fromRational (termAmount term)

-- | Submit the account part of each test row directly to the entry checker.
directCheck
    :: HatVal v
    => [(Side, Key, v)]
    -> Either (NonEmpty (EntryErrorOf AccountTitles v)) (Alg v (HatBase AccountTitles))
directCheck rows =
    checkedEntryTextBy resolve OrdinaryJournal
        [(sideText side, key, amount) | (key, (side, _, amount)) <- indexed]
  where
    indexed = zip (map (T.pack . ("term-" ++) . show) [0 :: Int ..]) rows
    accounts = [(key, account) | (key, (_, (account, _), _)) <- indexed]
    resolve key = maybe (Left (UnknownAccount key)) Right (lookup key accounts)
    sideText Debit  = "debit"
    sideText Credit = "credit"
    sideText Side   = "side"

-- | Check positive, negative, split, fractional, and zero invoice examples.
examples :: IO ()
examples = do
    let mixed = fixture [(reduced, 100), (standard, 200)]
        mixedItems = [(Debit, (Purchases, rateOf reduced), 100),
                      (Debit, (Purchases, rateOf standard), 200),
                      (Debit, (ConsumptionTaxPaid, rateOf reduced), 8),
                      (Debit, (ConsumptionTaxPaid, rateOf standard), 20),
                      (Credit, (AccountsPayable, wildcard), 328)]
    assert "8% and 10% invoice terms"
        (observedTerms buyer mixed) (== Right (Right mixedItems))
    let closingBase :: Invoice -> TermRole -> Key
        closingBase invoice role = case role of
            SettlementTerm -> (IncomeSummary, wildcard)
            _              -> specBase buyer invoice role
        closingSpec = buyer { specBase = closingBase }
        closingEntry = invoiceJournal closingSpec :: Invoice -> Entry MoneyDecimal
        expectedError = PostingNotAllowed 4 IncomeSummary ClosingOnly OrdinaryJournal
    assert "Check agreement / closing-only settlement account"
        (fmap closingEntry (parseFixture mixed)) $ \result -> case result of
            Right (Left (EntryRejected errors)) -> expectedError `elem` errors
            _                                  -> False
    let negative = fixture [(reduced, -100), (standard, -200)]
        reversed = [(opposite side, key, amount) | (side, key, amount) <- mixedItems]
    assert "Negative invoice without a reference reverses sides"
        (observedTerms buyer negative) (== Right (Right reversed))
    let split = inclusive { specBase = splitBase }
        taxed = fixture [(standard, 1000), (standard, 2000)]
        untaxed = fixture [(exempt, 1000), (exempt, 2000)]
    assert "InclusiveTaxSplit"
        (fmap (invoiceJournal split :: Invoice -> Entry MoneyDecimal) (parseFixture taxed))
        (== Right (Left (InclusiveTaxSplit (standard :| []))))
    let untaxedRows = [(Debit, (Purchases, rateOf exempt), 1000),
                       (Debit, (MerchandiseInventory, rateOf exempt), 2000),
                       (Credit, (AccountsPayable, wildcard), 3000)]
    assert "Tax-inclusive split bases with zero rate"
        (fmap (invoiceJournal split :: Invoice -> Entry MoneyDecimal) (parseFixture untaxed))
        (== Right (Right (journalForSides untaxedRows)))
    assert "UnrepresentableAmount / Double 0.1"
        (fmap (invoiceJournal buyer :: Invoice -> Entry Double)
            (parseFixture (fixture [(exempt, 1 % 10)])))
        (== Right (Left (UnrepresentableAmount 0 (1 % 10))))
    assert "Exact Double amount 1.5"
        (fmap (invoiceJournal buyer :: Invoice -> Entry Double)
            (parseFixture (fixture [(exempt, 3 % 2)])))
        (== Right (Right (journalForSides [(Debit, (Purchases, rateOf exempt), 1.5),
                                           (Credit, (AccountsPayable, wildcard), 1.5)])))
    let zero = parseFixture (fixture [(standard, 0)])
    assert "Zero invoice has no terms"
        (fmap (invoiceTerms buyer) zero) (== Right (Right []))
    assert "Zero invoice journal / EmptyEntry"
        (fmap (invoiceJournal buyer :: Invoice -> Entry Double) zero)
        (== Right (Left (EntryRejected (EmptyEntry :| []))))

-- | Assign the buyer accounts to a tax-exclusive entry.
buyer :: InvoiceJournalSpec Key
buyer = InvoiceJournalSpec BuyerBook TaxExclusiveAccounting (\_ role -> case role of
    LineTerm line    -> (Purchases, rateOf (lineTaxCategory line))
    TaxTerm subtotal -> (ConsumptionTaxPaid, rateOf (subtotalCategory subtotal))
    SettlementTerm   -> (AccountsPayable, wildcard))

-- | Use the buyer line bases for the tax breakdown terms.
inclusive :: InvoiceJournalSpec Key
inclusive = buyer { specTaxAccounting = TaxInclusiveAccounting }

-- | Assign the seller accounts to a tax-exclusive entry.
seller :: InvoiceJournalSpec Key
seller = InvoiceJournalSpec SellerBook TaxExclusiveAccounting (\_ role -> case role of
    LineTerm line    -> (Sales, rateOf (lineTaxCategory line))
    TaxTerm subtotal -> (ConsumptionTaxReceived, rateOf (subtotalCategory subtotal))
    SettlementTerm   -> (AccountsReceivable, wildcard))

-- | Assign a different account to line 2 of the split examples.
splitBase :: Invoice -> TermRole -> Key
splitBase invoice role = case role of
    LineTerm line | lineIdentifier line == "2" ->
        (MerchandiseInventory, rateOf (lineTaxCategory line))
    _ -> specBase buyer invoice role

-- | Put the category rate in the test base.
rateOf :: TaxCategory -> Rate
rateOf = Rate . categoryPercent

-- | Require the term stage to succeed before observing its result.
withTerms
    :: InvoiceJournalSpec Key
    -> Invoice
    -> ([InvoiceTerm Key] -> Property)
    -> Property
withTerms spec invoice check =
    case invoiceTerms spec invoice of
        Left errors -> counterexample (show errors) False
        Right terms -> check terms

-- | List the signed document amounts in their required term order.
sources :: Invoice -> [(TermRole, Rational)]
sources invoice =
    [(LineTerm row, amount) | row <- NE.toList (invoiceLines invoice)
                            , let Amount amount = lineAmount row]
    ++ [(TaxTerm row, amount) | row <- NE.toList (invoiceTaxSubtotals invoice)
                              , let Amount amount = subtotalTaxAmount row]
    ++ [(SettlementTerm, payable)]
  where
    Amount payable = totalPayable (invoiceTotals invoice)

-- | Compute the expected buyer side from the source and sign.
positiveSide :: TermRole -> Rational -> Side
positiveSide SettlementTerm amount
    | amount < 0 = Debit
    | otherwise  = Credit
positiveSide _ amount
    | amount < 0 = Credit
    | otherwise  = Debit

-- | Reverse the side for a seller or negative invoice.
opposite :: Side -> Side
opposite Debit  = Credit
opposite Credit = Debit
opposite Side   = Side

-- | Expose the role, side, base, and amount for comparison.
observe :: InvoiceTerm Key -> (TermRole, Side, Key, Rational)
observe term = (termRole term, termSide term, termBase term, termAmount term)

-- | Read example XML and return the terms as side, base, and amount triples.
observedTerms
    :: InvoiceJournalSpec Key
    -> Fixture
    -> Either (NonEmpty InvoiceError) (Either (NonEmpty TaxCategory) [(Side, Key, Rational)])
observedTerms spec model =
    fmap (fmap (map (\t -> (termSide t, termBase t, termAmount t))) . invoiceTerms spec)
         (parseFixture model)
