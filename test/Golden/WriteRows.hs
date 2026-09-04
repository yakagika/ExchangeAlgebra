{-# LANGUAGE OverloadedStrings #-}

module Golden.WriteRows
    ( writeRowsFixtureDir
    , writeRowsFixtures
    , renderRows
    ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, fromGregorian)

import           ExchangeAlgebra
import           ExchangeAlgebra.Bookkeeping
import qualified ExchangeAlgebra.Journal as EJ
import           ExchangeAlgebra.Write
                     ( accountLedgerRows
                     , accountLedgerRowsJournal
                     , bsRows
                     , compoundTrialBalanceRows
                     , journalRows
                     , plRows
                     , postClosingTrialBalanceRows
                     , worksheetRows
                     )

type MinBase = HatBase AccountTitles
type MinTransaction = Alg MoneyDecimal MinBase
type ADBase = HatBase (AccountTitles, Day)
type ADTransaction = Alg MoneyDecimal ADBase

writeRowsFixtureDir :: FilePath
writeRowsFixtureDir = "test/fixtures/write-rows-0510"

baselineCommit :: Text
baselineCommit = "250cbf15d9663f163f98f0ce3650f057de80170d"

fixture :: Text -> [[Text]] -> Text
fixture description rows =
    "# write-rows-0510 " <> description
        <> "; schema 1; commit " <> baselineCommit <> "\n"
        <> renderRows rows

-- | Render rows as readable TSV, escaping cell content without using 'show'.
renderRows :: [[Text]] -> Text
renderRows = T.unlines . L.map (T.intercalate "\t" . L.map escapeCell)
  where
    escapeCell = T.replace "\n" "\\n"
               . T.replace "\t" "\\t"
               . T.replace "\\" "\\\\"

writeRowsFixtures :: [(FilePath, Text)]
writeRowsFixtures =
    [ ( "ebex6-compound-trial-balance.tsv"
      , fixture "compoundTrialBalanceRows ex6AllEntries"
          (compoundTrialBalanceRows ex6AllEntries)
      )
    , ( "ebex7-compound-trial-balance.tsv"
      , fixture "compoundTrialBalanceRows (foldr (.+) Zero (map snd ex7AllBalanced))"
          (compoundTrialBalanceRows (foldr (.+) Zero (L.map snd ex7AllBalanced)))
      )
    , ( "ebex8-compound-trial-balance.tsv"
      , fixture "compoundTrialBalanceRows (foldr (.+) Zero (map snd ex8SimpleBalanced))"
          (compoundTrialBalanceRows (foldr (.+) Zero (L.map snd ex8SimpleBalanced)))
      )
    , ( "ebex8-journal.tsv"
      , fixture "journalRows ex8CorrectionLedger ex8GetDay"
          (journalRows ex8CorrectionLedger ex8GetDay)
      )
    , ( "ebex8-account-ledger.tsv"
      , fixture "accountLedgerRows [Cash, AccountsReceivable, Sales] ex8CorrectionLedger ex8GetDay"
          (accountLedgerRows [Cash, AccountsReceivable, Sales]
              ex8CorrectionLedger ex8GetDay)
      )
    , ( "ebex8-account-ledger-journal.tsv"
      , fixture "accountLedgerRowsJournal [Cash, AccountsReceivable, Sales] ex8Journal"
          (accountLedgerRowsJournal [Cash, AccountsReceivable, Sales] ex8Journal)
      )
    , ( "ebex9-worksheet.tsv"
      , fixture "worksheetRows ex9PreAdjustment ex9Adjustments"
          (worksheetRows ex9PreAdjustment ex9Adjustments)
      )
    , ( "ebex9-post-closing-trial-balance.tsv"
      , fixture "postClosingTrialBalanceRows ex9AfterClosing"
          (postClosingTrialBalanceRows ex9AfterClosing)
      )
    , ( "ebex9-bs.tsv"
      , fixture "bsRows ex9AfterClosing" (bsRows ex9AfterClosing)
      )
    , ( "ebex9-pl.tsv"
      , fixture "plRows ex9PostAdjustment" (plRows ex9PostAdjustment)
      )
    ]

-- ebex6 第9-15章: 現金預金, 商品売買, 債権債務の全28仕訳 (元の順序を維持).
ex6OverShortOccur, ex6OverShortFound, ex6OverShortClose :: MinTransaction
ex6OverShortOccur =
       3000 .@ Not :< CashOverShort
    .+ 3000 .@ Hat :< Cash
ex6OverShortFound =
       2000 .@ Not :< CommunicationExpenses
    .+ 2000 .@ Hat :< CashOverShort
ex6OverShortClose =
       1000 .@ Not :< MiscellaneousLoss
    .+ 1000 .@ Hat :< CashOverShort

ex6PettyCashAdvance, ex6PettyCashReplenish, ex6Overdraft :: MinTransaction
ex6PettyCashAdvance =
       30000 .@ Not :< PettyCash
    .+ 30000 .@ Hat :< CurrentDeposits
ex6PettyCashReplenish =
       8000 .@ Not :< BusinessTrip
    .+ 5000 .@ Not :< CommunicationExpenses
    .+ 13000 .@ Hat :< CurrentDeposits
ex6Overdraft =
       70000 .@ Not :< Purchases
    .+ 40000 .@ Hat :< CurrentDeposits
    .+ 30000 .@ Not :< BankOverdraft

ex6Purchase, ex6PurchaseReturn, ex6Sale, ex6SalesReturn :: MinTransaction
ex6Purchase =
       120000 .@ Not :< Purchases
    .+ 120000 .@ Not :< AccountsPayable
ex6PurchaseReturn =
       20000 .@ Hat :< AccountsPayable
    .+ 20000 .@ Hat :< Purchases
ex6Sale =
       200000 .@ Not :< AccountsReceivable
    .+ 200000 .@ Not :< Sales
ex6SalesReturn =
       30000 .@ Hat :< Sales
    .+ 30000 .@ Hat :< AccountsReceivable

ex6AdvancePaid, ex6AdvanceReceived :: MinTransaction
ex6AdvancePaid =
       15000 .@ Not :< AdvancesPaid
    .+ 15000 .@ Hat :< Cash
ex6AdvanceReceived =
       25000 .@ Not :< Cash
    .+ 25000 .@ Not :< AdvancesReceived

ex6CreditSale, ex6CreditCollect :: MinTransaction
ex6CreditSale =
       96000 .@ Not :< CreditCardReceivable
    .+ 4000 .@ Not :< PaymentFees
    .+ 100000 .@ Not :< Sales
ex6CreditCollect =
       96000 .@ Not :< CurrentDeposits
    .+ 96000 .@ Hat :< CreditCardReceivable

ex6NotesReceivable, ex6NotesPayable, ex6ErReceivable :: MinTransaction
ex6NotesReceivable =
       80000 .@ Not :< NotesReceivable
    .+ 80000 .@ Not :< Sales
ex6NotesPayable =
       60000 .@ Not :< Purchases
    .+ 60000 .@ Not :< NotesPayable
ex6ErReceivable =
       50000 .@ Not :< ElectronicallyRecordedReceivable
    .+ 50000 .@ Hat :< AccountsReceivable

ex6ErObligation, ex6NotesLoanGiven, ex6NotesLoanTaken :: MinTransaction
ex6ErObligation =
       40000 .@ Hat :< AccountsPayable
    .+ 40000 .@ Not :< ElectronicallyRecordedObligations
ex6NotesLoanGiven =
       150000 .@ Not :< NotesLoansReceivable
    .+ 150000 .@ Hat :< Cash
ex6NotesLoanTaken =
       200000 .@ Not :< CurrentDeposits
    .+ 200000 .@ Not :< NotesLoansPayable

ex6PaymentOnBehalf, ex6DepositsReceived, ex6SuspensePayment :: MinTransaction
ex6PaymentOnBehalf =
       7000 .@ Not :< PaymentsOnBehalf
    .+ 7000 .@ Hat :< Cash
ex6DepositsReceived =
       180000 .@ Not :< WageExpenditure
    .+ 12000 .@ Not :< DepositsReceived
    .+ 168000 .@ Hat :< Cash
ex6SuspensePayment =
       40000 .@ Not :< SuspensePayments
    .+ 40000 .@ Hat :< Cash

ex6SuspensePaymentSettle, ex6SuspenseReceipt :: MinTransaction
ex6SuspensePaymentSettle =
       35000 .@ Not :< BusinessTrip
    .+ 5000 .@ Not :< Cash
    .+ 40000 .@ Hat :< SuspensePayments
ex6SuspenseReceipt =
       60000 .@ Not :< CurrentDeposits
    .+ 60000 .@ Not :< SuspenseReceipts

ex6SuspenseReceiptSettle, ex6OtherReceivable, ex6OtherPayable :: MinTransaction
ex6SuspenseReceiptSettle =
       60000 .@ Hat :< SuspenseReceipts
    .+ 60000 .@ Hat :< AccountsReceivable
ex6OtherReceivable =
       100000 .@ Not :< OtherReceivables
    .+ 100000 .@ Hat :< Fixtures
ex6OtherPayable =
       90000 .@ Not :< Fixtures
    .+ 90000 .@ Not :< OtherPayables

ex6AllEntries :: MinTransaction
ex6AllEntries =
       ex6OverShortOccur .+ ex6OverShortFound .+ ex6OverShortClose
    .+ ex6PettyCashAdvance .+ ex6PettyCashReplenish .+ ex6Overdraft
    .+ ex6Purchase .+ ex6PurchaseReturn .+ ex6Sale .+ ex6SalesReturn
    .+ ex6AdvancePaid .+ ex6AdvanceReceived
    .+ ex6CreditSale .+ ex6CreditCollect
    .+ ex6NotesReceivable .+ ex6NotesPayable
    .+ ex6ErReceivable .+ ex6ErObligation
    .+ ex6NotesLoanGiven .+ ex6NotesLoanTaken
    .+ ex6PaymentOnBehalf .+ ex6DepositsReceived
    .+ ex6SuspensePayment .+ ex6SuspensePaymentSettle
    .+ ex6SuspenseReceipt .+ ex6SuspenseReceiptSettle
    .+ ex6OtherReceivable .+ ex6OtherPayable

-- ebex7 第16-18章: 貸倒れ, 固定資産, 減価償却の仕訳.
ex7Mk :: MkBase MinBase
ex7Mk = (:<)

ex7WriteOffWithinAllowance, ex7WriteOffShortfall :: MinTransaction
ex7WriteOffWithinAllowance =
       40000 .@ Hat :< AllowanceForDoubtfulAccounts
    .+ 40000 .@ Hat :< AccountsReceivable
ex7WriteOffShortfall =
       50000 .@ Hat :< AllowanceForDoubtfulAccounts
    .+ 20000 .@ Not :< BadDebtLoss
    .+ 70000 .@ Hat :< AccountsReceivable

ex7WriteOffCurrentPeriod, ex7Recovery :: MinTransaction
ex7WriteOffCurrentPeriod =
       10000 .@ Not :< BadDebtLoss
    .+ 10000 .@ Hat :< AccountsReceivable
ex7Recovery =
       8000 .@ Not :< Cash
    .+ 8000 .@ Not :< RecoveryOfBadDebts

ex7AllowanceReplenish, ex7AllowanceReset :: MinTransaction
ex7AllowanceReplenish = allowanceReplenishmentEntry ex7Mk 30000 18000
ex7AllowanceReset = allowanceResetEntries ex7Mk 30000 18000

ex7Acquire, ex7SellWithGain, ex7SellWithLoss :: MinTransaction
ex7Acquire =
       520000 .@ Not :< Fixtures
    .+ 520000 .@ Hat :< CurrentDeposits
ex7SellWithGain =
       360000 .@ Hat :< AccumulatedDepreciation
    .+ 300000 .@ Not :< Cash
    .+ 600000 .@ Hat :< Fixtures
    .+ 60000 .@ Not :< GainOnSalesOfFixedAssets
ex7SellWithLoss =
       360000 .@ Hat :< AccumulatedDepreciation
    .+ 200000 .@ Not :< Cash
    .+ 40000 .@ Not :< LossOnSalesOfFixedAssets
    .+ 600000 .@ Hat :< Fixtures

ex7AnnualDepreciation :: MoneyDecimal
ex7AnnualDepreciation = 60000

ex7Indirect, ex7Direct, ex7Monthly :: MinTransaction
ex7Indirect = depreciationIndirectEntry ex7Mk ex7AnnualDepreciation
ex7Direct = depreciationDirectEntry ex7Mk ex7AnnualDepreciation Building
ex7Monthly = depreciationIndirectEntry ex7Mk (72000 * 6 `divDec` 12)
  where
    divDec a b = a / fromIntegral b

ex7AllBalanced :: [(String, MinTransaction)]
ex7AllBalanced =
    [ ("ex16_writeOffWithinAllowance", ex7WriteOffWithinAllowance)
    , ("ex16_writeOffShortfall", ex7WriteOffShortfall)
    , ("ex16_writeOffCurrentPeriod", ex7WriteOffCurrentPeriod)
    , ("ex16_recovery", ex7Recovery)
    , ("ex16_allowanceReplenish", ex7AllowanceReplenish)
    , ("ex16_allowanceReset", ex7AllowanceReset)
    , ("ex17_acquire", ex7Acquire)
    , ("ex17_sellWithGain", ex7SellWithGain)
    , ("ex17_sellWithLoss", ex7SellWithLoss)
    , ("ex18_indirect", ex7Indirect)
    , ("ex18_direct", ex7Direct)
    , ("ex18_monthly", ex7Monthly)
    ]

-- ebex8 第19-23章: 資本, 訂正仕訳, 経過勘定, 税の仕訳.
ex8Mk :: MkBase MinBase
ex8Mk = (:<)

ex8GetDay :: ADBase -> Day
ex8GetDay (_ :< (_, day)) = day

ex8D :: Integer -> Int -> Int -> Day
ex8D = fromGregorian

ex8WrongEntry, ex8CancelEntry, ex8CorrectEntry :: ADTransaction
ex8WrongEntry =
       45000 .@ Not :< (Cash, ex8D 2024 3 10)
    .+ 45000 .@ Not :< (Sales, ex8D 2024 3 10)
ex8CancelEntry = reversingEntry ex8WrongEntry
ex8CorrectEntry =
       45000 .@ Not :< (Cash, ex8D 2024 3 15)
    .+ 45000 .@ Hat :< (AccountsReceivable, ex8D 2024 3 15)

ex8CorrectionLedger :: ADTransaction
ex8CorrectionLedger = ex8WrongEntry .+ ex8CancelEntry .+ ex8CorrectEntry

ex8IssueStock, ex8Dividend, ex8PayDividend :: MinTransaction
ex8IssueStock =
       (200 * 5000) .@ Not :< CurrentDeposits
    .+ (200 * 5000) .@ Not :< CapitalStock
ex8Dividend =
       330000 .@ Hat :< RetainedEarnings
    .+ 300000 .@ Not :< UnpaidDividends
    .+ 30000 .@ Not :< LegalRetainedEarnings
ex8PayDividend =
       300000 .@ Hat :< UnpaidDividends
    .+ 300000 .@ Hat :< CurrentDeposits

ex8Prepaid, ex8Unearned, ex8AccruedRev, ex8AccruedExp :: MinTransaction
ex8Prepaid = prepaidExpenseEntry ex8Mk 12000 RentExpense
ex8Unearned = unearnedRevenueEntry ex8Mk 9000 RentalIncome
ex8AccruedRev = accruedRevenueEntry ex8Mk 6000 InterestEarned
ex8AccruedExp = accruedExpenseEntry ex8Mk 7500 InterestExpense

ex8Reversal, ex8ConsumptionTax, ex8CorpTaxInterim, ex8CorpTaxSettle :: MinTransaction
ex8Reversal = reversingEntry
    (ex8Prepaid .+ ex8Unearned .+ ex8AccruedRev .+ ex8AccruedExp)
ex8ConsumptionTax = consumptionTaxSettlementEntry ex8Mk 20000 35000
ex8CorpTaxInterim = corporateTaxInterimEntry ex8Mk 40000
ex8CorpTaxSettle = corporateTaxSettlementEntries ex8Mk 90000 40000

ex8SimpleBalanced :: [(String, MinTransaction)]
ex8SimpleBalanced =
    [ ("ex19_issueStock", ex8IssueStock)
    , ("ex19_dividend", ex8Dividend)
    , ("ex19_payDividend", ex8PayDividend)
    , ("ex21_prepaid", ex8Prepaid)
    , ("ex21_unearned", ex8Unearned)
    , ("ex21_accruedRev", ex8AccruedRev)
    , ("ex21_accruedExp", ex8AccruedExp)
    , ("ex21_reversal", ex8Reversal)
    , ("ex23_consumptionTax", ex8ConsumptionTax)
    , ("ex23_corpTaxInterim", ex8CorpTaxInterim)
    , ("ex23_corpTaxSettle", ex8CorpTaxSettle)
    ]

ex8Journal :: EJ.Journal String MoneyDecimal ADBase
ex8Journal = EJ.fromMap (HM.fromList
    [ ("20-1 wrong entry", ex8WrongEntry)
    , ("20-2 cancel entry", ex8CancelEntry)
    , ("20-3 correct entry", ex8CorrectEntry)
    ])

-- ebex9 第24-25章: 決算整理から財務諸表までの総合仕訳.
ex9Mk :: MkBase MinBase
ex9Mk = (:<)

ex9Opening :: MinTransaction
ex9Opening =
       500000 .@ Not :< Cash
    .+ 300000 .@ Not :< CurrentDeposits
    .+ 200000 .@ Not :< AccountsReceivable
    .+ 80000 .@ Not :< MerchandiseInventory
    .+ 600000 .@ Not :< Fixtures
    .+ 120000 .@ Not :< AccumulatedDepreciation
    .+ 4000 .@ Not :< AllowanceForDoubtfulAccounts
    .+ 150000 .@ Not :< AccountsPayable
    .+ 200000 .@ Not :< LoansPayable
    .+ 1000000 .@ Not :< CapitalStock
    .+ 206000 .@ Not :< RetainedEarnings

ex9T01, ex9T02, ex9T03, ex9T04, ex9T05 :: MinTransaction
ex9T01 = 400000 .@ Not :< Purchases .+ 400000 .@ Not :< AccountsPayable
ex9T02 = 900000 .@ Not :< AccountsReceivable .+ 900000 .@ Not :< Sales
ex9T03 = 500000 .@ Not :< Cash .+ 500000 .@ Hat :< AccountsReceivable
ex9T04 = 350000 .@ Hat :< AccountsPayable .+ 350000 .@ Hat :< CurrentDeposits
ex9T05 = 180000 .@ Not :< WageExpenditure .+ 180000 .@ Hat :< Cash

ex9T06, ex9T07, ex9T08, ex9T09, ex9T10 :: MinTransaction
ex9T06 = 120000 .@ Not :< RentExpense .+ 120000 .@ Hat :< Cash
ex9T07 = 40000 .@ Not :< Cash .+ 40000 .@ Not :< ReceiptFee
ex9T08 = 15000 .@ Not :< CommunicationExpenses .+ 15000 .@ Hat :< Cash
ex9T09 = 25000 .@ Not :< BusinessTrip .+ 25000 .@ Hat :< Cash
ex9T10 = 6000 .@ Not :< InterestExpense .+ 6000 .@ Hat :< Cash

ex9T11, ex9T12, ex9T13, ex9T14, ex9T15 :: MinTransaction
ex9T11 = 100000 .@ Not :< Fixtures .+ 100000 .@ Not :< OtherPayables
ex9T12 =
       3000 .@ Hat :< AllowanceForDoubtfulAccounts
    .+ 3000 .@ Hat :< AccountsReceivable
ex9T13 = 100000 .@ Not :< Cash .+ 100000 .@ Hat :< CurrentDeposits
ex9T14 = 8000 .@ Not :< SuppliesExpenses .+ 8000 .@ Hat :< Cash
ex9T15 = 12000 .@ Not :< UtilitiesExpense .+ 12000 .@ Hat :< Cash

ex9PeriodTransactions :: MinTransaction
ex9PeriodTransactions =
       ex9T01 .+ ex9T02 .+ ex9T03 .+ ex9T04 .+ ex9T05 .+ ex9T06
    .+ ex9T07 .+ ex9T08 .+ ex9T09 .+ ex9T10 .+ ex9T11 .+ ex9T12
    .+ ex9T13 .+ ex9T14 .+ ex9T15

ex9PreAdjustment :: MinTransaction
ex9PreAdjustment = ex9Opening .+ ex9PeriodTransactions

ex9AdjCOGS, ex9AdjDepreciation, ex9AdjAllowance, ex9AdjPrepaid :: MinTransaction
ex9AdjCOGS = cogsAdjustmentEntries ex9Mk 80000 110000
ex9AdjDepreciation = depreciationIndirectEntry ex9Mk 70000
ex9AdjAllowance = allowanceReplenishmentEntry ex9Mk 7000 1000
ex9AdjPrepaid = prepaidExpenseEntry ex9Mk 30000 RentExpense

ex9Adjustments :: MinTransaction
ex9Adjustments = ex9AdjCOGS .+ ex9AdjDepreciation .+ ex9AdjAllowance .+ ex9AdjPrepaid

ex9PostAdjustment :: MinTransaction
ex9PostAdjustment = ex9PreAdjustment .+ ex9Adjustments

ex9NominalLedger :: MinTransaction
ex9NominalLedger = ExchangeAlgebra.filter isNominal ex9PostAdjustment
  where
    isNominal x = let division = (whatDiv . _hatBase) x
                  in division == Cost || division == Revenue

ex9IncomeSummary :: MinTransaction
ex9IncomeSummary = incomeSummaryAccount ex9NominalLedger

ex9AfterClosing :: MinTransaction
ex9AfterClosing = realLedger .+ netIncomeTransfer
    (  projByAccountTitle NetIncome ex9IncomeSummary
    .+ projByAccountTitle NetLoss ex9IncomeSummary
    )
  where
    realLedger = ExchangeAlgebra.filter isReal ex9PostAdjustment
    isReal x = let division = (whatDiv . _hatBase) x
               in division == Assets || division == Liability || division == Equity
