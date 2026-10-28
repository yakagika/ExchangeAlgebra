{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import ExchangeAlgebra
import qualified ExchangeAlgebra.Journal as EJ
import ExchangeAlgebra.Journal ((.|))
import qualified ExchangeAlgebra.Accounting.Audit as Audit
import ExchangeAlgebra.Accounting.Audit
    ( AuditResult(..)
    , Finding(..)
    )
import ExchangeAlgebra.Accounting.TrialBalance.Balance
    ( AccountBalance(..)
    , TrialBalance
    )

-- | A journal keyed by transaction identifiers, as an LLM submits it.
type Ledger = EJ.Journal Text MoneyDecimal (HatBase AccountTitles)

-- | Fail with the label unless the audit returns the expected result.
expect
    :: (Eq s, Eq f, Show s, Show f)
    => String
    -> AuditResult s f
    -> AuditResult s f
    -> IO ()
expect label actual expected =
    unless (actual == expected) $
        fail (label <> ": " <> show actual)

-- * Subject 1: trial balances after adjustment and after closing

-- | After adjustment: a current-account overdraft and an unresolved
-- suspense payment remain. Both are legitimate at this point, so they are
-- warnings, not errors.
adjusted :: TrialBalance MoneyDecimal
adjusted = 1000 .@ Not :< Cash .+ 1000 .@ Not :< CapitalStock
        .+ 200 .@ Not :< SuspensePayments .+ 200 .@ Hat :< Cash
        .+ 300 .@ Not :< Purchases .+ 300 .@ Hat :< CurrentDeposits
        .+ 500 .@ Not :< Cash .+ 500 .@ Not :< Sales

-- | Cash paid beyond what was on hand: cash cannot have a credit balance.
overspent :: TrialBalance MoneyDecimal
overspent = 1000 .@ Not :< Cash .+ 1000 .@ Not :< CapitalStock
         .+ 1200 .@ Not :< Purchases .+ 1200 .@ Hat :< Cash

-- | After closing, purchases reached retained earnings but sales were left
-- open instead of being transferred to income summary.
closedWithResidual :: TrialBalance MoneyDecimal
closedWithResidual = 1300 .@ Not :< Cash .+ 200 .@ Not :< SuspensePayments
                  .+ 300 .@ Hat :< RetainedEarnings
                  .+ 1000 .@ Not :< CapitalStock .+ 300 .@ Hat :< CurrentDeposits
                  .+ 500 .@ Not :< Sales

-- | A wildcard account makes every trial-balance audit report an error.
withWildcard :: TrialBalance MoneyDecimal
withWildcard = 100 .@ Not :< Cash .+ 100 .@ Not :< AccountTitle

-- * Subject 2: a journal submitted one transaction per note

-- | Rent paid twice with identical entries, a sales return, and a closing
-- entry. The repeated rent is a legitimate recurring entry, and the closing
-- entry is excluded from the contrary-posting audit.
ledger :: Ledger
ledger = ((1000 .@ Not :< Cash .+ 1000 .@ Not :< CapitalStock) .| "opening")
      .+ ((100 .@ Not :< RentExpense .+ 100 .@ Hat :< Cash) .| "rent-april")
      .+ ((100 .@ Not :< RentExpense .+ 100 .@ Hat :< Cash) .| "rent-may")
      .+ ((500 .@ Not :< Cash .+ 500 .@ Not :< Sales) .| "sale")
      .+ ((50 .@ Hat :< Sales .+ 50 .@ Hat :< Cash) .| "sales-return")
      .+ ((450 .@ Hat :< Sales .+ 450 .@ Hat :< IncomeSummary) .| "closing-sales")

main :: IO ()
main = do
    -- Subject 1. Each audit is called on its own; the caller picks the items.
    expect "impossible balances" (Audit.impossibleBalances adjusted) NoProblem
    expect "abnormal balances" (Audit.abnormalBalances adjusted)
        (Warning (Audit.AbnormalBalance CurrentDeposits Debit (CreditBalance 300) :| []))
    expect "temporary accounts" (Audit.temporaryAccountBalances adjusted)
        (Warning (Audit.TemporaryAccountBalance SuspensePayments (DebitBalance 200) :| []))
    expect "cash credit" (Audit.impossibleBalances overspent)
        (Error (AccountingFinding
            (Audit.ImpossibleBalance Cash (CreditBalance 200))
            :| []))
    expect "closing residuals" (Audit.closingResiduals closedWithResidual)
        (Error (AccountingFinding
            (Audit.ClosingResidual Sales (CreditBalance 500))
            :| []))
    expect "wildcard" (Audit.abnormalBalances withWildcard)
        (Error (StructureFinding
            (Audit.WildcardAccount (Not :< AccountTitle))
            :| []))

    -- Subject 2.
    expect "duplicates" (Audit.duplicateTransactions ledger)
        (Warning (Audit.DuplicateTransactions "rent-april" ("rent-may" :| []) :| []))
    expect "contrary postings" (Audit.contraryNominalPostings ledger)
        (Warning (Audit.ContraryNominalPosting "sales-return" 50 (Hat :< Sales) :| []))
