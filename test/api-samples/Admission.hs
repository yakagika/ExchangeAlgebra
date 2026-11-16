{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Either (isLeft)
import qualified ExchangeAlgebra.Journal.Core as Journal
import ExchangeAlgebra.Journal.Core ((.|))
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import ExchangeAlgebra.IO.Input.Admission (TxKey(..), EntityId(..), PeriodId(..), TxId(..))
import qualified ExchangeAlgebra.Accounting.Audit as Audit
import qualified ExchangeAlgebra.Accounting.TrialBalance as TrialBalance
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
import ExchangeAlgebra

key :: TxId -> TxKey
key = TxKey (EntityId "shop") (PeriodId "2026")

-- | Admit a journal, check it against a bank record, reject an unbalanced
-- journal, and derive statements from the admitted journal.
main :: IO ()
main = do
    let journal :: Journal.Journal TxKey MoneyDecimal (HatBase AccountTitles)
        journal = ((500 .@ Not :< Cash .+ 500 .@ Not :< CapitalStock) .| key (TxId "opening"))
               .+ ((100 .@ Not :< Cash .+ 100 .@ Not :< Sales)        .| key (TxId "sale"))
               .+ ((30  .@ Not :< RentExpense .+ 30 .@ Hat :< Cash)   .| key (TxId "rent"))

        unbalanced = (100 .@ Not :< Cash .+ 90 .@ Not :< Sales) .| key (TxId "sale")
    accepted <- either (fail . show . Admission.failureErrors) pure (Admission.admit journal)

    -- The bank records the cash receipt of the sale.
    let bank = 100 .@ Not :< Cash .| key (TxId "sale")
    either (fail . show) pure $
        Admission.checkEvidence accepted (Journal.projWithBase [Not :< Cash]) bank
    unless (isLeft (Admission.admit unbalanced)) (fail "unbalanced journal admitted")

    -- Reports come from the existing trial-balance and statement functions.
    let trialBalance = Journal.toAlg (Admission.admittedJournal accepted)
    statements <- either (fail . show) pure $
        Presentation.present (Presentation.jcciSecondGradeContext Presentation.Standalone)
            TrialBalance.BeforeClosing trialBalance

    -- The audits of the trial balance are separate; this trial balance has no finding.
    unless (  Audit.impossibleBalances trialBalance       == Audit.NoProblem
           && Audit.abnormalBalances trialBalance         == Audit.NoProblem
           && Audit.temporaryAccountBalances trialBalance == Audit.NoProblem)
        (fail "unexpected trial-balance finding")

    -- Print the rows of four statement sections.
    rows <- either (fail . show) pure $
        financialStatementRows
            [ Presentation.CurrentAssetsSection
            , Presentation.EquitySection
            , Presentation.RevenueSection
            , Presentation.ExpenseSection
            ]
            statements
    mapM_ print rows
