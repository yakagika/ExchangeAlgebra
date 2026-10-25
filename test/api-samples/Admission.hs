{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString as ByteString
import Control.Monad (unless)
import qualified ExchangeAlgebra.Journal.Core as Journal
import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Algebra.Transfer as Transfer
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
import ExchangeAlgebra (AccountTitles(..), MoneyDecimal)

-- | Admit two submitted entries and compare the catalog and algebra closings.
main :: IO ()
main = do
    let entity = Admission.EntityId "shop"
        period = Admission.PeriodId "2026"
        saleKey = Admission.TxKey entity period (Admission.TxId "sale")
        expenseKey = Admission.TxKey entity period (Admission.TxId "expense")
        closeKey = Admission.TxKey entity period (Admission.TxId "close")
        saleEvidence = Admission.EvidenceId "sale-total"
        expenseEvidence = Admission.EvidenceId "expense-total"
        rules =
            [ (saleKey, Admission.txRule Admission.Required
                [Admission.SupplySubmission Admission.Ordinary] (Just saleEvidence))
            , (expenseKey, Admission.txRule Admission.Required
                [Admission.SupplySubmission Admission.Ordinary] (Just expenseEvidence))
            , (closeKey, Admission.txRule Admission.Required
                [Admission.SupplyCatalog Admission.FinalStockKind] Nothing)
            ]
        submitted = Admission.Submission
            [ (saleKey, [(Admission.DebitSide, "Cash", 100), (Admission.CreditSide, "Sales", 100)])
            , (expenseKey, [(Admission.DebitSide, "RentExpense", 100)
                           , (Admission.CreditSide, "Cash", 100)])
            ]
            [Admission.Call (Admission.CallId "closing") entity period
                (Just closeKey) Admission.FinalStockCall]
    case Admission.txIdRegistry rules of
        Left problems -> fail (show problems)
        Right registry -> do
            let spec = Admission.AdmissionSpec registry
                    (Map.fromList [(saleEvidence, 100), (expenseEvidence, 100)])
                    Map.empty
                    (Set.fromList [Cash, Sales, RentExpense, NetIncome, NetLoss,
                                   RetainedEarnings])
            case Admission.admit spec submitted of
                Left failure -> fail (show (Admission.failurePhase failure,
                    Admission.failureErrors failure))
                Right accepted -> do
                    let adjusted = Journal.toAlg $ Admission.admittedJournal $
                            Admission.restrictAdmitted
                                ((<= Admission.ConsolidationStage) . Admission.admissionStage)
                                accepted
                        closed = Journal.toAlg (Admission.admittedJournal accepted)
                    let closesAgree = EA.bar closed ==
                            EA.bar (Transfer.finalStockTransfer adjusted)
                    print closesAgree
                    unless closesAgree (fail "closing paths disagree")
                    case Admission.deriveTrialBalance accepted of
                        Left findings -> fail (show findings)
                        Right trial -> do
                            print (Admission.admittedTrialBalance trial)
                            case Admission.presentAdmitted
                                (Presentation.jcciSecondGradeContext Presentation.Standalone)
                                trial of
                                Left issues -> fail (show issues)
                                Right statements -> ByteString.putStr
                                    (Admission.renderAdmittedStatements statements)
