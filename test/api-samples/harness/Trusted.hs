{-# LANGUAGE OverloadedStrings #-}

-- | Fix the coffee shop's trust boundary before any model response.
-- This sample uses the IO admission layer, not a new accounting abstraction.
-- Decoder and Main consume the key constructor and the fixed specification.
module Trusted (transactionKey, specification, augustAdjustment) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import ExchangeAlgebra.Accounting.Account (AccountTitles(..))

-- | Identify a September transaction for the single reporting entity.
transactionKey :: Text -> Admission.TxKey
transactionKey = Admission.TxKey (Admission.EntityId "roastery")
                    (Admission.PeriodId "2026-09") . Admission.TxId

-- | Historical August accrual, already included in the opening balance, in JPY.
-- It is context for the model, not an additional September ledger entry.
augustAdjustment :: Admission.RawPostings
augustAdjustment =
    [ ("Debit", "UtilitiesExpense", 10000)
    , ("Credit", "AccruedExpenses", 10000)
    ]

-- | Construct the fixed registry, independent evidence, facts, and vocabulary.
-- No values in this specification are taken from a model response.
specification :: Either String Admission.AdmissionSpec
specification = do
    registry <- either (Left . show) Right $ Admission.txIdRegistry $
        [ (transactionKey name, Admission.txRule Admission.Required supplies evidence)
        | (name, supplies, evidence) <-
            [ ("opening", [Admission.SupplyFacts (Admission.FactId "opening")
                              Admission.Opening], Nothing)
            , ("bank-rent", [Admission.SupplyFacts (Admission.FactId "bank-rent")
                                Admission.Ordinary], Nothing)
            , ("bank-collection", [Admission.SupplyFacts (Admission.FactId "bank-collection")
                                      Admission.Ordinary], Nothing)
            , ("reverse-august", [Admission.SupplySubmission Admission.Ordinary], Nothing)
            , ("coffee-sale", [Admission.SupplySubmission Admission.Ordinary],
                Just (Admission.EvidenceId "till-september"))
            , ("subscription", [Admission.SupplySubmission Admission.Ordinary], Nothing)
            , ("beans", [Admission.SupplySubmission Admission.Ordinary], Nothing)
            , ("wages", [Admission.SupplySubmission Admission.Ordinary],
                Just (Admission.EvidenceId "bank-payroll"))
            , ("utilities-paid", [Admission.SupplySubmission Admission.Ordinary], Nothing)
            , ("beans-used", [Admission.SupplySubmission Admission.Ordinary], Nothing)
            , ("subscription-expense", [Admission.SupplySubmission Admission.Adjustment], Nothing)
            , ("roasting-loss", [Admission.SupplySubmission Admission.Adjustment], Nothing)
            , ("depreciation", [Admission.SupplyCatalog Admission.DepIndirectKind], Nothing)
            , ("utilities-accrual", [Admission.SupplyCatalog Admission.AccruedExpenseKind], Nothing)
            , ("allowance", [Admission.SupplyCatalog Admission.AllowanceKind], Nothing)
            , ("close", [Admission.SupplyCatalog Admission.FinalStockKind], Nothing)
            ]
        ]
    pure Admission.AdmissionSpec
        { Admission.admissionRegistry = registry
        , Admission.admissionEvidence = Map.fromList
            [ (Admission.EvidenceId "till-september", 400000)
            , (Admission.EvidenceId "bank-payroll", 70000)
            ]
        , Admission.admissionFacts = Map.fromList
            [ (Admission.FactId "opening",
                [ ("Debit", "Cash", 20000)
                , ("Debit", "Deposits", 500000)
                , ("Debit", "AccountsReceivable", 30000)
                , ("Debit", "MerchandiseInventory", 100000)
                , ("Debit", "PrepaidExpenses", 12000)
                , ("Debit", "Machinery", 300000)
                , ("Credit", "AccruedExpenses", 10000)
                , ("Credit", "AccumulatedDepreciation", 30000)
                , ("Credit", "AllowanceForDoubtfulAccounts", 600)
                , ("Credit", "CapitalStock", 800000)
                , ("Credit", "RetainedEarnings", 121400)
                ])
            , (Admission.FactId "bank-rent",
                [ ("Debit", "RentExpense", 60000)
                , ("Credit", "Deposits", 60000)
                ])
            , (Admission.FactId "bank-collection",
                [ ("Debit", "Deposits", 10000)
                , ("Credit", "AccountsReceivable", 10000)
                ])
            ]
        , Admission.admissionVocabulary = Set.fromList
            [ Cash
            , Deposits
            , AccountsReceivable
            , MerchandiseInventory
            , PrepaidExpenses
            , Machinery
            , AccruedExpenses
            , AccumulatedDepreciation
            , AllowanceForDoubtfulAccounts
            , CapitalStock
            , RetainedEarnings
            , RentExpense
            , Sales
            , ServiceRevenue
            , WageExpenditure
            , UtilitiesExpense
            , SalesCost
            , CommunicationExpenses
            , InventoryShrinkageLoss
            , Depreciation
            , ProvisionForDoubtfulAccounts
            , ReversalOfAllowanceForDoubtfulAccounts
            , NetIncome
            , NetLoss
            ]
        }
