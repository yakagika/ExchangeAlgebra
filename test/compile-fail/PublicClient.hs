{-# LANGUAGE OverloadedStrings #-}

module PublicClient where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra.Base (AccountTitles(..))

-- The same installed-package environment must accept a complete public path.
acceptedLedger :: Either String LedgerView
acceptedLedger = do
    let key = TxKey (EntityId "company") (PeriodId "2026") (TxId "sale")
    registry <- either (Left . show) Right
        (txIdRegistry [(key, txRule Required [SupplySubmission Ordinary] Nothing)])
    let spec = AdmissionSpec registry Map.empty Map.empty (Set.fromList [Cash, Sales])
        submission = Submission
            [(key, [("debit", "Cash", 10), ("credit", "Sales", 10)])] []
    accepted <- either (Left . show) Right (admit spec submission)
    Right (deriveLedger accepted)
