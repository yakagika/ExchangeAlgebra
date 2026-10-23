{-# LANGUAGE OverloadedStrings #-}

module PublicClient where

import ExchangeAlgebra.Algebra.Core (Alg((:@), _val, _hatBase), (.@))
import ExchangeAlgebra.Algebra.Base (HatBase((:<)), Hat(Not), CountUnit(Yen))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra.Base (AccountTitles(..))

-- The same installed-package environment must accept a complete public path.
acceptedLedger :: Either String AdmissionJournal
acceptedLedger = do
    let key = TxKey (EntityId "company") (PeriodId "2026") (TxId "sale")
    registry <- either (Left . show) Right
        (txIdRegistry [(key, txRule Required [SupplySubmission Ordinary] Nothing)])
    let spec = AdmissionSpec registry Map.empty Map.empty (Set.fromList [Cash, Sales])
        submission = Submission
            [(key, [("debit", "Cash", 10), ("credit", "Sales", 10)])] []
    accepted <- either (Left . show) Right (admit spec submission)
    Right (admittedJournal accepted)

-- | Bundled selectors and the infix matching pattern remain public.
matchPosting :: Alg Double (HatBase CountUnit) -> Maybe (Double, HatBase CountUnit)
matchPosting entry@(_ :@ _) = Just (_val entry, _hatBase entry)
matchPosting _ = Nothing

-- | Checked construction remains available to external clients.
publicPosting :: Alg Double (HatBase CountUnit)
publicPosting = 1 .@ (Not :< Yen)
