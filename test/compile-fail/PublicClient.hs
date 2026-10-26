{-# LANGUAGE OverloadedStrings #-}

module PublicClient where

import ExchangeAlgebra.Algebra.Core (Alg((:@), _val, _hatBase), (.@))
import ExchangeAlgebra.Algebra.Base (HatBase((:<)), Hat(Not), CountUnit(Yen))
import ExchangeAlgebra.Algebra.Core (Redundant((.+)))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.Journal.Core (Journal, (.|))
import ExchangeAlgebra.Accounting.Adjustment (BasisPoints, basisPoints)
import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra.Base (AccountTitles(..))

-- The same installed-package environment must accept a complete public path.
acceptedLedger :: Either String (Journal String MoneyDecimal (HatBase AccountTitles))
acceptedLedger = do
    accepted <- either (Left . show) Right
        (admit ((10 .@ Not :< Cash .+ 10 .@ Not :< Sales) .| "sale"))
    either (Left . show) Right
        (checkEvidence accepted id (10 .@ Not :< Cash .| "sale"))
    Right (admittedJournal accepted)

-- | Bundled selectors and the infix matching pattern remain public.
matchPosting :: Alg Double (HatBase CountUnit) -> Maybe (Double, HatBase CountUnit)
matchPosting entry@(_ :@ _) = Just (_val entry, _hatBase entry)
matchPosting _ = Nothing

-- | Checked construction remains available to external clients.
publicPosting :: Alg Double (HatBase CountUnit)
publicPosting = 1 .@ (Not :< Yen)

-- | Checked rates are available without their representation constructor.
publicRate :: Either String BasisPoints
publicRate = either (Left . show) Right (basisPoints 147.5)
