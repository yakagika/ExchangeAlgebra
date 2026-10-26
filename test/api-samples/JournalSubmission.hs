{-# LANGUAGE OverloadedStrings #-}

import qualified ExchangeAlgebra.Journal.Core as Journal
import ExchangeAlgebra.Journal.Core ((.|))
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import ExchangeAlgebra.IO.Input.Admission (TxKey(..), EntityId(..), PeriodId(..), TxId(..))
import ExchangeAlgebra

key :: TxId -> TxKey
key = TxKey (EntityId "shop") (PeriodId "2026")

-- | Opening balances supplied by a trusted party are part of the journal.
opening, period :: Journal.Journal TxKey MoneyDecimal (HatBase AccountTitles)
opening = (500 .@ Not :< Cash .+ 500 .@ Not :< CapitalStock) .| key (TxId "opening")
period  = ((100 .@ Not :< Cash .+ 100 .@ Not :< Sales) .| key (TxId "sale"))
       .+ ((30 .@ Not :< RentExpense .+ 30 .@ Hat :< Cash) .| key (TxId "rent"))

-- | A model computes the closing entries itself, submits the whole journal,
-- and the caller confirms the opening balance and a cash receipt with the
-- trusted records.
main :: IO ()
main = do
    closing <- either (fail . show) pure (closingEntries (Journal.toAlg period))
    let submitted = opening .+ period .+ (closing .| key (TxId "close"))
    accepted <- either (fail . show . Admission.failureErrors) pure (Admission.admit submitted)
    let expected = ((500 .@ Not :< Cash) .| key (TxId "opening"))
                .+ ((100 .@ Not :< Cash) .| key (TxId "sale"))
    either (fail . show) pure $
        Admission.checkEvidence accepted (Journal.projWithBase [Not :< Cash]) expected
    print (length (Journal.toMap (Admission.admittedJournal accepted)))
