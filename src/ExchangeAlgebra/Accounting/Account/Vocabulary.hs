-- | Account metadata for bases that carry built-in or user-defined titles.
module ExchangeAlgebra.Accounting.Account.Vocabulary
    ( AccountVocabulary(..)
    , classifyAccountDivision
    ) where

import ExchangeAlgebra.Accounting.Account.Classification (AccountDivision, Side(..))
import ExchangeAlgebra.Accounting.Account.Registry
    ( AccountSpec(..), AccountSemantics, accountSpec, accountSemantics
    , classifyAccountContra )
import ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import ExchangeAlgebra.Algebra.Base.Representation (customError)
import ExchangeAlgebra.Algebra.Element.Representation (Element)
import GHC.Stack (HasCallStack)

-- | Supply account metadata for a title type.
--
-- All instances must satisfy these laws under exact equality:
--
-- * For each concrete title @t@, @vocabSpec t@ returns @Just spec@ and
--   @vocabDivision t == asDivision spec@.
-- * @vocabContra t == maybe False asIsContra (vocabSpec t)@.
-- * 'closingTarget' differs from the wildcard, has a specification with
--   division @Equity@, and has @vocabContra closingTarget == False@.
-- * Each endpoint in 'vocabReciprocalPairs' is concrete, has the
--   @ReciprocalAccount@ role in 'vocabSemantics', and is neither a Cost nor
--   a Revenue account. No title occurs more than once across all pairs.
class Element t => AccountVocabulary t where
    -- | Look up a title's specification. The wildcard returns 'Nothing'.
    vocabSpec :: t -> Maybe AccountSpec

    -- | Look up roles, posting capability, and reporting eligibility.
    -- The default returns 'Nothing' when the vocabulary supplies no metadata.
    vocabSemantics :: t -> Maybe AccountSemantics
    vocabSemantics _ = Nothing

    -- | Accounts that cannot have a balance on the given side after correct
    -- processing. Each title must be concrete and appear at most once. Each
    -- side must be 'Debit' or 'Credit'. The default list is empty.
    vocabImpossibleBalanceSides :: [(t, Side)]
    vocabImpossibleBalanceSides = []

    -- | Suspense accounts to check after adjustment. A remaining balance
    -- warrants a warning because the item may still be under investigation.
    -- The default list is empty.
    vocabSuspenseTitles :: [t]
    vocabSuspenseTitles = []

    -- | Additional accounts that closing must clear, even when
    -- 'vocabSemantics' supplies no metadata. The default list is empty.
    vocabAdditionalClosingResidualTitles :: [t]
    vocabAdditionalClosingResidualTitles = []

    -- | Pairs eliminated in a combined report, in audit-event order.
    -- The default list is empty.
    vocabReciprocalPairs :: [(t, t)]
    vocabReciprocalPairs = []

    -- | Name the account that receives revenue and cost balances at closing.
    closingTarget :: t

    -- | Read the account division. A title without a specification raises an error.
    vocabDivision :: HasCallStack => t -> AccountDivision
    vocabDivision t = case vocabSpec t of
        Just spec -> asDivision spec
        Nothing   -> customError "account title without a specification (wildcard)"

    -- | Test whether a title denotes a contra account.
    vocabContra :: t -> Bool
    vocabContra = maybe False asIsContra . vocabSpec

instance AccountVocabulary AccountTitles where
    vocabSpec = accountSpec
    vocabSemantics = accountSemantics
    vocabImpossibleBalanceSides =
        [ (Cash, Credit), (PettyCash, Credit)
        , (CapitalStock, Debit), (AccumulatedDepreciation, Debit)
        ]
    vocabSuspenseTitles = [SuspensePayments, SuspenseReceipts, SuspenseAccount]
    vocabAdditionalClosingResidualTitles = [CashOverShort]
    vocabReciprocalPairs = [(BranchCurrentAccount, HeadOfficeCurrentAccount)]
    closingTarget = RetainedEarnings
    vocabDivision = classifyAccountDivision
    vocabContra = classifyAccountContra

-- | Classify an account title into an account division (Assets/Equity/Liability/Cost/Revenue).
--
-- Complexity: O(1)
{-# INLINE classifyAccountDivision #-}
classifyAccountDivision :: HasCallStack => AccountTitles -> AccountDivision
classifyAccountDivision AccountTitle = customError "this is wildcard AccountTitle"
classifyAccountDivision title =
    case accountSpec title of
        Just spec -> asDivision spec
        Nothing   -> customError "this is wildcard AccountTitle"
