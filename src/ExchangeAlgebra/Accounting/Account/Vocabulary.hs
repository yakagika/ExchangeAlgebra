-- | Account metadata for bases that carry built-in or user-defined titles.
module ExchangeAlgebra.Accounting.Account.Vocabulary
    ( AccountVocabulary(..)
    , classifyAccountDivision
    ) where

import ExchangeAlgebra.Accounting.Account.Classification (AccountDivision)
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
class Element t => AccountVocabulary t where
    -- | Look up a title's specification. The wildcard returns 'Nothing'.
    vocabSpec :: t -> Maybe AccountSpec

    -- | Look up roles, posting capability, and reporting eligibility.
    -- The default returns 'Nothing' when the vocabulary supplies no metadata.
    vocabSemantics :: t -> Maybe AccountSemantics
    vocabSemantics _ = Nothing

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
