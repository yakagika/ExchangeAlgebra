-- | Metadata and processing semantics shared by the account table and registry.
module ExchangeAlgebra.Accounting.Account.Spec
    ( AccountSpec(..)
    , AccountSemantics(..)
    ) where

import Data.Text (Text)

import ExchangeAlgebra.Accounting.Account.Classification
    ( AccountDivision, AccountRole, ClosingRule, DivisionSemantics
    , FixedCurrent, HomeSideSemantics, PostingCapability, ReportingEligibility
    )

-- | All metadata attached to one concrete account title.
data AccountSpec = AccountSpec
    { asDivision    :: AccountDivision
    , asClosing     :: ClosingRule
      -- ^ Automatic closing policy. @NetIncome@ and @NetLoss@ are explicit
      -- 'NoClose' overrides: their division describes the P\/L presentation
      -- side, so division-derived closing would reverse their intended side.
      -- GrossProfit and OrdinaryProfit retain CloseByDivision for staged
      -- closing. Statement metrics live in
      -- "ExchangeAlgebra.Accounting.Statements.Metric".
    , asIsContra    :: Bool
    , asFixedCurrent :: FixedCurrent
    , asNameEn      :: Text
    , asNameJa      :: Text
    -- | Annotation-free Japanese display label (清書和文表示名) used for
    -- financial-statement lines
    -- ('ExchangeAlgebra.Accounting.Statements.Presentation.presentationLabel') and LLM-facing
    -- names ('ExchangeAlgebra.IO.Input.Assist.safeNameJa'). Unlike 'asNameJa' it never
    -- carries notes, legacy-name
    -- remarks, or constructor identifiers; the JCCI level-2 A-column sweep in
    -- the test suite pins it against the official standard names.
    , asLabelJa     :: Text
    , asDescription :: Text
    -- | Registry-local aliases only. Use 'ExchangeAlgebra.Accounting.Account.accountAliases' when constructing a
    -- parser or UI: that function also includes the frozen JCCI overlay.
    , asAliases     :: [Text]
    } deriving (Show, Eq)

-- | Processing and reporting semantics attached to a concrete account title.
--
-- This record deliberately does not replace 'AccountSpec.asDivision'. The
-- legacy division remains the exchange-algebra direction input, while this
-- layer states whether that value is a financial-statement classification,
-- a bookkeeping control class, or only an internal direction encoding.
data AccountSemantics = AccountSemantics
    { asemRoles                :: [AccountRole]
    , asemPostingCapability    :: PostingCapability
    , asemDivisionSemantics    :: DivisionSemantics
    , asemHomeSideSemantics    :: HomeSideSemantics
    , asemReportingEligibility :: ReportingEligibility
    } deriving (Show, Eq)
