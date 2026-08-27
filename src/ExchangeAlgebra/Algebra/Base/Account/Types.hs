{- |
Module      : ExchangeAlgebra.Algebra.Base.Account.Types
Description : Account metadata types shared by Base and the account registry.
-}
module ExchangeAlgebra.Algebra.Base.Account.Types
    ( AccountDivision(..)
    , Side(..)
    , ClosingRule(..)
    , FixedCurrent(..)
    , AccountRole(..)
    , PostingCapability(..)
    , DivisionSemantics(..)
    , HomeSideSemantics(..)
    , ReportingEligibility(..)
    ) where

-- | Account division (financial-statement classification). The 'AccountBase'
-- correspondence instance lives in "ExchangeAlgebra.Algebra.Base" (the class's
-- home module), so this declaration stays instance-free.
data AccountDivision = Assets       -- ^ Assets
                     | Equity       -- ^ Equity
                     | Liability    -- ^ Liability
                     | Cost         -- ^ Cost
                     | Revenue      -- ^ Revenue
                     deriving (Ord, Show, Eq)

-- | Credit/debit distinction. 'Side' is the wildcard used by legacy APIs.
data Side = Credit -- ^ Credit side.
          | Debit  -- ^ Debit side.
          | Side   -- ^ Wildcard.
          deriving (Ord, Show, Eq)

-- | Registry-level policy for automatic closing entries.
--
-- 'CloseByDivision' derives the transfer side from 'AccountDivision'.
-- 'NoClose' is an explicit override. Future policies may add explicit
-- keep/flip constructors without returning to an account-title case split.
data ClosingRule = CloseByDivision -- ^ Close Cost/Revenue accounts according to their division.
                 | NoClose         -- ^ Do not generate an automatic closing entry.
                 deriving (Show, Eq)

-- | Fixed/Current distinction. Used for classifying account titles as fixed or current.
data FixedCurrent = Fixed   -- ^ Fixed
                  | Current -- ^ Current
                  | Other   -- ^ Other (expenses, revenues, etc.)
                  deriving (Show, Eq)

-- | Accounting role of an account-basis coordinate. Roles are not assumed to
-- be mutually exclusive; see 'AccountSemantics' in the account registry.
data AccountRole
    = OrdinaryAccount
    | ContraAccount
    | ReciprocalAccount
    | SuspenseOrClearingAccount
    | ClosingDevice
    | AttributionAccount
    | PeriodResult
    | ReportingSubtotal
    deriving (Show, Eq)

-- | Context in which an account title may be used as a posting coordinate.
-- Enforcement is introduced by the checked-conversion API in a later land;
-- this type is the canonical metadata used by that gate.
data PostingCapability
    = OrdinaryPosting
    | ClosingOnly
    | ConsolidationOnly
    | EngineGeneratedOnly
    | NotPostable
    deriving (Show, Eq)

-- | Meaning of the legacy five-way 'AccountDivision' value.
--
-- This separates a genuine statement classification from a bookkeeping
-- control class or an internal direction encoding. The wrapped legacy value
-- remains available for 0.4.x-compatible algebraic behaviour.
data DivisionSemantics
    = StatementDivision AccountDivision
    | BookkeepingControlClass AccountDivision
    | DirectionEncoding AccountDivision
    | NoStatementDivision
    deriving (Show, Eq)

-- | Semantic status of an account's normal posting side.
data HomeSideSemantics
    = FixedHomeSide Side
    | ContextDependentHomeSide
    | NoFixedHomeSide
    | NoPostingSide
    deriving (Show, Eq)

-- | Coarse reporting eligibility. Actual presentation remains a function of
-- reporting context and policy and is implemented in a later land.
data ReportingEligibility
    = StatementEligible
    | ContextualPresentation
    | DerivedPresentation
    | NotPresented
    deriving (Show, Eq)
