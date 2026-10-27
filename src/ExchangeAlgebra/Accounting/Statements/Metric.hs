{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.Accounting.Statements.Metric
Description : Typed, read-only metrics derived from exchange-algebra coordinates.

Derived metrics are not posting coordinates.  In particular, profit and loss
are two directions of one period-result identity, represented by
'PeriodProfit' and 'PeriodLoss' with non-negative values.  The legacy
@AccountTitles@ constructors remain available for the established transfer
pipeline, but new reporting code should derive values without inserting those
coordinates.
-}
module ExchangeAlgebra.Accounting.Statements.Metric
    ( MetricId
    , mkMetricId
    , metricIdText
    , DerivedMetric(..)
    , PeriodResult(..)
    , MetricError(..)
    , metricForLegacyTitle
    , legacyTitlesForMetric
    , periodResultOfAlg
    , periodResultOf
    ) where

import qualified ExchangeAlgebra.Accounting.TrialBalance.Accum as Accum

import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra.Core.Representation (Alg, foldEntries)
import           ExchangeAlgebra.Algebra.Value.Class (HatVal(..), isErrorValue)
import           ExchangeAlgebra.Accounting.Account.Classification
                     ( AccountDivision(..), DivisionSemantics(..), Side(..) )
import           ExchangeAlgebra.Accounting.Account.Registry
                     ( AccountSemantics(asemDivisionSemantics), accountSemantics )
import           ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import           ExchangeAlgebra.Accounting.Exchange
                     ( ExBaseClass(getAccountTitle, whichSide) )
import           ExchangeAlgebra.Algebra.Base.Representation
                     ( Hat(..), HatBase, HatBaseClass(hat) )
import qualified ExchangeAlgebra.Accounting.TrialBalance.Validation as TB

-- | Stable identity for a caller-defined metric.  Display labels are supplied
-- separately by a reporting context.
newtype MetricId = MetricId Text
  deriving (Show, Eq, Ord)

-- | Construct a non-blank custom metric identity.
mkMetricId :: Text -> Maybe MetricId
mkMetricId raw
    | T.null normalized = Nothing
    | otherwise = Just (MetricId normalized)
  where
    normalized = T.strip raw

metricIdText :: MetricId -> Text
metricIdText (MetricId value) = value

-- | Identity of a value derived for reporting.  Profit versus loss is carried
-- by the result value, not duplicated as two metric identities.
data DerivedMetric
  = PeriodResultMetric
  | GrossProfitMetric
  | OrdinaryProfitMetric
  | CustomMetric MetricId
  deriving (Show, Eq, Ord)

-- | A period result with structural direction and a non-negative amount.
data PeriodResult v
  = PeriodProfit v
  | PeriodLoss v
  | PeriodBreakEven
  deriving (Show, Eq)

data MetricError
  = MetricNotAvailableAtStage TB.TrialBalanceStage
  | ResidualDerivedCoordinate AccountTitles
  | WildcardMetricSide AccountTitles
  | WildcardMetricAccount
    -- ^ A posting carries the wildcard account title, so its statement
    -- division cannot be determined.
  | InvalidMetricValue AccountTitles
  deriving (Show, Eq)

-- | Map an engine-only legacy coordinate to its reporting identity.
metricForLegacyTitle :: AccountTitles -> Maybe DerivedMetric
metricForLegacyTitle title = case title of
    NetIncome      -> Just PeriodResultMetric
    NetLoss        -> Just PeriodResultMetric
    GrossProfit    -> Just GrossProfitMetric
    OrdinaryProfit -> Just OrdinaryProfitMetric
    _              -> Nothing

-- | Legacy coordinates retained for one metric.  This is a migration aid,
-- not a list of statement lines.
legacyTitlesForMetric :: DerivedMetric -> [AccountTitles]
legacyTitlesForMetric metric = case metric of
    PeriodResultMetric   -> [NetIncome, NetLoss]
    GrossProfitMetric    -> [GrossProfit]
    OrdinaryProfitMetric -> [OrdinaryProfit]
    CustomMetric _       -> []

-- | Derive period profit or loss without adding a balancing coordinate.
-- Only genuine statement-classified Cost and Revenue titles participate;
-- direction-encoding legacy coordinates are therefore excluded structurally.
-- Invalid values and wildcard sides are returned as explicit errors.  For
-- externally sourced trial balances, prefer 'periodResultOf'.
periodResultOfAlg
    :: (HatVal v, ExBaseClass b)
    => Alg v b
    -> Either MetricError (PeriodResult v)
periodResultOfAlg alg
    | (_, base) : _ <- invalidEntries =
        Left (InvalidMetricValue (getAccountTitle base))
    | (_, base) : _ <- wildcardEntries =
        Left (WildcardMetricSide (getAccountTitle base))
    | signAccum result == Just EQ = Right PeriodBreakEven
    | signAccum result == Just GT = Right (PeriodProfit (roundAccum result))
    | otherwise = Right (PeriodLoss (roundAccum (negateAccum result)))
  where
    entries = foldEntries (\current value base -> (value, base) : current) [] alg
    invalidEntries = filter (isErrorValue . fst) entries
    wildcardEntries = filter ((== HatNot) . hat . snd) entries
    result = foldEntries collect emptyAccum alg
    collect totals value base
        | isStatementNominal (getAccountTitle base) =
            case whichSide base of
                Debit -> mergeAccum totals (negateAccum (addAccum value emptyAccum))
                Credit -> addAccum value totals
                Side -> totals
        | otherwise = totals

-- | Derive a period result from a before-closing trial balance.
-- The stage is stated by the caller; an after-closing trial balance has no
-- nominal balances left, so it is rejected instead of reporting break-even.
-- Invalid values, wildcard sides, and the wildcard account title are rejected
-- before any balance is accumulated, because accumulation determines the
-- debit or credit side of every posting.
-- A non-zero legacy derived coordinate is rejected to prevent a caller from
-- treating an already-derived balancing item as another nominal account.
-- Consolidation attribution coordinates do not participate, so the result is
-- the period result before attribution to owners and non-controlling interests.
-- No accounting finding is consulted; inspect
-- 'ExchangeAlgebra.Accounting.TrialBalance.Validation.trialBalanceFindings'
-- separately when the trial balance must also be audited.
periodResultOf
    :: HatVal v
    => TB.TrialBalanceStage
    -> Alg v (HatBase AccountTitles)
    -> Either MetricError (PeriodResult v)
periodResultOf stage alg
    | stage /= TB.BeforeClosing =
        Left (MetricNotAvailableAtStage stage)
    | issue : _ <- metricStructureErrors alg = Left issue
    | legacy : _ <- nonZeroLegacyTitles alg =
        Left (ResidualDerivedCoordinate legacy)
    | otherwise = periodResultOfAlg alg

isStatementNominal :: AccountTitles -> Bool
isStatementNominal title = case accountSemantics title of
    Just semantics -> case asemDivisionSemantics semantics of
        StatementDivision Cost -> True
        StatementDivision Revenue -> True
        _ -> False
    Nothing -> False

-- | Invalid values, wildcard sides, and the wildcard account title, found
-- without determining any posting side.
metricStructureErrors
    :: HatVal v
    => Alg v (HatBase AccountTitles)
    -> [MetricError]
metricStructureErrors = undefined -- D3

nonZeroLegacyTitles
    :: (HatVal v, ExBaseClass b)
    => Alg v b
    -> [AccountTitles]
nonZeroLegacyTitles alg =
    [ title
    | (title, total) <- M.toList totals
    , signAccum total /= Just EQ
    ]
  where
    totals = M.filterWithKey (\title _ -> case metricForLegacyTitle title of
        Nothing -> False
        Just _ -> True) (Accum.accountAccums alg)
