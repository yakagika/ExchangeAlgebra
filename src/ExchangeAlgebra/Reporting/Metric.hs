{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.Reporting.Metric
Description : Typed, read-only metrics derived from exchange-algebra coordinates.

Derived metrics are not posting coordinates.  In particular, profit and loss
are two directions of one period-result identity, represented by
'PeriodProfit' and 'PeriodLoss' with non-negative values.  The legacy
@AccountTitles@ constructors remain available for the established transfer
pipeline, but new reporting code should derive values without inserting those
coordinates.
-}
module ExchangeAlgebra.Reporting.Metric
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

import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra
                     ( Alg, HatVal(isErrorValue), foldEntries )
import           ExchangeAlgebra.Algebra.Base
                     ( AccountDivision(..)
                     , AccountSemantics(asemDivisionSemantics)
                     , AccountTitles(..)
                     , DivisionSemantics(..)
                     , ExBaseClass(getAccountTitle, whichSide)
                     , Hat(..)
                     , HatBaseClass(hat)
                     , Side(..)
                     , accountSemantics
                     )
import qualified ExchangeAlgebra.TrialBalance.Validation as TB

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
    | credit == debit = Right PeriodBreakEven
    | credit > debit = Right (PeriodProfit (credit - debit))
    | otherwise = Right (PeriodLoss (debit - credit))
  where
    entries = foldEntries (\current value base -> (value, base) : current) [] alg
    invalidEntries = filter (isErrorValue . fst) entries
    wildcardEntries = filter ((== HatNot) . hat . snd) entries
    (debit, credit) = foldEntries collect (0, 0) alg
    collect totals value base
        | isStatementNominal (getAccountTitle base) =
            addOnSide (whichSide base) value totals
        | otherwise = totals

-- | Derive a period result from an accepted before-closing trial balance.
-- A non-zero legacy derived coordinate is rejected to prevent a caller from
-- treating an already-derived balancing item as another nominal account.
-- Consolidation attribution coordinates do not participate, so the result is
-- the period result before attribution to owners and non-controlling interests.
periodResultOf
    :: HatVal v
    => TB.ValidatedTrialBalance v
    -> Either MetricError (PeriodResult v)
periodResultOf validated
    | TB.validatedStage validated /= TB.BeforeClosing =
        Left (MetricNotAvailableAtStage (TB.validatedStage validated))
    | legacy : _ <- nonZeroLegacyTitles alg =
        Left (ResidualDerivedCoordinate legacy)
    | otherwise = periodResultOfAlg alg
  where
    alg = TB.validatedTrialBalance validated

isStatementNominal :: AccountTitles -> Bool
isStatementNominal title = case accountSemantics title of
    Just semantics -> case asemDivisionSemantics semantics of
        StatementDivision Cost -> True
        StatementDivision Revenue -> True
        _ -> False
    Nothing -> False

addOnSide :: Num v => Side -> v -> (v, v) -> (v, v)
addOnSide Debit value (debit, credit) = (debit + value, credit)
addOnSide Credit value (debit, credit) = (debit, credit + value)
addOnSide Side _ totals = totals

nonZeroLegacyTitles
    :: (HatVal v, ExBaseClass b)
    => Alg v b
    -> [AccountTitles]
nonZeroLegacyTitles alg =
    [ title
    | (title, (debit, credit)) <- M.toList totals
    , debit /= credit
    ]
  where
    totals = foldEntries collect M.empty alg
    collect current value base = case metricForLegacyTitle title of
        Nothing -> current
        Just _ -> M.insertWith add title (sidePair (whichSide base) value) current
      where
        title = getAccountTitle base
    add (newDebit, newCredit) (oldDebit, oldCredit) =
        (newDebit + oldDebit, newCredit + oldCredit)
    sidePair Debit value = (value, 0)
    sidePair Credit value = (0, value)
    sidePair Side _ = (0, 0)
