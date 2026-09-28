module ExchangeAlgebra.Reporting.Metric
{-# DEPRECATED "Import ExchangeAlgebra.Accounting.Statements.Metric instead." #-}
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

import ExchangeAlgebra.Accounting.Statements.Metric
