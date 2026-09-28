{- |
Module      : ExchangeAlgebra.Accounting.Statements
Description : Group, derive, and present financial statement values.

Import this module for the complete statements API. Import a child module
when only grouping, metrics, or presentation is needed.

+--------------------------------+--------------------------------------------+
| Task                           | Entry point                                |
+================================+============================================+
| Group gross and contra amounts | 'presentGroups'                            |
+--------------------------------+--------------------------------------------+
| Derive a period result         | 'periodResultOf'                           |
+--------------------------------+--------------------------------------------+
| Present a validated balance    | 'present'                                  |
+--------------------------------+--------------------------------------------+

'present' accepts a validated trial balance and returns presentation results
with their audit events. It does not alter bookkeeping coordinates.
-}
module ExchangeAlgebra.Accounting.Statements
    ( PresentationGroup(..)
    , PresentationGroupDef(..)
    , defaultPresentationGrouping
    , presentationGroupOf
    , lookupGroupDef
    , groupNormalSide
    , groupingForDivisions
    , RelativeAmount(..)
    , relativeTo
    , addGross
    , GroupRowKind(..)
    , GroupRow(..)
    , GroupedPresentation(..)
    , presentGroups
    , MetricId
    , mkMetricId
    , metricIdText
    , DerivedMetric(..)
    , PeriodResult(..)
    , MetricError(..)
    , metricForLegacyTitle
    , legacyTitlesForMetric
    , periodResultOfAlg
    , periodResultOf
    , AccountingFramework(..)
    , ReportingScope(..)
    , PresentationProfile(..)
    , StatementSection(..)
    , StatementLine(..)
    , PresentationAllocation(..)
    , PresentationRelabel(..)
    , MaterialityTreatment(..)
    , MaterialityDecision(..)
    , ContraPresentationRule(..)
    , CustomMetricLabel(..)
    , SubtotalCoverage(..)
    , SubtotalDefinition(..)
    , StatementSubtotal(..)
    , ReportingContext(..)
    , jcciSecondGradeContext
    , PresentationAuditEvent(..)
    , PresentationIssue(..)
    , FinancialStatements(..)
    , presentationLabel
    , metricLabel
    , present
    ) where

import ExchangeAlgebra.Accounting.Statements.Group
import ExchangeAlgebra.Accounting.Statements.Metric
import ExchangeAlgebra.Accounting.Statements.Presentation
