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
| Present a trial balance        | 'present'                                  |
+--------------------------------+--------------------------------------------+

'present' accepts a trial balance over account titles and its stage, and
returns presentation results with their audit events and leaves the
bookkeeping bases intact. Use "ExchangeAlgebra.Accounting.Audit" to
inspect accounting findings separately.

Statements receive one reporting scope. A ledger with several components (entity,
period, department) is narrowed by the caller and mapped to account titles
with 'ExchangeAlgebra.Algebra.Core.mapMaybePosting' or
'ExchangeAlgebra.Algebra.Core.mapPosting' before presentation. The remaining
amounts are summed after grouping by account title.
-}
module ExchangeAlgebra.Accounting.Statements
    ( PresentationGroup(..)
    , PresentationGroupDef
    , PresentationGroupDefOf(..)
    , defaultPresentationGrouping
    , presentationGroupOf
    , lookupGroupDef
    , groupNormalSide
    , groupingForDivisions
    , RelativeAmount(..)
    , relativeTo
    , addGross
    , GroupRowKind
    , GroupRowKindOf(..)
    , GroupRow
    , GroupRowOf(..)
    , GroupedPresentation
    , GroupedPresentationOf(..)
    , presentGroups
    , MetricId
    , mkMetricId
    , metricIdText
    , DerivedMetric(..)
    , PeriodResult(..)
    , MetricError
    , MetricErrorOf(..)
    , metricForLegacyTitle
    , legacyTitlesForMetric
    , periodResultOfAlg
    , periodResultOf
    , AccountingFramework(..)
    , ReportingScope(..)
    , PresentationProfile(..)
    , StatementSection(..)
    , StatementLine
    , StatementLineOf(..)
    , PresentationAllocation
    , PresentationAllocationOf(..)
    , PresentationRelabel
    , PresentationRelabelOf(..)
    , MaterialityTreatment
    , MaterialityTreatmentOf(..)
    , MaterialityDecision
    , MaterialityDecisionOf(..)
    , ContraPresentationRule
    , ContraPresentationRuleOf(..)
    , CustomMetricLabel(..)
    , SubtotalCoverage(..)
    , SubtotalDefinition
    , SubtotalDefinitionOf(..)
    , StatementSubtotal(..)
    , ReportingContext
    , ReportingContextOf(..)
    , reportingContext
    , jcciSecondGradeContext
    , PresentationAuditEvent
    , PresentationAuditEventOf(..)
    , PresentationIssue
    , PresentationIssueOf(..)
    , FinancialStatements
    , FinancialStatementsOf(..)
    , presentationLabel
    , metricLabel
    , present
    ) where

import ExchangeAlgebra.Accounting.Statements.Group
import ExchangeAlgebra.Accounting.Statements.Metric
import ExchangeAlgebra.Accounting.Statements.Presentation
