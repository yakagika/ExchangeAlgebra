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
returns presentation results with their audit events. It does not alter
bookkeeping coordinates and does not judge whether the trial balance is normal
for accounting purposes; inspect
'ExchangeAlgebra.Accounting.TrialBalance.Validation.trialBalanceFindings' for
that audit.

Statements receive one reporting scope. A ledger with several axes (entity,
period, department) is narrowed by the caller and mapped to account titles
with 'ExchangeAlgebra.Algebra.Core.mapMaybePosting' or
'ExchangeAlgebra.Algebra.Core.mapPosting' before presentation. The remaining
axes are summed, and no axis name appears in the output.
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
