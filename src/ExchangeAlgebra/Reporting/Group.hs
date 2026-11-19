-- | Compatibility exports for statement presentation groups.
module ExchangeAlgebra.Reporting.Group
{-# DEPRECATED "Import ExchangeAlgebra.Accounting.Statements.Group instead." #-}
    ( -- * Groups
      PresentationGroup(..)
    , PresentationGroupDef
    , PresentationGroupDefOf(..)
    , defaultPresentationGrouping
    , presentationGroupOf
    , lookupGroupDef
    , groupNormalSide
    , groupingForDivisions
      -- * Amounts
    , RelativeAmount(..)
    , relativeTo
    , addGross
      -- * Grouped rows
    , GroupRowKind
    , GroupRowKindOf(..)
    , GroupRow
    , GroupRowOf(..)
    , GroupedPresentation
    , GroupedPresentationOf(..)
    , presentGroups
    ) where

import ExchangeAlgebra.Accounting.Statements.Group
