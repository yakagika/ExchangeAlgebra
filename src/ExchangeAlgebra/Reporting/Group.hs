module ExchangeAlgebra.Reporting.Group
{-# DEPRECATED "Import ExchangeAlgebra.Accounting.Statements.Group instead." #-}
    ( -- * Groups
      PresentationGroup(..)
    , PresentationGroupDef(..)
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
    , GroupRowKind(..)
    , GroupRow(..)
    , GroupedPresentation(..)
    , presentGroups
    ) where

import ExchangeAlgebra.Accounting.Statements.Group
