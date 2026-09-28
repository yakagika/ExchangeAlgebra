module ExchangeAlgebra.Bookkeeping
{-# DEPRECATED "Import ExchangeAlgebra.Accounting.Entries instead." #-}
    ( -- * Base injection
      MkBase
      -- * Cost of goods sold (売上原価, 3 分法)
    , cogsAdjustmentEntries
      -- * Depreciation (減価償却)
    , depreciationIndirectEntry
    , depreciationDirectEntry
      -- * Allowance for doubtful accounts (貸倒引当金)
    , allowanceReplenishmentEntry
    , allowanceResetEntries
      -- * Deferral / accrual (経過勘定)
    , prepaidExpenseEntry
    , unearnedRevenueEntry
    , accruedRevenueEntry
    , accruedExpenseEntry
    , reversingEntry
      -- * Tax settlement (消費税・法人税等)
    , consumptionTaxSettlementEntry
    , corporateTaxInterimEntry
    , corporateTaxSettlementEntries
      -- * Equity method (持分法)
    , equityMethodEarningsEntry
    , equityMethodDividendEntry
    , equityMethodEntries
    , equityMethodBalance
      -- * Prior-period error correction (前期修正)
    , priorPeriodErrorCorrection
    ) where

import ExchangeAlgebra.Accounting.Entries
