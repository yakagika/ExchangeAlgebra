{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Closed catalog requests at the IO input boundary. This package-private
-- vocabulary uses transaction identifiers, accounts, and monetary values.
-- Workflow and submission types use it without importing catalog execution.
-- Read operation identities before entity inputs and calls.
module ExchangeAlgebra.IO.Input.Admission.Catalog.Input where

import Data.List.NonEmpty (NonEmpty)

import ExchangeAlgebra.Accounting.Account (AccountTitles)
import ExchangeAlgebra.Accounting.Transaction (EntityId, TxKey)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | Closed operation identities used to authorize generated transactions.
data CatalogOperationKind
    = CogsKind -- ^ Cost-of-goods adjustment.
    | DepIndirectKind -- ^ Indirect depreciation.
    | DepDirectKind -- ^ Direct depreciation.
    | AllowanceKind -- ^ Allowance replenishment.
    | AllowanceRateKind -- ^ Rate-based allowance.
    | AllowanceResetKind -- ^ Allowance reset.
    | PrepaidKind -- ^ Prepaid expense.
    | UnearnedKind -- ^ Unearned revenue.
    | AccruedRevenueKind -- ^ Accrued revenue.
    | AccruedExpenseKind -- ^ Accrued expense.
    | ReverseEntryKind -- ^ Reverse a source entry.
    | ConsumptionTaxKind -- ^ Consumption tax settlement.
    | CorporateInterimKind -- ^ Interim corporate tax.
    | CorporateSettlementKind -- ^ Corporate tax settlement.
    | EquityEarningsKind -- ^ Equity-method earnings.
    | EquityDividendKind -- ^ Equity-method dividend.
    | EquityEntriesKind -- ^ Combined equity-method entries.
    | EquityBalanceKind -- ^ Equity-method balance query.
    | PriorErrorKind -- ^ Prior-period error correction.
    | FinalStockKind -- ^ Final-stock closing transfer.
    | StraightLineKind -- ^ Straight-line depreciation.
    | ConsolidateKind -- ^ Consolidation balance check.
    deriving (Eq, Ord, Show)

-- | Unresolved company input. The entity label must match each key's entity.
-- Only the library constructs resolved inputs and their provenance.
data EntityInput = EntityInput EntityId (NonEmpty TxKey) -- ^ Entity and its source keys.
    deriving (Eq, Show)

-- | Closed bookkeeping requests. Amount parameters use the same currency
-- unit as raw postings; allowance rates use basis points (0 through 10000).
-- Account parameters and amounts are validated before any builder executes.
data CatalogCall
    = Cogs MoneyDecimal MoneyDecimal -- ^ Beginning and ending inventory amounts, in that order.
    | DepIndirect MoneyDecimal -- ^ Indirect depreciation amount.
    | DepDirect MoneyDecimal AccountTitles -- ^ Direct depreciation amount and asset account.
    | Allowance MoneyDecimal MoneyDecimal -- ^ Estimate and current allowance.
    | AllowanceRate MoneyDecimal -- ^ Rate in basis points.
    | AllowanceReset MoneyDecimal MoneyDecimal -- ^ Estimate and current allowance.
    | Prepaid MoneyDecimal AccountTitles -- ^ Amount and expense account.
    | Unearned MoneyDecimal AccountTitles -- ^ Amount and revenue account.
    | AccruedRevenueCall MoneyDecimal AccountTitles -- ^ Amount and revenue account.
    | AccruedExpenseCall MoneyDecimal AccountTitles -- ^ Amount and expense account.
    | ReverseEntry TxKey -- ^ Exact key of the entry to reverse.
    | ConsumptionTax MoneyDecimal MoneyDecimal -- ^ Paid and received amounts.
    | CorporateInterim MoneyDecimal -- ^ Interim tax amount.
    | CorporateSettlement MoneyDecimal MoneyDecimal -- ^ Total tax, then interim payment.
    | EquityEarnings MoneyDecimal -- ^ Share of earnings.
    | EquityDividend MoneyDecimal -- ^ Dividend amount.
    | EquityEntries MoneyDecimal MoneyDecimal -- ^ Earnings share and dividend.
    | EquityBalance -- ^ Query the equity-method balance.
    | PriorError -- ^ Current amount, prior amount, expense, then asset.
        MoneyDecimal MoneyDecimal AccountTitles AccountTitles
    | FinalStock -- ^ Close final stock against the visible ledger.
    | StraightLine -- ^ Asset, annual amount, then True for the direct method.
        AccountTitles MoneyDecimal Bool
    | Consolidate (NonEmpty EntityInput) (NonEmpty TxKey) -- ^ Entities and elimination keys.
    deriving (Eq, Show)
