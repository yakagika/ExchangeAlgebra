{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Closed failures of bookkeeping catalog calls. This package-private
-- vocabulary uses accounts, transaction identifiers, and monetary values,
-- so catalog execution can report failures without the IO admission layer.
-- Read the parameter names before the parameter and execution problems.
-- The comparison records name both amounts of a failed comparison.
module ExchangeAlgebra.Accounting.Catalog where

import ExchangeAlgebra.Accounting.Account (AccountDivision, AccountTitles)
import ExchangeAlgebra.Accounting.Transaction (EntityId, TxKey)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | Names of catalog call parameters, in the order of the call fields.
data CatalogParameter
    = AmountParameter -- ^ Main amount of the call.
    | BeginningInventoryParameter -- ^ Beginning inventory amount.
    | EndingInventoryParameter -- ^ Ending inventory amount.
    | EstimateParameter -- ^ Estimated allowance.
    | CurrentParameter -- ^ Current allowance or current amount.
    | PriorParameter -- ^ Prior-period amount.
    | RateParameter -- ^ Allowance rate in basis points.
    | PaidParameter -- ^ Consumption tax paid.
    | ReceivedParameter -- ^ Consumption tax received.
    | TotalParameter -- ^ Total corporate tax.
    | InterimParameter -- ^ Interim corporate tax payment.
    | ShareParameter -- ^ Share of earnings.
    | DividendParameter -- ^ Dividend amount.
    | AnnualParameter -- ^ Annual depreciation amount.
    | AssetAccountParameter -- ^ Asset account.
    | ExpenseAccountParameter -- ^ Expense account.
    | RevenueAccountParameter -- ^ Revenue account.
    | PriorAssetAccountParameter -- ^ Asset account of a prior-period correction.
    deriving (Eq, Ord, Show)

-- | Two amounts that must be equal: the expected amount and the actual amount.
data AmountMismatch = AmountMismatch
    { expectedAmount :: MoneyDecimal -- ^ Amount required by the source.
    , actualAmount :: MoneyDecimal -- ^ Amount found in the submission.
    }
    deriving (Eq, Show)

-- | An amount compared with a limit. The constructor that holds this record
-- decides whether the limit is a lower or an upper bound.
data AmountLimit = AmountLimit
    { limitAmount :: MoneyDecimal -- ^ Bound given by another parameter.
    , limitedAmount :: MoneyDecimal -- ^ Amount compared with the bound.
    }
    deriving (Eq, Show)

-- | Debit and credit totals of an entry that does not balance.
data DebitCreditTotals = DebitCreditTotals
    { debitTotal :: MoneyDecimal -- ^ Sum of the debit side.
    , creditTotal :: MoneyDecimal -- ^ Sum of the credit side.
    }
    deriving (Eq, Show)

-- | Invalid parameters of one catalog call, found before any builder runs.
-- One problem is reported per failed check, in the order of the call fields.
data CatalogParameterProblem
    = NonPositiveParameter CatalogParameter MoneyDecimal
      -- ^ Parameter must be positive.
    | NegativeParameter CatalogParameter MoneyDecimal
      -- ^ Parameter must be nonnegative.
    | AccountOutsideDivision CatalogParameter AccountTitles AccountDivision
      -- ^ Account is not a concrete account of the expected division.
    | RateAboveLimit MoneyDecimal
      -- ^ Allowance rate exceeds 10000 basis points.
    | ReceivedBelowPaid AmountLimit
      -- ^ Received tax is below the paid tax, which is the lower bound.
    | InterimExceedsTotal AmountLimit
      -- ^ Interim tax exceeds the total tax, which is the upper bound.
    | BlankReferenceKey TxKey
      -- ^ Referenced transaction key is blank.
    | TooFewEntities Int
      -- ^ Consolidation names fewer than two distinct entities.
    | RepeatedEntity EntityId
      -- ^ First entity named twice in one consolidation.
    | RepeatedReference TxKey
      -- ^ First transaction key referenced twice in one call.
    deriving (Eq, Show)

-- | Failures of a catalog builder whose parameters were valid.
data CatalogExecutionProblem
    = InexactQuotient MoneyDecimal MoneyDecimal
      -- ^ Dividend and divisor of a quotient that is not exact.
    | ImbalancedConsolidation DebitCreditTotals
      -- ^ Consolidation entry does not balance.
    | AbnormalBalance AccountTitles
      -- ^ Account balance is on the side opposite to its normal side.
    deriving (Eq, Show)
