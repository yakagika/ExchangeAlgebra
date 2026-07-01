# SKILL-ea — ExchangeAlgebra harness cheatsheet for arm A

version: v1

## Changelog

- **v1** (2026-07-02): promoted from the hard-coded cheatsheet in `runner/arms.py`
  (pilot 2026-07-01). Incorporates pilot gotchas: `hiding (map)` requirement,
  MoneyDecimal→Decimal conversion (`toDecimal` + `truncate`; no RealFrac on
  MoneyDecimal), Hat/Not home-side conventions (incl. valuation accounts such as
  AccumulatedDepreciation being credit-home), and the ea_account_map rule for
  accounts absent from EA AccountTitles (e.g. ServiceRevenue).

---

## Cheatsheet (verified against GHC 9.10.2 + exchangealgebra 0.5.x)

```
-- *** MANDATORY IMPORTS (copy these exactly) ***
--   import ExchangeAlgebra hiding (map)   -- hide EA's 'map' to avoid ambiguity
--   import Data.Decimal (Decimal)         -- for toDecimal conversion
--   import Data.List (intercalate)
--
-- Types:
--   type MinBase = HatBase AccountTitles
--   type MinTransaction = Alg MoneyDecimal MinBase
--
-- Constructors (verified):
--   (<amount>) .@ Not :< <AccountTitle>   -- home-side posting (INCREASE)
--   (<amount>) .@ Hat :< <AccountTitle>   -- opposite-side posting (DECREASE)
--
-- *** Hat/Not home-side convention (CRITICAL — most common mistake) ***
--   Every account has a HOME side determined by its division:
--     Assets / Expenses(Cost)          → home = Debit
--     Liabilities / Equity / Revenue   → home = Credit
--   An INCREASE of an account = Not  (lands on its home side).
--   A  DECREASE of an account = Hat  (lands on the opposite side).
--   The output side is computed by whichSide, NOT by Hat/Not directly.
--
--   Examples:
--     debit Cash 1000 (increase)         →  1000 .@ Not :< Cash
--     credit Cash 300 (decrease/payment) →   300 .@ Hat :< Cash
--     credit Sales 1000 (revenue up)     →  1000 .@ Not :< Sales
--     CREDIT AccumulatedDepreciation (increase) → 2400 .@ Not :< AccumulatedDepreciation
--       -- AccumulatedDepreciation is a Liability-classified valuation account:
--       -- its home side is CREDIT, so an increase (Not) already lands on credit.
--       -- Same for AllowanceForDoubtfulAccounts. Do NOT use Hat for these
--       -- when the entry credits them.
--
-- Combining:
--   (.+) :: MinTransaction -> MinTransaction -> MinTransaction
--
-- Decompose to list:
--   toList :: MinTransaction -> [Alg MoneyDecimal MinBase]
--     -- each element is a single posting (one :@ one HatBase)
--
-- Accessors per posting element:
--   _hatBase  :: Alg MoneyDecimal MinBase -> MinBase        -- the HatBase part
--   _val      :: Alg MoneyDecimal MinBase -> MoneyDecimal   -- the amount
--   whichSide :: MinBase -> Side   -- Debit | Credit, Hat-flip included
--   getAccountTitle :: MinBase -> AccountTitles
--
-- Amount conversion (MoneyDecimal → Int):
--   toDecimal :: MoneyDecimal -> Decimal   -- unwrap to Data.Decimal
--   truncate  :: Decimal -> Int            -- Prelude truncate works on Decimal
--   (use truncate via toDecimal — MoneyDecimal itself has NO RealFrac instance,
--    so `round v :: Int` on a MoneyDecimal does NOT compile)
--
-- Common AccountTitles (EXACT constructor spelling):
--   Assets:      Cash, Deposits, CurrentDeposits, AccountsReceivable,
--                MerchandiseInventory, Products, Fixtures, Machinery, Building,
--                Land, Software, Patent, PrepaidExpenses, AccruedRevenue
--   Liabilities: AccountsPayable, LoansPayable, NotesPayable, AccruedExpenses,
--                UnearnedRevenue, AllowanceForDoubtfulAccounts,
--                AccumulatedDepreciation
--   Equity:      CapitalStock, RetainedEarnings
--   Revenue:     Sales, InterestEarned, ReceiptFee, RentalIncome
--   Expenses:    Purchases, WageExpenditure, RentExpense, Depreciation,
--                SuppliesExpenses, UtilitiesExpense, InterestExpense,
--                ProvisionForDoubtfulAccounts, AmortizationExpense,
--                CommunicationExpenses, SalesCost
--
-- *** Accounts NOT in AccountTitles ***
--   Some task chart names do not exist in EA (e.g. ServiceRevenue,
--   DepreciationExpense, Inventory). The task input provides an
--   "EA account mapping" (ea_account_map). ALWAYS use the mapped EA name:
--     ServiceRevenue      → (mapped, e.g. Sales)
--     DepreciationExpense → Depreciation
--     Inventory           → MerchandiseInventory
--   Never invent a constructor: unmapped names WILL NOT COMPILE.
--
-- Output: main MUST print a JSON array of postings to stdout, NOTHING ELSE.
--   Format: [{"side":"debit","account":"Cash","amount":1000},...]
--   "account" value: show (getAccountTitle b) — the constructor name as String.
--
-- Minimal working example (cash sale for 1000, verified):
-- ----------------------------------------
-- import ExchangeAlgebra hiding (map)
-- import Data.Decimal (Decimal)
-- import Data.List (intercalate)
--
-- type MinBase = HatBase AccountTitles
-- type MinTx   = Alg MoneyDecimal MinBase
--
-- sale :: MinTx
-- sale = 1000 .@ Not :< Cash .+ 1000 .@ Not :< Sales
--
-- postingToJSON :: Alg MoneyDecimal MinBase -> String
-- postingToJSON x =
--   let b    = _hatBase x
--       v    = toDecimal (_val x)
--       side = if whichSide b == Debit then "debit" else "credit"
--       acct = show (getAccountTitle b)
--       amt  = show (truncate v :: Int)
--   in "{\"side\":\"" ++ side ++ "\",\"account\":\"" ++ acct ++ "\",\"amount\":" ++ amt ++ "}"
--
-- main :: IO ()
-- main = putStrLn $ "[" ++ intercalate "," (map postingToJSON (toList sale)) ++ "]"
-- ----------------------------------------
```
