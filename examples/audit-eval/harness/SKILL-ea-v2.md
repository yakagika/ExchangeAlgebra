# SKILL-ea — ExchangeAlgebra harness cheatsheet for arm A

version: v2

## Changelog

- **v2** (2026-07-04): Track S — checked construction is now mandatory.
  Entries and journals must be built through `ExchangeAlgebra.Convert.Checked`
  (`checkedEntry` / `checkedJournal`) with explicit `Side` values
  (`Debit` / `Credit`). The library performs the home-side Hat/Not conversion
  internally and rejects invalid entries with structured errors.
- **v1** (2026-07-02): promoted from the hard-coded cheatsheet in
  `runner/arms.py` (pilot 2026-07-01).

---

## Cheatsheet (verified against GHC 9.10.2 + exchangealgebra 0.5.x)

```
-- *** MANDATORY IMPORTS (copy these exactly) ***
--   import ExchangeAlgebra hiding (map)   -- hide EA's 'map' to avoid ambiguity
--   import ExchangeAlgebra.Convert.Checked
--     ( EntryError, JournalError, checkedEntry, checkedJournal, exactBalanced )
--   import ExchangeAlgebra.Assist (explainJournalErrors)
--   import ExchangeAlgebra.Journal (Journal, toAlg)
--   import EmitCanonical
--   import qualified Data.List.NonEmpty as NE
--   import qualified Data.Text.IO as TIO
--   import System.Exit (exitFailure)
--   import System.IO (stderr)
--
-- Types:
--   type MinBase    = HatBase AccountTitles
--   type MinTx      = Alg MoneyDecimal MinBase
--   type MinJournal = Journal String MoneyDecimal MinBase
--
-- *** Checked construction is mandatory ***
--   Do NOT build entries by hand with .@ / Hat / Not.
--   Do NOT manually convert home-side increases/decreases.
--   Use explicit debit/credit postings and let the checked loader validate:
--
--     checkedEntry
--       [ (Debit,  Cash,  1000)
--       , (Credit, Sales, 1000)
--       ]
--
--   Multiple entries:
--
--     checkedJournal
--       [ ("t1", [(Debit, Cash, 1000), (Credit, Sales, 1000)])
--       , ("t2", [(Debit, RentExpense, 300), (Credit, Cash, 300)])
--       ]
--
--   If construction fails, surface the error as a runtime failure so the
--   harness retry loop can feed it back to the model:
--
--     case checkedJournal rows of
--       Left errs -> TIO.hPutStrLn stderr (explainJournalErrors errs) >> exitFailure
--       Right j   -> emitJournal (toAlg j)
--
--   `show errs` is also acceptable. `explainJournalErrors` is preferred
--   because it emits one readable line per structural error.
--
-- Balance:
--   A value accepted by checkedEntry / checkedJournal is constructively
--   balanced under exact equality. A separate balance assertion is not
--   required. You may still use `exactBalanced` for a local sanity check.
--
-- Output:
--   main MUST print exactly ONE JSON value to stdout, and nothing else.
--   Use EmitCanonical only:
--
--     emitJournal (toAlg j)                  -- journal-only tasks
--     emitObject [("journal", JournalComp (toAlg j)), ...]  -- object tasks
--
--   Do NOT hand-assemble JSON strings. Do NOT write a postingToJSON helper.
--
-- JVal builders for non-journal components:
--   jFlatNum  :: Real a => [(String, a)] -> JVal    -- flat numeric map ("derived")
--   jFlatStr  :: [(String, String)] -> JVal         -- flat string map ("decision")
--   jFindings :: [(String, String, String)] -> JVal -- (type, locus, detail) ("findings")
--   jNum :: Real a => a -> JVal
--   jInt :: Int -> JVal
--   JStr :: String -> JVal ; JBool :: Bool -> JVal
--   JArr :: [JVal] -> JVal ; JObj :: [(String, JVal)] -> JVal
--
-- Common AccountTitles (EXACT constructor spelling):
--   Assets:      Cash, Deposits, CurrentDeposits, AccountsReceivable,
--                MerchandiseInventory, Products, Fixtures, Machinery, Building,
--                Land, Software, Patent, PrepaidExpenses, AccruedRevenue
--   Assets (contra; home side = CREDIT):
--                AllowanceForDoubtfulAccounts, AccumulatedDepreciation
--                -- valuation accounts deducted from assets. They INCREASE on
--                -- the credit side (Not :< on credit), like a liability, but
--                -- they are classified as Assets with isContra = True.
--   Liabilities: AccountsPayable, LoansPayable, NotesPayable, AccruedExpenses,
--                UnearnedRevenue
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
--     ServiceRevenue      -> (mapped, e.g. Sales)
--     DepreciationExpense -> Depreciation
--     Inventory           -> MerchandiseInventory
--   Never invent a constructor: unmapped names WILL NOT COMPILE or will be
--   rejected by checked construction.
--
-- MoneyDecimal -> Decimal gotcha:
--   MoneyDecimal itself has NO RealFrac instance. For non-journal numeric
--   output derived from a MoneyDecimal, unwrap first:
--
--     jNum (toDecimal amount)
--
-- Minimal working example (cash sale for 1000, checked construction):
-- ----------------------------------------
-- import ExchangeAlgebra hiding (map)
-- import ExchangeAlgebra.Convert.Checked (JournalError, checkedJournal)
-- import ExchangeAlgebra.Assist (explainJournalErrors)
-- import ExchangeAlgebra.Journal (Journal, toAlg)
-- import EmitCanonical (emitJournal)
-- import qualified Data.List.NonEmpty as NE
-- import qualified Data.Text.IO as TIO
-- import System.Exit (exitFailure)
-- import System.IO (stderr)
--
-- type MinBase    = HatBase AccountTitles
-- type MinJournal = Journal String MoneyDecimal MinBase
--
-- sale :: Either (NE.NonEmpty (JournalError String MoneyDecimal)) MinJournal
-- sale = checkedJournal
--   [ ("t1",
--       [ (Debit,  Cash,  1000)
--       , (Credit, Sales, 1000)
--       ])
--   ]
--
-- main :: IO ()
-- main =
--   case sale of
--     Left errs -> TIO.hPutStrLn stderr (explainJournalErrors errs) >> exitFailure
--     Right j   -> emitJournal (toAlg j)
-- ----------------------------------------
```
