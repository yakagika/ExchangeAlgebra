# SKILL-ea — ExchangeAlgebra harness cheatsheet for arm A

version: v2.1

## Changelog

- **v2.1** (2026-08-16): V-Land 3 — added the complete JCCI level-2/3
  vocabulary, EDINET-aligned display names, five contra accounts, and the
  policy-ambiguous Japanese aliases documented below.
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
-- JCCI 2022 extension constructors (EXACT spelling):
--   Assets:
--     TimeDeposits, LoansReceivable, GiftCertificatesReceived,
--     SecurityDepositsPaid, SuppliesOnHand, ContractAssets,
--     IncomeTaxesRefundReceivable, WorkInProcess, DeferredTaxAssets, LeasedAssets,
--     ToolsAndInstruments, ConstructionInProgress, Goodwill, SoftwareInProgress,
--     LongTermPrepaidExpenses, DishonoredNotesReceivable, PrepaidPensionCost,
--     NetDefinedBenefitAsset, DepositsInSpecialAccounts, Structures,
--     LeaseholdRights, NonOperatingNotesReceivable,
--     NonOperatingElectronicallyRecordedReceivable
--   Liabilities:
--     RefundLiabilities, NonOperatingNotesPayable,
--     NonOperatingElectronicallyRecordedObligations, BonusesPayable,
--     AllowanceForRepairs, AllowanceForProductWarranties, AllowanceForBonuses,
--     DeferredTaxLiabilities, LeaseObligations, GuaranteeDepositsReceived,
--     AllowanceForRetirementBenefits, LongTermOtherPayables,
--     NetDefinedBenefitLiability
--   Equity:
--     StockSubscriptionDeposits, LegalCapitalSurplus, OtherCapitalSurplus,
--     DividendEqualizationReserve, RepairFundReserve, ConstructionFundReserve,
--     GeneralReserve, ValuationDifferenceOnOtherSecurities,
--     NonControllingInterests, CapitalSurplus, EarnedSurplus
--   Revenue:
--     ServiceRevenue, OperatingRevenue, GainOnSalesOfSecurities,
--     GainOnValuationOfSecurities, DividendsReceived, InterestOnSecurities,
--     GainOnSalesOfInvestmentSecurities, InsuranceGain, GainOnBargainPurchase,
--     ReversalOfAllowanceForRepairs, ReversalOfAllowanceForProductWarranties,
--     GainOnDonationOfFixedAssets, GainOnNationalSubsidies,
--     GainOnConstructionGrants, LandRentReceived, SalesRebates
--   Expenses:
--     CostOfServices, OperatingExpenses, InventoryShrinkageLoss,
--     LossOnValuationOfMerchandise, Bonuses, RetirementBenefitExpenses,
--     ProvisionForRepairs, ProvisionForBonuses, ProvisionForProductWarranties,
--     ResearchAndDevelopmentExpenses, AmortizationOfGoodwill,
--     AmortizationOfSoftware, AmortizationOfPatents, LeaseExpenses,
--     IncorporationExpenses, StockIssuanceCosts, BusinessCommencementExpenses,
--     DevelopmentExpenses, LossOnSalesOfElectronicallyRecordedReceivables,
--     LossOnSalesOfReceivables, LossOnSalesOfSecurities,
--     LossOnValuationOfSecurities, LossOnSalesOfInvestmentSecurities, LossOnFire,
--     LossOnRetirementOfFixedAssets, LossOnReductionOfFixedAssets,
--     AdditionalIncomeTaxesForPriorPeriods, RefundOfIncomeTaxes, PurchaseRebates,
--     WelfareExpenses, MaintenanceExpenses, StatutoryWelfareExpenses,
--     LandRentPaid, InsuranceExpense, RepairsExpense, StorageExpenses,
--     MembershipFees
--   Additional specialized accounts by registry division:
--     Assets: SuspenseAccount, ContraAccountForGuaranteeObligations,
--       BranchCurrentAccount, TradingSecurities, HeldToMaturityBonds,
--       SubsidiaryStocks, AffiliateStocks, AvailableForSaleSecurities
--     Liabilities: GuaranteeObligations, HeadOfficeCurrentAccount
--     Revenue: ForeignExchangeGains, NetLossAttributableToNCI
--     Expenses: ForeignExchangeLosses, IncomeTaxesAdjustment,
--       NetIncomeAttributableToNCI
--     Bookkeeping device: IncomeSummary. Its registry Assets/NoClose metadata
--       is a technical debit-side placeholder, not a balance-sheet asset; do
--       not use it in statement classification or expect automatic closing.
--
-- Additional contra accounts (reversed home side):
--   SalesRebates (Revenue -> Debit), RefundOfIncomeTaxes (Cost -> Credit),
--   PurchaseRebates (Cost -> Credit).
--
-- Japanese JCCI A/B labels are accepted by parseAccountTitle. These 21 labels
-- intentionally fail as AmbiguousAccount: 銀行預金, 〇〇商店, 貸付金, 仮払金,
-- 有価証券, 投資有価証券, 関係会社株式, 借入金, 未払金, 仮受金, 営業収益,
-- 有価証券運用益, 通信費, 地代家賃, 支払賃借料, 支払不動産賃借料, 営業費用,
-- 有価証券運用損, 為替差損益, 有価証券評価損益, 有価証券運用損益.
-- Never pick one silently. For Haskell source, prefer the exact constructors above.
--
-- *** Accounts NOT in AccountTitles ***
--   Some task chart names still do not exist in EA (e.g.
--   DepreciationExpense, Inventory). The task input provides an
--   "EA account mapping" (ea_account_map). ALWAYS use the mapped EA name:
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

## Transfer catalog

catalog version: v3 draft (2026-09-02). This section is the v3 addition; the
v2.1 text above is preserved verbatim for treatment continuity.

Use the curated catalog for closing adjustments, closing transfers, and
consolidation. Do not recreate a catalog operation with `checkedEntry`, and do
not calculate a value that the selected recipe derives. The harness loader will
reject unauthorized direct A′ postings to protected coordinates such as
`RetainedEarnings` and `IncomeSummary`; legitimate opening balances and owner
transactions use the harness's explicit authorized path. In arm A this remains
a mandatory treatment rule, because excluded library exports are still
technically importable. This section overrides the copied vocabulary list's
mere availability of `RetainedEarnings`. `NetIncome`, `NetLoss`, `GrossProfit`,
and `OrdinaryProfit` are also engine/legacy coordinates, not model output.

Add this import for the posting builders:

```haskell
import qualified ExchangeAlgebra.Bookkeeping as BK

type MinBase = HatBase AccountTitles
mk :: BK.MkBase MinBase
mk = (:<)
```

### Minimal examples

Each adopted library entry has a minimal invocation below. Amount arguments
must come from the task input or a named recipe, not an unreported hand
calculation.

```haskell
-- Periodic COGS: beginning inventory, ending inventory
cogs = BK.cogsAdjustmentEntries mk 100 50

-- Depreciation when the task already supplies the period amount
depIndirect = BK.depreciationIndirectEntry mk 24000
depDirect   = BK.depreciationDirectEntry mk 24000 Fixtures

-- Allowance methods: required ending estimate, current balance
allowanceDiff  = BK.allowanceReplenishmentEntry mk 2400 1000
allowanceReset = BK.allowanceResetEntries mk 2400 1000

-- Deferral and accrual
prepaid  = BK.prepaidExpenseEntry mk 4000 RentExpense
unearned = BK.unearnedRevenueEntry mk 4000 RentalIncome
accrRev  = BK.accruedRevenueEntry mk 2500 InterestEarned
accrExp  = BK.accruedExpenseEntry mk 2500 InterestExpense

-- Reverse a previously built and validated entry
openingReversal = BK.reversingEntry accrExp

-- Tax: both settlement builders are partial, so check their preconditions.
consumptionTax =
  if received >= paid
  then BK.consumptionTaxSettlementEntry mk paid received
  else error "refund position is outside this builder"
taxInterim = BK.corporateTaxInterimEntry mk 500000
taxClosing =
  if interim <= total
  then BK.corporateTaxSettlementEntries mk total interim
  else error "refund position is outside this builder"

-- Equity method
equityEarnings = BK.equityMethodEarningsEntry mk 438000
equityDividend = BK.equityMethodDividendEntry mk 800000
equityBoth     = BK.equityMethodEntries mk 438000 800000
equityCarryingAmount = BK.equityMethodBalance (acquisition .+ equityBoth)

-- Prior-period correction: current, prior, expense account, asset account
priorCorrection =
  BK.priorPeriodErrorCorrection
    mk 5500 11000 AmortizationExpense Patent

-- Registry-driven final closing. Do not post RetainedEarnings yourself.
closedLedger = finalStockTransfer beforeClosingLedger
```

`acquisition` and `beforeClosingLedger` in the examples stand for values already
constructed through checked input. They are not manually assembled with `.@`.
Calling `BK.equityMethodBalance` is the permitted engine projection; printing a
hand-computed literal in its place is not. The current earnings builder covers
an investee profit, not a loss.

### Named loader recipes

Two adopted A′ names fix compositions that are not yet same-named functions in
`src/`. Do not invent Haskell identifiers for them.

- `straightLineDepreciation` validates cost, salvage value, life, period, and
  an explicit rounding rule when division is inexact, derives a full-year
  amount, and delegates to the direct or indirect
  depreciation builder. In arm A code, use the existing depreciation builder
  only when the task already supplies the period amount; otherwise return the
  missing policy/rounding condition instead of silently hand-calculating it.
  Mid-period proration is outside this draft.
- `consolidateInternalTransactions` fixes the existing Journal-note provenance,
  independently balanced worksheet adjustments, and `bar`-netting composition.
  In arm A code, preserve entity notes and validate every elimination before
  combining them. Do not replace it with one batch-balanced journal.

Minimal A′ JSON invocations for the two proposed loader recipes are:

```json
{"name":"straightLineDepreciation","params":{"asset":"Fixtures","cost":120000,"salvage":0,"years":5,"period":1}}
{"name":"consolidateInternalTransactions","params":{"entities":[{"entity":"parent","txids":["p-sale"]},{"entity":"subsidiary","txids":["s-buy"]}],"eliminationTxids":["elim-sales"]}}
```

`equityMethodBalance` is an engine projection: emit the underlying checked
journal and let the harness derive the carrying amount. Do not print a
model-computed substitute.

### Excluded transfers

Do not use `incomeSummaryAccount`, `netIncomeTransfer`,
`grossProfitTransfer`, `ordinaryProfitTransfer`, `retainedEarningTransfer`, or
`finalStockTransferStep`. The first five depend on legacy intermediate/subtotal
coordinates; the step function is an incomplete closing operation. Do not build
arbitrary `TransTable` rules with `transfer` or `createTransfer`. Use
`Reporting.Metric` and `Reporting.Presentation` for statement metrics and
presentation.

### Judgment-layer escape hatch

Lease classification, revenue recognition, deferred tax, and bond
effective-interest accounting require policy predicates or parameters outside
EA. A balanced result proves only the downstream posting structure. Take policy
parameters from the task input. If a required parameter or applicable policy is
missing, give a conditional answer of the following form instead of choosing
silently:

```text
If <explicit policy condition>, use <catalog operation and parameters>.
Otherwise <the alternative treatment>. The input does not determine which
condition applies, so no unconditional posting is emitted.
```
