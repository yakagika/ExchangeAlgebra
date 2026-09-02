# ExchangeAlgebra transfer catalog

This catalog is the curated vocabulary for audit-eval arm A′ (EA-Gate) and arm A
(EA-Code). It was derived from the exported Haddock in
`ExchangeAlgebra.Algebra.Transfer`, `ExchangeAlgebra.Journal.Transfer`, and
`ExchangeAlgebra.Bookkeeping`.

The catalog distinguishes three guarantees:

1. **Bookkeeping layer**: a builder fixes the debit/credit routing and returns a
   balanced `Alg`. Closing transfers use registry closing metadata. Derived
   ledger, trial-balance, metric, and presentation values remain engine output.
2. **Consolidation and foreign-currency layer**: existing primitives are
   sufficient only after a named, validated recipe fixes their composition.
3. **Judgment layer**: EA can balance the postings selected by a policy, but it
   cannot establish that a lease, revenue, tax, or effective-interest policy was
   selected correctly.

`MkBase b = Hat -> AccountTitles -> b` is supplied by the loader. It is not an
agent parameter. In the signatures below, `v` is the exact non-negative value
type used by the harness, normally `MoneyDecimal`.

## Adopted library entries

All entries in this table are admitted catalog names. The context column names
the existing checked-conversion boundary. The guarantee column separately says
when the loader, rather than raw model postings, owns a result.

| Name | Module | Type signature | Parameters | Guarantee | Processing context | JGAAP use | Decision |
|---|---|---|---|---|---|---|---|
| `cogsAdjustmentEntries` | `ExchangeAlgebra.Bookkeeping` | `(HatVal v, ExBaseClass b) => MkBase b -> v -> v -> Alg v b` | beginning inventory, ending inventory | Four fixed 3-account-method postings; exact debit/credit equality; purchases and inventory sides fixed | `ClosingProcess` | Applicable to the periodic 3-account method | Adopt |
| `depreciationIndirectEntry` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> Alg v b` | depreciation amount | Dr Depreciation / Cr AccumulatedDepreciation; exact balance and sides | `ClosingProcess` | Applicable when the indirect method and amount are supplied | Adopt |
| `depreciationDirectEntry` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> AccountTitles -> Alg v b` | depreciation amount, depreciated asset | Dr Depreciation / Cr selected asset; exact balance and sides | `ClosingProcess` | Applicable when the direct method and amount are supplied | Adopt |
| `allowanceReplenishmentEntry` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> v -> Alg v b` | required ending allowance, current allowance | Computes only the shortage or release; fixes allowance expense/reversal and contra-asset sides; exact balance | `ClosingProcess` | Applicable to 差額補充法 | Adopt |
| `allowanceResetEntries` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> v -> Alg v b` | required ending allowance, current allowance | Releases the full current balance and records the full estimate; exact balance and sides | `ClosingProcess` | Applicable to 洗替法 | Adopt |
| `prepaidExpenseEntry` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> AccountTitles -> Alg v b` | prepaid amount, expense account | Dr PrepaidExpenses / Cr selected expense; exact balance and sides | `ClosingProcess` | Applicable when the unexpired amount is supplied | Adopt |
| `unearnedRevenueEntry` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> AccountTitles -> Alg v b` | unearned amount, revenue account | Dr selected revenue / Cr UnearnedRevenue; exact balance and sides | `ClosingProcess` | Applicable when the unearned amount is supplied | Adopt |
| `accruedRevenueEntry` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> AccountTitles -> Alg v b` | accrued amount, revenue account | Dr AccruedRevenue / Cr selected revenue; exact balance and sides | `ClosingProcess` | Applicable when the accrued amount is supplied | Adopt |
| `accruedExpenseEntry` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> AccountTitles -> Alg v b` | accrued amount, expense account | Dr selected expense / Cr AccruedExpenses; exact balance and sides | `ClosingProcess` | Applicable when the accrued amount is supplied | Adopt |
| `reversingEntry` | same | `Redundant a v b => a v b -> a v b` | previously validated entry; A′ refers to it by raw `sourceTxid` | Hat involution; `bar (x .+ reversingEntry x) = Zero`; preserves the correction trail before netting | `OrdinaryJournal` or `ClosingProcess`, according to the source entry | Applicable to opening reversals and correction entries; selection of the source entry remains external | Adopt |
| `consumptionTaxSettlementEntry` | same | `(HasCallStack, HatVal v, ExBaseClass b) => MkBase b -> v -> v -> Alg v b` | tax paid, tax received | Computes unpaid tax; fixes all three account sides; exact balance. Partial function: the loader/arm A code must check `received >= paid` before calling. | `ClosingProcess` | Applicable only to the documented net-payment case; refund position is rejected | Adopt |
| `corporateTaxInterimEntry` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> Alg v b` | interim payment | Dr PrepaidCorporateIncomeTaxes / Cr Cash; exact balance and sides | `OrdinaryJournal` | Applicable to interim payment | Adopt |
| `corporateTaxSettlementEntries` | same | `(HasCallStack, HatVal v, ExBaseClass b) => MkBase b -> v -> v -> Alg v b` | total tax, interim payment | Computes unpaid amount; fixes expense/prepayment/payable sides; exact balance. Partial function: check `interim <= total` before calling. | `ClosingProcess` | Applicable only when interim does not exceed total; refund position is rejected | Adopt |
| `equityMethodEarningsEntry` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> Alg v b` | investor share of investee net income | Dr InvestmentInAssociate / Cr EquityInEarningsOfInvestee; exact balance | `ConsolidationWorksheet` | Posting is valid once the share is supplied; ownership-policy correctness is external | Adopt |
| `equityMethodDividendEntry` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> Alg v b` | dividend received | Dr Cash / Cr InvestmentInAssociate; exact balance | `OrdinaryJournal` | Applicable under the equity method | Adopt |
| `equityMethodEntries` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> v -> Alg v b` | investor share, dividend | Composes the two equity-method builders without hand-written postings; exact balance | `ConsolidationWorksheet` | Applicable after method and share are determined | Adopt |
| `equityMethodBalance` | same | `(HatVal v, ExBaseClass b) => Alg v b -> v` | accumulated validated ledger | Projects and nets InvestmentInAssociate; derived carrying amount is not model-supplied | N/A, scalar engine projection rather than posting admission | Applicable to the equity-method roll-forward | Adopt |
| `priorPeriodErrorCorrection` | same | `(HatVal v, ExBaseClass b) => MkBase b -> v -> v -> AccountTitles -> AccountTitles -> Alg v b` | current-period part, prior-period part, expense account, affected asset | Forces current amount to expense, prior amount to RetainedEarnings, and their sum to the asset credit; exact balance | `ClosingProcess` via catalog authority | Applicable after the current/prior split has been determined | Adopt |
| `finalStockTransfer` | `ExchangeAlgebra.Algebra.Transfer` | `(HatVal n, ExBaseClass b) => Alg n b -> Alg n b` | validated before-closing ledger | Uses registry `CloseByDivision`, including contra-aware PIMO direction, transfers eligible P/L accounts to RetainedEarnings, then nets with `bar`; derived closing result is engine-owned | `EngineComputation` | Aligned with the account registry and the `Reporting.Metric` / `Reporting.Presentation` route | Adopt |

The identically named functions in `ExchangeAlgebra.Journal.Transfer` are not
separate catalog names. In particular, its `finalStockTransfer` gathers the
journal to the `plank` note before applying the final `bar`, so it does **not**
preserve txid notes. The loader must apply closing to the algebra value and keep
txid/call provenance in a separate table; it must not select the journal wrapper
on the assumption that notes survive.

## Excluded library entries

| Name | Module | Type signature | Haddock evidence or behavior | Decision and reason |
|---|---|---|---|---|
| `transfer` | `ExchangeAlgebra.Algebra.Transfer` | `(HatVal n, HatBaseClass b) => Alg n b -> TransTable n b -> Alg n b` | Applies an arbitrary caller-built relabeling table | Exclude from agent calls. It is infrastructure for reviewed recipes; exposing arbitrary rules would reopen the bypass. |
| `createTransfer` | same | `(HatVal n, ExBaseClass b) => [(b,b,(n -> n))] -> (Alg n b -> Alg n b)` | Compiles an arbitrary rule list | Exclude from agent calls for the same reason. |
| `incomeSummaryAccount` | same | `(HatVal n, ExBaseClass b) => Alg n b -> Alg n b` | Haddock: “legacy NetIncome/NetLoss balancing coordinate” and “not input for reporting presentation” | Exclude. Metrics must be derived from a validated before-closing trial balance. |
| `netIncomeTransfer` | same | `(HatVal n, ExBaseClass b) => Alg n b -> Alg n b` | Consumes the legacy engine-only NetIncome/NetLoss coordinates | Exclude as a public A′ call. `finalStockTransfer` is the curated closing path. |
| `grossProfitTransfer` | same | `(HatVal n, ExBaseClass b) => Alg n b -> Alg n b` | Haddock: “not a JGAAP gross-profit definition” and it excludes SalesCost and MerchandiseInventory | Exclude as SNA/simulation legacy. Use reporting metrics and presentation. |
| `ordinaryProfitTransfer` | same | `(HatVal n, ExBaseClass b) => Alg n b -> Alg n b` | Haddock: “not a complete JGAAP ordinary-profit definition” | Exclude as SNA/simulation legacy. Use the typed reporting metric API. |
| `retainedEarningTransfer` | same | `(HatVal n, ExBaseClass b) => Alg n b -> Alg n b` | Transfers only the legacy OrdinaryProfit coordinate | Exclude because its producer is excluded and incomplete for JGAAP. |
| `finalStockTransferStep` | same | `(HatVal n, ExBaseClass b) => Alg n b -> Alg n b` | Documented as an internal step and does not perform the final `bar` cancellation | Exclude. Partial closing is a bypass; expose only `finalStockTransfer`. |

`table`, `TransTable`, `(.->)`, and `(|%)` are construction syntax, not named
accounting operations. They remain library implementation primitives and are
not A′ call names.

## Named recipes in `examples/audit-eval/harness`

These signatures are designs for the loader layer. They do not claim that a
same-named function currently exists in `src/`.

| Name | Proposed signature | Layer | Guarantee and status | Decision |
|---|---|---|---|---|
| `straightLineDepreciation` | `AccountTitles -> MoneyDecimal -> MoneyDecimal -> PositiveInt -> PositiveInt -> DepreciationMethod -> Maybe RoundingPolicy -> Either CatalogError (Alg MoneyDecimal MinBase)` | Bookkeeping | Proposed types: `PositiveInt`, `DepreciationMethod`, `RoundingPolicy`, and `CatalogError` are loader-local, not current `src/` exports. Validates `salvage <= cost` and `period <= years`, computes a full-year amount under an explicit task rounding policy when division is inexact, then calls the direct or indirect builder. `period` records/validates the schedule position; it does not change a full-year straight-line amount. Mid-period proration is outside this draft. | Adopt for A′ |
| `consolidateInternalTransactions` | `Map TxId (Entity, Alg MoneyDecimal MinBase) -> NonEmpty TxId -> Either CatalogError (ValidatedConsolidationBatch Entity TxId MoneyDecimal)` | Consolidation | `Entity`, `TxId`, `CatalogError`, and `ValidatedConsolidationBatch` are proposed loader-local types. The batch retains validated sources and independently balanced adjustments but deliberately omits `WorksheetLinkage`, which cannot be derived from an unlabelled `Alg`. It verifies entity/txid references and applies the fixed `bar`-netting projection. | Adopt for A′ |
| `currencyTranslationWithCTA` | `FxPolicy -> Journal Entity MoneyDecimal FxBase -> Either FxError TranslationResult` | Foreign currency | All types other than `Journal` and `MoneyDecimal` in this signature are proposed. The composition must validate positive finite `FxRate`, partition accounts into non-overlapping policy buckets, apply `Transfer` rules to the `CountUnit` axis, and post only policy-explained translation differences to CTA. Never treat an unexplained balance residual as CTA. | Defer. The experiment has no FX category or authoritative real-problem ground truth. |

The `straightLineDepreciation` recipe is intentionally higher-level than
`depreciationIndirectEntry`: the existing builder guarantees the posting, but
not the arithmetic that produces its `amt` argument. The consolidation recipe
similarly fixes a composition that the library currently exposes only as
primitives.

The present `equityMethodEarningsEntry` is profit-direction only. An investee
loss requires a separate reverse-direction builder or an explicit scope
exclusion; raw consolidation postings are not an approved substitute.

## Judgment-layer boundary

The following are not call names in `aprime-calls.schema.json`:

| Area | Decision outside EA | What EA can still guarantee |
|---|---|---|
| Lease classification | PV/90%, term/75%, bargain option, and applicable standard | Balance and sides of the postings selected after classification |
| Revenue recognition | performance obligations, variable-consideration constraint, returns, SSP allocation, principal/agent | Balance and sides after a recognition policy supplies amounts and accounts |
| Deferred tax | temporary-difference inventory, tax rate, recognition, valuation allowance | Balance and sides after policy parameters are supplied |
| Effective-interest bonds | instrument terms, yield, schedule and rounding policy | Balance and sides of each generated amortization entry |

If a task requires one of these judgments and the input does not determine the
policy, the only valid escape hatch is a conditional answer that states the
missing condition. A balanced entry is not evidence that the policy selection
was correct.

## Count summary

- Library exports inventoried as accounting operations: 27.
- Library entries adopted: 19.
- Library entries excluded: 8.
- Loader recipes adopted: 2.
- Loader recipes deferred: 1 (`currencyTranslationWithCTA`).
- Draft callable catalog for A′: 21 names. The dispatcher does not yet exist in
  `LoadChecked.hs`; this count is the closed implementation target.

The security invariant is enforceable for A′ because its JSON enters the closed
loader. In arm A, excluded functions remain technically importable through the
library re-export; their exclusion is a prompt/treatment rule, not a Haskell
type-level prohibition.
