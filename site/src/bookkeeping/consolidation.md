# Consolidation

On February 1, the shop uses 3,000 yen from its bank deposits to form a wholly owned subsidiary. Prepare an opening consolidated balance sheet at that date from the two companies’ trial balances. A consolidation worksheet keeps the two source balances and the group adjustments (`WorksheetAdjustment`) as separate inputs.

Use exchangealgebra-0.6.1.1 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). Save the Haskell blocks in their displayed order as `Main.hs`.

## Prepare the two trial balances {#prepare-the-two-trial-balances}

Carry forward the shop’s January balance-sheet amounts from [Build the balance sheet](statements.md#build-the-balance-sheet). The program checks that the January ledger is in yen before dropping the currency element. Both February entries use the resulting reporting base, `HatBase AccountTitles`, with amounts in yen.

<details>
<summary>

Code carried over from earlier chapters

</summary>

```haskell
{{#include ../../code/consolidation.hs:setup}}
```

```haskell
{{#include ../../code/consolidation.hs:january}}
```

```haskell
{{#include ../../code/consolidation.hs:conversion}}
```

```haskell
{{#include ../../code/consolidation.hs:reporting}}
```

```haskell
{{#include ../../code/consolidation.hs:adjustment}}
```

```haskell
{{#include ../../code/consolidation.hs:closing}}
```

```haskell
{{#include ../../code/consolidation.hs:statement-display}}
```

</details>

In the parent’s books, debit subsidiary shares and credit deposits for 3,000 yen. In the parent’s standalone balance sheet, `SubsidiaryStocks` represents subsidiary shares carried at acquisition cost, with no equity-method adjustment. In the subsidiary’s books, debit cash and credit capital stock for 3,000 yen. Use `bar` to obtain the parent’s February 1 balances, then identify each entity’s balances with a `TrialBalanceSource`.

```haskell
{{#include ../../code/consolidation.hs:entity-balances}}
```

## Eliminate the investment and subsidiary capital {#eliminate-the-investment-and-subsidiary-capital}

The parent’s subsidiary shares and the subsidiary’s capital stock record the same contribution within the group. Eliminate them with a debit to `CapitalStock` and a credit to `SubsidiaryStocks`, each for 3,000 yen. Both terms decrease a balance, so both use `Hat`. Record the entry as a `WorksheetAdjustment` linked to both companies. The adjustment belongs to the consolidated worksheet.

```haskell
{{#include ../../code/consolidation.hs:elimination}}
```

## Link the statement columns {#link-the-statement-columns}

Supply the period result, its attribution, and the equity roll-forwards in a `WorksheetLinkage`. February’s result is zero at incorporation. January’s 1,100 yen is February’s opening retained earnings. With zero dividends and a zero February result, the closing figure used in the roll-forward and the balance-sheet figure are also 1,100 yen at the reporting date. The parent owns all the shares, so the noncontrolling-interest fields have zero amounts or `NoBalance`.

`validateConsolidationWorksheet` checks the balance of each source and adjustment, the adjustment’s source IDs and permitted accounts, and the equations among the supplied linkage figures. Cross-check those figures against the statement balances separately. The example takes retained earnings from the January ledger and checks the consolidated balances after combining.

```haskell
{{#include ../../code/consolidation.hs:linkage}}
```

## Inspect the consolidated balance sheet {#inspect-the-consolidated-balance-sheet}

`combinedWorksheet` adds the two trial balances and the elimination with `.+`, retaining terms on the same base. Apply `bar` explicitly to inspect the net totals, and present the combined ledger as a `Combined` balance sheet.

```haskell
{{#include ../../code/consolidation.hs:results}}
```

In your project directory, run `stack exec -- ghc -Wall -Werror Main.hs -o consolidation-example`, then run `./consolidation-example`.

```text
{{#include ../../code/consolidation.expected}}
```

The investment has no remaining balance. Capital stock is 10,000 yen, the parent’s own capital. The group shows cash of 8,300 yen and deposits of 2,000 yen. Together with inventory of 200 yen and prepaid expenses of 600 yen, assets total 11,100 yen. There are no liabilities, and equity totals 11,100 yen.

Tested with exchangealgebra-0.6.1.1 on 2026-11-20.

## Reference {#reference}

| Type or operation | Inputs and result |
|---|---|
| `TrialBalanceSource` | Identify one entity and supply its balanced trial balance over `HatBase AccountTitles`. |
| `WorksheetAdjustment` | Supply an independently balanced adjustment with its own ID and the nonempty list of source IDs it affects. |
| `WorksheetLinkage` | Supply the period result and its attribution, and reconcile opening balances, period movements, dividends, and closing balances for retained earnings and noncontrolling interests. |
| `validateConsolidationWorksheet input` | Validate the worksheet or return a nonempty list of `WorksheetError` values identifying the rejected inputs. |
| `combinedWorksheet`, `validatedSources`, and `validatedAdjustments` | Project the combined ledger, or retrieve the original source and adjustment columns to trace each contribution. |
