# Built-in account titles

The tables group concrete account titles by `asDivision`. Within each table, rows follow the order of `concreteAccountTitles`.

- **Constructor** is the Haskell constructor name.
- **English name** and **Japanese name** give the English name and the Japanese display name without notes.
- **Fixed/current** gives the fixed or current classification. `Other` covers accounts outside these classes.
- **Home side** gives the side on which you record an increase, or indicates that a fixed side does not apply.
- **Closing rule** gives the rule for automatic closing.
- **Roles** lists roles other than `OrdinaryAccount`, including `ContraAccount` for contra accounts. An account with only the ordinary role has a blank cell.

See [Account titles and their attributes](attributes.md) for the meaning of each value.

## Assets

{{#include ../../code/account-titles.expected:assets}}

## Liabilities

{{#include ../../code/account-titles.expected:liabilities}}

## Equity

{{#include ../../code/account-titles.expected:equity}}

## Revenue

{{#include ../../code/account-titles.expected:revenue}}

## Expenses

{{#include ../../code/account-titles.expected:expenses}}

The tables are generated from the account registry.

<details>
<summary>

Table generation code (excerpt)

</summary>

```haskell
{{#include ../../code/account-titles.hs:rows}}
```

</details>

Tested with exchangealgebra-0.6.0.1 on 2026-11-20.
