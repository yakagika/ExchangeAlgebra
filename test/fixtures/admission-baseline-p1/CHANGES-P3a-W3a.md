# P3a-W3a の golden の差

骨格の版は `79fe5a385a42b47c8b9984553260ece27da2cc95`.
率の検査は `admit` から `basisPoints` へ移る. 受理領域と diagnostic は同じである.

| case 名 | file と行 | 差の種類 | p3 列が `-` か |
|---|---|---|---|
| `invalid-allowance-rate` | `boundary.tsv:66` | 行を削除. 不正な率は `AllowanceRateCall` を作れず, `admit` に届かない | はい |

旧行の `RateAboveLimit 10001` は `Admission.Spec` の `testParameterTotality` と
`Admission.CatalogSpec` の `testClosedProblems` が `basisPoints` の `Left` として確認する.
負の率の `NegativeParameter RateParameter (-1)` も同じ test が確認する.
`boundary.tsv` の case 数は 64 から 63 へ変わる. 他の golden 行は変更しない.
