# P3a-W2b の golden の差

骨格の版は `8f26b4f1527f1af8993c4a2ed9a1293cb5ffb52a`.
受入の判定と生成仕訳は変えず, 観測に出る Kind の名前だけを変更した.
case 名と過去の段の記録は保持した.

| case 名 | file と行 | 差の種類 | p3 列が `-` か |
|---|---|---|---|
| `full-period` | `boundary.tsv:58` | Kind の改名: `DepIndirectKind` → `DepreciationIndirectKind` | いいえ |
| `DepIndirect` | `catalog.tsv:4` | Kind の改名: `DepIndirectKind` → `DepreciationIndirectKind` | はい |
| `DepDirect` | `catalog.tsv:5` | Kind の改名: `DepDirectKind` → `DepreciationDirectKind` | はい |

Kind の改名は 3 行, Call constructor の `Show` の差は 0 行.
`p3 = -` の 2 行も Kind の表示名だけが変わり, その他の観測は同じである.
