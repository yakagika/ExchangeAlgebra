# P3a-W2c の golden の差

骨格の版は `9cf715a5c65a4f997d8bf893e2642ff25b1a51d6`.
`FinalStockCall` の生成仕訳を `closingEntries` に切り替え,
base ごとの反対仕訳と利益剰余金への仕訳の組を保持する.

| case 名 | file と行 | 差の種類 | p3 列が `-` か |
|---|---|---|---|
| `full-period` | `boundary.tsv:58` | closing の posting が 3 本から 4 本へ変わる. ledger と Closed snapshot に現れる | いいえ |

生成 key は `TxKey (EntityId "A") (PeriodId "2026") (TxId "closing")`.
旧と新の生成仕訳を, golden と同じ `(AccountTitles, Hat, MoneyDecimal)` の形で示す.

```text
旧: [(RetainedEarnings,Not,80),(Depreciation,Hat,20),(Sales,Hat,100)]
新: [(RetainedEarnings,Hat,20),(RetainedEarnings,Not,100),(Depreciation,Hat,20),(Sales,Hat,100)]
```

`bar` で観測する純額は同じである. 利益剰余金の純額は貸方 80 で,
減価償却の反対仕訳は貸方 20, 売上の反対仕訳は借方 100 のままである.
借方の総額は利益剰余金の借方 20 が残るため 100 から 120 へ変わる.
この case には生成 key の証拠額の指定が無く, 受入の成否は変わらない.
証拠額 120 の受理と 100 の拒否は `Admission.Spec` の回帰 test で確認する.

`boundary.tsv:59` (`full-period-statements`) と `catalog.tsv:21` (`FinalStock`) は不変.
前者は純額の財務諸表, 後者は売上だけの締めなので posting の観測にも差がない.
3 file の全行を独立に再生成した候補と照合し, 変更は上記の 1 行だけである.
