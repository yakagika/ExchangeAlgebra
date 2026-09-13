# A′ v3 実装・検証報告

2026-09-13. git 操作・commit は行っていない. `src/`, `gen/`, `score.py`,
SKILL-ea-v3.md と chart-of-accounts / task 描画は編集していない.

A′ v3 loader と runner は実装済み. 全体受入は以下の指定外 2 点を残す.

- (d, 別 scope) `gen/accounts.py` の科目 mirror に EntertainmentExpenses,
  MeetingExpenses, NewspaperBooksExpenses, RawMaterials, GoodsInTransit が無い.
  `gen/tests/test_selftest.py:64` が失敗する. 着地先は coordinator の科目同期 land;
  その後, 下記全体 pytest を再実行する.
- (d, 別 scope) brief (c) の全欠陥拒否は現行 V full の契約では満たせない.
  `category_violation` / `balance_mismatch` は構造 gate の検出対象外.
  着地先は coordinator の V full / 宣言 §6 の整合裁定.
  今回は既存 V を変更せず, 実際の受理・拒否境界をテストに記録した.

## 変更ファイル

以下は `examples/audit-eval/` 相対. 新規は最後の 4 ファイル.

| ファイル | 実装内容 |
|---|---|
| harness/LoadChecked.hs | additive --contract v3, exact JSON/decimal, 21 call, trusted opening/task, context/stage/reference/重複/保護座標/allowlist 検査 |
| harness/EmitCanonical.hs | v2 printer を維持した txid 付き canonical printer |
| harness/aprime-calls.schema.json | call.txid, 3 call の事実入力形式, rounding 禁止 |
| harness/APRIME-CALLS.md | 実装後の契約・内部 trusted metadata・派生経路 |
| harness/BYPASS-GUARD-DESIGN.md | 実装 receipt の追記のみ |
| runner/arms.py | A′ v3 prompt・loader・trusted facts・Decimal・category 別 EA 派生 |
| runner/run.py | A′ contract flag, dispatch, resume/record metadata |
| README.md | A′ v3 選択・実行方法と制約 |
| runner/tests/test_conformance.py | (a)-(f) の conformance |
| runner/tests/test_aprime_catalog.py | 21 call と追加境界, 編集前 v2 byte fixtures |
| runner/tests/test_aprime_v3_runner.py | trusted 入力・Decimal・retry・dispatch |
| runner/tests/APRIME-V3-REPORT.md | 本報告と pin 一覧 |

## 検証

実行コマンド:

```sh
UV_CACHE_DIR=/private/tmp/uv-cache UV_PROJECT=examples/audit-eval uv run pytest examples/audit-eval -q
stack build --test --bench --no-run-benchmarks
```

- 全体 pytest: **166 passed, 1 failed**. 失敗は上記の既存科目 mirror のみ.
- 新規試験: **109 passed** = conformance 36 + catalog/boundary 66 + runner 7.
- CI と同じ stack build/test/bench build: 成功. 既存 runner/selftest.py も全 case 成功.
- Haskell 呼出は stack exec runghc, stack cache は ~/.stack.

| brief | 結果 |
|---|---|
| (a) | 4 kind × seeds 3/19 の 8 case と closing fixed_asset/mixed × seeds 3/19・50 取引の 4 case, 計 12 case. txid×side/account/amount multiset と canonical journal 由来 EA derived が GT と一致 |
| (b) | 指定の否定例 + possible_duplicate_effect の 9 case, 固定 raw reason を確認. closing id 予約・擬似締め・source 偽装などの追加境界も通過 |
| (c) | 4 kind の V full GT は全通過. gen/defects.py の 4 種は imbalance/hallucinated_account の 2 種が拒否, 意味的 2 種は受理. 全欠陥拒否という要件は未充足 |
| (d) | 4 kind × correct / derived の 1 科目 side 反転 / 非提出の 3 outcome, 計 12 判定が期待どおり |
| (e) | retry 3, models.toml の両 backend timeout 3600, --skill v3 の実際の選択先, SKILL と 5 role 文の SHA-256 を固定 |
| (f) | v2 引数省略/明示の 2 case. 別途, 編集前に退避した loader と stdout/stderr/exit を照合した 8 case の byte hash を固定. 既存 selftest も成功 |

## 21 call

全て dispatcher 実装済みで, 独立した仕訳期待値または projection/recipe の単体試験済み.

| Call | 単体試験 |
|---|---|
| `cogsAdjustmentEntries` | PASS |
| `depreciationIndirectEntry` | PASS |
| `depreciationDirectEntry` | PASS |
| `allowanceReplenishmentEntry` | PASS |
| `allowanceResetEntries` | PASS |
| `prepaidExpenseEntry` | PASS |
| `unearnedRevenueEntry` | PASS |
| `accruedRevenueEntry` | PASS |
| `accruedExpenseEntry` | PASS |
| `reversingEntry` | PASS |
| `consumptionTaxSettlementEntry` | PASS |
| `corporateTaxInterimEntry` | PASS |
| `corporateTaxSettlementEntries` | PASS |
| `equityMethodEarningsEntry` | PASS |
| `equityMethodDividendEntry` | PASS |
| `equityMethodEntries` | PASS |
| `equityMethodBalance` | PASS |
| `priorPeriodErrorCorrection` | PASS |
| `finalStockTransfer` | PASS |
| `straightLineDepreciation` | PASS |
| `consolidateInternalTransactions` | PASS |

## SHA-256 pins

role は以下に示す Python 定数の UTF-8 bytes. A の SKILL は role と別に file bytes を固定.
変更時は `test_conformance.py` の hash assertion が失敗する.

| 対象 | SHA-256 |
|---|---|
| SKILL-ea-v3.md | `9db2e477c1badf0712efbb0d301445c48210a017bb8931c2794c8acc1ed6bd05` |
| C (_ARM_C_ROLE) | `69ac16a854dd989f0be67296d02cd21acbbae844147f46b003123b7caff67e80` |
| Aprime (_ARM_APRIME_V3_ROLE) | `478e7f95b694eaad07333c7b5820de6a9a2cfcdca7206b34891ae21bbaabf965` |
| V (_ARM_V_FULL_ROLE) | `384d95277ae021b61ec394978ceb8964a760fb4791c85f79477a8a41b580df3f` |
| A (_EA_MINIMAL_ROLE) | `0b99b5c33ee5fd0b39cd6be4408b8c06f8fb784e94a33c76536936c93591a0b1` |
| B (_ARM_B_ROLE) | `c9f4c875889e7dcb03c57632cdddbc5aedcfd7e678db22ea709d7a86e1da32f5` |

編集前 v2 fixture の stdout hash (ケース順は test_aprime_catalog.py の V2_BASELINES).
全 8 case の stderr は空で SHA-256 =
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`, exit = 0.
これらは現在の runtime で編集前後の byte parity を確認した replay fixtures.

| Case | stdout SHA-256 |
|---|---|
| 0 | `b6514755d82779f29fe8946a1b637ad9df722e0f0d0745bfd5ecccfff009839e` |
| 1 | `951ee527119a5849818c5c66e0563f70bccdac569a3a5780c7ee319c362786d1` |
| 2 | `c455ff4071da3bf4e959b20fdd7818a0210c40720008c860ef8011ed80a8add4` |
| 3 | `6dad9f8b0e55ca015c6efe7d88398d58c0ec3cee8052f24165ccc2b2a07c7a7a` |
| 4 | `28c24f67a1f3e0806b78ec6418cffac1d3fbd37df8c9bca3d60e98252254a22f` |
| 5 | `a82b18fa4d01f841e8a67bb4bf710e9cf8d5da79f5e74025f3c23980069eac36` |
| 6 | `b6514755d82779f29fe8946a1b637ad9df722e0f0d0745bfd5ecccfff009839e` |
| 7 | `c00d9762bed6199cf4e4271fcb2b2fb6c29d85f899a6a7644b97704e3601a2f3` |

## 仕様差分と採用した解

- **src API の不足: なし.** 既存 checked conversion, Bookkeeping builders,
  Algebra.Transfer と bar で構成した. consolidateInternalTransactions は既存
  primitive を閉じた recipe として組み合わせ, full ValidatedWorksheet の生成は主張しない.
- **既存 schema の amount 要求と「モデルは金額を計算しない」の衝突:**
  allowanceReplenishmentEntry に rate_basis_points,
  prepaidExpenseEntry に payment/coverage/next-period months,
  accruedExpenseEntry に principal/rate/months の代替 params を追加.
  call 名は 21 のまま, 既存 amount/estimate 形式も残した. schema と APRIME-CALLS を更新.
- **期中取引と決算整理の同額重複:** trusted `task.ordinary_txids` を内部入力へ additive に追加.
  runner が given.transactions の数値 amount 付き id のみから作る.
  モデルの task は拒否し, モデルの sources 主張だけでは重複検査から除外されない.
  これにより期中の depreciation/accrual と別の adj call を異なる stage として検査する.
- **sources の部分 coverage:** opening と parameter-only 取引に架空の scalar amount を作らない.
  指定 source id の存在・canonical debit total を call 実行後に照合する.
  call でその source 取引を生成した場合も金額照合を行う. v2 の全 coverage 動作は維持.
- **closing / consolidation の派生:** gen/DeriveEA.hs の mode 入力は仕訳を新たに生成する recipe.
  既に生成済みの canonical journal に再適用すると二重計上になるため, A′ 内で category に応じて
  既存 derive_fn を呼ぶ. closing は full / pre-closing の EA 出力を closingDerivedPairs と同じ
  key 規則で合成し, consolidation は消去済みの canonical journal を直接派生する.
  txid は DeriveEA が既存対応する entry へ写す. gen の編集は不要.
- **保護境界:** closing_txid は raw にも予約. GeneralReserve を保護し,
  raw の P/L と equity が同一 txid に共存する擬似締めを拒否する.
  Cash/CapitalStock の通常取引は許可. 株式交付費と資本を同一 raw txid に含める複合取引は
  この実験境界では対象外と文書化した.
- **数値範囲:** JSON 文法と MoneyDecimal の表現範囲を分離.
  表現できない小数も structured data error で返し, infrastructure failure としない.
  straightLineDepreciation の rounding 案内を prompt/schema から除き, hash を確定した.
- **連結の entity 帰属:** モデルが宣言する distinct entity / disjoint txid 集合と独立 balance を検証.
  経済的な entity 帰属の正しさは独立証明しない. Coordinator の unscored linkage 裁定に従う.

## 独立レビューと裁定

別ベンダ Claude の read-only review と修正範囲の再レビューを実施.
初回指摘の正当な期中取引の duplicate_effect は fixed_asset seed19/count50 で親が再現し,
上記 trusted ordinary_txids と 4 stress case で解消した. 再レビューは修正群の残存 blocker なし.
レビュー側は実行を行わず, hash/GT/byte の実測検証は親が担当した.

| 指摘 | 裁定・根拠 |
|---|---|
| 正当な期中取引の誤拒否 | task.ordinary_txids で stage を区別. sources では免除しない. [harness/LoadChecked.hs:891](../../harness/LoadChecked.hs#L891) |
| raw closing id / P/L-to-equity | raw id 予約と group の分類検査を追加. [harness/LoadChecked.hs:903](../../harness/LoadChecked.hs#L903) |
| 表現範囲外小数が infrastructure error | 文法と数値範囲を分離し data rejection. [harness/LoadChecked.hs:1043](../../harness/LoadChecked.hs#L1043) |
| rounding prompt 矛盾 | 禁止を明記し A′ role hash を更新. [runner/arms.py:509](../../runner/arms.py#L509) |
| ordinary_txids の説明不足 | 数値 amount 付き given 取引から作る規則を追記済み. [harness/APRIME-CALLS.md:285](../../harness/APRIME-CALLS.md#L285) |
| CapitalStock 等を全て raw 禁止にする案 | 通常の Cash/CapitalStock は保持し, 擬似締めを group 単位で拒否する案を採用. [harness/LoadChecked.hs:909](../../harness/LoadChecked.hs#L909) |

## 指定外の数値処理

(d, 別 scope) 既存 score.py の `_posting_fields` は float で金額を比較する.
また runner/run.py の外側 record は既存 `default=str` により Decimal を文字列化するが,
A′ の `json_str` は exact numeric literal を保持する. 今回の生成 suite は整数なので
conformance の GT 一致には影響しない. 小数を含む将来の採点・record 契約は coordinator の
数値契約拡張へ引き継ぐ. loader と A′ model/loader 往復の精度検証とは区別する.
