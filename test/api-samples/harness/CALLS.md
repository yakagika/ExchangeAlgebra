# 珈琲店の 2026 年 9 月の仕訳提出仕様

entity は `roastery`, period は `2026-09`, 通貨は JPY. 以下の事実を使い,
月初の振り戻し, 未記帳取引, 決算整理, 締めを含む JSON を 1 個返す.
Markdown の fence や説明文は回答へ含めない. 棄却の通知を受けたら全体を再提出する.
この仕様と `calls.schema.json` は手書きの利用者側 protocol である.
対象 API は依頼で指定された origin/develop 2e3318c の公開 Haddock.

出典: `ExchangeAlgebra.IO.Input.Admission` の Description / Admit one transaction,
Identifiers and entries (`EntityId`, `PeriodId`, `TxKey`), Untrusted submission (`Submission`).
JSON 形式, JPY, 日付と txid の対応は library の形式からの推測ではなく, このハーネスの規約.

## 回答の構造

最上位は `{"transactions": [...], "calls": [...]}` のみ.
`transactions` の各要素は `{"txid": "...", "postings": [...]}`.
posting は `{"side": "Debit", "account": "Cash", "amount": 400000}` の形.
`side` は `Debit` または `Credit`. 勘定は後述の英語識別子をそのまま使う.
posting の金額は正, catalog の金額引数は非負. この題材では全て整数の円でよい.
負数で増減を表さず借貸を入れ替える. 1 仕訳内の借方合計と貸方合計は等しくする.
配列の重複を取り除く処理はしないため, txid と call id は重複させない.
未定義の field は使わない. entity と period は回答へ書かず, executor が固定値を使う.

出典: 同 module の `RawPostings`, `Submission`, `Call`, `CatalogCall`, `admit`.
decoder は `ExchangeAlgebra.IO.Input.normalizeTitle` で借貸名を正規化し,
`DebitSide` / `CreditSide` に読む. 勘定名の変換は同 module の `parseAccountTitle`
(Documentation). JSON field 名と envelope はハーネスで設計した.
厳密な金額変換は `ExchangeAlgebra.Algebra.Value.moneyDecimalFromScientific` の契約を使う.
JSON Schema は回答の形式を記述する. executor は aeson で読み取り, 会計の検査は `admit` に渡す.

## 信頼側の事実と証憑

次の 3 txid は信頼側が供給する. 回答に入れない.

| txid | 日時と内容 | 借方 | 貸方 |
|---|---|---|---|
| opening | 9/1, 8 月末の整理と締めを反映した残高 | 下表 | 下表 |
| bank-rent | 9/5, 銀行確定の家賃 | RentExpense 60000 | Deposits 60000 |
| bank-collection | 9/10, 銀行確定の売掛回収 | Deposits 10000 | AccountsReceivable 10000 |

| 期首の勘定 | 借方 | 貸方 |
|---|---:|---:|
| Cash | 20000 | |
| Deposits | 500000 | |
| AccountsReceivable | 30000 | |
| MerchandiseInventory | 100000 | |
| PrepaidExpenses | 12000 | |
| Machinery | 300000 | |
| AccruedExpenses | | 10000 |
| AccumulatedDepreciation | | 30000 |
| AllowanceForDoubtfulAccounts | | 600 |
| CapitalStock | | 800000 |
| RetainedEarnings | | 121400 |

期首借貸合計はいずれも 962000. 8/31 の未払光熱費の整理仕訳は
Debit UtilitiesExpense 10000 / Credit AccruedExpenses 10000.
この履歴は期首残高へ反映済み. 9 月へ再計上しない. 9/1 に逆仕訳を出すための参照事実とする.

独立の証憑 `till-september` は 9 月の現金売上の借方合計 400000,
`bank-payroll` は給与支払の借方合計 70000 を示す. どちらも提出前に信頼側が保有する.
給与の銀行明細は金額の照合に使い, 給与仕訳そのものは model が提出する.
証憑の金額を提出から計算し直して上書きしない.

出典: Admission の Trusted specification (`SupplyFacts`, `AdmissionSpec`, `EvidenceId`,
`txRule`), Admission and diagnostics (`EvidenceMismatch`). 金額, 履歴の参照方法, 処理日付は
信頼側の題材設定. `AdmissionSpec` に非加算の前期参照専用の field は見当たらず,
8 月の履歴は `Trusted.augustAdjustment` と本仕様で与える.

## 必須の生仕訳

次の 9 txid をそれぞれ 1 回だけ `transactions` に入れる. 行内の借貸を postings に展開する.

| txid | 内容と日付 | 借方 | 貸方 |
|---|---|---|---|
| reverse-august | 9/1 未払光熱費の振り戻し | AccruedExpenses 10000 | UtilitiesExpense 10000 |
| coffee-sale | 9 月の店頭売上集計 | Cash 400000 | Sales 400000 |
| subscription | 9 月の豆の定期便売上, 入金済み | Deposits 50000 | ServiceRevenue 50000 |
| beans | 9/12 豆の仕入, 預金払い | MerchandiseInventory 80000 | Deposits 80000 |
| wages | 9/25 給与, 預金払い | WageExpenditure 70000 | Deposits 70000 |
| utilities-paid | 9/20 前月未払分を含む光熱費支払 | UtilitiesExpense 15000 | Deposits 15000 |
| beans-used | 月中払出の原価 | SalesCost 120000 | MerchandiseInventory 120000 |
| subscription-expense | 9/30 前払ソフト利用料の月割り | CommunicationExpenses 1000 | PrepaidExpenses 1000 |
| roasting-loss | 9/30 焙煎目減りの棚卸評価減 | InventoryShrinkageLoss 3000 | MerchandiseInventory 3000 |

前払ソフト利用料は 9/1 開始の 12 か月分 12000, 今月分は 12000 / 12 = 1000.
豆は継続記録法としてここでは直接在庫を増減する. 払出後の帳簿在庫は 60000,
目減りを認識すると 57000. 前払消費と焙煎目減りを直接表す catalog 操作は公開の
`CatalogCall` 一覧に見当たらなかったため, 生仕訳を使う.

独自勘定 SubscriptionRevenue と RoastingLoss は Admission の固定 `AccountTitles` に
追加できなかった. それぞれ `ServiceRevenue`, `InventoryShrinkageLoss` に代える.
独自名を account に送らない.

出典: Admission の Trusted specification / Untrusted submission (`SupplySubmission`,
`Role`, `CatalogCall`, `Submission`). 勘定の意味は `ExchangeAlgebra.Accounting.Account` の
`AccountTitles` の説明. 月割りと個別仕訳は題材の会計判断であり, library の自動推論ではない.
振り戻しは生仕訳なので `ReverseEntryCall` の参照検査を受けない. 内容の正しさは信頼側の review 対象.

## 必須の catalog 呼出し

`calls` へ以下の順に 4 件入れる. `id` は非空で相互に異なる文字列ならよい.
表の id はそのまま使える. 全て生成 txid が必須.

| id | txid | operation | その他の field | 効果 |
|---|---|---|---|---|
| dep | depreciation | DepIndirect | amount: 5000 | Depreciation / AccumulatedDepreciation |
| accrual | utilities-accrual | AccruedExpense | amount: 8000, account: UtilitiesExpense | 今月未払光熱費 |
| allowance | allowance | Allowance | estimate: 1000, current: 600 | 貸倒引当金を 400 積み増す |
| closing | close | FinalStock | なし | 全収益と費用を RetainedEarnings へ締める |

例: `{"id":"dep","txid":"depreciation","operation":"DepIndirect","amount":5000}`.
他の catalog 操作はこの executor の呼出し仕様には含めない.
`FinalStock` は最後に置く. 最初の 3 件は AdjustmentStage, 締めは ClosingStage として扱われる.
生仕訳の `subscription-expense` と `roasting-loss` も信頼側の登録で Adjustment としている.
この例は 1 主体 1 期間だけを報告する.

出典: Admission の Untrusted submission (`CatalogCall`, `catalogStage`, `catalogKind`, `Call`),
Admission and diagnostics (`Stage`, `StageRegression`), Observations and derivation.
仕訳の効果は `ExchangeAlgebra.Accounting.Entries` の Description / Design と
`depreciationIndirectEntry`, `accruedExpenseEntry`, `allowanceReplenishmentEntry`.
締めの意味は `ExchangeAlgebra.Accounting.Closing` の Description / Laws L2,
`ExchangeAlgebra.Algebra.Transfer.finalStockTransfer`.
JSON の操作名は手書きの protocol の綴りである. Haskell ではそれぞれ
`DepreciationIndirectCall`, `AccruedExpenseCall`, `AllowanceCall`, `FinalStockCall` に対応する.

## 勘定語彙と棄却後の修正

使える語彙は次のとおり. 生成される勘定も含む.
`Cash`, `Deposits`, `AccountsReceivable`, `MerchandiseInventory`, `PrepaidExpenses`,
`Machinery`, `AccruedExpenses`, `AccumulatedDepreciation`, `AllowanceForDoubtfulAccounts`,
`CapitalStock`, `RetainedEarnings`, `RentExpense`, `Sales`, `ServiceRevenue`,
`WageExpenditure`, `UtilitiesExpense`, `SalesCost`, `CommunicationExpenses`,
`InventoryShrinkageLoss`, `Depreciation`, `ProvisionForDoubtfulAccounts`,
`ReversalOfAllowanceForDoubtfulAccounts`, `NetIncome`, `NetLoss`.
語彙にあることは直接の生仕訳を許可する意味ではない. 締めは catalog で行う.

通知は修正指示と `AdmissionError` の詳細を含む. `EvidenceMismatch` なら上記の独立金額へ,
`InvalidEntry` なら借貸と各 posting へ, `UnknownTransaction` なら登録 txid へ直す.
複数の通知があれば全て直して全体を返す. 失敗した stage の後続の検査は未実行のことがある.
信頼側の fact / evidence が無いという通知は model では修復できないため operator に知らせる.
上限は 3 試行. 受理後は `deriveTrialBalance` と `presentAdmitted` の検査も通った場合に諸表を出す.

出典: Admission の `AdmissionSpec.admissionVocabulary`, `admit` の pipeline / Laws,
`AdmissionError`, `deriveTrialBalance`, `presentAdmitted`. 通知文と上限はハーネスで決めた.
受理は経済的な仕訳内容の全面保証ではなく, 公開契約の範囲での出所, 均衡, 証憑合計の保証である.

## 実行

checkout の Stack 環境に exchangealgebra と aeson が準備済みなら,
`test/api-samples/harness/check.sh` を実行する. `STACK_ARGS` は Stack option の空白区切りの列.
新規の download は行わない. compile 後, 5 筋書きの試行と通知を表示し,
受理した各筋書きについて試算表と adjusted/final の損益計算書と貸借対照表を出す.
成功時の最終行は `All five scenarios completed with the expected outcomes.`.
一時 build directory は終了時に除去する. 棄却が上限まで続く筋書きも検証の一部なので正常終了する.

出典: 実行方式と判定は依頼書と本ハーネスの規約. library の機能保証から導いたものではない.
