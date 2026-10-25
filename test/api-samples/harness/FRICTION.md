# 公開 API で月次ハーネスを書いた記録

対象は依頼で指定された origin/develop 2e3318c の公開 Haddock. 記録は実装中に追記する.

## F1. 独自勘定を受入へ渡せない

部品: `Trusted.hs:86`, `CALLS.md` の勘定語彙.
したかったこと: SubscriptionRevenue と RoastingLoss を使う.
起きたこと: `ExchangeAlgebra.IO.Input.Admission.Entry`, `RawPostings`,
`AdmissionSpec.admissionVocabulary` は `AccountTitles` を使う固定型.
`test/api-samples/CustomAccount.hs` の拡張型を `Submission` へ渡す口は公開型に無かった.
回避: 実装前にここへ記録し, サブスクリプション収入は `ServiceRevenue`, 焙煎ロスは
`InventoryShrinkageLoss` に代える. 固有科目名は台帳の座標として残らない.
API: `ExchangeAlgebra.IO.Input.Admission` の `Entry`, `AdmissionSpec`, `Submission`.

## F2. 前期の参照事実と当期に加算する fact を分ける必要があった

部品: `Trusted.hs:21`, `Trusted.hs:63`, `ModelRetry.hs:26`, `CALLS.md` の信頼側の事実.
したかったこと: 8/31 の整理仕訳を信頼側の事実として渡し, 9/1 にその逆仕訳を出させる.
起きたこと: `AdmissionSpec.admissionFacts` の fact は registry の供給元として台帳へ入る.
非加算の履歴を置く別の field は公開型に無かった. `ReverseEntry` の説明は
「Exact key of the entry to reverse」のみで, どの期, role, provenance を参照できるかの表は
見つからなかった. `ReferencePeriodMismatch` 等の失敗型はあるが, それだけで前期参照の可否は決められない.
回避: 8 月の整理を `augustAdjustment` と呼出し仕様の参照事実に置き,
9 月には整理後の期首残高だけを fact として入れた. model は月初逆仕訳を生仕訳で提出する.
この方法で受理と諸表生成に成功した. catalog の前期参照を実際に棄却させたわけではなく,
非対応とは断定しない. 生仕訳と前期整理の意味的な対応は `admit` の参照検査の対象外になる.
API: `ExchangeAlgebra.IO.Input.Admission` の `AdmissionSpec`, `SupplyFacts`,
`ReverseEntry`, `ReferenceFailure`, `admit`.

## 型から導けそうなのに手で書いたもの (F3)

部品: `Decoder.hs:55`, `Feedback.hs:13`, `CALLS.md`, `calls.schema.json`.
したかったこと: 公開 catalog の型と失敗型を元に, model に渡す仕様と通知の整合を保つ.
起きたこと: 公開型の一覧は読めるが, JSON 表現や schema は今回読んだ公開資料に無かった.
`CatalogOperationKind` と `CatalogCall` から `catalogKind` / `catalogStage` は得られる.
一方で JSON field, 引数の単位や意味, 全操作の stage 表は自動的には得られず手で転記した.
回避: 題材で許す 4 操作だけを明示し, 次の部分を手で書いた.

- constructor と引数から書けそうなもの: `CALLS.md` の各 operation の field,
  schema の `oneOf` と `required`, `Decoder.call` の分岐と constructor 対応.
- 失敗 constructor から書けそうなもの: `Feedback.repair` の `AdmissionError` ごとの分岐.
  修正のための言葉自体には人の判断が要る.
- registry と vocabulary の値から書けそうなもの: txid / supply / required の表,
  `CALLS.md` と schema の勘定一覧. 一覧を複数箇所へ転記した.
- 名前の対応: protocol の `AccruedExpense` と Haskell の `AccruedExpenseCall`.
  この表記は利用者が選んだもので, library が要求する JSON 名ではない.

これらは型からの生成機能が必要だという設計提案の確定ではない. 手書き量と drift の箇所の記録である.
金額の単位, 個別取引の会計判断, 証憑の出所は型だけでは決められない.
API: `ExchangeAlgebra.IO.Input.Admission.CatalogCall`, `CatalogOperationKind`,
`AdmissionError`, `registryRules`, `ruleSupplies`, `AdmissionSpec`.

## F4. 仕訳の説明関数を受入の失敗全体にはそのまま使えなかった

部品: `Feedback.hs:13`, `Feedback.hs:32`, `Feedback.hs:34`, `ModelRetry.hs:104`.
したかったこと: 棄却を model が修正できる文にして返す.
起きたこと: 最初は全ての通知を自作した. 実行後の追加探索で
`IO.Input.Assist.explainEntryError` を見つけたが, 引数は `EntryError` であり,
外側の `AdmissionError` を扱う関数ではなかった. `IO.Output.Admission` も諸表の renderer のみ.
`EvidenceMismatch` の 2 金額には field 名が無く, 個別説明も「Evidence amount differs」のみなので,
どちらが期待値かを資料だけから読み取りにくかった.
回避: `InvalidEntry` 内の失敗には `explainEntryError` を使うよう直した.
外側は自作文にして constructor と全詳細を `show` で添えた.
2 金額を推測でラベル付けせず, CALLS の独立金額へ戻るよう指示する.
実行では `EvidenceMismatch ... 400000 399999`,
`InvalidEntry ... Imbalanced {_debitTotal = 70000, _creditTotal = 69000}`,
`UnknownTransaction` と `MissingRequiredTransaction` を受け, 各 2 回目に受理された.
全 constructor の文は書いたが, 実行で確認した棄却はこの 3 種類の筋書きだけである.
API: `ExchangeAlgebra.IO.Input.Assist.explainEntryError`,
`ExchangeAlgebra.IO.Input.Admission.AdmissionError`, `EvidenceMismatch`, `admit`.

## F5. catalog を呼ぶ側に必要な stage 表を手で組み立てた

部品: `Decoder.hs:55`, `ModelRetry.hs:51`, `CALLS.md` の catalog 呼出し.
したかったこと: model に, 生仕訳と catalog 呼出しをどの順で出すか説明する.
起きたこと: `Stage` の意味, `catalogStage` の型, `StageRegression` の説明はあるが,
今回使う各操作を stage に対応させた表は読んだ Admission の説明中に無かった.
生仕訳には trusted registry 側の `Role` があり, call には操作から決まる `Stage` があるため,
提出順序の説明をこの二つの情報から組み立てた.
回避: 最初の 3 操作を整理, `FinalStock` を最後とした固定列を書いた.
全体の受理と締め後の諸表生成は実行で確認したが, stage 順の不正提出の試験は増やしていない.
これ以上の操作の stage 対応はこの見本では確かめていない.
API: `ExchangeAlgebra.IO.Input.Admission.catalogStage`, `Role`, `Stage`, `StageRegression`.

## F6. 受理から諸表まで別の検査を通す必要があった

部品: `Reports.hs:19`, `Reports.hs:36`, `Reports.hs:41`.
したかったこと: 受理した仕訳から試算表, 損益計算書, 貸借対照表を出す.
起きたこと: `admit` の成功だけでは諸表の値は得られず,
`deriveTrialBalance` と `presentAdmitted` がそれぞれ別の失敗型を返す.
`ReportingContext` に 10 field があり, `jcciSecondGradeContext` の項目は型のみで,
どの分類判断を既定で使うかの説明が無かったため, 最初は必要な設定の範囲を判断できなかった.
回避: 既存の許可された `test/api-samples/Admission.hs` と公開 Presentation の例で使われる
`jcciSecondGradeContext Standalone` を採用し, 各段階の失敗を握りつぶさず表示して終了する.
この題材では全て成功した. adjusted/final の科目行 CSV を公開 renderer で出した.
CSV は収益, 費用, 資産, 負債, 純資産の section を持つが, 任意の帳票レイアウトは作っていない.
締め照合は `netPairMapByExact` の非零 Not/Hat 残高を使い,
`closingEntries` と legacy `finalStockTransfer` の結果を catalog と比較した.
`bar` 後の構築表現や仕訳の順序を等価性の物差しにしなかった.
API: `ExchangeAlgebra.IO.Input.Admission.deriveTrialBalance`, `presentAdmitted`,
`renderAdmittedStatements`, `ExchangeAlgebra.Accounting.Statements.Presentation.jcciSecondGradeContext`,
`ExchangeAlgebra.Accounting.Closing.closingEntries`, `ExchangeAlgebra.Algebra.Exact.netPairMapByExact`.

## 信頼側が決めたこと

model の提出を読む前に決められなかった信頼条件は無い. entity, period,
期首残高, 8 月の整理仕訳, 2 件の銀行確定仕訳, 2 件の独立証憑額,
全 txid の必須性と供給経路, 勘定語彙, 許す catalog 操作を事前に固定した.
提出後に証憑額を計算し直したり, 必須 txid を減らしたりはしていない.

API 調査後に決めた表現上の選択は, 独自勘定の代替 (F1),
前期履歴を当期 fact に加算しない置き方 (F2), 月割りと目減りの生仕訳である.
これらも最初の提出を受け取る前に固定した. `AllowanceCall` の estimate と current は model の引数であり,
引数が信頼側の経済的見積りと一致する保証を `admit` が与えるとは考えていない.
月割りの計算, 原価, 目減り, 振り戻しの意味的な正しさも今回の人による題材設定に依存する.

## 実行結果と情報源の留保

2026-10-18, GHC 9.10.2, この clone の exchangealgebra-0.5.3.0 で compile と実行に成功した.
first-pass は 1 回, evidence-repair / balance-repair / registry-repair は各 2 回で受理.
exhaustion は 3 回とも EvidenceMismatch で終了し, 最後の通知を表示した.
受理 4 件すべてで, catalog / closingEntries / finalStockTransfer の exact 残高が一致した.
費用 272400, 収益 450000, 当月利益 177600, 締め後利益剰余金 299000.
純額の資産は 1107000, 負債 8000, 純資産 1099000.
独自勘定のままの受入, catalog による前期参照の振り戻しは書けていない. 理由は F1 / F2.

環境の準備では package DB に library が未登録だったためローカル build が要った.
OS の sandbox で network を拒否して実行した. compiler warning に既存 `src/` の行断片が
自動表示された. 明示的に source file を開いたり検索したりはしていないが,
「内部の内容を全く見ていない」という厳密な条件には留保がある. この断片は API 判断には使っていない.
既存 library の warning と linker warning は担当外として変更していない.
JSON の構文は確認した. 環境に `jsonschema` が無いため draft の meta-schema による検証は未実施.
追加依存や network による導入は行っていない. 読取りと実行の順序は `READ-LOG.md` に残した.

## 全体の所感: 最も困った 3 点

1. 独自勘定を代数の基底に載せる例はあっても, その型を Admission へ渡せなかった (F1).
2. 前期の整理仕訳を当期残高と混ぜずに参照させる入口を公開説明だけでは組み立てられず,
   生仕訳にした結果, 振り戻しと元仕訳の対応が受入検査の外へ出た (F2).
3. catalog 型から呼出し仕様, schema, parser の三つへ転記する量が多かった.
   特に操作名と引数 field を変えるとき, 同時に直す箇所が分散する (F3).
