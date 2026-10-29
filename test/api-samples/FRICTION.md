# API samples: 利用者として詰まった点

対象はこの checkout の `exchangealgebra-0.5.3.0` である. 見本の API 選定には `stack haddock --no-haddock-deps exchangealgebra` で生成した公開 Haddock, `README.md`, `examples/README.md`, 既存の examples を使った. 初期調査の source 抜粋で一部の関数本体が表示されたため, 「本体を読まずに書く」という条件は厳密には満たせなかった. 以下の行番号はこの directory の見本を指す. 見本は `./test/api-samples/check.sh` で compile と実行を確認した.

## 1. 単一期の総勘定元帳に日付が必要

- 題材と行: `Bookkeeping.hs:33`.
- したかったこと: 日付軸のない仕訳から, 講義用に現金の総勘定元帳を表示する.
- 起きたこと: `accountLedgerRows Cash ledger` は compile せず, 科目はリスト, 基底から `Day` を返す関数も必須だった. 日付が基底にない場合の短い入口を見つけられなかった.
- 回避: `[Cash]` と `const (fromGregorian 2026 12 31)` を渡した. 全仕訳が同日として表示されるため, 元の取引日を復元する方法ではない.
- API: `ExchangeAlgebra.IO.Output.Statements.accountLedgerRows`.

## 2. 決算前の名目勘定を利用者が切り出す

- 題材と行: `Bookkeeping.hs:28-31`, `Readout.hs:18-20`.
- したかったこと: 期中の台帳から損益を求め, 決算振替と利益剰余金への着地を順に示す.
- 起きたこと: 既存の講義例では `incomeSummaryAccount` へ名目勘定だけを渡していた. 全台帳を渡すと実在勘定を含む貸借差額が 0 になり, 利益の説明にならない. 科目分類による抽出を利用者側で書く必要があった.
- 回避: `EA.filter` と `whatDiv . _hatBase` で `Cost` / `Revenue` を選んだ. その後 `netIncomeTransfer` と `finalStockTransfer` を使った.
- API: `ExchangeAlgebra.Algebra.filter`, `ExchangeAlgebra.Algebra.Base.whatDiv`, `ExchangeAlgebra.Algebra.Transfer.incomeSummaryAccount` / `netIncomeTransfer` / `finalStockTransfer`.

## 3. 期またぎの繰越は手で組み立てる

- 題材と行: `MultiPeriod.hs:20-27`.
- したかったこと: `(期, 取引)` を note にした Journal から当期だけを読み, 決算後残高を翌期首へ繰り越す.
- 起きたこと: 期軸の検索には `filterByAxis 0 (NoteAxisKey period)` を使えたが, 期末 Journal から翌期首 Journal を作る一つの操作は見つけられなかった. `projWithNote` は note の完全な値の列を要求する.
- 回避: `filterByAxis` → `toAlg` → `finalStockTransfer` → `(.|)` を見本で接続した. この接続自体が利用者側の繰越手順である.
- API: `ExchangeAlgebra.Journal.filterByAxis` / `toAlg` / `(.|)`, `ExchangeAlgebra.Algebra.Transfer.finalStockTransfer`.

## 4. 科目ごとの集計にはキー関数の形を合わせる

- 題材と行: `Readout.hs:21-25`.
- したかったこと: 現金残高, 科目別残高, 貸借一致, 利益剰余金を同じ台帳から読む.
- 起きたこと: `balanceMapBy getAccountTitle ledger` は型エラーになった. `balanceMapBy` のキー関数は `BasePart b -> Maybe k` であり, `getAccountTitle` はその形でない. 借貸を混ぜて負の値を作る signed readout と非負の対を返す readout も別名である.
- 回避: 勘定名が基底そのものの見本では `netPairMapBy Just` を使い, 現金と利益剰余金は `bar . projByAccountTitle` で読んだ.
- API: `ExchangeAlgebra.Algebra.balanceMapBy` / `netPairMapBy`, `ExchangeAlgebra.Algebra.projByAccountTitle`.

## 5. 受入 API の縮小

旧 API の登録簿, 役割, 証憑 ID, catalog, 締めと諸表の導出は削除した. 見本は `admit journal` と `checkEvidence` を使う.

## 7. 取引網の係数から解析用配列へ橋渡しできない

- 題材と行: `Network.hs:8-24`.
- したかったこと: 取引網と投入係数を一度定義し, その値から Leontief 逆行列と需要ショック後の産出を読む.
- 起きたこと: `inputCoefficients` は `InputCoefficients` を返す一方, `leontiefInverse` は `IOArray (Int, Int) Double` を要求する. 公開 API に両者をつなぐ関数を見つけられなかった. `InputCoefficients` の定義と配列の係数を同じ数値で二重に書いた.
- 回避: 2 × 2 の係数を `newListArray` に再記入し, 逆行列の行と需要ベクトルを `zipWith (*)` で掛けた. この利用者側の計算は係数変更時に不一致を生み得る.
- API: `ExchangeAlgebra.Simulate.Network.inputCoefficients` / `inputsOf`, `ExchangeAlgebra.Simulate.Analysis.leontiefInverse`.

## 8. `rippleEffect` は同じ 1 始まり配列で実行時に落ちた

- 題材と行: `Network.hs:16-24`.
- したかったこと: Leontief 逆行列に加え, 公開の `rippleEffect` で波及を得る.
- 起きたこと: 境界 `((1, 1), (2, 2))`, 要素 `[0, 0.2, 0.3, 0]` の配列では `leontiefInverse` は成功したが, `rippleEffect 3 matrix` は `Error in array index` で実行を止めた. `((0, 0), (1, 1))` では始点 `(1, 1)` を前提にする pattern の非網羅例外になった. この範囲の配列前提は公開 Haddock だけでは掴みにくかった.
- 回避: `rippleEffect` を見本の実行経路から外し, 逆行列と需要ベクトルから産出を計算した. `rippleEffect` 自体は未使用のままである.
- API: `ExchangeAlgebra.Simulate.Analysis.rippleEffect` / `leontiefInverse`.

## 9. 独自基底の会計出力に instance の記述が要る

- 題材と行: `CustomBase.hs:8-29`, `CustomBase.hs:40-43`.
- したかったこと: 既定科目に部門軸を加え, 題材 1 と同じ試算表と諸表を出す.
- 起きたこと: `Element` と `BaseClass` に加え, `ExBaseClass` の `getAccountTitle` / `setAccountTitle` を利用者が定義する必要があった. 出力では `Cash` が部門別の複数行になり, `RetainedEarnings` も分割して現れた. 出力行から部門名は見えず, 試算表・諸表を科目ごとに一行で読む用途にはそのまま使いにくい.
- 回避: 必要な instance を書き, 部門別の内訳は `projByAccountTitle` の代数値として表示した. 報告行の見た目は変換しなかった.
- API: `ExchangeAlgebra.Algebra.Base.Element` / `BaseClass` / `ExBaseClass`, `ExchangeAlgebra.IO.Output.Statements.compoundTrialBalanceRows` / `ExchangeAlgebra.Accounting.Statements.present`.

## 10. 独自勘定は基礎代数で使えても会計分類へ入らない

- 題材と行: `CustomAccount.hs:9-34`.
- したかったこと: 既定科目にない `CarbonReserve` を題材 3 の残高・均衡・決算・諸表の流れへ足す.
- 起きたこと: `decL ledger` の compile で `ExBaseClass (HatBase MyAccount)` がないと指摘された. `ExBaseClass` の科目取得・設定は `AccountTitles` を前提とし, `MyAccount` の新しい値を既定科目の分類へ渡す方法を見つけられなかった. 決算振替と既定の諸表出力も同じ境界に当たる.
- 回避: `Element` / `BaseClass` を定義し, `bar`, `proj`, `netPairMapBy Just` までを表示した. 借貸一致と諸表はこの見本では書けなかった.
- API: `ExchangeAlgebra.Algebra.Base.ExBaseClass`, `ExchangeAlgebra.Algebra.decL` / `decR`, `ExchangeAlgebra.Accounting.Statements.present`, `ExchangeAlgebra.Algebra.netPairMapBy`.

## 全体の所感: 最も困った 3 点

1. 独自勘定が基礎代数から会計読出し・決算・諸表へ進めない. 拡張した勘定に分類を与える公開の経路が見つからなかった.
2. 取引網の係数と解析用配列が別表現で, `rippleEffect` も試した 2 × 2 配列で実行時に落ちた. 網を一度定義して波及まで進む見本を書けなかった.
3. 決算は名目勘定の抽出, 期末繰越, 受入後の二経路比較を利用者が接続する. 小さな帳簿でも複数 module の関数と冗長列の扱いを先に理解する必要があった.

旧 net readout の再公開 module は 0.6 で削除. 各関数は `Algebra` / `Journal` から import する.
