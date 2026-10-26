# 受入 API の設計ノート

この文書は `ExchangeAlgebra.IO.Input.Admission` の実装判断を記録する開発者向けの設計ノートである.
受入は, 信頼できない書き手 (言語モデルなど) が書いた仕訳帳について, 会計的な整合性を取引ごとに確かめる.
仕訳の経済的な意味, 勘定の選び方の適否, 演習の正解との一致は確かめない.

## 範囲

受入が持つのは次の 2 つだけである.

| 関数 | 問い | 結果 |
|---|---|---|
| `admit` | 仕訳帳の各取引 (note) が, それ単独で会計的に整合しているか | `Either (AdmissionFailure n b) (Admitted n b)` |
| `checkEvidence` | 受入済みの仕訳帳が, 信頼側の記録 (証憑) と一致するか | `Either (NonEmpty (EvidenceMismatch m b)) ()` |

受入は仕訳を作らない. 決算整理と締めの仕訳は, 書き手が会計の関数
(`ExchangeAlgebra.Accounting.Adjustment`, `closingEntries` など) で計算して仕訳帳に含める.
期首の残高のように信頼側が与える事実も仕訳帳に含めて渡し, 信頼側の記録との一致を `checkEvidence` で確かめる.
試算表と諸表は, 受入の後に `admittedJournal` から既存の関数
(`validateTrialBalance`, `Presentation.present`, `ExchangeAlgebra.IO.Output.Statements` の CSV 出力) で作る.

使ってよい勘定の一覧, 特定の勘定への直接の記帳の禁止, 損益と純資産の間の振替の禁止のような規則は,
実験や道具の側の規則であり library には置かない. 例えば売上を利益剰余金へ直接振り替える提出も受け入れられ,
それを見つけるのは正解との照合である.

## 型

`admit` は note `n` と基底 `b` について多相で, 値は `MoneyDecimal` に固定する.

```haskell
admit :: (Note n, ExBaseClass b)
      => Journal n MoneyDecimal b
      -> Either (AdmissionFailure n b) (Admitted n b)
```

値を固定するのは, 金額の厳密な和と, 証憑との値の等号比較を, 有限の 10 進数で行うためである.
`TxKey = TxKey EntityId PeriodId TxId` は既定の note の型で, 会社・期間・取引を分ける.
他の `Note` の型も使える.

`Admitted` と `AdmissionFailure` の構築子は非公開で, `admit` だけが作る.
`Admitted` は「取引ごとの整合性を確かめた仕訳帳」であることを型で表す印で,
`checkEvidence` と利用者の関数が引数の型で検査済みを要求できるようにする.
中身は `admittedJournal` で取り出し, 既存の関数にそのまま渡せる. 印のほかに機構は持たない.
どちらの型にも, 中身から値を作り直す instance (`Generic`, `Read`, `Binary` など) を実装しない.
保証は `unsafeCoerce` と内部 module を使わないことを前提とする. EA の module は Safe Haskell ではない.

## admit の検査

各 note の各 posting について次を確かめる.

| 検査 | 違反 | 理由 |
|---|---|---|
| 値が正 | `NonPositiveAmount n b v` | posting の値は非負で, 0 の項は台帳に置かない |
| 側が `Hat` か `Not` | `WildcardSide n b` | 台帳の項の Hat / Not は借方か貸方かを決める値である. wildcard の `HatNot` は問い合わせにだけ使う |
| 勘定が記帳できる | `NotPostableAccount n b` | どの処理でも記帳できない勘定 (wildcard の `AccountTitle`) を拒む |

さらに, `WildcardSide` と `NotPostableAccount` の無い note について, 借方の合計と貸方の合計が厳密に等しいことを確かめる
(`Imbalanced n debit credit`). 借方・貸方は `decL` (Debit) と `decR` (Credit) で決め, Hat / Not の符号では決めない.
現金売上 `100 .@ Not :< Cash .+ 100 .@ Not :< Sales` は, 借方が Cash, 貸方が Sales で釣り合う.
和は厳密に累算し, 報告する値だけを 1 回丸める.
`HatNot` の項に `decL` を掛けると例外になり, wildcard の勘定の区分を問うても例外になるので,
これらを含む note では貸借を比べない.

`(.@)` は `HatNot` と負の値と有限でない値を拒むので, 公開 API で作った仕訳帳では値と側の検査は通常は落ちない.
それでも検査を残すのは, 費用が小さく, 内部の構築子で作った値も同じ型で届くからである.

違反は全部集めて返し, 最初の 1 件で止めない. 並びは note の昇順, note の中は posting の基底の昇順で,
`Imbalanced` はその note の最後に置く. 成功したときは仕訳帳を変えずに `Admitted` に入れる.
note を足しも消しもせず, 同じ基底の項をまとめず, `bar` を掛けない.

検査しないもの:

- 取引の key の重複. 同じ note の元を足すと仕訳帳の中で 1 つの元にまとまるので, 仕訳帳には重複を区別する情報が残らない.
  重複は仕訳帳を作る前の変換 (JSON の読み込みなど) で確かめる. 既存の `checkedJournalIn` も仕訳帳を作る前に `DuplicateTxId` を検出する.
- 空の取引. 仕訳帳の正規形は値 0 の note を持たない.
- 処理によって変わる記帳の権限 (締めでだけ記帳できる勘定など). 受入は処理を知らない.
- 取引をまたぐ残高, 締めの有無, 証憑との一致 (`checkEvidence` で確かめる), 正解との一致.

法則:

- `admit j == Right a` ならば `admittedJournal a == j`.
- note が重ならない `j1`, `j2` について, `admit j1` と `admit j2` が成功すれば `admit (j1 .+ j2)` も成功する.
- `admit Zero` は成功する.

## checkEvidence の照合

```haskell
checkEvidence
    :: (Note m, HatBaseClass b)
    => Admitted n b
    -> (Journal n MoneyDecimal b -> Journal m MoneyDecimal b)
    -> Journal m MoneyDecimal b
    -> Either (NonEmpty (EvidenceMismatch m b)) ()
```

証憑 (銀行の明細, 現金の実査, 信頼側が与える期首の残高など) は, 期待する元の仕訳帳で渡す.
まず受入済みの仕訳帳に選び方 `f` を掛ける. `f` は射影, 振替, `bar`, note の付け替えなど,
利用者が要るだけ選んでまとめる関数である. `checkEvidence` 自身は集約せず, `bar` を呼ばない.

期待する posting 1 つ (note `m`, 基底 `b`, 値 `v`) を, `f` の結果への問い合わせとして片方向に照合する.
期待の元が pattern, `f` の結果の元が台帳の entry である.

- note: `plank` の note は全ての note に当たる. それ以外の note は自分自身にだけ当たる.
- 基底: 問い合わせの向きで照合する. 期待の基底の wildcard の軸 (勘定の `AccountTitle` など) は, その軸のどの値にも当たる.
  対称の `.==` は使わない.
- 側: 期待の基底の側は `Hat` か `Not` で書く (`(.@)` は `HatNot` を拒む).
  1 つの基底の両側を 1 つの正味の値として比べたいときは, `f` で `bar` を掛け, 結果の側を書く.

当たる posting がちょうど 1 つで値が厳密に等しければ一致である. それ以外は次の不一致になる.

| 当たる posting | 結果 |
|---|---|
| 0 個 | `EvidenceMissing m b v` |
| 1 個で値が違う | `EvidenceDiffers m b expected found` |
| 2 個以上 | `EvidenceAmbiguous m b values` (`f` で 1 つにまとめる. 同じ基底の両側なら `bar`, 別の勘定なら振替) |

posting は 1 つずつ数える. 同じ基底の seq の各項は別の posting で, 1 つにしたければ `f` で `bar` を掛ける.
期待側に同じ note と基底の posting が複数あれば, それぞれを独立の期待として照合する.
どの期待にも当たらない `f` の結果の元は無視する. 証憑は「あるべきもの」を述べ, 仕訳帳の全体を述べない.
全ての期待を照合し, 不一致は期待の note と基底の昇順で全部返す.

例: `f` の結果が `"sale"` の現金 100 だけのとき, 期待が `"sale"` の現金 100 なら一致,
`plank` の note の現金 100 でも一致, `"rent"` の現金 100 なら `EvidenceMissing` になる.

法則:

- `checkEvidence a f Zero == Right ()`.
- `e` の各 posting が, 上の片方向の照合で `admittedJournal a` のちょうど 1 つの posting に当たり, 値が等しければ
  `checkEvidence a id e == Right ()`. 前提の「ちょうど 1 つ」は外せない. 例えば `plank` の note の現金 100 と
  `"sale"` の現金 200 を持つ仕訳帳で, 現金だけを射影した元を期待にすると, `plank` の期待が 2 つに当たり
  `EvidenceAmbiguous` になる.
- 期待ごとの検査なので, `checkEvidence a f (e1 .+ e2)` が成功するのは `checkEvidence a f e1` と `checkEvidence a f e2` が
  ともに成功するときに限る.

## module の配置

| module | 公開 | 内容 |
|---|---|---|
| `ExchangeAlgebra.IO.Input.Admission` | 公開 | 入口. 取引の key, `admit`, `checkEvidence` と診断の型 |
| `...Admission.Diagnostic` | 非公開 | `AdmissionError`, `AdmissionFailure`, `EvidenceMismatch` |
| `...Admission.Representation` | 非公開 | `Admitted` と `admittedJournal` |
| `...Admission.Engine` | 非公開 | `admit` |
| `...Admission.Evidence` | 非公開 | `checkEvidence` |
| `ExchangeAlgebra.Accounting.Transaction` | 公開 | `TxKey`, `EntityId`, `PeriodId`, `TxId`, `Entry`, `isBlankKey` |
| `ExchangeAlgebra.Accounting.Equivalence` | 公開 | 取引の map の同値判定 (受入とは独立) |

構築子を置く非公開 module は, `package.yaml` と生成される `exchangealgebra.cabal` の両方で library の `other-modules` に登録する.

## 同値判定

`ExchangeAlgebra.Accounting.Equivalence` は受入とは独立した関数で, 受入していない `Entry` の map にも使える.
`isEquivalentUpTo` は全ての方式でキー集合の一致を要求し, 会社・期間・取引の境界を保つ.
posting の比較は `(Hat, AccountTitles, MoneyDecimal)` を sort した多重集合で行い, `Alg` の `Eq` や仕訳帳全体の `bar` で代用しない.

| 方式 | 正規化の範囲 |
|---|---|
| `PostingMultiset` | 行の順序だけを無視する |
| `NetWithinTransaction` | 各取引の `Alg` を個別に `bar` する |
| `NetAccountsInTransactions` | 指定した取引の指定した勘定だけ `bar` し, 他は厳密に比べる |

`closingDifferences` と `isClosingEquivalent` は 2 つの締めを取引の key ごとに比べる.
利益剰余金以外の勘定は借方・貸方の側ごとの厳密な合計で, 利益剰余金は貸方から借方を引いた厳密な正味で比べる.
