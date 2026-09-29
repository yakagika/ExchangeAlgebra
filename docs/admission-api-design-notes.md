# 受入 API の設計ノート

この文書は `ExchangeAlgebra.IO.Input.Admission` の実装判断を記録する開発者向けの設計ノートである.
対象は固定された受入仕様の下での受入と導出であり, 仕訳の経済的意味や消去内容の正しさを保証しない.
金額型は `MoneyDecimal`, 基底は `HatBase AccountTitles` に固定する.

## 契約と公開境界

信頼側が `TxIdRegistry`, 証憑, 与件, 語彙から `AdmissionSpec` を用意し,
実行器が提出全体を `Submission` として `admit` に渡す.
呼出し側が提出を削除・選別してから渡したことや, 仕様そのものを偽装したことは検出対象にできない.
固定された仕様と公開 API の下で, 未受入の Journal を新しい導出経路に渡すことは型検査で拒否される.
`unsafeCoerce` と Safe Haskell 外の手段は保証対象外とする.

`Admitted`, `AdmittedTrialBalance`, `AdmittedStatements`, `ResolvedInput`, `TxRule`,
`TxIdRegistry` の構築子と record field は公開しない. これらに `Generic`, `Data`, `Read`,
`FromJSON`, `Binary`, `Semigroup`, `Monoid` は実装しない. 型引数はないため role annotation は不要である.
観測値を取り出す getter は通常の関数であり, record 更新の入口にはならない.
getter が返した Journal や Map を変更しても, 受入済み型へ戻す公開関数はない.

公開入口は `ExchangeAlgebra.IO.Input.Admission`, 識別子と仕訳は
`ExchangeAlgebra.Accounting.Transaction`, 同値判定は `ExchangeAlgebra.Accounting.Equivalence`,
受入済み諸表の CSV 出力は `ExchangeAlgebra.IO.Output.Admission` に置く.
受入の入口も同じ識別子と出力関数を再公開する.
構築子を置く module を含む非公開 module は, `package.yaml` と生成される
`exchangealgebra.cabal` の両方で library の `other-modules` に登録する.
既存 Write, Bookkeeping, Checked API は変更せず, `JournalCert` からの昇格口も作らない.
公開済みの既存 API に対しては additive であり, 廃止する機能はない.
未リリースの Admission API は, 供給方法の集合化に伴って getter と診断型を改訂した.

## 入力の型

識別子 `EntityId`, `PeriodId`, `TxId`, `FactId`, `EvidenceId`, `CallId` はそれぞれ `Text` の newtype
である. `TxKey = TxKey EntityId PeriodId TxId` により会社・期間・取引を分ける.
単一会社の場合も会社と期間を明示させ, 暗黙の現在期・既定会社を置かない.
空白だけの識別子は拒否する. 表記の正規化はせず, 大文字小文字も区別する.

`TxRule` は必須性, 許す供給方法の集合, 証憑を独立に保持する.
役割は raw と facts の供給方法に属し, catalog では操作の種類から決まる.
`txRule` がルールを組み, `txIdRegistry` が一覧の重複・空キー・空の許可集合,
facts の混在・証憑, 複数の raw 役割を検査する.
重複はルールが同一でも拒否し, `Map.fromList` による上書きより前に検出する.
照会専用操作を取引の供給元として登録することも拒否する.

`RawPostings = [(Text, Text, MoneyDecimal)]` は side, account, amount の列である.
raw と与件は同じ形式とし, 両方とも `checkedEntryTextIn` を通す.
証憑の金額は取引の借方総額であり, 残高や各行の金額ではない.
与件・証憑の store は信頼側の Map とし, 提出側には書き換え口を渡さない.

`Submission` は raw の `(TxKey, RawPostings)` のリストと `Call` のリストを持つ.
リストのまま受けることで, 同じキーの重複を入口で検出できる.
各 `Call` は独立した `CallId`, 会社, 期間, `Maybe TxKey`, 閉じた `CatalogCall` を持つ.
生成操作には `Just key`, `EquityBalance` と `Consolidate` には `Nothing` を要求する.
生成操作の結果が零であることは成功であり, 必須キーを満たす.
零仕訳は Journal の保存表現から落ちる場合があるため, 検証済み metadata にキーの存在を保持する.

## 検査順とエラー

| 段階 | 検査対象 | 失敗時の進行 |
|---|---|---|
| 所属と供給元 | raw 全件, 生成予定キー全件, 与件, 必須性, 衝突, call ID, 引数, 呼出し順 | 全件を集めて終了 |
| 仕訳 | raw ごとの保護科目・P/L と純資産間振替禁止, 与件と raw の parse, 正値, 文脈, 貸借均衡 | 各仕訳のエラーを集めて終了 |
| 語彙 | 検証済み与件・raw の科目 | 全件を集めて終了 |
| catalog | 参照解決, 可視範囲, builder, 生成科目, 生成仕訳検査 | 失敗した call 以降を停止 |
| 証憑 | registry 全域の証憑義務と各取引の借方総額 | 不一致を全件集めて終了 |

生成結果は事前にキー・役割・供給元を検査し, builder の実行直後に実際の posting を検査する.
生成値が存在する前にその値を検査することはできないため, これらを分離する.
後段の集計を未検証の値で続けない. catalog の後続 call は先行結果を参照しうるため,
実行時の失敗後に後続を実行しない. 全 call の引数・順序は実行前にまとめて検査する.
証憑 ID を持つ提出だけを事前に選別する処理はなく, 未知の均衡仕訳も所属検査で拒否する.
`Optional` で未供給の取引には金額照合を要求しないが, 宣言した証憑・与件の参照先は仕様に必要である.

raw の直接記帳禁止は, 利益剰余金などの保護された科目と科目の属性から決める.
加えて, 1 取引に P/L 科目と純資産科目を混在させる raw 振替を拒否する.
与件は信頼側の入力なので, 検証済みの期首利益剰余金等を持てる.
生成結果には操作ごとの許可科目と語彙の両方を適用する.
同じ会社・期間・stage の raw と生成結果に同じ posting がある場合は,
同じ効果の二重計上として拒否する.

## stage と参照解決

| 操作・役割 | stage | 処理文脈 |
|---|---|---|
| `Ordinary`, `Opening`; interim tax, dividend, reversal | `OrdinaryStage` | OrdinaryJournal; Opening のみ EngineComputation |
| `Adjustment`; 通常の整理操作 | `AdjustmentStage` | ClosingProcess |
| `Elimination`; equity earnings, equity entries, consolidation | `ConsolidationStage` | ConsolidationWorksheet |
| `Closing`; final stock transfer | `ClosingStage` | EngineComputation |
| equity balance query | `QueryStage` | EngineComputation |

equity earnings / entries の生成仕訳は役割 `Adjustment`, 実行 stage `ConsolidationStage` とする.
役割と stage は同一概念ではないため, 生成 metadata は両方を保持する.
各 call の累積 ledger は同じ会社・期間かつ当該 stage 以下の仕訳に限る.
先行していない生成結果と未来 stage の raw を参照できない.
閉じた catalog の実行関数・許可科目・stage を利用者が差し替える口は作らない.

連結には `EntityInput EntityId (NonEmpty TxKey)` と消去キー列を渡す.
会社の自己申告はキーの会社と照合し, 期間は call の期間と照合する.
会社側の入力は `Ordinary` または `Opening`, 消去入力は `Elimination` を要求する.
与件は registry の `SupplyFacts` からのみ取り出し, raw / catalog の出所も検査済み metadata から取得する.
同じキーの繰り返し参照は call 間も含めて拒否する.
失敗は `UnresolvedReference CallId TxKey Role ReferenceFailure` で返す.

逆仕訳の参照解決は連結と区別する. `ReverseEntry` の参照元は `Ordinary` の
`SubmissionProvenance` に限定し, 期首与件・通常の与件・先行 catalog の生成結果を拒否する.
連結で認める与件参照を逆仕訳にも適用すると, 保護された期首利益剰余金を提出側が反転できるためである.

連結の実行は解決済み参照を一度ずつ足し, `bar` 後の貸借均衡を検査して零仕訳を返す.
元仕訳と消去仕訳はすでに Journal に一度だけ存在する.
この操作は参照の出所を保証するが, 消去内容の会計的正しさを保証しない.

## 導出と snapshot

主状態は `Journal TxKey MoneyDecimal (HatBase AccountTitles)` とする.
Map に独立した残高を蓄積せず, 必要な残高は Journal の射影と既存 readout から導く.
metadata はキーの役割・stage・出所と零件成功の存在を保持する.
`admit` は期中, 整理後・連結後かつ締切前, 全仕訳の 3 つの cumulative snapshot を保持する.
getter はこれらを `DuringPeriod`, `Adjusted`, `Closed` として観測させる.

`deriveTrialBalance` は最終 snapshot と整理後 snapshot をそれぞれ既存の
`validateTrialBalance strictTrialBalancePolicy` に渡し, 受入元と一緒に保持する.
締め取引が存在すれば最終 snapshot の検証 stage は `AfterClosing`, なければ `BeforeClosing` である.
独立した残高・説明・再分類を提出側から差し込む引数は置かない.
この初期 API は strict policy を固定するため, 説明付き仮勘定等を通す緩和経路は提供しない.

`presentAdmitted` は調整後と最終の両方を既存 `present` へ渡す.
締めで P/L が消える前の表示と, 利益剰余金に振り替えた後の表示を分けて保持する.
`renderAdmittedStatements` は UTF-8 CSV で両 snapshot を出力する.
会社・期間を複数含む受入値の導出は全 Journal の合計である.
それらを合計することの会計上の適切さは信頼側が決める.

## 同値判定

`isEquivalentUpTo` は受入とは独立した関数であり, 未受入の Entry の Map にも使える.
全方式でキー集合の一致を要求し, 会社・期間・取引の境界を保存する.
posting の比較は `(Hat, AccountTitles, MoneyDecimal)` を sort した多重集合で行う.
既存 Alg の Eq や Journal 全体の `bar` を代用にしない.

| 方式 | 正規化の範囲 |
|---|---|
| `PostingMultiset` | 行順だけを無視する |
| `NetWithinTransaction` | 各取引の Alg を個別に `bar` する |
| `NetAccountsInTransactions` | 指定取引の指定科目だけ `bar` し, 他は厳密比較する |

`Entry` の基底軸は科目だけに固定している. 正規化後の金額の比較は厳密な等号である.
ただし, この checkout の既存 `bar` は複数 posting の表現に対して, `MoneyDecimal` にも
`1e-13 + 1e-12 * max(abs(hatTotal), abs(notTotal))` の許容幅を適用し, その範囲の残差を落とす.
`NetWithinTransaction` と指定科目の netting は, 仕様どおり既存 `bar` のこの性質を継承する.
例えば Cash の Not 側 1e12 と Hat 側 1e12 - 0.5 の差は正確には 0.5 だが,
`bar` 後は零と同値になる. `PostingMultiset` と指定外科目の厳密比較にはこの許容幅を適用しない.
単一の atomic posting は微小額でも `bar` がそのまま返す. 複数 posting の差が 1e-14 となる
絶対許容幅の反例と, 単一 posting の 1e-14 が保存される例をそれぞれテストに持つ.
既存 `bar` の変更や独自の厳密 netting への置換は今回の設計・変更範囲に含めない.
受入済み値に wildcard はない. 未受入の wildcard Hat を含む値が比較に来た場合は,
その取引を厳密比較し, wildcard を含む値に netting を適用しない.

## module と既存 API の対応

| module | 責務 | 再利用する既存 API |
|---|---|---|
| Admission | 公開入口 | 下位の受入・導出関数を限定して export |
| Accounting.Transaction | 識別子と仕訳 | MoneyDecimal, Alg, Note |
| Admission.Catalog.Input / Workflow | 閉じた操作の入力 / 役割・段階・出所 | 取引識別子, MoneyDecimal |
| Admission.Registry.Definition / Submission / Diagnostic | registry 宣言 / 提出 / 診断 | Catalog.Input, Workflow, EntryError |
| Admission.Representation | 非公開の検証済み表現 | Journal, ValidatedTrialBalance |
| Registry | 重複を保全した registry 構築 | containers の Map |
| Catalog | 固定 22 操作と会計方針 | Bookkeeping builders, finalStockTransfer |
| Engine | 受入の固定順序と参照解決 | checkedEntryTextIn, checkedEntryIn, Journal.projWithNote, toAlg |
| Derive | snapshot の検証と諸表の導出 | validateTrialBalance, present |
| IO.Output.Admission | 受入済み諸表の CSV 出力 | FinancialStatements |
| Accounting.Equivalence | 指定された観測での比較 | Alg.filter, bar, toList |

受入専用の拡張点は registry, facts, evidence, vocabulary という値であり, 新しい型クラスは作らない.
catalog は今回承認された閉じた機能であり, model の関数を library へ注入する拡張点はない.
定数として課題の金額・会社・期間を持ち込まない.
単一会社の決算と複数会社の連結を異なる fixture として同じ入口で検証する.
各 module は library の機能単位であり, 受入進捗は coordinator によるレビューと land で確定する.

追加の実装判断は本書の識別子, stage, snapshot, strict policy, CSV の各節に記録した.
既存の代数法則を増やす変更ではない. 新しい法則は registry の一覧保存と重複拒否,
受入成功時の出所・均衡・証憑義務, 各比較方式の同値関係であり, Haddock と property test に対応させる.

実装規模の集計は空行と `--` コメント行を除いた Haskell の行数で行う.
新規 library module を kit, 新規 test module を test として数え, module ごとの責務を上表で確認する.
catalog と受入エンジンはそれぞれ独立した責務として保持し, モジュールの移動を規模削減とは数えない.

## 公開 API の型と関数

入力の構築子を公開する型は `EntityId`, `PeriodId`, `TxId`, `FactId`, `EvidenceId`, `CallId`,
`TxKey`, `Role`, `Presence`, `Supply`, `AdmissionSpec`, `CatalogOperationKind`, `CatalogCall`,
`EntityInput`, `Call`, `Submission` である. 診断・観測の構築子を公開する型は
`RegistryError`, `AdmissionError`, `ReferenceFailure`, `Stage`, `Provenance`, `CallAudit`,
`Snapshot`, `Equivalence` である. 構築子を隠す公開型は `TxRule`, `TxIdRegistry`, `Admitted`,
`AdmittedTrialBalance`, `AdmittedStatements` とする.

```haskell
type Entry = Alg MoneyDecimal (HatBase AccountTitles)
type RawPostings = [(Text, Text, MoneyDecimal)]
type AdmissionJournal = Journal TxKey MoneyDecimal (HatBase AccountTitles)
type LedgerView = Map TxKey Entry

txRule :: Presence -> [Supply] -> Maybe EvidenceId -> TxRule
rulePresence :: TxRule -> Presence
ruleSupplies :: TxRule -> Set Supply
ruleEvidence :: TxRule -> Maybe EvidenceId
txIdRegistry :: [(TxKey, TxRule)] -> Either (NonEmpty RegistryError) TxIdRegistry
registryRules :: TxIdRegistry -> Map TxKey TxRule
catalogKind :: CatalogCall -> CatalogOperationKind
catalogStage :: CatalogCall -> Stage
isGenerating :: CatalogOperationKind -> Bool

admit :: AdmissionSpec -> Submission -> Either (NonEmpty AdmissionError) Admitted
admittedJournal :: Admitted -> AdmissionJournal
admittedSnapshot :: Snapshot -> Admitted -> LedgerView
admittedAudit :: Admitted -> [CallAudit]
deriveLedger :: Admitted -> LedgerView
deriveTrialBalance
    :: Admitted
    -> Either (NonEmpty (TBFinding MoneyDecimal)) AdmittedTrialBalance
admittedTrialBalance :: AdmittedTrialBalance -> Entry
admittedAdjustedTrialBalance :: AdmittedTrialBalance -> Entry
presentAdmitted
    :: ReportingContext MoneyDecimal
    -> AdmittedTrialBalance
    -> Either (NonEmpty (PresentationIssue MoneyDecimal)) AdmittedStatements
admittedFinancialStatements :: AdmittedStatements -> FinancialStatements MoneyDecimal
admittedClosingStatements :: AdmittedStatements -> FinancialStatements MoneyDecimal
renderAdmittedStatements :: AdmittedStatements -> ByteString

isEquivalentUpTo :: Equivalence -> Map TxKey Entry -> Map TxKey Entry -> Bool
```

公開入力 record の field は次のとおりである. 受入済み型の getter とは異なり,
これらは信頼側の仕様または未受入の提出を組み立てる record selector である.

```haskell
admissionRegistry :: AdmissionSpec -> TxIdRegistry
admissionEvidence :: AdmissionSpec -> Map EvidenceId MoneyDecimal
admissionFacts :: AdmissionSpec -> Map FactId RawPostings
admissionVocabulary :: AdmissionSpec -> Set AccountTitles
submissionPostings :: Submission -> [(TxKey, RawPostings)]
submissionCalls :: Submission -> [Call]
callId :: Call -> CallId
callEntity :: Call -> EntityId
callPeriod :: Call -> PeriodId
callGenerated :: Call -> Maybe TxKey
callBody :: Call -> CatalogCall
auditCall :: CallAudit -> CallId
auditOperation :: CallAudit -> CatalogOperationKind
auditGenerated :: CallAudit -> Maybe TxKey
auditReferences :: CallAudit -> [(TxKey, Provenance)]
auditProjection :: CallAudit -> Maybe MoneyDecimal
```
