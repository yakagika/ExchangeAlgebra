# post-land2 fixtures

`suggest.tsv` は **Land 2 (Definition 7 contra 改訂) 後の期待出力そのもの**。
Spec の `testLand2SuggestClosedDiff` が byte 一致を検査し, pre-land1 fixture との
差分が contra 2 科目の description 変更で説明できることを併せて検査する。

注意: header 行は Land 1 の golden 形式 (`# pre-land1 ...; commit 2d91646...`) を
共有している (Spec の `goldenHeader`/`goldenCommit` と byte 一致させるため)。
「pre-land1」は **format の名前**であり, 内容は Land 2 時点 (ab6043f 以降) の出力。
