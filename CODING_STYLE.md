<!--
seed_id: haskell-coding-style
seed_version: 2026-09-20
copied_at: 2026-09-23
ported_change_ids: [cl-2026-09-12-initial-rules@4477f3, cl-2026-09-12-haddock-scope@bb2db6, cl-2026-09-12-when-unless@8e8c6c, cl-2026-09-12-library-exceptions@72990e, cl-2026-09-12-strictness-scope@d6edff, cl-2026-09-14-type-synonyms@be2648, cl-2026-09-14-totality-typed-index@814503, cl-2026-09-14-module-budget@206d98, cl-2026-09-14-naming-errors-laws@8221e5, cl-2026-09-16-sum-type-layout@65eb99, cl-2026-09-19-american-spelling@008605, cl-2026-09-20-line-width@431fa2, cl-2026-09-20-module-order@c89621, cl-2026-09-20-defaults-and-names@324fbd, cl-2026-09-20-design-iteration@dc7c2f]
declined_change_ids: []
scope: haskell-library
repo: haskell-exchange-algebra
line_width: 100
sequence_threshold: 3
do_not_auto_sync: true
-->

# Haskell コーディング規則 (移転済み)

本 file の本文は 2026-09-23 に参照型の manifest へ移した. 正本は次の 2 つである.

- 規則本文: Orchestrator の profile `haskell-coding-style` (scope haskell-library) の view.
- 本 repo の特化: `coding/local/haskell-library.md` (manifest は `coding/coding-style.yaml`).

`coding/` は machine-local で公開 repo には含めない (`.git/info/exclude`). 上の header (`line_width`,
`sequence_threshold`) は Haskell の style lint が読むために残している. lint が manifest から値を読むように
なったら本 file を削除する.
