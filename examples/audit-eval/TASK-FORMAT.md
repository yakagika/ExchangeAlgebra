# TASK-FORMAT v2 — task JSON contract for the 23-task pilot

Extends the v1 format (journal-only, bare posting array) to the heterogeneous
ground-truth shapes of the representative task set (#4–#23, source of truth:
`audit-harness docs/t3-tasks-representative.md`). v1 task files remain valid:
a task without `expected_output` is treated as journal-only.

## Task JSON fields

```jsonc
{
  "id": "...",                    // = YAML id
  "category": "...",              // journalize|closing|statements|consolidation|audit
  "topic": "leases",              // optional descriptive tag; does not select scoring
  "difficulty": "mechanical",     // mechanical | judgment
  "ea_coverage": "ok",            // ok | needs-extension
  "audit": false,                 // bool (from YAML)
  "source": { ... },              // provenance object, transcribed verbatim from YAML
  "prompt": "...",                // YAML prompt (unchanged)
  "given": { ... },               // ALL YAML input.* keys, structure preserved,
                                  // plus chart_of_accounts + ea_account_map (see below)
  "expected_output": {            // ABSENT for v1 journal-only tasks
    "components": ["journal", "derived"],   // subset of journal|derived|findings|decision
    "format_note": "...",         // 1–3 sentence English description of the exact
                                  // output object, appended to the system prompt
    "format_note_side": "..."     // optional side-contract variant of format_note
  },
  "ground_truth": { ... }         // see per-component shapes below
}
```

`category` is the closed 5-value process enum. Accounting subject matter is an
optional `topic` tag, such as `leases`, `revenue`, `tax`, or `bonds`; it does
not create another category or change scoring. Foreign exchange (`fx`) is out
of scope for the second-round suite.

Primary correctness is category-dependent. For the four bookkeeping categories
(`journalize`, `closing`, `statements`, `consolidation`), every submitted ledger
and trial-balance `(side, amount)` pair must match GT. For `audit`, submitted
`(type, locus)` finding pairs must match GT exactly.

This primary rule governs the sealed, generated mechanical suite. The curated
23-task set is an unsealed development/Discussion example set and includes
judgment and textbook tasks whose narrower `expected_output` contracts do not
all contain side-contract ledger/TB pairs or audit findings; those examples are
not observations in the confirmatory primary estimand.

Generated task IDs use
`gen-<kind>-<template>-<seed:06d>-<count:02d>`, where `<kind>` is the process
category and `seed` is `source.seed`. Frozen experiment-1 compatibility fixtures
may retain their pre-v2 IDs. Manifest clusters use
`<template>-<source.seed:06d>`: count levels from the same template and generator
seed belong to one cluster.

## Model output contract

- **journal-only** (v1): bare JSON array of postings
  `[{"side":"debit"|"credit","account":"...","amount":<positive number>}, ...]`.
- **anything else**: exactly ONE JSON **object** whose keys are the
  `expected_output.components`:
  - `"journal"`: posting array as above. Extra keys per posting (`entry`,
    `date`, `entity`) are allowed and ignored by the scorer.
  - `"derived"`: flat string→number|string map (see key naming below). String
    leaves are allowed only for side-contract keys ending in `.side` or
    `.balance_side`.
  - `"findings"`: array of `{"type": "...", "locus": "...", "detail": "..."}`
    (`detail` free text, unscored).
  - `"decision"`: flat string→string map (id → label, lower-case labels).
- **judgment escape-hatch tasks** (`ground_truth.escape_hatch_expected: true`):
  the object must additionally contain EITHER
  `"policy_assumed": "<policy>"` (with the answer computed under that policy)
  OR `"alternatives": {"<policy>": {<derived map>}, ...}` covering the
  plausible policies. A bare unconditional answer scores 0 on the
  escape-hatch metric even if numerically right under one policy.

## ground_truth per-component shapes

- `journal`: `[{side, account, amount}]`, amounts positive. Flatten dated /
  multi-part / multi-entity journals into ONE array; keep `entry`/`date`/
  `entity` tags for readability (scorer ignores them). Multi-entity books
  (e.g. lessee + lessor) go into the same array — the multiset matcher and the
  balance check remain valid.
- `derived`: FLAT map of numeric leaves, plus the scored side-contract string
  leaves `*.side` and `*.balance_side`. Flatten nested YAML with dot
  keys (`"operating.total_adjustments": -203500`). Sign conventions follow the
  YAML verbatim. Other non-numeric leaves (booleans, prose) are NOT scored — either
  omit them or park them outside `derived` (see "unscored extras").
- `findings`: `[{type, locus}]` (+ optional detail). `type` from the YAML;
  `locus` = the YAML locus / account / entry id as a string.
- `decision`: flat map, labels lower-cased (`{"a": "operating"}`).
- `policy_conditional`: map policy → flat derived-style map
  (`{"straight_line": {"expense": 2400}, ...}`) + `escape_hatch_expected: true`.
- **Unscored extras are allowed** in `ground_truth` under other keys
  (`schedule`, `analysis_note`, `findings_note`, …) for documentation; the
  scorer reads only the five keys above.

## Scoring semantics (score.py v2)

- `journal` → existing mapping-aware posting matcher: `journal_accuracy`,
  `balance_violation`, `account_validity` / `hallucinated_accounts`; the EA
  oracle runs on the journal component only.
- `derived` → `derived_accuracy` = fraction of GT entries matched. Key match
  is case-/separator-insensitive on flattened keys (model may emit nested
  objects; flatten before comparison). Value match tolerance:
  `abs(diff) <= 0.51 or rel <= 1e-3` (textbook rounding to $1).
- `findings` → `findings_recall` and `findings_precision` on normalized
  `(type, locus)` pairs.
- `decision` → `decision_accuracy` = exact label match fraction (lower-cased).
- escape-hatch → `escape_ok ∈ {0,1}`: 1 iff `alternatives` (≥2 policies, each
  matching its `policy_conditional` values within tolerance) or
  `policy_assumed` + values matching that policy's GT. Headline accuracy for
  such tasks = escape_ok.
- Headline `numeric_accuracy` = micro-average over all GT items of the present
  components (journal postings + derived entries + findings-recall items +
  decision entries). Per-component metrics are kept alongside.
- Tasks without a `journal` component: `balance_violation` / `account_validity`
  are computed only if the model output contains postings; `verification_gap`
  = None (oracle skipped).
- **Oracle gating**: the EA oracle is skipped (verification_gap = None) for
  tasks with `ea_coverage != "ok"` — EA cannot express those journals, so
  "would EA reject this" is not a meaningful counterfactual there.

## ea_account_map construction rules

EA-canonical names = constructor names of `AccountTitles`
(`src/ExchangeAlgebra/Algebra/Base/Element.hs` in this worktree — verify by
grep, do NOT trust memory). The V-Land 3 catalog contains 232 concrete
constructors and covers every distinct normalized A/B query in the JCCI 2022
fixture. Coverage means a unique `Right` or a deliberately enumerated
`AmbiguousAccount`; it never means first-match resolution. This is label-level
coverage: source rows marked `COLLAPSE` merge distinct JCCI accounts into one EA
constructor and must not be treated as lossless sub-ledger coverage.

1. Every `chart_of_accounts` entry gets a map entry (identity mapping when the
   GT name IS an EA constructor).
2. GT-journal accounts missing from the chart also get map entries.
3. A JCCI A/B Japanese label may be resolved with `parseAccountTitle`, but the
   task mapping must record the resulting canonical constructor. If the label
   is policy-ambiguous (`銀行預金`, `〇〇商店`, `貸付金`, `仮払金`, `有価証券`,
   `投資有価証券`, `関係会社株式`, `借入金`, `未払金`, `仮受金`, `営業収益`,
   `有価証券運用益`, `通信費`, `地代家賃`, `支払賃借料`, `支払不動産賃借料`,
   `営業費用`, `有価証券運用損`, `為替差損益`, `有価証券評価損益`, or
   `有価証券運用損益`), the task must select the
   contextually correct constructor and record the choice in
   `map_note`; do not select by candidate order.
4. GT name with a clear non-JCCI EA counterpart → map to it
   (`Inventory → MerchandiseInventory`).
5. GT name with NO EA counterpart:
   - if a defensible provisional alias exists, use it and record
     `"map_note"` (task-level key inside `given`) explaining the choice —
     this is an accounting-review point;
   - on `needs-extension` tasks, map to itself + `map_note`
     "no EA counterpart (needs-extension)". The oracle is skipped for these
     tasks anyway (see oracle gating).

## Fidelity rules for conversion (agents)

- GT values are TRANSCRIBED from the YAML, never recomputed or "fixed".
  If a YAML value looks wrong, transcribe it and add `"transcription_note"`.
- Do not drop GT items silently. Schedules / statements too large to score
  in full go under an unscored key (e.g. `"schedule"`), with the scoreable
  summary values (totals, end balances) in `derived`.
- Keep the YAML `review:` / `confidence:` strings as `"review"` /
  `"confidence"` top-level keys (documentation; unscored).
