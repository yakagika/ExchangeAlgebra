"""
runner/score.py — GT comparison and per-run metrics for audit-eval.

Metrics
-------
numeric_accuracy   : headline — micro-average over ALL present GT components
                     (journal postings + derived entries + findings-recall
                     items + decision entries). Escape-hatch tasks (see
                     below) use escape_ok as the headline instead. For v1
                     journal-only tasks (no expected_output) this reduces to
                     exactly the old journal-only value.
journal_accuracy   : fraction of GT postings exactly matched (canonical
                     account + side + amount) — same matcher as v1
                     numeric_accuracy, applied to the "journal" component.
raw_journal_accuracy : arm A' only — journal_accuracy of the model's FIRST-pass
                     raw postings, before the checked loader accepted / canonically
                     re-printed them. Comparing raw vs journal_accuracy isolates the
                     model's own competence from the gate-tutor effect of A'
                     reconcileSources amount feedback. None for other arms.
derived_accuracy   : fraction of GT "derived" entries matched (flattened,
                     key-normalized, numeric-tolerant).
findings_recall    : fraction of GT "findings" (type, locus) pairs matched.
findings_precision : fraction of model "findings" (type, locus) pairs that
                     were correct.
decision_accuracy  : fraction of GT "decision" entries with an exact
                     (lower-cased) label match.
escape_ok          : 0/1 — for ground_truth.escape_hatch_expected tasks only;
                     1 iff the model hedges across policies correctly (see
                     TASK-FORMAT.md "Scoring semantics").
balance_violation  : True if Sigma debit != Sigma credit in the model's
                     journal component; None if the model output has no
                     postings to check (parse failure, no journal component
                     expected, or the model omitted the journal key).
account_validity   : True if ALL journal-component accounts resolve via
                     chart / ea_account_map / synonyms; None under the same
                     conditions as balance_violation.
compile_fail       : True if arm A/B/D build or execution failed (passed through from arm result)
parse_fail         : True if model output could not be parsed to canonical JSON
first_pass_valid   : True iff the first generated response was structurally
                     valid without any retry. False means validity required
                     feedback or never converged; None means the arm did not
                     report the measurement.
verification_gap   : (EA oracle) 1 if the model's journal component contains
                     an error that EA would structurally reject (imbalance /
                     account outside EA AccountTitles / category violation /
                     non-positive amount), else 0. None when the oracle was
                     not applied (see "Oracle gating" below) or the output
                     was unparseable / had no journal component.
convergence_iterations : attempts needed until a structurally-valid output (P4 retry
                     loop). Equals max_iters with converged=False when the loop was
                     exhausted without success.
timed_out          : True if the arm stopped because a backend call exceeded its
                     timeout (recorded as non-convergence, NOT retried — Track S
                     pilot policy). A timed_out cell has converged=False and its
                     accuracy metrics reflect no usable output; separating it from a
                     genuine wrong answer keeps the large-N latency wall visible.
posting_complete   : True iff model postings exactly match the GT multiset on
                     txid, side, resolved account, and amount.
outcome            : experiment-2 three-way result: committed_correct,
                     committed_incorrect, or refused. None for categories
                     outside the five-category experiment-2 contract.

Oracle gating (TASK-FORMAT.md v2, 2026-07-02)
----------------------------------------------
The EA oracle is skipped (verification_gap = None) whenever:
  - task["ea_coverage"] != "ok" (EA cannot express the journal — "would EA
    reject this" is not a meaningful counterfactual), OR
  - the task's output contract does not include a "journal" component, OR
  - the model output has no journal-component postings (parse failure, wrong
    shape, or the key was simply omitted).
Only the model's journal-component postings are fed to the oracle (not the
whole output object).

Arm Aprime accepts only postings that passed LoadChecked.hs checked
construction and source reconciliation, so it is not included in the default
oracle_arms (B,C). Add it explicitly only for smoke/debug runs.

Account-name resolution (2026-07-02, spec: docs/t3-task-schema.md
「勘定名の EA canonical 対応」)
-----------------------------------------------------------------
GT names (US-GAAP textbook English) and EA AccountTitles (日商簿記系 canonical) are
systematically divergent. Scoring is mapping-aware: for each chart account we accept
  - the GT name itself,
  - its `ea_account_map` target (EA canonical name),
  - normalization-dictionary synonyms (case-insensitive, A/R → AccountsReceivable, …).
A hallucinated account is ONLY a name that resolves through none of these.

ea_account_map is MANY-TO-ONE in real tasks (e.g. journalize-adjusting-entries-
kieso-ch02-001 maps both Supplies AND PrepaidInsurance to EA 'PrepaidExpenses'),
so resolution is collision-aware: an alias resolves to a LIST of GT candidate
names, not a single name. The journal matcher accepts a model posting against
any remaining GT posting whose (side, amount) match and whose account is among
the candidates (greedy; the first amount-matching GT posting is consumed).
Without this, arm A/D answering in the correct EA vocabulary would fail to
match the later-registered GT account — re-introducing at the many-to-one
boundary the name-translation bias P1 removed.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any, Optional


# ---------------------------------------------------------------------------
# Normalization dictionary (synonyms → normalized key of the canonical name)
# ---------------------------------------------------------------------------

def _norm_key(name: str) -> str:
    """
    Lower-case and strip separators, so 'Accounts Receivable' == 'AccountsReceivable'
    and (reused for derived/decision keys) 'operating.total' == 'operating_total'.
    """
    return (
        str(name).lower()
        .replace(" ", "").replace("_", "").replace("-", "").replace("/", "")
        .replace(".", "")
    )


# synonym (normalized) → canonical account name (normalized)
_SYNONYMS: dict[str, str] = {
    "ar":                     "accountsreceivable",
    "tradereceivable":        "accountsreceivable",
    "tradereceivables":       "accountsreceivable",
    "ap":                     "accountspayable",
    "tradepayable":           "accountspayable",
    "tradepayables":          "accountspayable",
    "cogs":                   "costofgoodssold",
    "accumdep":               "accumulateddepreciation",
    "accumulateddep":         "accumulateddepreciation",
    "depreciation":           "depreciationexpense",   # EA 'Depreciation' == GT 'DepreciationExpense'
    "merchandise":            "merchandiseinventory",
    "inventories":            "inventory",
    "salesrevenue":           "sales",
}


def _canon(name: str) -> str:
    """Normalize then apply the synonym dictionary."""
    key = _norm_key(name)
    return _SYNONYMS.get(key, key)


# ---------------------------------------------------------------------------
# Task-specific resolver: alias (normalized) → GT canonical candidate list
# ---------------------------------------------------------------------------

def build_resolver(task: dict) -> dict[str, list[str]]:
    """
    Build a resolution table for one task: alias key → ORDERED LIST of GT
    canonical account names the alias may stand for.

    Every chart-of-accounts entry contributes:
      GT name, its ea_account_map target, and their synonym expansions,
    all mapping to the GT name (the canonical scoring identity).
    GT-journal accounts not in the chart are added defensively.

    ea_account_map is many-to-one in real tasks (several GT names sharing one
    EA target), so an alias key may accumulate MULTIPLE candidates — hence a
    list, appended in registration order (chart order first), never clobbered.
    """
    given = task.get("given", {})
    chart: list[str] = given.get("chart_of_accounts", []) or []
    ea_map: dict[str, str] = given.get("ea_account_map", {}) or {}

    resolver: dict[str, list[str]] = {}

    def register(alias: str, canonical: str) -> None:
        for key in {_norm_key(alias), _canon(alias)}:
            bucket = resolver.setdefault(key, [])
            if canonical not in bucket:
                bucket.append(canonical)

    for gt_name in chart:
        register(gt_name, gt_name)
        mapped = ea_map.get(gt_name)
        if mapped:
            register(mapped, gt_name)

    # Safety: GT journal may reference accounts missing from the chart.
    gt_journal = task.get("ground_truth", {}).get("journal", [])
    for p in gt_journal:
        acct = str(p.get("account", "")).strip()
        if acct:
            register(acct, acct)
            mapped = ea_map.get(acct)
            if mapped:
                register(mapped, acct)

    return resolver


def resolve_account(name: str, resolver: dict[str, list[str]]) -> list[str]:
    """
    Resolve an output account name to its GT canonical candidates (ordered,
    deduplicated across the norm/synonym key paths). Empty list = unresolved
    (hallucinated).
    """
    candidates: list[str] = []
    for key in (_norm_key(name), _canon(name)):
        for c in resolver.get(key, []):
            if c not in candidates:
                candidates.append(c)
    return candidates


# ---------------------------------------------------------------------------
# Posting normalization
# ---------------------------------------------------------------------------

def _posting_fields(p: dict) -> tuple[str, str, float]:
    """Return (side, raw_account, amount) from a posting dict."""
    return (
        str(p.get("side", "")).lower().strip(),
        str(p.get("account", "")).strip(),
        float(p.get("amount", 0)),
    )


def _match_journal(model_journal: list, gt_journal: list,
                   resolver: dict[str, list[str]]) -> int:
    """
    Collision-aware greedy multiset matcher. GT postings keep their raw GT
    account name as the canonical identity (defensive registration in
    build_resolver guarantees it is a valid resolution target). Each model
    posting consumes the FIRST remaining GT posting whose (side, amount) match
    and whose account is among the model account's resolution candidates —
    so two model postings on the same EA name (e.g. PrepaidExpenses) can match
    two distinct colliding GT accounts (Supplies@580, PrepaidInsurance@150),
    disambiguated by amount.
    """
    remaining = [_posting_fields(p) for p in gt_journal]
    matched = 0
    for p in model_journal:
        side, acct, amount = _posting_fields(p)
        candidates = resolve_account(acct, resolver)
        if not candidates:
            continue
        for idx, (g_side, g_acct, g_amount) in enumerate(remaining):
            if g_side == side and g_amount == amount and g_acct in candidates:
                matched += 1
                del remaining[idx]
                break
    return matched


def _posting_complete(
    model_journal: Any,
    gt_journal: list,
    resolver: dict[str, list[str]],
) -> bool:
    """Exact txid-aware multiset equality for experiment-2 postings."""
    if not isinstance(model_journal, list) or len(model_journal) != len(gt_journal):
        return False

    remaining: list[tuple[str, str, str, float]] = []
    try:
        for posting in gt_journal:
            txid = posting.get("entry")
            if not isinstance(txid, str) or not txid:
                return False
            side, account, amount = _posting_fields(posting)
            remaining.append((txid, side, account, amount))

        for posting in model_journal:
            if not isinstance(posting, dict):
                return False
            txid = posting.get("txid")
            if not isinstance(txid, str) or not txid:
                return False
            side, account, amount = _posting_fields(posting)
            candidates = resolve_account(account, resolver)
            if not candidates:
                return False
            for index, (gt_txid, gt_side, gt_account, gt_amount) in enumerate(remaining):
                if (
                    txid == gt_txid
                    and side == gt_side
                    and amount == gt_amount
                    and gt_account in candidates
                ):
                    del remaining[index]
                    break
            else:
                return False
    except (AttributeError, TypeError, ValueError, OverflowError):
        return False
    return not remaining


def _to_ea_postings(parsed: list, task: dict, resolver: dict[str, list[str]]) -> list[dict]:
    """
    Translate output postings into EA-canonical vocabulary before feeding the
    EA oracle (mapping-aware, mirroring P1 scoring).

    Rationale: arm C legitimately answers in GT vocabulary (`Inventory`,
    `DepreciationExpense`). Feeding raw GT names to the oracle would flag them
    as hallucinated (not EA constructors) and systematically set gap=1 for
    correct answers — re-introducing the name-translation bias P1 removed.
    The oracle question is the counterfactual "would EA reject this journal
    if injected as arm A would inject it?", so each account is resolved to its
    GT-canonical identity and then mapped through ea_account_map. Names that
    resolve nowhere are passed through raw — the oracle then flags them as
    hallucinated, which is exactly the ②-error we want to detect.

    Collision note: when an alias resolves to multiple GT candidates, they all
    share the same EA target by construction (a collision IS several GT names
    mapping to one EA name), so any candidate yields the same EA name. We pick
    deterministically: the first candidate with an ea_map entry, else the
    first candidate.
    """
    ea_map: dict[str, str] = (task.get("given", {}) or {}).get("ea_account_map", {}) or {}
    out = []
    for p in parsed:
        acct = str(p.get("account", "")).strip()
        candidates = resolve_account(acct, resolver)
        if candidates:
            canonical = next((c for c in candidates if c in ea_map), candidates[0])
            ea_name = ea_map.get(canonical, canonical)
        else:
            ea_name = acct   # unresolvable → oracle flags hallucinated_account
        out.append({
            "side":    str(p.get("side", "")).lower().strip(),
            "account": ea_name,
            "amount":  float(p.get("amount", 0)),
        })
    return out


# ---------------------------------------------------------------------------
# derived / findings / decision matchers (TASK-FORMAT.md v2 scoring semantics)
# ---------------------------------------------------------------------------

def _flatten_numeric(obj: Any, prefix: str = "") -> dict[str, float]:
    """
    Flatten a (possibly nested) mapping into a flat dotted-key -> float map,
    keeping numeric leaves only. Booleans and other non-numeric leaves
    (strings, lists, None) are dropped silently — TASK-FORMAT.md "derived"
    semantics ("Non-numeric leaves ... are NOT scored").
    """
    out: dict[str, float] = {}
    if isinstance(obj, dict):
        for k, v in obj.items():
            key = f"{prefix}.{k}" if prefix else str(k)
            out.update(_flatten_numeric(v, key))
    elif isinstance(obj, bool):
        pass
    elif isinstance(obj, (int, float)):
        out[prefix] = float(obj)
    return out


def _numeric_close(a: float, b: float) -> bool:
    """Textbook-rounding tolerance: abs(diff) <= 0.51 or rel(diff) <= 1e-3."""
    if abs(a - b) <= 0.51:
        return True
    if b != 0 and abs(a - b) / abs(b) <= 1e-3:
        return True
    return False


def _match_derived(model_derived: Any, gt_derived: dict) -> tuple[int, int]:
    """Return (matched, total) for a derived-map comparison (tolerant match)."""
    gt_flat = _flatten_numeric(gt_derived)
    model_flat = _flatten_numeric(model_derived) if isinstance(model_derived, (dict,)) else {}
    model_norm = {_norm_key(k): v for k, v in model_flat.items()}

    matched = 0
    for k, gt_v in gt_flat.items():
        mv = model_norm.get(_norm_key(k))
        if mv is not None and _numeric_close(mv, gt_v):
            matched += 1
    return matched, len(gt_flat)


def _is_side_contract_key(key: str) -> bool:
    return (
        key.startswith("ledger.")
        and (key.endswith(".balance_side") or key.endswith(".balance_amount"))
    ) or (
        key.startswith("trial_balance.")
        and (key.endswith(".side") or key.endswith(".amount"))
    )


def _is_v1_signed_balance_key(key: str) -> bool:
    if key.startswith("ledger.") and key.endswith(".balance"):
        account = key[len("ledger.") : -len(".balance")]
        return bool(account) and "." not in account
    if key.startswith("trial_balance."):
        account = key[len("trial_balance.") :]
        return bool(account) and "." not in account
    return False


def _side_pair_roots(derived: dict[str, Any]) -> list[tuple[str, str, str]]:
    """Return every side-contract identity declared by either half of a pair."""
    pairs: dict[str, tuple[str, str]] = {}
    for key in derived:
        if key.startswith("ledger.") and key.endswith(".balance_side"):
            root = key.removesuffix("_side")
            pairs[root] = (root + "_side", root + "_amount")
        elif key.startswith("ledger.") and key.endswith(".balance_amount"):
            root = key.removesuffix("_amount")
            pairs[root] = (root + "_side", root + "_amount")
        elif key.startswith("trial_balance.") and key.endswith(".side"):
            root = key.removesuffix(".side")
            pairs[root] = (root + ".side", root + ".amount")
        elif key.startswith("trial_balance.") and key.endswith(".amount"):
            root = key.removesuffix(".amount")
            pairs[root] = (root + ".side", root + ".amount")
    return [(identity, *pairs[identity]) for identity in sorted(pairs)]


def _validate_side_ground_truth(gt_derived: dict[str, Any]) -> None:
    """Fail closed when a side-contract ground truth is malformed."""
    for key in gt_derived:
        if not _is_v1_signed_balance_key(key):
            continue
        if key.startswith("ledger."):
            side_key = key + "_side"
            amount_key = key + "_amount"
        else:
            side_key = key + ".side"
            amount_key = key + ".amount"
        missing = [
            pair_key
            for pair_key in (side_key, amount_key)
            if pair_key not in gt_derived
        ]
        if missing:
            raise ValueError(
                f"side contract requires paired side/amount GT for {key}: "
                f"missing {', '.join(missing)}"
            )

    for identity, side_key, amount_key in _side_pair_roots(gt_derived):
        missing = [key for key in (side_key, amount_key) if key not in gt_derived]
        if missing:
            raise ValueError(
                f"incomplete side-contract ground truth for {identity}: "
                f"missing {', '.join(missing)}"
            )
        side = gt_derived[side_key]
        amount = gt_derived[amount_key]
        if not isinstance(side, str) or side.strip().lower() not in {
            "debit", "credit", "zero"
        }:
            raise ValueError(f"invalid side-contract side for {identity}: {side!r}")
        if (
            not isinstance(amount, (int, float))
            or isinstance(amount, bool)
            or not math.isfinite(float(amount))
            or amount < 0
        ):
            raise ValueError(f"invalid side-contract amount for {identity}: {amount!r}")
        if side.strip().lower() == "zero" and float(amount) != 0.0:
            raise ValueError(
                f"zero side must have amount 0 for {identity}: {amount!r}"
            )


def _match_derived_contract(
    model_derived: Any,
    gt_derived: dict,
    scoring_contract: str,
) -> tuple[int, int]:
    """Match derived values under the frozen v1 or side-aware contract."""
    if scoring_contract not in {"v1", "side"}:
        raise ValueError(f"unknown scoring contract: {scoring_contract!r}")

    # v2 fields coexist with v1 fields in generated ground truth. They are
    # invisible to the frozen v1 scorer, preserving confirmatory results.
    if scoring_contract == "v1":
        gt_v1 = {k: v for k, v in gt_derived.items() if not _is_side_contract_key(k)}
        return _match_derived(model_derived, gt_v1)

    _validate_side_ground_truth(gt_derived)

    # Ledger debit/credit totals, arbitrary derived metrics, and financial
    # statements keep their v1 numeric comparison. Financial statements are
    # intentionally not side-aware in this Land; that belongs to the reporting
    # pipeline Land.
    gt_numeric = {
        k: v
        for k, v in gt_derived.items()
        if not _is_side_contract_key(k) and not _is_v1_signed_balance_key(k)
    }
    numeric_matched, numeric_total = _match_derived(model_derived, gt_numeric)

    model_map = model_derived if isinstance(model_derived, dict) else {}
    model_norm = {_norm_key(k): v for k, v in model_map.items()}
    side_matched = 0
    side_total = 0
    for _identity, side_key, amount_key in _side_pair_roots(gt_derived):
        side_total += 1
        gt_side = str(gt_derived.get(side_key, "")).strip().lower()
        gt_amount = gt_derived.get(amount_key)
        model_side = model_norm.get(_norm_key(side_key))
        model_amount = model_norm.get(_norm_key(amount_key))
        normalized_model_side = str(model_side).strip().lower()
        if (
            gt_side in {"debit", "credit", "zero"}
            and isinstance(gt_amount, (int, float))
            and not isinstance(gt_amount, bool)
            and normalized_model_side == gt_side
            and isinstance(model_amount, (int, float))
            and not isinstance(model_amount, bool)
            and math.isfinite(float(model_amount))
            and model_amount >= 0
            and not (
                normalized_model_side == "zero" and float(model_amount) != 0.0
            )
            and _numeric_close(float(model_amount), float(gt_amount))
        ):
            side_matched += 1
    return numeric_matched + side_matched, numeric_total + side_total


# Finding-type synonyms (normalized) → taxonomy type (normalized). The task
# format_note gives models the fixed taxonomy, so this is a safety net for
# near-miss vocabulary, mirroring the account-name P1 rationale: we measure
# defect detection, not label translation.
_FINDING_TYPE_SYNONYMS: dict[str, str] = {
    "unbalanced":            "imbalance",
    "unbalancedentry":       "imbalance",
    "notbalanced":           "imbalance",
    "debitcreditmismatch":   "imbalance",
    "unknownaccount":        "nonexistentaccount",
    "invalidaccount":        "nonexistentaccount",
    "hallucinatedaccount":   "nonexistentaccount",
    "accountnotinchart":     "nonexistentaccount",
    "balancediscrepancy":    "balancemismatch",
    "wrongbalance":          "balancemismatch",
    "misclassification":     "categoryviolation",
    "wrongcategory":         "categoryviolation",
    "categoryerror":         "categoryviolation",
}


def _norm_finding(f: dict) -> tuple[str, str]:
    ftype = _norm_key(str(f.get("type", "")))
    return (_FINDING_TYPE_SYNONYMS.get(ftype, ftype), _norm_key(str(f.get("locus", ""))))


def _match_findings(model_findings: Any, gt_findings: list) -> tuple[int, int, int]:
    """Return (matched, gt_total, model_total) via multiset match on (type, locus)."""
    gt_list = [_norm_finding(f) for f in (gt_findings or []) if isinstance(f, dict)]
    model_list = (
        [_norm_finding(f) for f in model_findings if isinstance(f, dict)]
        if isinstance(model_findings, list) else []
    )
    remaining = list(gt_list)
    matched = 0
    for mf in model_list:
        if mf in remaining:
            matched += 1
            remaining.remove(mf)
    return matched, len(gt_list), len(model_list)


_BOOKKEEPING_OUTCOME_CATEGORIES = {
    "journalize", "closing", "statements", "consolidation",
}


def _ledger_trial_balance_pairs_match(model_derived: Any, gt_derived: dict) -> bool:
    """Whether every ledger/TB side-amount pair matches, with no extra pair."""
    if not isinstance(model_derived, dict):
        return False
    relevant_gt = {
        key: value
        for key, value in gt_derived.items()
        if _is_side_contract_key(key)
    }
    gt_pairs = _side_pair_roots(relevant_gt)
    if not gt_pairs:
        return False
    matched, total = _match_derived_contract(model_derived, relevant_gt, "side")
    model_pair_count = len(_side_pair_roots(model_derived))
    return matched == total and model_pair_count == total


def _outcome(
    task: dict,
    arm_result: dict,
    parsed: Any,
    scoring_contract: str,
) -> Optional[str]:
    """Map one submission to the pre-registered experiment-2 outcome."""
    # The three-way outcome is defined by side contract v2.  Leaving it
    # unpopulated for frozen-v1 runs prevents a new metric from silently
    # reinterpreting legacy cells under a contract their prompts did not use.
    if scoring_contract != "side":
        return None

    category = str(task.get("category", "")).strip().lower()
    if category not in _BOOKKEEPING_OUTCOME_CATEGORIES | {"audit"}:
        return None

    refused = (
        bool(arm_result.get("timed_out"))
        or bool(arm_result.get("parse_fail", True))
        or parsed is None
        or arm_result.get("converged") is False
    )
    if refused:
        return "refused"

    gt = task.get("ground_truth", {}) or {}
    if category in _BOOKKEEPING_OUTCOME_CATEGORIES:
        model_derived = parsed.get("derived") if isinstance(parsed, dict) else None
        correct = _ledger_trial_balance_pairs_match(
            model_derived, gt.get("derived", {}) or {}
        )
    else:
        model_findings = parsed.get("findings") if isinstance(parsed, dict) else None
        matched, gt_total, model_total = _match_findings(
            model_findings, gt.get("findings", []) or []
        )
        # Exact multiset equality gives recall=precision=1.  The explicit
        # empty/empty case is correct even though both ratio metrics are None.
        correct = matched == gt_total == model_total
    return "committed_correct" if correct else "committed_incorrect"


def _match_decision(model_decision: Any, gt_decision: dict) -> tuple[int, int]:
    """Return (matched, total) — exact lower-cased label match per GT key."""
    gt = gt_decision or {}
    model = model_decision if isinstance(model_decision, dict) else {}
    model_norm = {_norm_key(k): v for k, v in model.items()}

    matched = 0
    for k, gt_v in gt.items():
        mv = model.get(k)
        if mv is None:
            mv = model_norm.get(_norm_key(k))
        if mv is not None and str(mv).strip().lower() == str(gt_v).strip().lower():
            matched += 1
    return matched, len(gt)


def _policy_map_matches(model_map: Any, gt_map: dict) -> bool:
    """All GT entries in gt_map must be present (normalized) and within tolerance in model_map."""
    matched, total = _match_derived(model_map, gt_map)
    return total > 0 and matched == total


def _score_escape_hatch(parsed_obj: dict, gt: dict) -> int:
    """
    escape_ok in {0, 1} — TASK-FORMAT.md "Scoring semantics":
    1 iff "alternatives" covers >=2 GT policies each matching within
    tolerance, or "policy_assumed" + the "derived" component matches that
    policy's GT within tolerance. A bare answer (neither key) scores 0.
    """
    policy_conditional = gt.get("policy_conditional", {}) or {}
    if not isinstance(parsed_obj, dict):
        return 0

    alternatives = parsed_obj.get("alternatives")
    if isinstance(alternatives, dict):
        matches = 0
        alt_norm = {_norm_key(k): v for k, v in alternatives.items()}
        for policy, gt_map in policy_conditional.items():
            model_map = alternatives.get(policy, alt_norm.get(_norm_key(policy)))
            if model_map is not None and _policy_map_matches(model_map, gt_map):
                matches += 1
        if matches >= 2:
            return 1

    policy_assumed = parsed_obj.get("policy_assumed")
    if isinstance(policy_assumed, str):
        pc_norm = {_norm_key(k): v for k, v in policy_conditional.items()}
        gt_map = policy_conditional.get(policy_assumed, pc_norm.get(_norm_key(policy_assumed)))
        if gt_map is not None:
            model_derived = parsed_obj.get("derived")
            if _policy_map_matches(model_derived, gt_map):
                return 1

    return 0


# ---------------------------------------------------------------------------
# Main scoring function
# ---------------------------------------------------------------------------

def score(
    task: dict,
    arm_result: dict,
    arm_name: str,
    worktree_root: Optional[Path] = None,
    oracle_arms: tuple[str, ...] = ("B", "C"),
    scoring_contract: str = "v1",
) -> dict[str, Any]:
    """
    Compute metrics for one (task, arm) pair.

    Parameters
    ----------
    task          : parsed task JSON dict
    arm_result    : output dict from arms.arm_c() / arm_a() / arm_b() / arm_d()
    arm_name      : "C", "A", "B", "D"
    worktree_root : EA worktree root (needed for the EA oracle); None disables oracle
    oracle_arms   : arms the EA oracle is applied to (default B and C — the arms
                    without construction-time structural guarantees)

    Returns
    -------
    metrics dict (see module docstring).
    """
    parse_fail   = arm_result.get("parse_fail", True)
    compile_fail = arm_result.get("compile_fail", False)
    parsed       = arm_result.get("parsed")        # list (v1) or dict (v2) or None

    gt = task.get("ground_truth", {}) or {}
    expected = task.get("expected_output")
    escape_hatch = bool(gt.get("escape_hatch_expected"))

    resolver = build_resolver(task)

    # ---- Extract the journal component (model output + GT) -------------------
    if expected is None:
        # v1 legacy: `parsed` IS the journal (bare posting array).
        has_journal_component = True
        model_journal = parsed if (not parse_fail and isinstance(parsed, list)) else None
    else:
        components = expected.get("components", []) or []
        has_journal_component = "journal" in components
        if has_journal_component and not parse_fail and isinstance(parsed, dict):
            mj = parsed.get("journal")
            model_journal = mj if isinstance(mj, list) else None
        else:
            model_journal = None

    gt_journal = gt.get("journal", []) or [] if has_journal_component else []
    posting_complete = _posting_complete(model_journal, gt_journal, resolver)

    # ---- balance_violation / account_validity (model postings only) ----------
    balance_violation: Optional[bool] = None
    account_validity: Optional[bool] = None
    hallucinated_accounts: list[str] = []

    if model_journal is not None:
        debit_total  = sum(float(p.get("amount", 0)) for p in model_journal
                           if str(p.get("side", "")).lower() == "debit")
        credit_total = sum(float(p.get("amount", 0)) for p in model_journal
                           if str(p.get("side", "")).lower() == "credit")
        balance_violation = abs(debit_total - credit_total) > 0.01

        # An account is valid iff it resolves to at least one GT candidate.
        account_validity = True
        for p in model_journal:
            acct = str(p.get("account", "")).strip()
            if not resolve_account(acct, resolver):
                account_validity = False
                hallucinated_accounts.append(acct)

    # ---- journal_accuracy (collision-aware mapping-aware multiset match) ------
    journal_matched = 0
    journal_accuracy: Optional[float] = None

    if model_journal is not None and gt_journal:
        journal_matched = _match_journal(model_journal, gt_journal, resolver)
        journal_accuracy = journal_matched / len(gt_journal)

    # ---- raw_journal_accuracy (arm A': first-pass model postings, pre-gate) ---
    # The accuracy of the model's OWN first attempt before the checked loader
    # accepted / canonically re-printed it. Comparing this to journal_accuracy
    # isolates the model's competence from the gate-tutor effect of A'
    # reconcileSources amount feedback (cross-check). None for arms that do not
    # report raw_first_journal, or when there is no GT journal to match.
    raw_journal_accuracy: Optional[float] = None
    raw_first_journal = arm_result.get("raw_first_journal")
    if isinstance(raw_first_journal, list) and gt_journal:
        raw_matched = _match_journal(raw_first_journal, gt_journal, resolver)
        raw_journal_accuracy = raw_matched / len(gt_journal)

    # ---- derived_accuracy ------------------------------------------------------
    derived_accuracy: Optional[float] = None
    derived_matched = derived_total = 0
    model_derived: Any = None
    if expected is not None and "derived" in (expected.get("components") or []):
        gt_derived = gt.get("derived", {}) or {}
        model_derived = parsed.get("derived") if (not parse_fail and isinstance(parsed, dict)) else None
        derived_matched, derived_total = _match_derived_contract(
            model_derived, gt_derived, scoring_contract
        )
        if derived_total > 0:
            derived_accuracy = derived_matched / derived_total

    # ---- findings_recall / findings_precision ----------------------------------
    findings_recall: Optional[float] = None
    findings_precision: Optional[float] = None
    findings_matched = findings_gt_total = findings_model_total = 0
    if expected is not None and "findings" in (expected.get("components") or []):
        gt_findings = gt.get("findings", []) or []
        model_findings = parsed.get("findings") if (not parse_fail and isinstance(parsed, dict)) else None
        findings_matched, findings_gt_total, findings_model_total = _match_findings(model_findings, gt_findings)
        if findings_gt_total > 0:
            findings_recall = findings_matched / findings_gt_total
        if findings_model_total > 0:
            findings_precision = findings_matched / findings_model_total

    # ---- decision_accuracy -----------------------------------------------------
    decision_accuracy: Optional[float] = None
    decision_matched = decision_total = 0
    if expected is not None and "decision" in (expected.get("components") or []):
        gt_decision = gt.get("decision", {}) or {}
        model_decision = parsed.get("decision") if (not parse_fail and isinstance(parsed, dict)) else None
        decision_matched, decision_total = _match_decision(model_decision, gt_decision)
        if decision_total > 0:
            decision_accuracy = decision_matched / decision_total

    # ---- escape_ok --------------------------------------------------------------
    escape_ok: Optional[int] = None
    if escape_hatch:
        escape_ok = 0 if (parse_fail or not isinstance(parsed, dict)) else _score_escape_hatch(parsed, gt)

    # ---- headline numeric_accuracy (micro-average over present components) ----
    if escape_hatch:
        numeric_accuracy = float(escape_ok) if escape_ok is not None else None
    else:
        total_matched = journal_matched + derived_matched + findings_matched + decision_matched
        total_items = len(gt_journal) + derived_total + findings_gt_total + decision_total
        numeric_accuracy = (total_matched / total_items) if total_items > 0 else None

    # ---- verification_gap (EA oracle) -------------------------------------------
    # Gated off (verification_gap stays None) when: ea_coverage != "ok", the
    # task's contract has no "journal" component, or the model output has no
    # journal-component postings to check.
    verification_gap: Optional[int] = None
    violation_types: list[str] = []
    oracle_verdict: Optional[dict] = None

    oracle_eligible = (
        task.get("ea_coverage", "ok") == "ok"
        and has_journal_component
        and arm_name in oracle_arms
        and worktree_root is not None
        and model_journal is not None
    )
    if oracle_eligible:
        from runner.build import run_oracle
        import json as _json

        ea_postings = _to_ea_postings(model_journal, task, resolver)
        oracle_verdict = run_oracle(_json.dumps(ea_postings), worktree_root)
        if oracle_verdict is not None and oracle_verdict.get("oracle_ok"):
            violation_types = oracle_verdict.get("violation_types", [])
            # category_violation (side-consistency for nominal accounts) is
            # advisory only: closing transfers and consolidation eliminations
            # legitimately post contra-side (credit Purchases, debit Sales) and
            # EA represents them via Hat — its type system does NOT reject
            # them. Counting them as verification_gap overstates what EA
            # guarantees by construction and flagged GT-correct answers in the
            # 5-seed Track R pilot (10/10 gap=1 cells were this false
            # positive). The gap counterfactual is therefore restricted to
            # errors EA truly rejects: unresolvable account, non-positive
            # amount, imbalance.
            structural = [v for v in violation_types if v != "category_violation"]
            verification_gap = 1 if structural else 0

    # ---- convergence (P4 retry loop) -----------------------------------------
    iterations = arm_result.get("iterations")
    converged  = arm_result.get("converged")
    first_pass_valid = arm_result.get("first_pass_valid")
    timed_out = arm_result.get("timed_out", False)
    outcome = _outcome(
        task,
        arm_result,
        parsed,
        scoring_contract,
    )

    return {
        "task_id":                task["id"],
        "arm":                    arm_name,
        "numeric_accuracy":       numeric_accuracy,
        "journal_accuracy":       journal_accuracy,
        "raw_journal_accuracy":   raw_journal_accuracy,
        "derived_accuracy":       derived_accuracy,
        "findings_recall":        findings_recall,
        "findings_precision":     findings_precision,
        "decision_accuracy":      decision_accuracy,
        "escape_ok":              escape_ok,
        "balance_violation":      balance_violation,
        "account_validity":       account_validity,
        "hallucinated_accounts":  hallucinated_accounts,
        "compile_fail":           compile_fail,
        "parse_fail":             parse_fail,
        "first_pass_valid":       first_pass_valid,
        "verification_gap":       verification_gap,
        "violation_types":        violation_types,
        "oracle_verdict":         oracle_verdict,
        "convergence_iterations": iterations,
        "converged":              converged,
        "timed_out":              timed_out,
        "posting_complete":       posting_complete,
        "outcome":                outcome,
    }
