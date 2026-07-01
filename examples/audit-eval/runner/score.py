"""
runner/score.py — GT comparison and per-run metrics for audit-eval.

Metrics
-------
numeric_accuracy   : fraction of GT postings exactly matched (canonical account + side + amount)
balance_violation  : True if Σdebit ≠ Σcredit in the model output
account_validity   : True if ALL output accounts resolve via chart / ea_account_map / synonyms
compile_fail       : True if arm A/B build or execution failed (passed through from arm result)
parse_fail         : True if model output could not be parsed to canonical JSON
verification_gap   : (arm B/C, EA oracle) 1 if output contains an error that EA would
                     structurally reject (imbalance / account outside EA AccountTitles /
                     category violation / non-positive amount), else 0. None when the
                     oracle was not applied or the output was unparseable.
convergence_iterations : attempts needed until a structurally-valid output (P4 retry
                     loop). Equals max_iters with converged=False when the loop was
                     exhausted without success.

Account-name resolution (2026-07-02, spec: docs/t3-task-schema.md
「勘定名の EA canonical 対応」)
-----------------------------------------------------------------
GT names (US-GAAP textbook English) and EA AccountTitles (日商簿記系 canonical) are
systematically divergent. Scoring is mapping-aware: for each chart account we accept
  - the GT name itself,
  - its `ea_account_map` target (EA canonical name),
  - normalization-dictionary synonyms (case-insensitive, A/R → AccountsReceivable, …).
A hallucinated account is ONLY a name that resolves through none of these.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Optional


# ---------------------------------------------------------------------------
# Normalization dictionary (synonyms → normalized key of the canonical name)
# ---------------------------------------------------------------------------

def _norm_key(name: str) -> str:
    """Lower-case and strip separators so 'Accounts Receivable' == 'AccountsReceivable'."""
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
# Task-specific resolver: alias (normalized) → canonical GT account name
# ---------------------------------------------------------------------------

def build_resolver(task: dict) -> dict[str, str]:
    """
    Build a resolution table for one task.

    Every chart-of-accounts entry contributes:
      GT name, its ea_account_map target, and their synonym expansions,
    all mapping to the GT name (the canonical scoring identity).
    GT-journal accounts not in the chart are added defensively.
    """
    given = task.get("given", {})
    chart: list[str] = given.get("chart_of_accounts", []) or []
    ea_map: dict[str, str] = given.get("ea_account_map", {}) or {}

    resolver: dict[str, str] = {}

    def register(alias: str, canonical: str) -> None:
        for key in {_norm_key(alias), _canon(alias)}:
            resolver.setdefault(key, canonical)

    for gt_name in chart:
        register(gt_name, gt_name)
        mapped = ea_map.get(gt_name)
        if mapped:
            register(mapped, gt_name)

    # Safety: GT journal may reference accounts missing from the chart.
    for p in task.get("ground_truth", {}).get("journal", []):
        acct = str(p.get("account", "")).strip()
        if acct:
            register(acct, acct)
            mapped = ea_map.get(acct)
            if mapped:
                register(mapped, acct)

    return resolver


def resolve_account(name: str, resolver: dict[str, str]) -> Optional[str]:
    """Resolve an output account name to its canonical GT name, or None."""
    for key in (_norm_key(name), _canon(name)):
        if key in resolver:
            return resolver[key]
    return None


# ---------------------------------------------------------------------------
# Posting normalization
# ---------------------------------------------------------------------------

def _normalize_posting(p: dict, resolver: dict[str, str]) -> tuple[str, Optional[str], float]:
    """Return (side, canonical_account_or_None, amount) from a posting dict."""
    return (
        str(p.get("side", "")).lower().strip(),
        resolve_account(str(p.get("account", "")).strip(), resolver),
        float(p.get("amount", 0)),
    )


def _to_ea_postings(parsed: list, task: dict, resolver: dict[str, str]) -> list[dict]:
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
    """
    ea_map: dict[str, str] = (task.get("given", {}) or {}).get("ea_account_map", {}) or {}
    out = []
    for p in parsed:
        acct = str(p.get("account", "")).strip()
        canonical = resolve_account(acct, resolver)
        if canonical is not None:
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
# Main scoring function
# ---------------------------------------------------------------------------

def score(
    task: dict,
    arm_result: dict,
    arm_name: str,
    worktree_root: Optional[Path] = None,
    oracle_arms: tuple[str, ...] = ("B", "C"),
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
    parsed       = arm_result.get("parsed")        # list of posting dicts or None

    resolver = build_resolver(task)

    # ---- balance_violation --------------------------------------------------
    balance_violation = False
    if not parse_fail and isinstance(parsed, list):
        debit_total  = sum(float(p.get("amount", 0)) for p in parsed
                           if str(p.get("side", "")).lower() == "debit")
        credit_total = sum(float(p.get("amount", 0)) for p in parsed
                           if str(p.get("side", "")).lower() == "credit")
        balance_violation = abs(debit_total - credit_total) > 0.01

    # ---- account_validity (mapping-aware) -----------------------------------
    account_validity = True
    hallucinated_accounts: list[str] = []

    if not parse_fail and isinstance(parsed, list) and resolver:
        for p in parsed:
            acct = str(p.get("account", "")).strip()
            if resolve_account(acct, resolver) is None:
                account_validity = False
                hallucinated_accounts.append(acct)

    # ---- numeric_accuracy (mapping-aware) ------------------------------------
    gt_journal = task.get("ground_truth", {}).get("journal", [])
    gt_postings = [_normalize_posting(p, resolver) for p in gt_journal]
    matched = 0
    numeric_accuracy: Optional[float] = None

    if not parse_fail and isinstance(parsed, list) and gt_postings:
        output_postings = [_normalize_posting(p, resolver) for p in parsed]
        remaining_gt = list(gt_postings)
        for op in output_postings:
            if op[1] is not None and op in remaining_gt:
                matched += 1
                remaining_gt.remove(op)
        numeric_accuracy = matched / len(gt_postings)

    # ---- verification_gap (EA oracle, arm B/C) -------------------------------
    verification_gap: Optional[int] = None
    violation_types: list[str] = []
    oracle_verdict: Optional[dict] = None

    if (
        arm_name in oracle_arms
        and worktree_root is not None
        and not parse_fail
        and isinstance(parsed, list)
    ):
        from runner.build import run_oracle
        import json as _json

        ea_postings = _to_ea_postings(parsed, task, resolver)
        oracle_verdict = run_oracle(_json.dumps(ea_postings), worktree_root)
        if oracle_verdict is not None and oracle_verdict.get("oracle_ok"):
            verification_gap = 1 if oracle_verdict.get("verification_gap") else 0
            violation_types = oracle_verdict.get("violation_types", [])

    # ---- convergence (P4 retry loop) -----------------------------------------
    iterations = arm_result.get("iterations")
    converged  = arm_result.get("converged")

    return {
        "task_id":                task["id"],
        "arm":                    arm_name,
        "numeric_accuracy":       numeric_accuracy,
        "balance_violation":      balance_violation,
        "account_validity":       account_validity,
        "hallucinated_accounts":  hallucinated_accounts,
        "compile_fail":           compile_fail,
        "parse_fail":             parse_fail,
        "verification_gap":       verification_gap,
        "violation_types":        violation_types,
        "oracle_verdict":         oracle_verdict,
        "convergence_iterations": iterations,
        "converged":              converged,
    }
