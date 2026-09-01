#!/usr/bin/env python3
"""Replay frozen A-prime submissions through two LoadChecked executables.

This tool is deliberately read-only with respect to its inputs.  It verifies
the frozen JSONL hashes, reconstructs the final model journal from raw_output,
and writes a separate versioned replay bundle.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import platform
import re
import subprocess
import sys
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any


HERE = Path(__file__).resolve()
AUDIT_EVAL = HERE.parents[1]
sys.path.insert(0, str(AUDIT_EVAL))

from runner.arms import (  # noqa: E402
    _derived_for_contract,
    _extract_json,
    _task_derivable,
    _truncate_verdict,
)
from runner.run import task_bundle_digest  # noqa: E402
from runner.score import score  # noqa: E402


EXPECTED_JSONL_HASHES = {
    "20260817_confirmatory_codex.jsonl": "429a48192ca4ebe8facbee4eb3df8c40fc62686e73b0da4569393cba0637df3c",
    "20260820_confirmatory_qwen.jsonl": "3e38575a54599562ba93f4e087e95905c67b57178ffe56c2af3f3f88a3552b40",
    "20260821_confirmatory_qwen_part2.jsonl": "0c8663936003ab66be4c6aaee860c99c77401db6e6fc4dc27f20fde9abce6a95",
    "20260824_confirmatory_qwen_part3.jsonl": "320551b8346d2ec1596def629ee670f33a9117a481722e3838cd4bc91165ecb4",
    "20260825_confirmatory_qwen_part4.jsonl": "a8f00c7f513e7bf9cc81aefce60ccee5b49fda5ed664777c23ea02dee7fb4a58",
    "20260825_confirmatory_qwen_part5.jsonl": "b789136f1cb0e64b42d5195750c5c35975c8631d2bc3b6300d13ee6a08fb9545",
    "20260826_confirmatory_qwen_part6.jsonl": "46be2757a5972741a3c1ff6c65fd77cf444263267557cacf39d93afa4d23fd4f",
    "20260827_confirmatory_qwen_part7.jsonl": "42969fcb63a78f05f528ba27db223f6f990b056927cc261a1b588d84c99d0d81",
}

EXPECTED_META_HASHES = {
    "20260817_confirmatory_codex.meta.json": "e9bdb76a46afd70576d2efdec87834a0f99245560662164c513f00b60ccd0672",
    "20260820_confirmatory_qwen.meta.json": "85b40f1a06e076ad910db0e51e0e15634284b8ad138be621c0bc6b99334c38f1",
    "20260821_confirmatory_qwen_part2.meta.json": "aad05578464b6457159f801c839b572989942c68640f5f027e4ae12174f5a027",
    "20260824_confirmatory_qwen_part3.meta.json": "779a2ca37e47eb24a1cfd93c39d75aaa5b559357bcc341c8b61b8f541bbe2bc1",
    "20260825_confirmatory_qwen_part4.meta.json": "a8a2f0f6bffe1b968fbcfa120991510b9d38a7f659349f097451a0665cda3c31",
    "20260825_confirmatory_qwen_part5.meta.json": "f5af59fba3d301ae5687c2c4e1237c955b829c5188520bc2c63dddfcd40a55e9",
    "20260826_confirmatory_qwen_part6.meta.json": "b9e9ee68d751d3e048f3697414d9b5eced1993fbdfff2bf638837b54731ffa9f",
    "20260827_confirmatory_qwen_part7.meta.json": "eed9f8d78a34564d0da40e18de18cc672192ba06651b4ba9a2ab365325c0b64c",
}

EXPECTED_TASK_BUNDLE_SHA256 = "7e34eed1a3ee77e7f98981eee11abccd686f93024c7db3edab1dafa099a6aa2f"
EXPECTED_GENERATOR_MANIFEST_SHA256 = "98c38f0235babbe81fe6a68a8d055a2b2cf9df1c3b268727cfbd1a3b3b827526"
SCORING_CONTRACT = "v1"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def command_output(command: list[str], cwd: Path | None = None) -> str:
    result = subprocess.run(command, cwd=cwd, text=True, capture_output=True, check=True)
    return result.stdout.strip()


def gate(executable: Path, payload: dict[str, Any]) -> dict[str, Any]:
    result = subprocess.run(
        [str(executable)],
        input=json.dumps(payload, ensure_ascii=False),
        text=True,
        capture_output=True,
        timeout=30,
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"gate failed ({executable}, exit={result.returncode}): {result.stderr.strip()}"
        )
    try:
        verdict = json.loads(result.stdout)
    except json.JSONDecodeError as exc:
        raise RuntimeError(f"gate emitted invalid JSON ({executable})") from exc
    if not isinstance(verdict, dict) or not isinstance(verdict.get("ok"), bool):
        raise RuntimeError(f"gate emitted an invalid verdict ({executable})")
    return verdict


def sources(task: dict[str, Any]) -> list[dict[str, Any]]:
    transactions = (task.get("given") or {}).get("transactions")
    if not isinstance(transactions, list) or not transactions:
        raise ValueError(f"task {task.get('id')} has no source transactions")
    result = []
    for transaction in transactions:
        amount = transaction.get("amount") if isinstance(transaction, dict) else None
        if (
            not isinstance(transaction, dict)
            or "id" not in transaction
            or isinstance(amount, bool)
            or not isinstance(amount, (int, float))
        ):
            raise ValueError(f"task {task.get('id')} has an invalid source transaction")
        result.append({"id": transaction["id"], "amount": amount})
    return result


def final_parsed(record: dict[str, Any]) -> Any:
    raw = record.get("raw_output")
    if not isinstance(raw, str):
        raise ValueError("top-level raw_output is missing")
    extracted = _extract_json(raw, prefer_object=True)
    if extracted is None:
        raise ValueError("final raw_output has no parseable JSON")
    return json.loads(extracted)


def final_journal(record: dict[str, Any]) -> list[dict[str, Any]]:
    parsed = final_parsed(record)
    journal = parsed.get("journal") if isinstance(parsed, dict) else parsed
    if not isinstance(journal, list):
        raise ValueError("final raw_output has no journal array")
    return journal


def constructor_name(error: Any, fallback: str) -> str:
    match = re.match(r"([A-Za-z][A-Za-z0-9_']*)", str(error))
    return match.group(1) if match else fallback


def error_fingerprint(verdict: dict[str, Any]) -> dict[str, Any]:
    entries = []
    for block in verdict.get("entry_errors", []) or []:
        if not isinstance(block, dict):
            continue
        entries.append(
            {
                "txid": str(block.get("txid", "")),
                "classes": sorted(
                    constructor_name(error, "UnparsedEntryError")
                    for error in (block.get("errors", []) or [])
                ),
            }
        )
    return {
        "entries": sorted(entries, key=lambda item: (item["txid"], item["classes"])),
        "sources": sorted(
            constructor_name(error, "SourceError")
            for error in (verdict.get("source_errors", []) or [])
        ),
        # LoadChecked exposes input-error prose, not stable constructors. Keep
        # only multiplicity at this layer; raw/rich changes are recorded
        # independently as feedback_changed.
        "inputs": ["InputError"] * len(verdict.get("input_errors", []) or []),
    }


def error_classes(verdict: dict[str, Any]) -> list[str]:
    fingerprint = error_fingerprint(verdict)
    classes = [name for entry in fingerprint["entries"] for name in entry["classes"]]
    classes.extend(fingerprint["sources"])
    classes.extend(fingerprint["inputs"])
    return sorted(set(classes))


def comparison(old: dict[str, Any], new: dict[str, Any]) -> str:
    if old["ok"] and not new["ok"]:
        return "accept_to_reject"
    if not old["ok"] and new["ok"]:
        return "reject_to_accept"
    if error_fingerprint(old) != error_fingerprint(new):
        return "error_class_only"
    if old["ok"] and old.get("journal") != new.get("journal"):
        return "canonical_change"
    return "same"


def outcome(converged: bool, metrics: dict[str, Any] | None) -> str:
    if not converged:
        return "refused"
    accuracy = None if metrics is None else metrics.get("numeric_accuracy")
    return "correct" if accuracy == 1.0 else "unsafe"


def derive(executable: Path, journal: list[dict[str, Any]]) -> dict[str, Any]:
    result = subprocess.run(
        [str(executable)],
        input=json.dumps(journal, ensure_ascii=False),
        text=True,
        capture_output=True,
        timeout=30,
    )
    if result.returncode != 0:
        raise RuntimeError(f"DeriveEA failed: {result.stderr.strip()}")
    parsed = json.loads(result.stdout)
    derived = parsed.get("derived") if isinstance(parsed, dict) else None
    if not isinstance(derived, dict):
        raise RuntimeError("DeriveEA emitted no derived map")
    return _derived_for_contract(derived, SCORING_CONTRACT)


def counterfactual(
    record: dict[str, Any],
    task: dict[str, Any],
    verdict: dict[str, Any],
    derive_executable: Path,
) -> tuple[str, dict[str, Any] | None]:
    if not verdict["ok"]:
        return "refused", None
    canonical = verdict.get("journal")
    if not isinstance(canonical, list):
        raise ValueError("accepted verdict has no canonical journal")
    arm_result = copy.deepcopy(record["arm_result"])
    parsed = final_parsed(record)
    if isinstance(parsed, dict):
        parsed["journal"] = canonical
        if _task_derivable(task):
            parsed["derived"] = derive(derive_executable, canonical)
        arm_result["parsed"] = parsed
    elif isinstance(parsed, list):
        arm_result["parsed"] = canonical
    else:
        raise ValueError("historical accepted result has no parsed output")
    arm_result["converged"] = True
    arm_result["parse_fail"] = False
    metrics = score(
        task,
        arm_result,
        "Aprime",
        worktree_root=None,
        oracle_arms=(),
        scoring_contract=SCORING_CONTRACT,
    )
    return outcome(True, metrics), metrics


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--metrics-dir", type=Path, required=True)
    parser.add_argument("--tasks-dir", type=Path, required=True)
    parser.add_argument("--old-root", type=Path, required=True)
    parser.add_argument("--new-root", type=Path, required=True)
    parser.add_argument("--old-gate", type=Path, required=True)
    parser.add_argument("--new-gate", type=Path, required=True)
    parser.add_argument("--new-derive", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.output_dir.exists():
        raise SystemExit(f"refusing to overwrite existing output: {args.output_dir}")
    for executable in (args.old_gate, args.new_gate, args.new_derive):
        if not executable.is_file():
            raise SystemExit(f"required executable not found: {executable}")

    raw_paths = [args.metrics_dir / name for name in EXPECTED_JSONL_HASHES]
    raw_hashes = {}
    for path in raw_paths:
        actual = sha256(path)
        expected = EXPECTED_JSONL_HASHES[path.name]
        if actual != expected:
            raise SystemExit(f"frozen hash mismatch: {path.name}: {actual} != {expected}")
        raw_hashes[path.name] = actual
    meta_paths = sorted(args.metrics_dir.glob("202608*_confirmatory*.meta.json"))
    meta_hashes = {path.name: sha256(path) for path in meta_paths}
    for name, expected in EXPECTED_META_HASHES.items():
        actual = meta_hashes.get(name)
        if actual != expected:
            raise SystemExit(f"frozen meta hash mismatch: {name}: {actual} != {expected}")
    if set(meta_hashes) != set(EXPECTED_META_HASHES):
        raise SystemExit(f"unexpected confirmatory meta files: {sorted(meta_hashes)}")

    generator_manifest = args.tasks_dir / "manifest.json"
    if sha256(generator_manifest) != EXPECTED_GENERATOR_MANIFEST_SHA256:
        raise SystemExit("frozen generator manifest hash mismatch")

    records = []
    all_record_count = 0
    for path in raw_paths:
        with path.open(encoding="utf-8") as handle:
            for line_number, line in enumerate(handle, 1):
                all_record_count += 1
                record = json.loads(line)
                if record.get("arm") == "Aprime":
                    records.append((path.name, line_number, record))
    if all_record_count != 1800 or len(records) != 360:
        raise SystemExit(f"unexpected completeness: all={all_record_count}, Aprime={len(records)}")

    task_ids = sorted({record["task_id"] for _, _, record in records})
    bundle_digest = task_bundle_digest(args.tasks_dir, task_ids)
    if bundle_digest != EXPECTED_TASK_BUNDLE_SHA256:
        raise SystemExit(f"frozen task bundle mismatch: {bundle_digest}")
    task_hashes = {
        task_id: sha256(args.tasks_dir / f"{task_id}.json") for task_id in task_ids
    }

    # Fail on environment/provenance collection before starting the expensive
    # replay. describe --dirty makes an uncommitted checkout visible.
    old_commit = command_output(["git", "rev-parse", "HEAD"], args.old_root)
    new_commit = command_output(["git", "rev-parse", "HEAD"], args.new_root)
    old_describe = command_output(["git", "describe", "--dirty", "--always"], args.old_root)
    new_describe = command_output(["git", "describe", "--dirty", "--always"], args.new_root)
    implementation_commit = command_output(["git", "rev-parse", "HEAD"], HERE.parents[3])
    implementation_describe = command_output(
        ["git", "describe", "--dirty", "--always"], HERE.parents[3]
    )
    implementation_status = command_output(
        ["git", "status", "--porcelain", "--untracked-files=all"], HERE.parents[3]
    )
    ghc_version = command_output(
        [
            "stack", "--stack-yaml", str(args.new_root / "stack.yaml"),
            "exec", "ghc", "--", "--numeric-version",
        ],
        args.new_root,
    )
    stack_version = command_output(["stack", "--numeric-version"])
    old_ghc_version = command_output(
        [
            "stack", "--stack-yaml", str(args.old_root / "stack.yaml"),
            "exec", "ghc", "--", "--numeric-version",
        ],
        args.old_root,
    )
    old_gate_hash = sha256(args.old_gate)
    new_gate_hash = sha256(args.new_gate)
    new_derive_hash = sha256(args.new_derive)
    meta_lineage = {}
    for path in meta_paths:
        meta = json.loads(path.read_text(encoding="utf-8"))
        meta_lineage[path.name] = meta.get("git")
        recorded_bundle = meta.get("task_bundle_sha256")
        if recorded_bundle is not None and recorded_bundle != bundle_digest:
            raise SystemExit(f"meta task-bundle lineage mismatch: {path.name}")

    # Reproduce every historical final verdict before invoking the new gate.
    # This is a hard compatibility preflight, not a post-hoc assertion.
    legacy_cache: dict[tuple[str, int], dict[str, Any]] = {}
    historical_checks = 0
    historical_canonical_checks = 0
    historical_full_checks = 0
    historical_prefix_checks = 0
    for source_file, line_number, record in records:
        task = json.loads(
            (args.tasks_dir / f"{record['task_id']}.json").read_text(encoding="utf-8")
        )
        payload = {"postings": final_journal(record), "sources": sources(task)}
        old_verdict = gate(args.old_gate, payload)
        stored = record["arm_result"].get("loadchecked_verdict")
        stored_match = stored == _truncate_verdict(old_verdict)
        historical_checks += int(stored_match)
        canonical_match = None
        if old_verdict["ok"]:
            historical_parsed = record["arm_result"].get("parsed")
            historical_journal = (
                historical_parsed.get("journal")
                if isinstance(historical_parsed, dict)
                else historical_parsed
            )
            canonical_match = old_verdict.get("journal") == historical_journal
            historical_canonical_checks += int(canonical_match)
            check_kind = "accepted_canonical_exact"
        elif isinstance(stored, str) and stored.endswith("…[truncated]"):
            historical_prefix_checks += int(stored_match)
            check_kind = "rejected_prefix_only"
        else:
            historical_full_checks += int(stored_match)
            check_kind = "rejected_verdict_exact"
        legacy_cache[(source_file, line_number)] = {
            "verdict": old_verdict,
            "stored_match": stored_match,
            "canonical_match": canonical_match,
            "check_kind": check_kind,
        }
    if historical_checks != 360:
        raise SystemExit(f"legacy verdict reproduction failed: {historical_checks}/360")
    if historical_canonical_checks + historical_full_checks + historical_prefix_checks != 360:
        raise SystemExit("legacy exact/prefix checks did not cover all final records")

    args.output_dir.mkdir(parents=True)
    replay_path = args.output_dir / "replay.jsonl"
    counts: Counter[str] = Counter()
    by_model: dict[str, Counter[str]] = defaultdict(Counter)
    outcome_counts: dict[str, Counter[str]] = defaultdict(Counter)
    control_outcome_checks = 0
    first_count = 0
    first_equals_final = 0
    missing_first = []
    feedback_changes: Counter[str] = Counter()
    changed_records = []

    with replay_path.open("w", encoding="utf-8") as output:
        for source_file, line_number, record in records:
            task_path = args.tasks_dir / f"{record['task_id']}.json"
            task = json.loads(task_path.read_text(encoding="utf-8"))
            model = "codex" if "codex" in source_file else "qwen"
            final = final_journal(record)
            submissions = [("final", final)]
            first = record["arm_result"].get("raw_first_journal")
            if isinstance(first, list):
                submissions.insert(0, ("first", first))
                first_count += 1
                if first == final:
                    first_equals_final += 1
            else:
                missing_first.append(
                    {
                        "task_id": record["task_id"],
                        "model": model,
                        "repeat": record.get("repeat"),
                        "source_file": source_file,
                        "source_line": line_number,
                    }
                )

            historical_outcome = outcome(
                bool(record["arm_result"].get("converged")), record.get("metrics")
            )
            control_metrics = score(
                task,
                copy.deepcopy(record["arm_result"]),
                "Aprime",
                worktree_root=None,
                oracle_arms=(),
                scoring_contract=SCORING_CONTRACT,
            )
            control_outcome = outcome(
                bool(record["arm_result"].get("converged")), control_metrics
            )
            control_outcome_checks += int(control_outcome == historical_outcome)
            for which, postings in submissions:
                payload = {"postings": postings, "sources": sources(task)}
                old_verdict = (
                    legacy_cache[(source_file, line_number)]["verdict"]
                    if which == "final"
                    else gate(args.old_gate, payload)
                )
                new_verdict = gate(args.new_gate, payload)
                category = comparison(old_verdict, new_verdict)
                counts[f"{which}:{category}"] += 1
                by_model[model][f"{which}:{category}"] += 1
                feedback_changed = any(
                    old_verdict.get(key) != new_verdict.get(key) for key in ("raw", "rich")
                )
                feedback_changes[f"{which}:{str(feedback_changed).lower()}"] += 1

                historical_match = None
                historical_canonical_match = None
                legacy_check_kind = None
                new_outcome = None
                new_metrics = None
                if which == "final":
                    legacy = legacy_cache[(source_file, line_number)]
                    historical_match = legacy["stored_match"]
                    historical_canonical_match = legacy["canonical_match"]
                    legacy_check_kind = legacy["check_kind"]
                    new_outcome, new_metrics = counterfactual(
                        record, task, new_verdict, args.new_derive
                    )
                    outcome_counts[model][f"historical:{historical_outcome}"] += 1
                    outcome_counts[model][f"new:{new_outcome}"] += 1

                result = {
                    "source_file": source_file,
                    "source_line": line_number,
                    "source_sha256": raw_hashes[source_file],
                    "task_id": record["task_id"],
                    "seed": record.get("seed"),
                    "repeat": record.get("repeat"),
                    "model": model,
                    "which": which,
                    "old_ok": old_verdict["ok"],
                    "new_ok": new_verdict["ok"],
                    "old_error_classes": error_classes(old_verdict),
                    "new_error_classes": error_classes(new_verdict),
                    "old_error_fingerprint": error_fingerprint(old_verdict),
                    "new_error_fingerprint": error_fingerprint(new_verdict),
                    "difference": category,
                    "feedback_changed": feedback_changed,
                    "historical_verdict_match": historical_match,
                    "historical_canonical_match": historical_canonical_match,
                    "legacy_check_kind": legacy_check_kind,
                    "historical_outcome": historical_outcome if which == "final" else None,
                    "control_outcome": control_outcome if which == "final" else None,
                    "new_outcome": new_outcome,
                    "new_numeric_accuracy": None if new_metrics is None else new_metrics.get("numeric_accuracy"),
                }
                output.write(json.dumps(result, ensure_ascii=False, sort_keys=True) + "\n")
                if category != "same" or feedback_changed:
                    changed_records.append(result)

    if first_count != 358:
        raise SystemExit(f"unexpected first submission count: {first_count} != 358")
    if control_outcome_checks != 360:
        raise SystemExit(f"current-scorer control failed: {control_outcome_checks}/360")
    final_raw_hashes = {path.name: sha256(path) for path in raw_paths}
    final_meta_hashes = {path.name: sha256(path) for path in meta_paths}
    final_task_hashes = {
        task_id: sha256(args.tasks_dir / f"{task_id}.json") for task_id in task_ids
    }
    if (
        final_raw_hashes != raw_hashes
        or final_meta_hashes != meta_hashes
        or final_task_hashes != task_hashes
        or sha256(generator_manifest) != EXPECTED_GENERATOR_MANIFEST_SHA256
        or sha256(args.old_gate) != old_gate_hash
        or sha256(args.new_gate) != new_gate_hash
        or sha256(args.new_derive) != new_derive_hash
    ):
        raise SystemExit("a frozen input changed during replay")

    metadata = {
        "schema_version": 1,
        "scoring_contract": SCORING_CONTRACT,
        "implementation_commit": implementation_commit,
        "implementation_describe": implementation_describe,
        "implementation_status_porcelain": implementation_status,
        "implementation_source_sha256": sha256(HERE),
        "implementation_readme_sha256": sha256(HERE.with_name("README.md")),
        "old_gate_commit": old_commit,
        "old_gate_describe": old_describe,
        "new_gate_commit": new_commit,
        "new_gate_describe": new_describe,
        "historical_run_git_lineage": meta_lineage,
        "python": platform.python_version(),
        "platform": platform.platform(),
        "ghc": ghc_version,
        "old_ghc": old_ghc_version,
        "stack": stack_version,
        "raw_sha256": raw_hashes,
        "meta_sha256": meta_hashes,
        "task_bundle_sha256": bundle_digest,
        "task_sha256": task_hashes,
        "generator_manifest_sha256": sha256(generator_manifest),
        "old_gate_executable_sha256": old_gate_hash,
        "new_gate_executable_sha256": new_gate_hash,
        "new_derive_executable_sha256": new_derive_hash,
        "all_records": all_record_count,
        "aprime_records": len(records),
        "first_submissions": first_count,
        "final_submissions": len(records),
        "first_equals_final": first_equals_final,
        "distinct_submissions_within_run": len(records) + first_count - first_equals_final,
        "scope": "per-submission gate compatibility; no model regeneration",
        "trajectory_caveat": "feedback changed in 9 historical runs, including one run that historically recovered to correct",
    }
    summary = {
        "difference_counts": dict(sorted(counts.items())),
        "difference_counts_by_model": {k: dict(sorted(v.items())) for k, v in sorted(by_model.items())},
        "outcome_counts_by_model": {k: dict(sorted(v.items())) for k, v in sorted(outcome_counts.items())},
        "legacy_verdict_matches": historical_checks,
        "legacy_accepted_canonical_exact": historical_canonical_checks,
        "legacy_rejected_verdict_exact": historical_full_checks,
        "legacy_rejected_prefix_only": historical_prefix_checks,
        "current_scorer_control_outcome_matches": control_outcome_checks,
        "feedback_change_counts": dict(sorted(feedback_changes.items())),
        "first_equals_final": first_equals_final,
        "distinct_submissions_within_run": len(records) + first_count - first_equals_final,
        "missing_first_submissions": missing_first,
        "changed_run_count": len(
            {(row["source_file"], row["source_line"]) for row in changed_records}
        ),
        "changed_task_count": len({row["task_id"] for row in changed_records}),
        "changed_records": changed_records,
    }
    (args.output_dir / "replay.meta.json").write_text(
        json.dumps(metadata, indent=2, ensure_ascii=False, sort_keys=True) + "\n", encoding="utf-8"
    )
    (args.output_dir / "summary.json").write_text(
        json.dumps(summary, indent=2, ensure_ascii=False, sort_keys=True) + "\n", encoding="utf-8"
    )
    print(json.dumps(summary, indent=2, ensure_ascii=False, sort_keys=True))


if __name__ == "__main__":
    main()
