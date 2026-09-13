from __future__ import annotations

import ast
import json
import subprocess
import sys
from pathlib import Path


EVAL_DIR = Path(__file__).resolve().parents[2]
REPO_ROOT = Path(__file__).resolve().parents[4]
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from gen.accounts import ACCOUNT_DIVISIONS
from runner.arms import _build_user_prompt
from runner.run import run_one
from runner.score import _canon, _match_journal, build_resolver, resolve_account


DATA_DIR = EVAL_DIR / "runner" / "data"
TOOLS_DIR = EVAL_DIR / "runner" / "tools"


def _generated(script: str) -> str:
    return subprocess.run(
        [sys.executable, str(TOOLS_DIR / script)],
        cwd=EVAL_DIR,
        check=True,
        capture_output=True,
        text=True,
    ).stdout


def test_standard_chart_is_current() -> None:
    assert _generated("build_standard_chart.py") == (
        DATA_DIR / "standard-chart-jcci.json"
    ).read_text(encoding="utf-8")


def test_jcci_alias_mirror_is_current() -> None:
    assert _generated("build_jcci_aliases.py") == (
        DATA_DIR / "jcci-aliases.json"
    ).read_text(encoding="utf-8")


def test_standard_chart_covers_generator_accounts_and_excludes_notes() -> None:
    chart = json.loads((DATA_DIR / "standard-chart-jcci.json").read_text(encoding="utf-8"))
    chart_accounts = {row["account"] for row in chart}
    used: set[str] = set()
    for filename in ("templates.py", "kinds.py", "defects.py"):
        tree = ast.parse((EVAL_DIR / "gen" / filename).read_text(encoding="utf-8"))
        used.update(
            node.value
            for node in ast.walk(tree)
            if isinstance(node, ast.Constant)
            and isinstance(node.value, str)
            and node.value in ACCOUNT_DIVISIONS
        )

    assert used <= chart_accounts
    assert all("手形" not in row["name_ja"] for row in chart)
    assert [row["account"] for row in chart] == sorted(chart_accounts)


def test_standard_chart_prompt_block_and_all_arm_plumbing(monkeypatch, tmp_path: Path) -> None:
    task = {
        "id": "chart-test",
        "category": "journalize",
        "prompt": "仕訳せよ。",
        "given": {
            "chart_of_accounts": ["TaskOnlyAccount"],
            "accounts": {"TaskOnlyAccount": "asset"},
        },
        "ground_truth": {"journal": []},
    }
    prompt = _build_user_prompt(task, include_task_chart="standard")
    expected_block = (
        "Standard chart of accounts (JCCI level 2 / 3):\n"
        + (DATA_DIR / "standard-chart-jcci.json").read_text(encoding="utf-8").rstrip("\n")
    )
    assert expected_block in prompt
    assert "TaskOnlyAccount" not in prompt

    seen: dict[str, str] = {}

    class Backend:
        configured_model = "test"
        configured_effort = None
        effective_model = None
        effective_model_source = None
        cli_version = None
        last_call = None
        tool_event_count = 0

    monkeypatch.setattr("runner.run.backend_from_config", lambda _: Backend())
    monkeypatch.setattr(
        "runner.run.score",
        lambda *args, **kwargs: {"infra_missing": False},
    )

    def fake_arm(name: str):
        def call(*args, **kwargs):
            seen[name] = kwargs["include_task_chart"]
            return {"parsed": [], "parse_fail": False}
        return call

    for name in ("C", "V", "B", "A", "Aprime"):
        monkeypatch.setattr(f"runner.run.arm_{name.lower()}", fake_arm(name))
        run_one(
            task=task,
            arm_name=name,
            model_key="test",
            backend_cfg={"model": "test"},
            seed=0,
            dry_run=False,
            max_iters=1,
            oracle_arms=(),
            skill="v1",
            aprime_feedback="raw",
            c_retries=0,
            c_ea_map=False,
            chart_of_accounts="standard",
            v_gate="full",
            scoring_contract="v1",
        )

    assert seen == {name: "standard" for name in ("C", "V", "B", "A", "Aprime")}


def _resolver_for(*accounts: str) -> dict[str, list[str]]:
    return build_resolver(
        {
            "given": {
                "chart_of_accounts": list(accounts),
                "ea_account_map": {account: account for account in accounts},
            },
            "ground_truth": {"journal": []},
        }
    )


def test_japanese_and_existing_english_account_aliases() -> None:
    resolver = _resolver_for(
        "Cash",
        "AccountsReceivable",
        "AccumulatedDepreciation",
        "AccruedExpenses",
        "WageExpenditure",
        "CostOfGoodsSold",
    )
    expected = {
        "現金": "Cash",
        "売掛金": "AccountsReceivable",
        "減価償却累計額": "AccumulatedDepreciation",
        "備品減価償却累計額": "AccumulatedDepreciation",
        "未払費用": "AccruedExpenses",
        "未払利息": "AccruedExpenses",
        "給料手当": "WageExpenditure",
        "ar": "AccountsReceivable",
        "cogs": "CostOfGoodsSold",
    }
    for alias, account in expected.items():
        assert resolve_account(alias, resolver) == [account]

    assert _canon("  現金  ") == "cash"
    assert _canon("ＡＲ") == "accountsreceivable"


def test_goods_is_not_merchandise_inventory_alias() -> None:
    resolver = _resolver_for("MerchandiseInventory")
    assert "MerchandiseInventory" not in resolve_account("商品", resolver)


def test_ambiguous_japanese_alias_keeps_candidates_for_amount_matching() -> None:
    accounts = ("Deposits", "TimeDeposits", "DepositsInSpecialAccounts")
    resolver = _resolver_for(*accounts)
    assert resolve_account("銀行預金", resolver) == list(accounts)

    ground_truth = [
        {"side": "debit", "account": account, "amount": amount}
        for account, amount in zip(accounts, (100, 200, 300))
    ]
    model = [
        {"side": "debit", "account": "銀行預金", "amount": amount}
        for amount in (300, 100, 200)
    ]
    assert _match_journal(model, ground_truth, resolver) == 3
