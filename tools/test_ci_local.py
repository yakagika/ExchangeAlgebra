#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = []
# ///
"""Check ci-local's workflow reader with temporary fixtures and the repository workflow.

Run copied readers in subprocesses and check their output and exit status.
"""

from __future__ import annotations

import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path


class CiLocalTests(unittest.TestCase):
    """Exercise supported steps and reject unsupported syntax before execution."""

    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        (self.root / "tools").mkdir()
        (self.root / ".github" / "workflows").mkdir(parents=True)
        self.repository = Path(__file__).resolve().parent.parent
        self.script = self.root / "tools" / "ci-local.py"
        shutil.copyfile(self.repository / "tools" / "ci-local.py", self.script)

    def run_steps(
        self,
        steps: str,
        *,
        list_only: bool = True,
        job_keys: str = "",
        item_indent: int = 6,
    ) -> subprocess.CompletedProcess[str]:
        """Run the copied reader against a fixture containing both default jobs."""
        workflow = (
            "jobs:\n"
            "  build-test:\n"
            "    runs-on: ubuntu-latest\n"
            + textwrap.indent(job_keys, "    ")
            + "    steps:\n"
            + textwrap.indent(textwrap.dedent(steps).strip() + "\n", " " * item_indent)
            + "  build-test-no-visualize:\n"
            "    runs-on: ubuntu-latest\n"
            "    steps:\n"
        )
        (self.root / ".github" / "workflows" / "ci.yml").write_text(workflow, encoding="utf-8")
        return subprocess.run(
            [sys.executable, str(self.script)] + (["--list"] if list_only else []),
            capture_output=True,
            text=True,
            cwd=self.root,
        )

    def test_unnamed_run_steps(self) -> None:
        result = self.run_steps("""
            - run: echo A
            - run: |
                echo B
                echo C
        """)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.count("[build-test] (unnamed step at line "), 2)
        self.assertIn("  $ echo A\n", result.stdout)
        self.assertIn("  $ echo B\necho C\n", result.stdout)

    def test_run_before_name(self) -> None:
        result = self.run_steps("""
            - run: echo A
              name: Inline command
            - run: |
                echo B
                echo C
              name: Block command
        """)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            result.stdout,
            "[build-test] Inline command\n  $ echo A\n"
            "[build-test] Block command\n  $ echo B\necho C\n",
        )

    def test_literal_chomping_indicators(self) -> None:
        for indicator in ("|", "|-", "|+"):
            with self.subTest(indicator=indicator):
                result = self.run_steps(f"""
                    - name: Literal command
                      run: {indicator}
                        echo A
                        echo B
                """)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(
                    result.stdout,
                    "[build-test] Literal command\n  $ echo A\necho B\n",
                )

    def test_unsupported_block_indicators(self) -> None:
        for indicator in (">", ">-", "|2"):
            with self.subTest(indicator=indicator):
                result = self.run_steps(f"""
                    - name: Unsupported block
                      run: {indicator}
                        echo A
                        echo B
                """)
                self.assertEqual(result.returncode, 2, result.stderr)
                self.assertIn("build-test:", result.stderr)
                self.assertIn("line 6", result.stderr)
                self.assertIn("not interpreted", result.stderr)

    def test_plain_scalar_continuation(self) -> None:
        result = self.run_steps("""
            - name: Continued command
              run: echo a
                && echo b
        """)
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertIn("build-test:", result.stderr)
        self.assertIn("line 7", result.stderr)
        self.assertIn("not interpreted", result.stderr)

    def test_step_items_at_steps_indent(self) -> None:
        result = self.run_steps("- run: echo A", item_indent=4)
        self.assertEqual(result.returncode, 2, result.stderr)

    def test_unsupported_job_keys(self) -> None:
        for job_keys in ("if: always()\n", "env:\n  SAMPLE: value\n"):
            with self.subTest(job_keys=job_keys):
                result = self.run_steps("- run: echo A", job_keys=job_keys)
                self.assertEqual(result.returncode, 2, result.stderr)
                self.assertIn("not interpreted", result.stderr)

    def test_rejection_prevents_all_execution(self) -> None:
        marker = self.root / "marker"
        result = self.run_steps(
            f"""
            - run: touch {shlex.quote(str(marker))}
            - name: Unsupported block
              run: >
                echo A
                echo B
            """,
            list_only=False,
        )
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertFalse(marker.exists())
        self.assertEqual(result.stdout, "")

    def test_repository_workflow(self) -> None:
        result = subprocess.run(
            [sys.executable, str(self.repository / "tools" / "ci-local.py"), "--list"],
            capture_output=True,
            text=True,
            cwd=self.repository,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        selected = {"build-test", "build-test-no-visualize"}
        expected = []
        job = None
        name = None
        for line in (
            (self.repository / ".github" / "workflows" / "ci.yml")
            .read_text(encoding="utf-8")
            .splitlines()
        ):
            job_match = re.match(r"^  ([A-Za-z0-9_-]+):$", line)
            if job_match:
                job = job_match.group(1)
                name = None
            if line.startswith("      - "):
                name = None
                if line.startswith("      - name: "):
                    name = line.partition("name: ")[2]
            if (
                job in selected
                and name is not None
                and not name.startswith("Install system deps")
                and line.startswith("        run:")
            ):
                expected.append(f"[{job}] {name}")
        self.assertTrue(expected)
        self.assertEqual(
            [line for line in result.stdout.splitlines() if line.startswith("[")],
            expected,
        )


if __name__ == "__main__":
    unittest.main()
