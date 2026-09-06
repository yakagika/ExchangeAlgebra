import csv
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


RCR_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(RCR_DIR))
import make_figures  # noqa: E402


class MakeFiguresTest(unittest.TestCase):
    def test_record_calculation_keeps_75_checks(self):
        values = make_figures.calculate(policy="record")
        self.assertEqual(len(values[-1]), 75)

    def test_platform_data_passes_all_replication_criteria(self):
        criteria = make_figures.assess(
            make_figures.calculate(policy="replicate")
        )
        self.assertEqual(len(criteria), 7)
        self.assertEqual([criterion[1] for criterion in criteria], ["PASS"] * 7)

    def test_smoke_and_incomplete_inputs_are_not_assessed(self):
        with make_figures.RAW_TSV.open(newline="") as source:
            reader = csv.DictReader(source, delimiter="\t")
            fieldnames = reader.fieldnames
            selected = [
                row for row in reader
                if (row["series"] == "scaling"
                    and row["config"] == "N200-K20-T50-seq-double")
                or (row["series"] == "parallel"
                    and row["config"] == "N1000-seq")
                or (row["series"] == "parallel"
                    and row["config"] == "N1000-par16"
                    and row["cores"] == "1")
            ]
        with tempfile.TemporaryDirectory() as temporary_directory:
            raw_path = Path(temporary_directory) / "smoke.tsv"
            with raw_path.open("w", newline="") as target:
                writer = csv.DictWriter(target, fieldnames=fieldnames, delimiter="\t")
                writer.writeheader()
                writer.writerows(selected)
            values = make_figures.calculate(
                raw_path=raw_path,
                dense_raw_path=Path(temporary_directory) / "missing-dense.tsv",
                policy="replicate",
            )
            smoke = make_figures.assess(values, smoke=True)
            incomplete = make_figures.assess(values)
        self.assertEqual([criterion[1] for criterion in smoke], ["NOT-ASSESSED"] * 7)
        self.assertTrue(all(criterion[1] == "NOT-ASSESSED" for criterion in incomplete))

    def test_status_column_controls_clean_filter(self):
        fieldnames = ["series", "config", "cores", "rep", "elapsed", "maxres", "status"]
        raw_rows = [
            {
                "series": "heavy",
                "config": "N1000-heavy-par16",
                "cores": "4",
                "rep": "1",
                "elapsed": "5.0",
                "maxres": "100",
                "status": "timeout",
            },
            {
                "series": "heavy",
                "config": "N1000-heavy-par16",
                "cores": "4",
                "rep": "2",
                "elapsed": "NA",
                "maxres": "100",
                "status": "ok",
            },
            {
                "series": "heavy",
                "config": "N1000-heavy-par16",
                "cores": "4",
                "rep": "3",
                "elapsed": "4.0",
                "maxres": "100",
                "status": "ok",
            },
        ]
        with tempfile.TemporaryDirectory() as temporary_directory:
            raw_path = Path(temporary_directory) / "status.tsv"
            with raw_path.open("w", newline="") as target:
                writer = csv.DictWriter(target, fieldnames=fieldnames, delimiter="\t")
                writer.writeheader()
                writer.writerows(raw_rows)
            loaded = make_figures.rows(raw_path)
            clean = make_figures._measurements(
                {"data": loaded}, "heavy", "N1000-heavy-par16", 4, clean=True
            )
        self.assertEqual(clean, [4.0])
        self.assertFalse(loaded[0]["_clean"])
        self.assertFalse(loaded[1]["_clean"])

    def test_table_cli_retains_final_line(self):
        completed = subprocess.run(
            [sys.executable, str(RCR_DIR / "make_figures.py"), "--table"],
            check=True,
            capture_output=True,
            text=True,
        )
        self.assertEqual(completed.stdout.splitlines()[-1], "all 75 checks passed")


if __name__ == "__main__":
    unittest.main()
