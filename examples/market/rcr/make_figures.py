"""Generate and verify the experiment figures used by the TOMACS manuscript.

Canonical copy: ExchangeAlgebra `examples/market/rcr/make_figures.py` (the RCR
artifact package). The paper repository vendors this file under `LaTeX/fig/`.

Run `uv run --no-project --with matplotlib python make_figures.py` to generate
figures, or `python3 make_figures.py --table` (stdlib only) to check tables and
manuscript numbers. All values are calculated from
data/round5-overnight/overnight-raw.tsv or data/dense-baseline-v0.4.0.0/
dense-raw.tsv (next to this file) and asserted at manuscript precision.
"""

import argparse
import csv
import math
import statistics
from pathlib import Path


HERE = Path(__file__).resolve().parent
RAW_TSV = HERE / "data" / "round5-overnight" / "overnight-raw.tsv"
RAW_DENSE_TSV = HERE / "data" / "dense-baseline-v0.4.0.0" / "dense-raw.tsv"

# Scaling: N -> (mean seconds, sample sd seconds, displayed decimal places).
EXPECTED_SCALING = {
    200: (0.740, 0.012, 3),
    500: (2.076, 0.024, 3),
    1000: (4.649, 0.120, 3),
    2000: (10.30, 0.26, 2),
    4000: (23.16, 0.23, 2),
    8000: (56.37, 1.56, 2),
    16000: (128.4, 1.5, 1),
    32000: (307.5, 7.4, 1),
    64000: (768.0, 10.2, 1),
}
# Light plot: (speedup means, propagated sample sds), ordered by listed cores.
EXPECTED_LIGHT = (
    [0.86, 1.71, 1.51, 1.48, 1.40, 1.39],
    [0.02, 0.04, 0.04, 0.05, 0.04, 0.04],
)
# Heavy plot: (speedup means, propagated sample sds), ordered by listed cores.
EXPECTED_HEAVY = ([2.92, 4.35, 4.75], [0.21, 0.17, 0.20])
# Heavy runs: cores -> number of clean repetitions (elapsed > 1.0 seconds).
EXPECTED_HEAVY_CLEAN = {
    4: 2,
    8: 4,
    10: 5,
    12: 4,
    14: 1,
}
# Heavy text-only results: cores -> displayed speedup.
EXPECTED_HEAVY_TEXT = {
    12: 4.85,
    14: 4.83,
}
# Scaling residency: N -> (mean residency, displayed unit, displayed decimals).
EXPECTED_SCALING_MEMORY = {
    200: (41.5, "MiB", 1),
    500: (100.6, "MiB", 1),
    1000: (234.1, "MiB", 1),
    2000: (395.6, "MiB", 1),
    4000: (831.6, "MiB", 1),
    8000: (1.86, "GiB", 2),
    16000: (3.35, "GiB", 2),
    32000: (6.77, "GiB", 2),
    64000: (12.75, "GiB", 2),
}
# Value type: configuration -> (mean seconds, sample sd seconds, decimals).
EXPECTED_VALUETYPE = {
    "N200-double": (0.753, 0.024, 3),
    "N200-decimal": (5.24, 0.20, 2),
    "N1000-double": (4.65, 0.03, 2),
    "N1000-decimal": (28.0, 1.4, 1),
}
# Value type ratio: N -> displayed decimal-over-double runtime ratio.
EXPECTED_VALUETYPE_RATIO = {
    200: 6.9,
    1000: 6.0,
}
# Memory: configuration -> (mean seconds, sample sd seconds, residency MiB).
EXPECTED_MEMORY = {
    "N1000-retainAll": (4.89, 0.23, 248.6),
    "N1000-recent2-spill": (8.33, 0.21, 14.8),
}
# Memory summary: displayed residency ratio and spill-time overhead in seconds.
EXPECTED_MEMORY_RATIO = 16.8
EXPECTED_MEMORY_OVERHEAD = 3.4
# Dense Fig. 1 baseline: mean, sample sd, 50-period/single-scenario time, ratio.
EXPECTED_DENSE = (59.5, 2.5, 14.9, 20.1)


def check(checks, name, computed, expected, ndigits):
    """Record a named check and assert that its rounded value matches the manuscript."""
    checks.append(name)
    if round(computed, ndigits) != expected:
        raise AssertionError(f"{name}: computed {computed!r} != expected {expected!r}")


def _number(value):
    """Parse a TSV number, returning NaN for missing/non-finite measurements."""
    try:
        number = float(value)
    except (TypeError, ValueError):
        return math.nan
    return number if math.isfinite(number) else math.nan


def rows(raw_path=RAW_TSV):
    """Read raw TSV measurements and return rows with numeric fields converted."""
    with Path(raw_path).open(newline="") as raw_file:
        reader = csv.DictReader(raw_file, delimiter="\t")
        result = []
        for row in reader:
            elapsed = _number(row.get("elapsed"))
            maxres = _number(row.get("maxres"))
            try:
                cores = int(row.get("cores", ""))
            except (TypeError, ValueError):
                cores = None
            status = row.get("status")
            clean = math.isfinite(elapsed) and (
                status == "ok" if status is not None else elapsed > 1.0
            )
            result.append({
                **row,
                "cores": cores,
                "elapsed": elapsed,
                "maxres": maxres,
                "_clean": clean,
            })
        return result


def dense_rows(dense_raw_path=RAW_DENSE_TSV):
    """Read the dense-baseline TSV and return its elapsed measurements in seconds."""
    with Path(dense_raw_path).open(newline="") as raw_file:
        return [
            _number(row.get("elapsed"))
            for row in csv.DictReader(raw_file, delimiter="\t")
        ]


def pick(data, series, config, cores=None, clean=False):
    """Return matching raw rows, optionally retaining only elapsed times above one second."""
    selected = [
        row
        for row in data
        if row["series"] == series
        and row["config"] == config
        and (cores is None or row["cores"] == cores)
        and (not clean or row["_clean"])
    ]
    if not selected:
        raise AssertionError(f"no raw rows for {series}/{config}/cores={cores}")
    return selected


def ms(data):
    """Return the elapsed-time sample mean and sample standard deviation for rows."""
    elapsed = [row["elapsed"] for row in data]
    return statistics.mean(elapsed), statistics.stdev(elapsed)


def speed(base, parallel):
    """Return a speedup and its propagated standard deviation from two mean/sd pairs."""
    base_mean, base_sd = base
    parallel_mean, parallel_sd = parallel
    speedup = base_mean / parallel_mean
    speedup_sd = speedup * math.sqrt(
        (base_sd / base_mean) ** 2 + (parallel_sd / parallel_mean) ** 2
    )
    return speedup, speedup_sd


def _calculate_record(raw_path, dense_raw_path):
    """Calculate all plot/table values, assert manuscript checks, and return them."""
    data = rows(raw_path)
    checks = []
    scaling = []
    for n, (expected_mean, expected_sd, ndigits) in EXPECTED_SCALING.items():
        series = "scaling" if n <= 2000 else "scalingext"
        measurements = pick(data, series, f"N{n}-K20-T50-seq-double")
        mean, sd = ms(measurements)
        residency = statistics.mean(row["maxres"] for row in measurements)
        check(checks, f"scaling N={n} mean", mean, expected_mean, ndigits)
        check(checks, f"scaling N={n} sd", sd, expected_sd, ndigits)
        expected_residency, unit, residency_digits = EXPECTED_SCALING_MEMORY[n]
        divisor = 2**20 if unit == "MiB" else 2**30
        check(checks, f"scaling N={n} residency", residency / divisor,
              expected_residency, residency_digits)
        scaling.append((n, mean, sd, residency, ndigits))

    dense_elapsed = dense_rows(dense_raw_path)
    if len(dense_elapsed) != 5:
        raise AssertionError(
            f"dense baseline repetition count: computed {len(dense_elapsed)!r} != expected 5"
        )
    dense_mean = statistics.mean(dense_elapsed)
    dense_sd = statistics.stdev(dense_elapsed)
    dense_normalized = dense_mean / 4
    dense_ratio = dense_normalized / scaling[0][1]
    for name, computed, expected in zip(
        ["dense baseline mean", "dense baseline sd", "dense baseline normalized",
         "dense baseline/sparse ratio"],
        [dense_mean, dense_sd, dense_normalized, dense_ratio],
        EXPECTED_DENSE,
    ):
        check(checks, name, computed, expected, 1)
    dense = (dense_mean, dense_sd, dense_normalized, dense_ratio)

    light = []
    for cores, expected, expected_sd in zip([1, 4, 8, 10, 12, 14], *EXPECTED_LIGHT):
        speedup, speedup_sd = speed(
            ms(pick(data, "parallel", "N1000-seq", 1)),
            ms(pick(data, "parallel", "N1000-par16", cores)),
        )
        check(checks, f"light {cores}c speedup", speedup, expected, 2)
        check(checks, f"light {cores}c speedup sd", speedup_sd, expected_sd, 2)
        light.append((cores, speedup, speedup_sd))

    base_rows = pick(data, "heavy", "N1000-heavy-seq", 1, clean=True)
    if len(base_rows) != 5:
        raise AssertionError(
            f"heavy baseline clean count: computed {len(base_rows)!r} != expected 5"
        )
    base = ms(base_rows)
    check(checks, "heavy baseline mean", base[0], 30.265, 3)
    all_heavy = {}
    for cores, count in EXPECTED_HEAVY_CLEAN.items():
        measurements = pick(data, "heavy", "N1000-heavy-par16", cores, clean=True)
        checks.append(f"heavy {cores}c clean count")
        if len(measurements) != count:
            raise AssertionError(
                f"heavy {cores}c clean count: computed {len(measurements)!r} "
                f"!= expected {count!r}"
            )
        if len(measurements) > 1:
            all_heavy[cores] = speed(base, ms(measurements))
        else:
            all_heavy[cores] = (base[0] / measurements[0]["elapsed"], None)

    heavy = []
    for cores, expected, expected_sd in zip([4, 8, 10], *EXPECTED_HEAVY):
        speedup, speedup_sd = all_heavy[cores]
        check(checks, f"heavy {cores}c speedup", speedup, expected, 2)
        check(checks, f"heavy {cores}c speedup sd", speedup_sd, expected_sd, 2)
        heavy.append((cores, speedup, speedup_sd))
    for cores, expected in EXPECTED_HEAVY_TEXT.items():
        check(checks, f"heavy {cores}c text speedup", all_heavy[cores][0], expected, 2)

    value_types = {}
    for config, (expected_mean, expected_sd, ndigits) in EXPECTED_VALUETYPE.items():
        mean, sd = ms(pick(data, "valuetype", config, 4))
        check(checks, f"valuetype {config} mean", mean, expected_mean, ndigits)
        check(checks, f"valuetype {config} sd", sd, expected_sd, ndigits)
        value_types[config] = (mean, sd)

    ratios = {}
    for n, expected in EXPECTED_VALUETYPE_RATIO.items():
        ratio = value_types[f"N{n}-decimal"][0] / value_types[f"N{n}-double"][0]
        check(checks, f"valuetype N={n} decimal/double", ratio, expected, 1)
        ratios[n] = ratio

    memory = {}
    for config, (expected_mean, expected_sd, expected_residency) in EXPECTED_MEMORY.items():
        measurements = pick(data, "memory", config, 4)
        mean, sd = ms(measurements)
        residency = statistics.mean(row["maxres"] for row in measurements) / 2**20
        check(checks, f"memory {config} mean", mean, expected_mean, 2)
        check(checks, f"memory {config} sd", sd, expected_sd, 2)
        check(checks, f"memory {config} residency", residency, expected_residency, 1)
        memory[config] = (mean, sd, residency)

    memory_ratio = memory["N1000-retainAll"][2] / memory["N1000-recent2-spill"][2]
    overhead = memory["N1000-recent2-spill"][0] - memory["N1000-retainAll"][0]
    check(checks, "memory retainAll/spill residency", memory_ratio,
          EXPECTED_MEMORY_RATIO, 1)
    check(checks, "memory spill overhead", overhead, EXPECTED_MEMORY_OVERHEAD, 1)
    return (scaling, dense, light, heavy, value_types, ratios, memory, memory_ratio,
            overhead, checks)


def calculate(raw_path=RAW_TSV, dense_raw_path=RAW_DENSE_TSV, policy="record"):
    """Calculate record values or load measurements for replication assessment."""
    if policy == "record":
        return _calculate_record(raw_path, dense_raw_path)
    if policy != "replicate":
        raise ValueError(f"unknown calculation policy: {policy}")
    try:
        data = rows(raw_path)
    except (OSError, csv.Error):
        data = []
    try:
        dense = [value for value in dense_rows(dense_raw_path) if math.isfinite(value)]
    except (OSError, csv.Error):
        dense = []
    return {"data": data, "dense": dense}


def _measurements(values, series, config, cores=None, clean=False, field="elapsed"):
    selected = [
        row[field]
        for row in values["data"]
        if row.get("series") == series
        and row.get("config") == config
        and (cores is None or row.get("cores") == cores)
        and (not clean or row.get("_clean", False))
        and math.isfinite(row.get(field, math.nan))
    ]
    return selected


def _mean(values, series, config, cores=None, clean=False, field="elapsed"):
    measurements = _measurements(values, series, config, cores, clean, field)
    return (statistics.mean(measurements), len(measurements)) if measurements else (None, 0)


def _slope(points):
    """Return the ordinary least-squares slope in log-log space."""
    xs = [math.log(x) for x, _ in points]
    ys = [math.log(y) for _, y in points]
    xmean = statistics.mean(xs)
    ymean = statistics.mean(ys)
    return sum((x - xmean) * (y - ymean) for x, y in zip(xs, ys)) / sum(
        (x - xmean) ** 2 for x in xs
    )


def assess(values, smoke=False):
    """Assess the seven ratio/shape replication criteria."""
    names = [
        "fig1a scaling slope",
        "fig1b dense/sparse ratio",
        "fig2 light max speedup",
        "fig2 heavy speedup at max cores",
        "table1 residency slope",
        "valuetype decimal/double",
        "memory residency ratio",
    ]
    references = [
        "1.0-1.35 (paper ~= 1.2)",
        ">= 10 (paper 20.1)",
        "1.3-2.5 (paper 1.71)",
        ">= 3.0 (paper 4.75 at 10 cores)",
        "0.9-1.1 (linear)",
        ">= 3 at N=200 and N=1000 (paper 6.9 / 6.0)",
        ">= 8 (paper 16.8)",
    ]
    if smoke:
        return [
            (name, "NOT-ASSESSED", "-", reference, "smoke run")
            for name, reference in zip(names, references)
        ]

    results = []
    timing_points = []
    residency_points = []
    scaling_counts = []
    for n in EXPECTED_SCALING:
        series = "scaling" if n <= 2000 else "scalingext"
        timing, timing_count = _mean(
            values, series, f"N{n}-K20-T50-seq-double"
        )
        residency, residency_count = _mean(
            values, series, f"N{n}-K20-T50-seq-double", field="maxres"
        )
        scaling_counts.append(f"N{n}:{timing_count}")
        if timing_count >= 3 and timing is not None and timing > 0:
            timing_points.append((n, timing))
        if residency_count >= 3 and residency is not None and residency > 0:
            residency_points.append((n, residency))

    if len(timing_points) < 7:
        results.append((names[0], "NOT-ASSESSED", f"{len(timing_points)} points",
                        references[0], "reps " + ", ".join(scaling_counts)))
    else:
        slope = _slope(timing_points)
        verdict = "PASS" if 1.0 <= slope <= 1.35 else "FAIL"
        results.append((names[0], verdict, f"{slope:.3f}", references[0],
                        f"{len(timing_points)} points"))

    sparse, sparse_count = _mean(
        values, "scaling", "N200-K20-T50-seq-double"
    )
    dense_count = len(values["dense"])
    if sparse_count < 3 or dense_count < 3 or sparse is None:
        results.append((names[1], "NOT-ASSESSED", "-", references[1],
                        f"sparse reps={sparse_count}, dense reps={dense_count}"))
    else:
        ratio = (statistics.mean(values["dense"]) / 4) / sparse
        results.append((names[1], "PASS" if ratio >= 10 else "FAIL",
                        f"{ratio:.3f}", references[1],
                        f"sparse reps={sparse_count}, dense reps={dense_count}"))

    light_base, light_base_count = _mean(values, "parallel", "N1000-seq", 1)
    light_speeds = []
    light_counts = []
    light_cores = sorted({
        row.get("cores") for row in values["data"]
        if row.get("series") == "parallel"
        and row.get("config") == "N1000-par16"
        and row.get("cores") is not None and row.get("cores") > 1
    })
    for cores in light_cores:
        mean, count = _mean(values, "parallel", "N1000-par16", cores)
        light_counts.append(f"{cores}c:{count}")
        if count >= 3 and mean is not None and mean > 0 and light_base is not None:
            light_speeds.append((cores, light_base / mean))
    if light_base_count < 3 or len(light_speeds) < 2:
        results.append((names[2], "NOT-ASSESSED", "-", references[2],
                        f"baseline reps={light_base_count}; " + ", ".join(light_counts)))
    else:
        peak_core, peak = max(light_speeds, key=lambda item: item[1])
        tail = light_speeds[-1][1]
        results.append((names[2], "PASS" if 1.3 <= peak <= 2.5 else "FAIL",
                        f"{peak:.3f}", references[2],
                        f"peak={peak_core}c; max-core={light_speeds[-1][0]}c/{tail:.3f}"))

    heavy_base, heavy_base_count = _mean(
        values, "heavy", "N1000-heavy-seq", 1, clean=True
    )
    heavy_speeds = []
    heavy_fractions = []
    heavy_cores = sorted({
        row.get("cores") for row in values["data"]
        if row.get("series") == "heavy"
        and row.get("config") == "N1000-heavy-par16"
        and row.get("cores") is not None
    })
    for cores in heavy_cores:
        all_rows = [
            row for row in values["data"]
            if row.get("series") == "heavy"
            and row.get("config") == "N1000-heavy-par16"
            and row.get("cores") == cores
        ]
        mean, count = _mean(
            values, "heavy", "N1000-heavy-par16", cores, clean=True
        )
        heavy_fractions.append(f"{cores}c:{count}/{len(all_rows)}")
        if count >= 3 and mean is not None and mean > 0 and heavy_base is not None:
            heavy_speeds.append((cores, heavy_base / mean))
    if heavy_base_count < 3 or not heavy_speeds:
        results.append((names[3], "NOT-ASSESSED", "-", references[3],
                        f"baseline clean={heavy_base_count}; clean fractions "
                        + ", ".join(heavy_fractions)))
    else:
        cores, speedup = heavy_speeds[-1]
        results.append((names[3], "PASS" if speedup >= 3 else "FAIL",
                        f"{speedup:.3f}", references[3],
                        f"max eligible={cores}c; clean fractions "
                        + ", ".join(heavy_fractions)))

    if len(residency_points) < 7:
        results.append((names[4], "NOT-ASSESSED", f"{len(residency_points)} points",
                        references[4], "each point requires >=3 finite maxres values"))
    else:
        slope = _slope(residency_points)
        results.append((names[4], "PASS" if 0.9 <= slope <= 1.1 else "FAIL",
                        f"{slope:.3f}", references[4],
                        f"{len(residency_points)} points"))

    type_ratios = []
    type_counts = []
    for n in (200, 1000):
        decimal, decimal_count = _mean(values, "valuetype", f"N{n}-decimal", 4)
        double, double_count = _mean(values, "valuetype", f"N{n}-double", 4)
        type_counts.append(f"N{n} decimal/double={decimal_count}/{double_count}")
        if (decimal_count >= 3 and double_count >= 3 and decimal is not None
                and double is not None and double > 0):
            type_ratios.append((n, decimal / double))
    if len(type_ratios) != 2:
        results.append((names[5], "NOT-ASSESSED", "-", references[5],
                        "; ".join(type_counts)))
    else:
        computed = ", ".join(f"N{n}={ratio:.3f}" for n, ratio in type_ratios)
        results.append((names[5],
                        "PASS" if all(ratio >= 3 for _, ratio in type_ratios) else "FAIL",
                        computed, references[5], "; ".join(type_counts)))

    memory_values = []
    memory_counts = []
    for config in ("N1000-retainAll", "N1000-recent2-spill"):
        mean, count = _mean(values, "memory", config, 4, field="maxres")
        memory_counts.append(f"{config}={count}")
        if count >= 3 and mean is not None and mean > 0:
            memory_values.append(mean)
    if len(memory_values) != 2:
        results.append((names[6], "NOT-ASSESSED", "-", references[6],
                        ", ".join(memory_counts)))
    else:
        ratio = memory_values[0] / memory_values[1]
        results.append((names[6], "PASS" if ratio >= 8 else "FAIL",
                        f"{ratio:.3f}", references[6], ", ".join(memory_counts)))
    return results


def fmt(value, ndigits):
    """Format a computed value with exactly the manuscript's number of decimals."""
    return f"{value:.{ndigits}f}"


def table(values):
    """Print computed and manuscript table values after all assertions have passed."""
    scaling, dense, _, _, value_types, ratios, memory, memory_ratio, overhead, checks = values
    print("Table 1 (tab:scaling)")
    for n, mean, sd, residency, ndigits in scaling:
        expected_mean, expected_sd, _ = EXPECTED_SCALING[n]
        expected_residency, unit, residency_digits = EXPECTED_SCALING_MEMORY[n]
        divisor = 2**20 if unit == "MiB" else 2**30
        computed_residency = residency / divisor
        print(
            f"N={n}: computed {fmt(mean, ndigits)} +/- {fmt(sd, ndigits)} s; "
            f"{fmt(computed_residency, residency_digits)} {unit} | manuscript "
            f"{fmt(expected_mean, ndigits)} +/- {fmt(expected_sd, ndigits)} s; "
            f"{fmt(expected_residency, residency_digits)} {unit}"
        )
    dense_mean, dense_sd, dense_normalized, dense_ratio = dense
    print("dense baseline (Fig. 1, Sec. 7.1)")
    print(f"mean +/- sd: computed {fmt(dense_mean, 1)} +/- {fmt(dense_sd, 1)} s | "
          f"manuscript {fmt(EXPECTED_DENSE[0], 1)} +/- {fmt(EXPECTED_DENSE[1], 1)} s")
    print(f"normalized: computed {fmt(dense_normalized, 1)} s | "
          f"manuscript {fmt(EXPECTED_DENSE[2], 1)} s")
    print(f"dense/sparse ratio: computed {fmt(dense_ratio, 1)} | "
          f"manuscript {fmt(EXPECTED_DENSE[3], 1)}")
    print("valuetype (Sec. 7)")
    for config, (expected_mean, expected_sd, ndigits) in EXPECTED_VALUETYPE.items():
        mean, sd = value_types[config]
        print(
            f"{config}: computed {fmt(mean, ndigits)} +/- {fmt(sd, ndigits)} s | "
            f"manuscript {fmt(expected_mean, ndigits)} +/- {fmt(expected_sd, ndigits)} s"
        )
    for n, expected in EXPECTED_VALUETYPE_RATIO.items():
        print(
            f"N={n} decimal/double: computed {fmt(ratios[n], 1)} | "
            f"manuscript {fmt(expected, 1)}"
        )
    print("memory (Sec. 7)")
    for config, (expected_mean, expected_sd, expected_residency) in EXPECTED_MEMORY.items():
        mean, sd, residency = memory[config]
        print(
            f"{config}: computed {fmt(mean, 2)} +/- {fmt(sd, 2)} s; "
            f"{fmt(residency, 1)} MiB | manuscript {fmt(expected_mean, 2)} +/- "
            f"{fmt(expected_sd, 2)} s; {fmt(expected_residency, 1)} MiB"
        )
    print(f"retainAll/spill residency: computed {fmt(memory_ratio, 1)} | manuscript "
          f"{fmt(EXPECTED_MEMORY_RATIO, 1)}")
    print(f"spill overhead: computed {fmt(overhead, 1)} s | manuscript "
          f"{fmt(EXPECTED_MEMORY_OVERHEAD, 1)} s")
    print(f"all {len(checks)} checks passed")


def replication_table(criteria):
    """Print computed/reference values and replication verdicts."""
    print("criterion | verdict | computed | reference | note")
    for name, verdict, computed, reference, note in criteria:
        print(f"{name} | {verdict} | {computed} | {reference} | {note}")


def plot_replicated(values, out_dir):
    """Write non-canonical replication figures from whatever data are available."""
    import matplotlib.pyplot as plt

    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    scaling = []
    for n in EXPECTED_SCALING:
        series = "scaling" if n <= 2000 else "scalingext"
        mean, count = _mean(values, series, f"N{n}-K20-T50-seq-double")
        if count and mean is not None and mean > 0:
            scaling.append((n, mean))
    fig, ax = plt.subplots()
    if scaling:
        ax.loglog([point[0] for point in scaling], [point[1] for point in scaling], "o-")
    ax.set_xlabel("number of firms N")
    ax.set_ylabel("wall-clock time per 50-period run (s)")
    ax.grid(True, which="major", linestyle=":")
    scaling_path = out_dir / "scaling-replicated.pdf"
    fig.savefig(scaling_path, bbox_inches="tight")
    plt.close(fig)

    fig, ax = plt.subplots()
    for series, base_config, parallel_config, label, clean in (
        ("parallel", "N1000-seq", "N1000-par16", "light", False),
        ("heavy", "N1000-heavy-seq", "N1000-heavy-par16", "heavy", True),
    ):
        base, base_count = _mean(values, series, base_config, 1, clean=clean)
        points = []
        cores_values = sorted({
            row.get("cores") for row in values["data"]
            if row.get("series") == series
            and row.get("config") == parallel_config
            and row.get("cores") is not None
        })
        for cores in cores_values:
            mean, count = _mean(values, series, parallel_config, cores, clean=clean)
            if base_count and count and base is not None and mean is not None and mean > 0:
                points.append((cores, base / mean))
        if points:
            ax.plot([point[0] for point in points], [point[1] for point in points],
                    "o-", label=label)
    ax.set_xlabel("cores p")
    ax.set_ylabel("end-to-end speedup vs. sequential")
    ax.grid(True, which="major", linestyle=":")
    if ax.lines:
        ax.legend()
    speedup_path = out_dir / "speedup-replicated.pdf"
    fig.savefig(speedup_path, bbox_inches="tight")
    plt.close(fig)
    print("wrote", scaling_path, "and", speedup_path)


def plot(values):
    """Lazily import Matplotlib and write two figures from unrounded computed values."""
    # Delayed import and font registration make --table stdlib-only.
    import glob

    import matplotlib
    import matplotlib.font_manager as font_manager
    import matplotlib.pyplot as plt

    libertine_glob = (
        "/usr/local/texlive/2026/texmf-dist/fonts/opentype/public/libertine/"
        "LinLibertine_*.otf"
    )
    for font_file in glob.glob(libertine_glob):
        font_manager.fontManager.addfont(font_file)
    matplotlib.rcParams.update({
        "font.family": "serif",
        "font.serif": ["Linux Libertine O", "STIXGeneral"],
        "mathtext.fontset": "custom",
        "mathtext.rm": "Linux Libertine O",
        "mathtext.it": "Linux Libertine O:italic",
        "mathtext.bf": "Linux Libertine O:bold",
        "font.size": 8,
        "axes.labelsize": 8,
        "legend.fontsize": 7.5,
        "xtick.labelsize": 7.5,
        "ytick.labelsize": 7.5,
        "axes.linewidth": 0.5,
        "grid.linestyle": ":",
        "grid.linewidth": 0.5,
        "grid.color": "0.6",
        "legend.framealpha": 1.0,
        "legend.edgecolor": "0.3",
        "legend.fancybox": False,
        "pdf.fonttype": 42,
    })

    # Okabe-Ito
    blue = "#0072B2"
    vermillion = "#D55E00"
    tw = 395.8225 / 72.27  # acmsmall \textwidth in inches
    scaling, dense, light, heavy = values[:4]

    # ---------------------------------------------------------------- Fig. 1
    # Round 5 idle-machine rerun (2026-06-13); raw in data/round5-overnight/
    ns, means, sds = zip(*[(n, mean, sd) for n, mean, sd, _, _ in scaling])
    fig, ax = plt.subplots(figsize=(0.62 * tw, 0.46 * tw))
    ax.set_xscale("log")
    ax.set_yscale("log")
    ax.errorbar(ns, means, yerr=sds, color=blue, marker="o", markersize=3.5,
                linewidth=1.2, capsize=1.5, elinewidth=0.6, zorder=3,
                label="sparse $(G,A)$, sequential")
    ax.plot([200, 64000], [means[0], means[0] * (64000 / 200) ** 1.2],
            color="black", linestyle="--", linewidth=0.9, zorder=2,
            label=r"$\propto N^{1.2}$")
    # Source: EA v0.4.0.0 dense all-pairs example `examples/basic/simulateEx2.hs`
    # (sim2), remeasured five times on the platform of record (M3 Max, x86_64
    # GHC 9.10.2). N=200, T=100, and two concurrent scenarios are normalized
    # by /2 and /2 to 50 periods and one scenario; raw data are in
    # data/dense-baseline-v0.4.0.0/. The former fixed 18 s point, derived from
    # a 72 s SICE draft value, was a development run under different conditions.
    ax.plot([200], [dense[2]], color=vermillion, marker="s", markersize=4.5,
            linestyle="none", zorder=3,
            label="dense all-pairs baseline (normalized)")
    ax.set_xlabel("number of firms $N$")
    ax.set_ylabel("wall-clock time per 50-period run (s)")
    ax.set_xticks([200, 1000, 4000, 16000, 64000])
    ax.set_xticklabels(["200", "1000", "4000", "16000", "64000"])
    ax.set_yticks([1, 10, 100, 1000])
    ax.set_yticklabels(["1", "10", "100", "1000"])
    ax.tick_params(which="minor", length=1.5)
    ax.grid(True, which="major")
    handles, labels = ax.get_legend_handles_labels()
    order = [labels.index(label) for label in ["sparse $(G,A)$, sequential",
             r"$\propto N^{1.2}$", "dense all-pairs baseline (normalized)"]]
    ax.legend([handles[index] for index in order], [labels[index] for index in order],
              loc="upper left", handlelength=1.6, borderpad=0.35,
              labelspacing=0.3, borderaxespad=0.3)
    fig.savefig(HERE / "scaling.pdf", bbox_inches="tight", pad_inches=0.02)
    plt.close(fig)

    # ---------------------------------------------------------------- Fig. 2
    # Speedups vs. the *sequential* 1-core run of each regime (light: N1000-seq
    # 4.930 s; heavy: N1000-heavy-seq 30.265 s) -- conservative relative to the
    # par16 1-core builds (e.g. light par16@1 = 5.734 s); sd propagated through
    # the ratio as S * sqrt((sd_seq/T_seq)^2 + (sd_par/T_par)^2).
    #
    # Light series: clean throughout (no exclusions); matches the `parallel`
    # rows of data/round5-overnight/round5-aggregate.md directly.
    #
    # Heavy series: the pre-mitigation build hit the speculative-evaluation
    # hazard (Sec. 5.2) in 9 of its 25 parallel runs, which appear in
    # overnight-raw.tsv as sub-second exits (elapsed < 1 s, ~12 MiB peak vs.
    # ~6-11 s and ~200 MiB for a real run). Those <<loop>> runs are excluded;
    # values are means over clean repetitions only (vs. the 5/5-clean 1-core
    # mean). We stop the plotted heavy series at 10 cores -- the largest config
    # with 5/5 clean repetitions. NOTE: round5-aggregate.md lists the
    # *unfiltered* heavy means, which differ from the clean-run values used here.
    plot_light, speed_light, sd_light = zip(*light)
    plot_heavy, speed_heavy, sd_heavy = zip(*heavy)
    fig, ax = plt.subplots(figsize=(0.62 * tw, 0.42 * tw))
    ax.errorbar(plot_light, speed_light, yerr=sd_light, color=blue, marker="o",
                markersize=4, linewidth=1.2, capsize=2, elinewidth=0.7,
                zorder=3, label="light per-agent work ($N=1000$)")
    ax.errorbar(plot_heavy, speed_heavy, yerr=sd_heavy, color=vermillion,
                marker="^", markersize=4.5, linewidth=1.2, capsize=2,
                elinewidth=0.7, linestyle="--", zorder=3,
                label="heavy per-agent work")
    ax.set_xlabel("cores $p$")
    ax.set_ylabel("end-to-end speedup vs. sequential")
    ax.set_xticks([1, 4, 8, 10, 12, 14])
    ax.set_ylim(0, 5.6)
    ax.grid(True, which="major")
    # the empty band between the two series, clear of both lines
    ax.legend(loc="center right")
    fig.savefig(HERE / "speedup.pdf", bbox_inches="tight", pad_inches=0.02)
    plt.close(fig)
    print("wrote", HERE / "scaling.pdf", "and", HERE / "speedup.pdf")


def main():
    """Parse output mode, calculate asserted values, then print or plot them."""
    parser = argparse.ArgumentParser()
    modes = parser.add_mutually_exclusive_group()
    modes.add_argument("--table", action="store_true")
    modes.add_argument("--replicate", action="store_true")
    parser.add_argument("--raw", type=Path, default=RAW_TSV)
    parser.add_argument("--dense-raw", type=Path, default=RAW_DENSE_TSV)
    parser.add_argument("--out-dir", type=Path)
    parser.add_argument("--smoke", action="store_true")
    arguments = parser.parse_args()
    if arguments.smoke and not arguments.replicate:
        parser.error("--smoke requires --replicate")
    policy = "replicate" if arguments.replicate else "record"
    values = calculate(arguments.raw, arguments.dense_raw, policy=policy)
    if arguments.replicate:
        criteria = assess(values, smoke=arguments.smoke)
        replication_table(criteria)
        try:
            plot_replicated(values, arguments.out_dir or arguments.raw.parent)
        except ImportError:
            print("matplotlib unavailable; replication figures skipped")
        failures = sum(verdict == "FAIL" for _, verdict, _, _, _ in criteria)
        not_assessed = sum(
            verdict == "NOT-ASSESSED" for _, verdict, _, _, _ in criteria
        )
        if failures:
            print("REPLICATION: FAIL")
            return 1
        if not_assessed:
            print(f"REPLICATION: PARTIAL ({not_assessed} criteria not assessed)")
            return 0
        print("REPLICATION: PASS")
        return 0
    if arguments.table:
        table(values)
    else:
        plot(values)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
