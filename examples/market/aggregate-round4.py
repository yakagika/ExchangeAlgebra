#!/usr/bin/env python3
# aggregate-round4.py -- Round 4 (series 1/2/4): mean +/- sd aggregator.
#
# Reads the per-rep TSV written by run-round4.sh (one line per rep):
#
#   series \t config \t cores \t rep \t elapsed \t maxres \t prod
#
# where elapsed is seconds, maxres is bytes, prod is the GHC "Productivity"
# percentage (or empty). Emits, per (series, config, cores), the mean +/- sd of
# elapsed and maxres over the reps, as a Markdown table. stdlib only.
#
# Usage:
#   ./aggregate-round4.py result/round4-raw.tsv
#   ./run-round4.sh scaling | ./aggregate-round4.py -      # read stdin
#
# Lines that are blank or start with '#' are ignored. A malformed numeric field
# is skipped with a warning (the run is still summarised from the good reps).

import sys
from statistics import mean, pstdev, stdev


def fmt_secs(v):
    return f"{v:.3f}"


def fmt_bytes(b):
    # human-friendly MiB/GiB alongside raw, since maxres spans 10MB..17GB
    if b >= 1 << 30:
        return f"{b / (1 << 30):.2f} GiB"
    if b >= 1 << 20:
        return f"{b / (1 << 20):.1f} MiB"
    return f"{b:.0f} B"


def sd(xs):
    # sample sd for n>=2, else 0.0 (single rep)
    return stdev(xs) if len(xs) >= 2 else 0.0


def main(argv):
    if len(argv) != 1:
        sys.exit("usage: aggregate-round4.py TSV   (use - for stdin)")
    path = argv[0]
    fh = sys.stdin if path == "-" else open(path)

    # key = (series, config, cores) -> {"elapsed": [...], "maxres": [...]}
    groups = {}
    order = []
    for ln, raw in enumerate(fh, 1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        parts = line.split("\t")
        if len(parts) < 6:
            sys.stderr.write(f"line {ln}: too few columns, skipped\n")
            continue
        series, config, cores, rep, elapsed, maxres = parts[:6]
        # skip the TSV header row written by run-round4.sh
        if series == "series" and config == "config":
            continue
        key = (series, config, cores)
        if key not in groups:
            groups[key] = {"elapsed": [], "maxres": []}
            order.append(key)
        try:
            groups[key]["elapsed"].append(float(elapsed))
        except ValueError:
            sys.stderr.write(f"line {ln}: bad elapsed {elapsed!r}, skipped\n")
        try:
            groups[key]["maxres"].append(float(maxres))
        except ValueError:
            sys.stderr.write(f"line {ln}: bad maxres {maxres!r}, skipped\n")

    if path != "-":
        fh.close()

    print("## Round 4 aggregate (mean +/- sd over reps)")
    print()
    print("| series | config | cores | reps | elapsed mean (s) | elapsed sd | "
          "maxres mean | maxres sd |")
    print("|---|---|---:|---:|---:|---:|---:|---:|")
    for key in order:
        series, config, cores = key
        e = groups[key]["elapsed"]
        m = groups[key]["maxres"]
        n = max(len(e), len(m))
        e_mean = mean(e) if e else float("nan")
        e_sd = sd(e)
        m_mean = mean(m) if m else float("nan")
        m_sd = sd(m)
        print(
            f"| {series} | {config} | {cores} | {n} | "
            f"{fmt_secs(e_mean)} | {fmt_secs(e_sd)} | "
            f"{fmt_bytes(m_mean)} | {fmt_bytes(m_sd)} |"
        )


if __name__ == "__main__":
    main(sys.argv[1:])
