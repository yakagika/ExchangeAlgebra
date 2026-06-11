#!/usr/bin/env python3
# fp-error-profile.py -- Round 4 (series 3): FP error profile.
#
# Compares the Double dump (marketEx1) against the Decimal dump (marketEx1d),
# both produced by EA_DUMP_OBSERVABLES on the *same* seed/params, and reports
# the relative-error distribution per observable class. Decimal is treated as
# the exact reference x; Double is the approximation d.
#
#   relative error  =  |d - x| / max(|x|, eps)
#
# Output: a Markdown table with, per observable class, the max / median
# relative error and the row count. stdlib only (no numpy / uv).
#
# Usage:
#   ./fp-error-profile.py DOUBLE.csv DECIMAL.csv [--eps 1e-9] [--label N=200]
#
# The two CSVs must share the firm,observable,value schema written by
# MarketModel's writeObservableCsv (sorted by observable,firm so rows align,
# but we key on (firm,observable) defensively rather than relying on order).

import csv
import sys
from statistics import median


def load(path):
    """firm,observable,value CSV -> {(firm, observable): float value}."""
    out = {}
    with open(path, newline="") as fh:
        rdr = csv.DictReader(fh)
        if rdr.fieldnames != ["firm", "observable", "value"]:
            sys.exit(
                f"{path}: unexpected header {rdr.fieldnames!r} "
                "(expected firm,observable,value)"
            )
        for row in rdr:
            key = (row["firm"], row["observable"])
            out[key] = float(row["value"])
    return out


def main(argv):
    eps = 1e-9
    label = None
    pos = []
    i = 0
    while i < len(argv):
        a = argv[i]
        if a == "--eps":
            eps = float(argv[i + 1]); i += 2
        elif a == "--label":
            label = argv[i + 1]; i += 2
        elif a.startswith("--"):
            sys.exit(f"unknown flag {a}")
        else:
            pos.append(a); i += 1
    if len(pos) != 2:
        sys.exit("usage: fp-error-profile.py DOUBLE.csv DECIMAL.csv "
                 "[--eps E] [--label L]")
    dpath, xpath = pos
    dbl = load(dpath)      # approximation d (Double)
    dec = load(xpath)      # reference x     (Decimal)

    keys = set(dbl) & set(dec)
    missing = (set(dbl) | set(dec)) - keys
    if missing:
        sys.stderr.write(
            f"warning: {len(missing)} key(s) present in only one file, skipped\n"
        )

    # group relative errors by observable class
    by_obs = {}
    for (firm, obs) in keys:
        x = dec[(firm, obs)]
        d = dbl[(firm, obs)]
        rel = abs(d - x) / max(abs(x), eps)
        by_obs.setdefault(obs, []).append(rel)

    title = "## FP error profile" + (f" ({label})" if label else "")
    print(title)
    print()
    print(f"- reference (exact x): `{xpath}` (Decimal)")
    print(f"- approximation (d):   `{dpath}` (Double)")
    print(f"- eps = {eps:g}; relative error = |d - x| / max(|x|, eps)")
    print()
    print("| observable | n | max rel.err | median rel.err |")
    print("|---|---:|---:|---:|")
    for obs in sorted(by_obs):
        vals = by_obs[obs]
        print(f"| {obs} | {len(vals)} | {max(vals):.3e} | {median(vals):.3e} |")
    # overall row
    allvals = [v for vs in by_obs.values() for v in vs]
    if allvals:
        print(f"| **all** | {len(allvals)} | {max(allvals):.3e} | "
              f"{median(allvals):.3e} |")


if __name__ == "__main__":
    main(sys.argv[1:])
