#!/usr/bin/env bash
# run-market-experiments.sh — Phase 5 (feat/market-scale-experiments) measurement
# harness for the marketEx1 / marketEx1d executables (MarketModel).
#
# This script only *runs* the configurations and captures raw logs + +RTS -s
# stats; the coordinator aggregates them afterwards (it does not compute any
# summary table itself, by design — same split as scratch-gate/run-gate.sh).
#
# Methodology (identical to the Phase 2 Gate, GATE_RESULTS_PHASE2.md):
#   * one process per measured configuration (so +RTS -s describes it alone);
#   * 2 reps per config (the coordinator takes the faster wall = "Total elapsed");
#   * raw RTS blocks appended to result/raw-<series>.log;
#   * a one-line-per-run TSV echoed to stdout (mode, config, rep, elapsed, maxres,
#     prod) for quick eyeballing.
#
# Usage:
#   ./run-market-experiments.sh [series ...]
# where each series is one of:
#   scaling    parallel    valuetype    memory    all
# (default: all). Example:  ./run-market-experiments.sh scaling parallel
#
# ----------------------------------------------------------------------------
# The E7 series (plan MARKET_SCALE_PLAN.md), reproduced here as the source of
# truth for what each series runs:
#
#  (i)  scaling   : N ∈ {200, 500, 1000, 2000}, K=20, T=50, seq, MoneyDouble.
#                   The main structural-scaling curve (vs SICE N=200 dense).
#  (ii) parallel  : N=1000 heavy stages, Sequential vs ParChunk at -N {1,4,8,14}.
#                   Records the DET check (seq vs par final norm identical).
#  (iii)valuetype : marketEx1 (MoneyDouble) vs marketEx1d (MoneyDecimal) at
#                   N ∈ {200, 1000}, K=20, T=50, seq.  exact/fast trade-off.
#  (iv) memory    : N=1000 with RetainAll vs RetainRecent 2 (+ spill) — max
#                   residency comparison (O(window) vs O(T) memory).
#
# Every run prints marketEx1's own DET-relevant summary (final ledger norm and
# final-term shortage) to its stdout, captured into the raw log, so the
# coordinator can verify Sequential and ParChunk agree.
# ----------------------------------------------------------------------------
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
RESULT="$HERE/result"
mkdir -p "$RESULT"

# Run one configuration: name, exe, cores, rep, then EA_* env assignments.
# Captures both stdout (the model summary: norm + shortage) and the +RTS -s
# block into result/raw-<series>.log, and echoes a TSV line.
run_one () {
  local series="$1"; shift
  local label="$1";  shift
  local exe="$1";    shift
  local cores="$1";  shift
  local rep="$1";    shift
  # remaining args are EA_* = value assignments
  local logf="$RESULT/raw-${series}.log"

  # build the env prefix
  local envs=("$@")

  local out
  out="$(env "${envs[@]}" stack exec --system-ghc "$exe" -- +RTS "-N${cores}" -s 2>&1)"

  local elapsed maxres prod
  # Robust elapsed extraction: take the "<num>s elapsed" token from the *Total*
  # line specifically (anchored to "Total" so it cannot match the INIT/MUT/GC
  # lines, which also print "Ns elapsed"), regardless of whitespace/parenthesis
  # layout. GHC prints e.g. "Total   time  123.905s  (106.894s elapsed)"; an
  # awk $5 column split mis-fields when there is no space after ')'.
  elapsed=$(printf '%s\n' "$out" | grep -E '^[[:space:]]*Total[[:space:]]+time' \
              | grep -oE '[0-9.]+s elapsed' | head -1 | grep -oE '[0-9.]+')
  maxres=$(printf '%s\n'  "$out" | awk '/maximum residency/ {gsub(/,/,"",$1); print $1}')
  prod=$(printf '%s\n'    "$out" | awk '/Productivity/ {print $2}')

  printf '%s\t%s\tN%s\trep%s\telapsed=%ss\tmaxres=%sB\tprod=%s\n' \
    "$series" "$label" "$cores" "$rep" "$elapsed" "$maxres" "$prod"

  {
    echo "### series=$series label=$label exe=$exe cores=$cores rep=$rep env=${envs[*]}"
    printf '%s\n' "$out"
    echo
  } >> "$logf"
}

# ---- (i) scaling -----------------------------------------------------------
series_scaling () {
  : > "$RESULT/raw-scaling.log"
  for n in 200 500 1000 2000; do
    for rep in 1 2; do
      run_one scaling "N${n}-K20-T50-seq-double" marketEx1 4 "$rep" \
        "EA_N=$n" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq"
    done
  done
}

# ---- (ii) parallel ---------------------------------------------------------
series_parallel () {
  : > "$RESULT/raw-parallel.log"
  # Sequential baseline (at -N1) then ParChunk across cores. Same N/K/T so the
  # coordinator can read speedup directly; the chunk is fixed (input-size only).
  local CHUNK=16
  for rep in 1 2; do
    run_one parallel "N1000-seq" marketEx1 1 "$rep" \
      "EA_N=1000" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq"
  done
  for c in 1 4 8 14; do
    for rep in 1 2; do
      run_one parallel "N1000-par${CHUNK}" marketEx1 "$c" "$rep" \
        "EA_N=1000" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=par:${CHUNK}"
    done
  done
}

# ---- (iii) valuetype -------------------------------------------------------
series_valuetype () {
  : > "$RESULT/raw-valuetype.log"
  for n in 200 1000; do
    for rep in 1 2; do
      run_one valuetype "N${n}-double" marketEx1  4 "$rep" \
        "EA_N=$n" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq"
      run_one valuetype "N${n}-decimal" marketEx1d 4 "$rep" \
        "EA_N=$n" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq"
    done
  done
}

# ---- (iv) memory -----------------------------------------------------------
series_memory () {
  : > "$RESULT/raw-memory.log"
  local SPILL="$RESULT/market-spill.bin"
  for rep in 1 2; do
    run_one memory "N1000-retainAll" marketEx1 4 "$rep" \
      "EA_N=1000" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq" "EA_RETAIN=all"
  done
  for rep in 1 2; do
    rm -f "$SPILL"   # append-mode spill: clear before each rep
    run_one memory "N1000-recent2-spill" marketEx1 4 "$rep" \
      "EA_N=1000" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq" \
      "EA_RETAIN=recent:2" "EA_SPILL=$SPILL"
  done
  rm -f "$SPILL"
}

SERIES_ARGS=("$@")
if [ "${#SERIES_ARGS[@]}" -eq 0 ]; then
  SERIES_ARGS=(all)
fi

for s in "${SERIES_ARGS[@]}"; do
  case "$s" in
    scaling)   series_scaling ;;
    parallel)  series_parallel ;;
    valuetype) series_valuetype ;;
    memory)    series_memory ;;
    all)       series_scaling; series_parallel; series_valuetype; series_memory ;;
    *) echo "unknown series: $s (expected scaling|parallel|valuetype|memory|all)" >&2; exit 2 ;;
  esac
done
