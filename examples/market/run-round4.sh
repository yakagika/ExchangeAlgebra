#!/usr/bin/env bash
# run-round4.sh -- Round 4 measurement harness (handoff
# easp-2026-06-11-phase3-measurement-batch). Like run-market-experiments.sh it
# only *runs* the marketEx1/marketEx1d executables and captures raw +RTS -s
# logs; the coordinator aggregates with aggregate-round4.py / fp-error-profile.py.
#
# Difference from Round 3 / run-market-experiments.sh:
#   * each config is run EA_REPS times (default 5, was "2, take faster"), so the
#     coordinator computes mean +/- sd instead of a single fastest wall;
#   * one TSV line PER REP is echoed to stdout AND appended to
#     result/round4-raw.tsv, with columns:
#         series  config  cores  rep  elapsed  maxres  prod
#     (elapsed in seconds, maxres in bytes, prod = GHC Productivity %), which is
#     exactly what aggregate-round4.py consumes.
#
# Usage:
#   ./run-round4.sh [series ...]
# series: scaling | parallel | valuetype | memory | scalingext | heavy14 | all
#         (default: all). Run from the REPOSITORY ROOT so the root stack.yaml
#         (local lib) is used, not examples/stack.yaml (Hackage pin):
#             ./examples/market/run-round4.sh scaling
#
# Env knobs:
#   EA_REPS=<n>     reps per config (default 5).
#   EA_SMOKE=1      shrink every series to a single small point (N=200, 1 core
#                   set) for a fast end-to-end check of the harness wiring.
#   EA_OPT64=1      add the optional N=64000 point to scalingext (~17GB; only if
#                   N=32000 stayed within the predicted residency).
#
# Series detail (marketEx1 unless noted; K=20, T=50, NET=er, seq unless noted):
#   scaling     : N = 200 / 500 / 1000 / 2000               (seq, Double)
#   parallel    : N=1000, seq baseline + par:16 @ 1/4/8/14 cores
#   valuetype   : N = 200 / 1000, Double (marketEx1) vs Decimal (marketEx1d)
#   memory      : N=1000, RetainAll vs RetainRecent 2 (+ spill)
#   scalingext  : N = 4000 / 8000 / 16000 / 32000 (+64000 if EA_OPT64=1).
#                 Per-N safety valve: after rep1 the measured maxres is compared
#                 against a prediction (~0.26 MB/firm); if it exceeds 1.5x the
#                 prediction the series ABORTS at that N (message on stderr) so a
#                 runaway N never tries to allocate past the 20GB cap.
#   heavy14     : NOT IMPLEMENTED -- see the note printed by series_heavy14.
#                 The Round 2 "heavy" regime was the O(N^2) all-firm demand
#                 rebuild, removed in commit 1402eaa (Phase 5 fix2). The current
#                 model exposes no env knob to restore it, so the heavy 14-core
#                 point cannot be reproduced from this binary; reproducing it
#                 needs a pre-1402eaa build. This series only prints that note
#                 and exits non-zero (the coordinator decides separately).
#
# ----------------------------------------------------------------------------
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
RESULT="$HERE/result"
mkdir -p "$RESULT"

REPS="${EA_REPS:-5}"
SMOKE="${EA_SMOKE:-0}"
TSV="$RESULT/round4-raw.tsv"

# Fresh TSV + header on each top-level invocation.
printf 'series\tconfig\tcores\trep\telapsed\tmaxres\tprod\n' > "$TSV"

# Run one rep of one configuration: series label exe cores rep [EA_*=v ...].
# Captures stdout+RTS into result/raw-round4-<series>.log, echoes ONE TSV line
# to stdout, and appends the same line to $TSV. Returns maxres (bytes) via the
# global REPLY_MAXRES so the scalingext safety valve can read rep1.
REPLY_MAXRES=""
run_one () {
  local series="$1"; shift
  local label="$1";  shift
  local exe="$1";    shift
  local cores="$1";  shift
  local rep="$1";    shift
  local envs=("$@")               # remaining args are EA_*=value
  local logf="$RESULT/raw-round4-${series}.log"

  local out
  out="$(env "${envs[@]}" stack exec --system-ghc "$exe" -- +RTS "-N${cores}" -s 2>&1)"

  local elapsed maxres prod
  # Anchor elapsed to the "Total time" line (other lines also say "Ns elapsed").
  elapsed=$(printf '%s\n' "$out" | grep -E '^[[:space:]]*Total[[:space:]]+time' \
              | grep -oE '[0-9.]+s elapsed' | head -1 | grep -oE '[0-9.]+')
  maxres=$(printf '%s\n'  "$out" | awk '/maximum residency/ {gsub(/,/,"",$1); print $1}')
  prod=$(printf '%s\n'    "$out" | awk '/Productivity/ {print $2}')

  : "${elapsed:=NA}" "${maxres:=NA}" "${prod:=NA}"
  REPLY_MAXRES="$maxres"

  # TSV: series config cores rep elapsed maxres prod
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$series" "$label" "$cores" "$rep" "$elapsed" "$maxres" "$prod" | tee -a "$TSV"

  {
    echo "### series=$series label=$label exe=$exe cores=$cores rep=$rep env=${envs[*]}"
    printf '%s\n' "$out"
    echo
  } >> "$logf"
}

# reps helper: run a config for rep in 1..REPS.
reps_of () {
  local series="$1" label="$2" exe="$3" cores="$4"; shift 4
  local rep
  for rep in $(seq 1 "$REPS"); do
    run_one "$series" "$label" "$exe" "$cores" "$rep" "$@"
  done
}

# ---- (i) scaling -----------------------------------------------------------
series_scaling () {
  : > "$RESULT/raw-round4-scaling.log"
  local ns="200 500 1000 2000"
  [ "$SMOKE" = 1 ] && ns="200"
  local n
  for n in $ns; do
    reps_of scaling "N${n}-K20-T50-seq-double" marketEx1 4 \
      "EA_N=$n" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq"
  done
}

# ---- (ii) parallel ---------------------------------------------------------
series_parallel () {
  : > "$RESULT/raw-round4-parallel.log"
  local CHUNK=16
  reps_of parallel "N1000-seq" marketEx1 1 \
    "EA_N=1000" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq"
  local cs="1 4 8 14"
  [ "$SMOKE" = 1 ] && cs="1"
  local c
  for c in $cs; do
    reps_of parallel "N1000-par${CHUNK}" marketEx1 "$c" \
      "EA_N=1000" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=par:${CHUNK}"
  done
}

# ---- (iii) valuetype -------------------------------------------------------
series_valuetype () {
  : > "$RESULT/raw-round4-valuetype.log"
  local ns="200 1000"
  [ "$SMOKE" = 1 ] && ns="200"
  local n
  for n in $ns; do
    reps_of valuetype "N${n}-double" marketEx1  4 \
      "EA_N=$n" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq"
    reps_of valuetype "N${n}-decimal" marketEx1d 4 \
      "EA_N=$n" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq"
  done
}

# ---- (iv) memory -----------------------------------------------------------
series_memory () {
  : > "$RESULT/raw-round4-memory.log"
  local SPILL="$RESULT/market-spill.bin"
  reps_of memory "N1000-retainAll" marketEx1 4 \
    "EA_N=1000" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq" "EA_RETAIN=all"
  local rep
  for rep in $(seq 1 "$REPS"); do
    rm -f "$SPILL"   # append-mode spill: clear before each rep
    run_one memory "N1000-recent2-spill" marketEx1 4 "$rep" \
      "EA_N=1000" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq" \
      "EA_RETAIN=recent:2" "EA_SPILL=$SPILL"
  done
  rm -f "$SPILL"
}

# ---- scalingext (extended scaling with per-N safety valve) -----------------
# prediction: ~0.26 MB/firm (Round 3 measured, Double, RetainAll). abort the
# series at the first N whose rep1 maxres exceeds 1.5x the prediction.
series_scalingext () {
  : > "$RESULT/raw-round4-scalingext.log"
  local ns="4000 8000 16000 32000"
  [ "${EA_OPT64:-0}" = 1 ] && ns="$ns 64000"
  [ "$SMOKE" = 1 ] && ns="4000"
  local n predict_bytes limit_bytes rep
  for n in $ns; do
    # 0.26 MB/firm -> bytes; 1.5x tolerance.
    predict_bytes=$(awk "BEGIN{printf \"%.0f\", $n * 0.26 * 1048576}")
    limit_bytes=$(awk "BEGIN{printf \"%.0f\", $predict_bytes * 1.5}")
    for rep in $(seq 1 "$REPS"); do
      run_one scalingext "N${n}-K20-T50-seq-double" marketEx1 4 "$rep" \
        "EA_N=$n" "EA_K=20" "EA_T=50" "EA_NET=er" "EA_PAR=seq"
      if [ "$rep" = 1 ]; then
        local mr="$REPLY_MAXRES"
        if [ "$mr" != "NA" ] && [ -n "$mr" ]; then
          # integer compare (maxres is bytes)
          if [ "$mr" -gt "$limit_bytes" ] 2>/dev/null; then
            echo "run-round4: ABORT scalingext at N=$n -- rep1 maxres ${mr}B" \
                 "exceeds 1.5x prediction (${limit_bytes}B, predict ${predict_bytes}B)." \
                 "Stopping the series; smaller-N reps already recorded." >&2
            return 0
          fi
        else
          echo "run-round4: WARN scalingext N=$n rep1 maxres unparsed;" \
               "continuing without the safety check for this N." >&2
        fi
      fi
    done
  done
}

# ---- heavy14 (NOT reproducible from this binary) ---------------------------
series_heavy14 () {
  cat >&2 <<'EOF'
run-round4: series 'heavy14' is intentionally NOT implemented.

  The Round 2 "heavy" regime was the per-term O(N^2) all-firm demand rebuild
  (openingMap over the whole (Carryover,t) note inside every per-agent lambda).
  That cost was removed in commit 1402eaa (Phase 5 fix2): demand is now a single
  indexed per-firm read (openingOf), so each term is O(N log N). The current
  MarketModel exposes NO environment knob to restore the heavy path, and adding
  one would be a model behaviour change (out of scope for a measurement-only
  task).

  => Reproducing the heavy 14-core point requires a pre-1402eaa binary
     (e.g. build marketEx1 at commit 39d38a5 or be589aa). The coordinator should
     decide whether to (a) check out an older commit for that one point, or
     (b) drop the heavy 8->14 cost-model point from Round 4. This series exits
     non-zero so 'all' does not silently skip it.
EOF
  return 3
}

SERIES_ARGS=("$@")
if [ "${#SERIES_ARGS[@]}" -eq 0 ]; then
  SERIES_ARGS=(all)
fi

rc=0
for s in "${SERIES_ARGS[@]}"; do
  case "$s" in
    scaling)    series_scaling ;;
    parallel)   series_parallel ;;
    valuetype)  series_valuetype ;;
    memory)     series_memory ;;
    scalingext) series_scalingext ;;
    heavy14)    series_heavy14 || rc=$? ;;
    all)        series_scaling; series_parallel; series_valuetype
                series_memory; series_scalingext ;;
    *) echo "unknown series: $s (expected scaling|parallel|valuetype|memory|scalingext|heavy14|all)" >&2
       exit 2 ;;
  esac
done

echo "run-round4: TSV written to $TSV" >&2
exit "$rc"
