#!/usr/bin/env bash
# run-overnight.sh -- idle-environment re-measurement of every paper series
# (ea-scaling-paper section 7, "Round 5"). The Round 4 numbers were taken while
# the machine was in interactive use; this wrapper reruns the same harness on
# an otherwise idle machine, with environment preflight, provenance capture,
# and cooldowns between runs so back-to-back configs do not heat-soak the chip.
#
# Launch from the bench-rerun worktree ROOT right before leaving the machine:
#     caffeinate -dims ./examples/market/run-overnight.sh
# (caffeinate prevents sleep for the duration; have the machine on AC power,
#  user apps closed. Total expected wall ~2.5-3h, dominated by scalingext.)
#
# What it runs (all 5 reps, mean+-sd downstream via aggregate-round4.py):
#   1. parallel   : run-round4.sh, cores extended to 1/4/8/10/12/14 (EA_CORES)
#   2. scaling    : run-round4.sh, N=200..2000
#   3. valuetype  : run-round4.sh, Double vs Decimal at N=200/1000
#   4. memory     : run-round4.sh, RetainAll vs RetainRecent2+spill
#   5. scalingext : run-round4.sh, N=4000..64000 (EA_OPT64=1)
#   6. heavy      : pre-fix2 binary (worktree at dd35d80) seq + par @4/8/10/12/14;
#                   <<loop>> reps are recorded as elapsed=NA (they are data:
#                   the paper cites the hazard rate of the old binary).
#
# Outputs (under examples/market/result/ of THIS worktree):
#   overnight-env.txt        environment + provenance snapshot
#   overnight-raw.tsv        cumulative TSV (series config cores rep elapsed maxres prod)
#   raw-round4-<series>.log  raw +RTS -s blocks (per series, from run-round4.sh)
#   raw-overnight-heavy.log  raw blocks of the heavy series
#   OVERNIGHT_DONE           marker written on completion
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
RESULT="$HERE/result"
HEAVY_WT="${EA_HEAVY_WT:-/tmp/exchangealgebra-bench-heavy}"
HEAVY_TIMEOUT="${EA_HEAVY_TIMEOUT:-600}"   # seconds per heavy rep before kill
CUMTSV="$RESULT/overnight-raw.tsv"
mkdir -p "$RESULT"

# Pin the exact compiler the Round 4 binaries used (x86_64 GHC 9.10.2 under
# Rosetta 2 -- deliberate: keeps the rerun comparable with Round 4 and the
# dense SICE baseline; the native-arch check is a separate workstream).
export PATH="$HOME/.stack/programs/x86_64-osx/ghc-9.10.2/bin:$PATH"

note () { printf '[overnight] %s\n' "$*"; }
die  () { printf '[overnight] ABORT: %s\n' "$*" >&2; exit 1; }

# ---- preflight -------------------------------------------------------------
command -v ghc >/dev/null || die "ghc not on PATH after pin"
ghc --version | grep -q 9.10.2 || die "pinned ghc is not 9.10.2"
pmset -g batt | grep -q "AC Power" || die "not on AC power"

LOAD="$(sysctl -n vm.loadavg | awk '{print $2}')"
awk "BEGIN{exit !($LOAD < 2.0)}" || die "load average $LOAD >= 2.0 -- machine not idle"

tmutil status 2>/dev/null | grep -q "Running = 1" && die "Time Machine backup in progress"

BIN="$(cd "$ROOT" && stack exec --system-ghc which marketEx1 2>/dev/null)"
[ -n "$BIN" ] || die "marketEx1 not built in $ROOT"
file "$BIN" | grep -q x86_64 || die "marketEx1 is not the x86_64 build"
HBIN="$(cd "$HEAVY_WT" && stack exec --system-ghc which marketEx1 2>/dev/null)"
[ -n "$HBIN" ] || die "heavy marketEx1 not built in $HEAVY_WT"

# ---- provenance snapshot ----------------------------------------------------
{
  echo "## overnight rerun environment snapshot"
  date
  sw_vers
  sysctl -n machdep.cpu.brand_string hw.ncpu hw.memsize \
            hw.perflevel0.physicalcpu hw.perflevel1.physicalcpu
  echo "load: $(sysctl -n vm.loadavg)"
  pmset -g therm
  pmset -g batt | head -2
  mdutil -s / 2>/dev/null
  echo "ghc: $(ghc --version)  [$(command -v ghc)]"
  echo "stack: $(stack --version | head -1)"
  echo "marketEx1: $BIN"; file "$BIN"
  echo "heavy marketEx1: $HBIN"; file "$HBIN"
  echo "## main worktree"
  git -C "$ROOT" log --oneline -1; git -C "$ROOT" status --short
  echo "## harness diff vs HEAD (uncommitted harness edits, if any)"
  git -C "$ROOT" diff -- examples/market/
  echo "## heavy worktree"
  git -C "$HEAVY_WT" log --oneline -1; git -C "$HEAVY_WT" status --short
} > "$RESULT/overnight-env.txt" 2>&1
note "environment snapshot -> result/overnight-env.txt"

# ---- helpers ----------------------------------------------------------------
printf 'series\tconfig\tcores\trep\telapsed\tmaxres\tprod\n' > "$CUMTSV"

# Run one run-round4.sh series in its own invocation (fresh round4-raw.tsv),
# then fold its TSV body into the cumulative file.
round4_series () {
  local series="$1" cooldown="$2"; shift 2
  note "series $series (cooldown ${cooldown}s)"
  ( cd "$ROOT" && env EA_REPS=5 EA_COOLDOWN="$cooldown" "$@" \
      ./examples/market/run-round4.sh "$series" )
  tail -n +2 "$RESULT/round4-raw.tsv" >> "$CUMTSV"
}

# One heavy rep with a watchdog (the pre-fix2 binary can die with <<loop>>;
# elapsed stays NA in that case and the raw block records the failure).
heavy_one () {
  local label="$1" cores="$2" rep="$3" par="$4"
  local logf="$RESULT/raw-overnight-heavy.log"
  local outf; outf="$(mktemp)"
  ( cd "$HEAVY_WT" && env EA_N=1000 EA_K=20 EA_T=50 EA_NET=er "EA_PAR=$par" \
      stack exec --system-ghc marketEx1 -- +RTS "-N${cores}" -s ) >"$outf" 2>&1 &
  local pid=$! waited=0
  while kill -0 "$pid" 2>/dev/null; do
    sleep 5; waited=$((waited + 5))
    if [ "$waited" -ge "$HEAVY_TIMEOUT" ]; then
      kill -9 "$pid" 2>/dev/null
      echo "[overnight] TIMEOUT after ${HEAVY_TIMEOUT}s" >> "$outf"
      break
    fi
  done
  wait "$pid" 2>/dev/null

  local elapsed maxres prod
  elapsed=$(grep -E '^[[:space:]]*Total[[:space:]]+time' "$outf" \
              | grep -oE '[0-9.]+s elapsed' | head -1 | grep -oE '[0-9.]+')
  maxres=$(awk '/maximum residency/ {gsub(/,/,"",$1); print $1}' "$outf")
  prod=$(awk '/Productivity/ {print $2}' "$outf")
  : "${elapsed:=NA}" "${maxres:=NA}" "${prod:=NA}"
  printf 'heavy\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$label" "$cores" "$rep" "$elapsed" "$maxres" "$prod" | tee -a "$CUMTSV"
  {
    echo "### series=heavy label=$label cores=$cores rep=$rep par=$par"
    cat "$outf"; echo
  } >> "$logf"
  rm -f "$outf"
  sleep 10
}

series_heavy () {
  : > "$RESULT/raw-overnight-heavy.log"
  note "series heavy (pre-fix2 binary @ $HEAVY_WT)"
  local rep c
  for rep in 1 2 3 4 5; do heavy_one "N1000-heavy-seq" 1 "$rep" seq; done
  for c in 4 8 10 12 14; do
    for rep in 1 2 3 4 5; do heavy_one "N1000-heavy-par16" "$c" "$rep" par:16; done
  done
}

# ---- the night --------------------------------------------------------------
START="$(date +%s)"
note "start: $(date)"

round4_series parallel  10 EA_CORES="1 4 8 10 12 14"
round4_series scaling   10
round4_series valuetype 10
round4_series memory    10
series_heavy
round4_series scalingext 30 EA_OPT64=1

END="$(date +%s)"
{
  echo "completed: $(date)"
  echo "wall: $(( (END - START) / 60 )) min"
  echo "post-run load: $(sysctl -n vm.loadavg)"
  pmset -g therm
} >> "$RESULT/overnight-env.txt"
touch "$RESULT/OVERNIGHT_DONE"
note "DONE in $(( (END - START) / 60 )) min -- results in $RESULT"
osascript -e 'display notification "overnight bench complete" with title "EA Round 5"' 2>/dev/null || true
