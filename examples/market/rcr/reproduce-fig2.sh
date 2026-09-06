#!/usr/bin/env bash
set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=lib.sh
. "$SCRIPT_DIR/lib.sh"

usage() { echo "Usage: $0 [options]"; rcr_usage_common; }
rcr_parse_options "$@"
[ "$RCR_HELP" -eq 1 ] && { usage; exit 0; }
command -v python3 >/dev/null 2>&1 || rcr_die "python3 is required"
rcr_init_run
ensure_revision light "$LIGHT_REV"; light_wt=$REPLY_REVISION_DIR
ensure_revision heavy "$HEAVY_REV"; heavy_wt=$REPLY_REVISION_DIR

(cd "$light_wt" && rcr_stack build exchangealgebra-examples:exe:marketEx1 \
  exchangealgebra-examples:exe:marketEx1d) || rcr_die "light build failed"
rcr_record_revision light "$light_wt" "$LIGHT_REV"
light_bin=$(cd "$light_wt" && stack exec --system-ghc -- which marketEx1) || rcr_die "cannot find marketEx1"
rcr_record_binary marketEx1 "$light_bin"
light_decimal_bin=$(cd "$light_wt" && stack exec --system-ghc -- which marketEx1d) || rcr_die "cannot find marketEx1d"
rcr_record_binary marketEx1d "$light_decimal_bin"
rcr_filter_cores "1 4 8 10 12 14"
EA_LIGHT_CORES=$REPLY_CORES
export EA_LIGHT_CORES
rcr_run_light_series "$light_wt" parallel

(cd "$heavy_wt" && rcr_stack build exchangealgebra-examples:exe:marketEx1) || rcr_die "heavy build failed"
rcr_record_revision heavy "$heavy_wt" "$HEAVY_REV"
heavy_bin=$(cd "$heavy_wt" && stack exec --system-ghc -- which marketEx1) || rcr_die "cannot find heavy marketEx1"
rcr_record_binary heavy-marketEx1 "$heavy_bin"
if ! rcr_has_series heavy; then
  : > "$RUN_DIR/raw-heavy.log"
  heavy_timeout=${EA_HEAVY_TIMEOUT:-600}
  case "$heavy_timeout" in *[!0-9]*|'') rcr_die "EA_HEAVY_TIMEOUT must be a positive integer" ;; esac
  [ "$heavy_timeout" -gt 0 ] || rcr_die "EA_HEAVY_TIMEOUT must be a positive integer"
  requested_cores=${EA_HEAVY_CORES:-4 8 10 12 14}
  [ "$RCR_SMOKE" -eq 1 ] && requested_cores=4
  rcr_filter_cores "$requested_cores"; heavy_cores=$REPLY_CORES

  heavy_one() {
    label=$1; cores=$2; rep=$3; par=$4
    out=$(mktemp "$RUN_DIR/heavy-rep.XXXXXX") || rcr_die "mktemp failed"
    # exec: the background pid must be the setsid()'d process itself (the new
    # process-group leader), not an intermediate subshell, or the group kill
    # below would miss stack/marketEx1 (the run-overnight.sh watchdog defect).
    (cd "$heavy_wt" && exec python3 -c 'import os,sys; os.setsid(); os.execvp(sys.argv[2], sys.argv[2:])' -- \
      env EA_N=1000 EA_K=20 EA_T=50 EA_NET=er EA_PAR="$par" \
      stack exec --system-ghc marketEx1 -- +RTS "-N$cores" -s) > "$out" 2>&1 &
    pid=$!; waited=0; timed_out=0
    while kill -0 "$pid" 2>/dev/null; do
      sleep 1; waited=$((waited + 1))
      if [ "$waited" -ge "$heavy_timeout" ]; then
        kill -9 -- "-$pid" 2>/dev/null || true
        echo "[rcr] TIMEOUT after ${heavy_timeout}s" >> "$out"
        timed_out=1
        break
      fi
    done
    wait "$pid" 2>/dev/null || true
    elapsed=$(awk '/^[[:space:]]*Total[[:space:]]+time/ { for (i=1;i<=NF;i++) if ($i == "elapsed)") { gsub(/s/,"",$(i-1)); print $(i-1); exit } }' "$out")
    maxres=$(awk '/maximum residency/ {gsub(/,/,"",$1); print $1; exit}' "$out")
    prod=$(awk '/Productivity/ {print $2; exit}' "$out")
    : "${elapsed:=NA}" "${maxres:=NA}" "${prod:=NA}"
    if [ "$timed_out" -eq 1 ]; then
      status=timeout
    elif grep -q '<<loop>>' "$out" || { [ "$elapsed" != NA ] && awk -v e="$elapsed" 'BEGIN {exit !(e < 1.0)}'; }; then
      status=loop
    elif [ "$elapsed" = NA ]; then
      status=error
    else
      status=ok
    fi
    printf 'heavy\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
      "$label" "$cores" "$rep" "$elapsed" "$maxres" "$prod" "$status" | tee -a "$RAW_TSV"
    { echo "### series=heavy label=$label cores=$cores rep=$rep par=$par status=$status"; cat "$out"; echo; } >> "$RUN_DIR/raw-heavy.log"
    rm -f "$out"
    [ "$RCR_COOLDOWN" -gt 0 ] && sleep "$RCR_COOLDOWN"
  }

  rep=1
  while [ "$rep" -le "$RCR_REPS" ]; do heavy_one N1000-heavy-seq 1 "$rep" seq; rep=$((rep + 1)); done
  # shellcheck disable=SC2086
  for cores in $heavy_cores; do
    rep=1
    while [ "$rep" -le "$RCR_REPS" ]; do heavy_one N1000-heavy-par16 "$cores" "$rep" par:16; rep=$((rep + 1)); done
  done
else
  rcr_note "skip heavy: rows already exist in $RAW_TSV"
fi
rcr_make_figures; rc=$?
rcr_finish_metadata
exit "$rc"
