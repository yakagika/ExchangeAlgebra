#!/usr/bin/env bash
set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=lib.sh
. "$SCRIPT_DIR/lib.sh"

usage() { echo "Usage: $0 [options]"; rcr_usage_common; }
rcr_parse_options "$@"
[ "$RCR_HELP" -eq 1 ] && { usage; exit 0; }
command -v python3 >/dev/null 2>&1 || rcr_die "python3 is required"
[ -x /usr/bin/time ] || rcr_die "/usr/bin/time is required"
rcr_preflight_revision_sources
rcr_init_run
command -v stack >/dev/null 2>&1 || rcr_die "stack is required"

if [ "$(uname -s)" = Darwin ]; then
  mem_bytes=$(sysctl -n hw.memsize 2>/dev/null || echo 0)
else
  mem_bytes=$(awk '/MemTotal:/ {printf "%.0f", $2*1024}' /proc/meminfo 2>/dev/null || echo 0)
fi
required_gib=16
[ "$RCR_OPT64" -eq 0 ] && required_gib=8
required_bytes=$((required_gib * 1024 * 1024 * 1024))
[ "$mem_bytes" -ge "$required_bytes" ] 2>/dev/null || rcr_warn \
  "recommended RAM is ${required_gib} GiB; detected approximately $((mem_bytes / 1024 / 1024 / 1024)) GiB"

rcr_common_args
# The per-figure scripts are run through bash so that an archive extractor that
# drops the executable bit (e.g. python3 -m zipfile) still works.
RCR_DEFER_ASSESS=1
export RCR_DEFER_ASSESS
bash "$SCRIPT_DIR/reproduce-fig1.sh" "${RCR_COMMON_ARGS[@]}" || exit $?
bash "$SCRIPT_DIR/reproduce-fig2.sh" "${RCR_COMMON_ARGS[@]}" || exit $?
bash "$SCRIPT_DIR/reproduce-table1.sh" "${RCR_COMMON_ARGS[@]}" || exit $?
bash "$SCRIPT_DIR/reproduce-sec7-text.sh" "${RCR_COMMON_ARGS[@]}" || exit $?
unset RCR_DEFER_ASSESS

# This final invocation is authoritative for reproduce-all.sh's exit status.
rcr_make_figures; rc=$?
rcr_finish_metadata
exit "$rc"
