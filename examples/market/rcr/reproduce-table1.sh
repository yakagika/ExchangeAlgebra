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
(cd "$light_wt" && rcr_stack build exchangealgebra-examples:exe:marketEx1 \
  exchangealgebra-examples:exe:marketEx1d) || rcr_die "light build failed"
rcr_record_revision light "$light_wt" "$LIGHT_REV"
light_bin=$(cd "$light_wt" && stack exec --system-ghc -- which marketEx1) || rcr_die "cannot find marketEx1"
rcr_record_binary marketEx1 "$light_bin"
light_decimal_bin=$(cd "$light_wt" && stack exec --system-ghc -- which marketEx1d) || rcr_die "cannot find marketEx1d"
rcr_record_binary marketEx1d "$light_decimal_bin"
rcr_run_light_series "$light_wt" scaling
rcr_run_light_series "$light_wt" scalingext

echo "Table 2 reproduction (successful reps only)"
printf 'N\twall mean +/- sd (s)\tpeak residency mean (bytes)\n'
awk -F '\t' '
  BEGIN { OFS="\t" }
  ($1=="scaling" || $1=="scalingext") && $8=="ok" {
    n=$2; sub(/^N/,"",n); sub(/-.*/,"",n)
    count[n]++; sum[n]+=$5; sumsq[n]+=$5*$5; mem[n]+=$6
  }
  END {
    for (n in count) {
      mean=sum[n]/count[n]
      sd=(count[n]>1 ? sqrt((sumsq[n]-sum[n]*sum[n]/count[n])/(count[n]-1)) : 0)
      printf "%d\t%.3f +/- %.3f\t%.0f\n", n, mean, sd, mem[n]/count[n]
    }
  }' "$RAW_TSV" | sort -n
rcr_make_figures; rc=$?
rcr_finish_metadata
exit "$rc"
