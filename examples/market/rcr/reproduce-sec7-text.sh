#!/usr/bin/env bash
# Section 7 in-text numbers: the value-type series (Sec. 7.3, Double vs Decimal
# at N=200/1000) and the memory/retention series (Sec. 7.4, RetainAll vs
# RetainRecent 2 + spill at N=1000). Both come from the light revision.
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
rcr_run_light_series "$light_wt" valuetype
rcr_run_light_series "$light_wt" memory

echo "Section 7 text values (successful reps only)"
printf 'series\tconfig\treps\twall mean +/- sd (s)\tpeak residency mean (bytes)\n'
awk -F '\t' '
  BEGIN { OFS="\t" }
  ($1=="valuetype" || $1=="memory") && $8=="ok" {
    k=$1 "\t" $2
    count[k]++; sum[k]+=$5; sumsq[k]+=$5*$5; mem[k]+=$6
  }
  END {
    for (k in count) {
      mean=sum[k]/count[k]
      sd=(count[k]>1 ? sqrt((sumsq[k]-sum[k]*sum[k]/count[k])/(count[k]-1)) : 0)
      printf "%s\t%d\t%.3f +/- %.3f\t%.0f\n", k, count[k], mean, sd, mem[k]/count[k]
    }
  }' "$RAW_TSV" | sort
rcr_make_figures; rc=$?
rcr_finish_metadata
exit "$rc"
