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
rcr_init_run
ensure_revision light "$LIGHT_REV"; light_wt=$REPLY_REVISION_DIR
ensure_revision dense "$DENSE_REV"; dense_wt=$REPLY_REVISION_DIR

(cd "$light_wt" && rcr_stack build exchangealgebra-examples:exe:marketEx1 \
  exchangealgebra-examples:exe:marketEx1d) || rcr_die "light build failed"
rcr_record_revision light "$light_wt" "$LIGHT_REV"
light_bin=$(cd "$light_wt" && stack exec --system-ghc -- which marketEx1) || rcr_die "cannot find marketEx1"
rcr_record_binary marketEx1 "$light_bin"
light_decimal_bin=$(cd "$light_wt" && stack exec --system-ghc -- which marketEx1d) || rcr_die "cannot find marketEx1d"
rcr_record_binary marketEx1d "$light_decimal_bin"

rcr_run_light_series "$light_wt" scaling
rcr_run_light_series "$light_wt" scalingext

(cd "$dense_wt/examples" && rcr_stack build exchangealgebra-examples:exe:sim2) || rcr_die "dense build failed"
rcr_record_revision dense "$dense_wt" "$DENSE_REV"
dense_bin=$(cd "$dense_wt/examples" && stack exec --system-ghc -- which sim2) || rcr_die "cannot find sim2"
rcr_record_binary sim2 "$dense_bin"
if [ ! -s "$DENSE_TSV" ]; then
  printf 'rep\telapsed\tgc_elapsed\tmut_elapsed\tmaxres\tprod\n' > "$DENSE_TSV"
  : > "$RUN_DIR/dense-raw.log"
  spill_dir="$dense_wt/examples/basic/result/csv/simulateEx2/spill"
  mkdir -p "$dense_wt/examples/basic/result/csv/simulateEx2" || rcr_die "cannot create the sim2 output directory"
  rep=1
  while [ "$rep" -le "$RCR_REPS" ]; do
    out=$(mktemp "$RUN_DIR/dense-rep.XXXXXX") || rcr_die "mktemp failed"
    rm -rf "${spill_dir:?}"
    if [ "$(uname -s)" = Darwin ]; then
      (cd "$dense_wt" && /usr/bin/time -l stack --stack-yaml examples/stack.yaml \
        exec --system-ghc sim2 -- +RTS -N4 -s) > "$out" 2>&1
    else
      (cd "$dense_wt" && /usr/bin/time -v stack --stack-yaml examples/stack.yaml \
        exec --system-ghc sim2 -- +RTS -N4 -s) > "$out" 2>&1
    fi
    elapsed=$(awk '/^[[:space:]]*Total[[:space:]]+time/ { for (i=1;i<=NF;i++) if ($i == "elapsed)") { gsub(/s/,"",$(i-1)); print $(i-1); exit } }' "$out")
    gc_elapsed=$(awk '/^[[:space:]]*GC[[:space:]]+time/ { for (i=1;i<=NF;i++) if ($i == "elapsed)") { gsub(/s/,"",$(i-1)); print $(i-1); exit } }' "$out")
    mut_elapsed=$(awk '/^[[:space:]]*MUT[[:space:]]+time/ { for (i=1;i<=NF;i++) if ($i == "elapsed)") { gsub(/s/,"",$(i-1)); print $(i-1); exit } }' "$out")
    prod=$(awk '/Productivity/ { print $2; exit }' "$out")
    if [ "$(uname -s)" = Darwin ]; then
      maxres=$(awk '/maximum resident set size/ { print $1; exit }' "$out")
    else
      maxres=$(awk -F ': *' '/Maximum resident set size \(kbytes\)/ { printf "%.0f\n", $2 * 1024; exit }' "$out")
    fi
    : "${elapsed:=NA}" "${gc_elapsed:=NA}" "${mut_elapsed:=NA}" "${maxres:=NA}" "${prod:=NA}"
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$rep" "$elapsed" "$gc_elapsed" "$mut_elapsed" "$maxres" "$prod" >> "$DENSE_TSV"
    { echo "===== dense rep $rep ====="; cat "$out"; echo "===== end rep $rep ====="; } >> "$RUN_DIR/dense-raw.log"
    rm -f "$out"
    [ "$RCR_COOLDOWN" -gt 0 ] && sleep "$RCR_COOLDOWN"
    rep=$((rep + 1))
  done
else
  rcr_note "skip dense: rows already exist in $DENSE_TSV"
fi

dense_mean=$(awk -F '\t' 'NR>1 && $2!="NA" {s+=$2;n++} END {if(n) printf "%.6f",s/n}' "$DENSE_TSV")
sparse_mean=$(awk -F '\t' '$1=="scaling" && $2=="N200-K20-T50-seq-double" && $8=="ok" {s+=$5;n++} END {if(n) printf "%.6f",s/n}' "$RAW_TSV")
[ -n "$dense_mean" ] && [ -n "$sparse_mean" ] || rcr_die "cannot calculate dense/sparse comparison"
awk -v d="$dense_mean" -v s="$sparse_mean" 'BEGIN {printf "Fig. 1 dense/sparse: (dense mean / 4) / sparse mean = %.3f\n", (d/4)/s}'
echo "Normalization divides by 2 for 100 -> 50 periods and by 2 for two -> one scenario."
echo "This assumes linear scaling in both dimensions, deliberately favoring the dense baseline."
rcr_make_figures; rc=$?
rcr_finish_metadata
exit "$rc"
