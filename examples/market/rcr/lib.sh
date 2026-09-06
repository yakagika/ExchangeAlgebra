#!/usr/bin/env bash
# Shared helpers for the TOMACS RCR reproduction scripts.

set -u

RCR_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$RCR_DIR/../../.." && pwd)"
LIGHT_REV=9d0d769
HEAVY_REV=dd35d80
DENSE_REV=v0.4.0.0

rcr_note() { printf '[rcr] %s\n' "$*"; }
rcr_warn() { printf '[rcr] WARNING: %s\n' "$*" >&2; }
rcr_die() { printf '[rcr] ERROR: %s\n' "$*" >&2; exit 2; }

rcr_usage_common() {
  cat <<'EOF'
  --smoke         Run one repetition of one point per series.
  --reps N        Repetitions per configuration (default: EA_REPS or 5).
  --no-64k        Omit the optional N=64000 scaling point.
  --x86_64        On Apple Silicon, run x86_64 Stack/GHC through Rosetta 2.
  --run-dir DIR   Output directory (default: rcr/runs/<UTC timestamp>).
  --cooldown S    Seconds between repetitions (default: EA_COOLDOWN or 10).
  -h, --help      Show help.
EOF
}

rcr_parse_options() {
  RCR_SMOKE=0
  RCR_REPS="${EA_REPS:-5}"
  RCR_OPT64=1
  RCR_X86=0
  RCR_RUN_DIR=""
  RCR_COOLDOWN="${EA_COOLDOWN:-10}"
  RCR_HELP=0
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --smoke) RCR_SMOKE=1; RCR_REPS=1 ;;
      --reps)
        [ "$#" -ge 2 ] || rcr_die "--reps requires a value"
        RCR_REPS=$2; shift ;;
      --no-64k) RCR_OPT64=0 ;;
      --x86_64) RCR_X86=1 ;;
      --run-dir)
        [ "$#" -ge 2 ] || rcr_die "--run-dir requires a value"
        RCR_RUN_DIR=$2; shift ;;
      --cooldown)
        [ "$#" -ge 2 ] || rcr_die "--cooldown requires a value"
        RCR_COOLDOWN=$2; shift ;;
      -h|--help) RCR_HELP=1 ;;
      *) rcr_die "unknown option: $1" ;;
    esac
    shift
  done
  case "$RCR_REPS" in *[!0-9]*|'') rcr_die "--reps must be a positive integer" ;; esac
  [ "$RCR_REPS" -gt 0 ] || rcr_die "--reps must be a positive integer"
  case "$RCR_COOLDOWN" in *[!0-9]*|'') rcr_die "--cooldown must be a non-negative integer" ;; esac
  if [ -z "$RCR_RUN_DIR" ]; then
    RCR_RUN_DIR="$RCR_DIR/runs/$(date -u +%Y%m%dT%H%M%SZ)"
  fi
}

rcr_init_run() {
  mkdir -p "$RCR_RUN_DIR" || rcr_die "cannot create run directory: $RCR_RUN_DIR"
  RUN_DIR="$(cd "$RCR_RUN_DIR" && pwd)"
  RAW_TSV="$RUN_DIR/raw.tsv"
  DENSE_TSV="$RUN_DIR/dense-raw.tsv"
  ENV_FILE="$RUN_DIR/env.txt"
  if [ ! -f "$RAW_TSV" ]; then
    printf 'series\tconfig\tcores\trep\telapsed\tmaxres\tprod\tstatus\n' > "$RAW_TSV"
  fi
  RCR_START_EPOCH=$(date +%s)
  rcr_setup_arch
  rcr_record_base_metadata
}

rcr_setup_arch() {
  if [ "$RCR_X86" -ne 1 ]; then
    return
  fi
  [ "$(uname -s)" = Darwin ] || rcr_die "--x86_64 is supported only on macOS Apple Silicon"
  [ "$(uname -m)" = arm64 ] || rcr_die "--x86_64 requires an Apple Silicon (arm64) host"
  EA_STACK_X86="${EA_STACK_X86:-/usr/local/bin/stack}"
  [ -x "$EA_STACK_X86" ] || rcr_die "x86_64 stack is not executable: $EA_STACK_X86"
  file -L "$EA_STACK_X86" | grep -q 'x86_64' || rcr_die "EA_STACK_X86 is not an x86_64 binary: $EA_STACK_X86"
  export EA_STACK_X86
  mkdir -p "$RUN_DIR/bin"
  cat > "$RUN_DIR/bin/stack" <<'EOF'
#!/usr/bin/env bash
set -u
exec arch -x86_64 "${EA_STACK_X86:?EA_STACK_X86 is not set}" "$@"
EOF
  chmod +x "$RUN_DIR/bin/stack"
  PATH="$RUN_DIR/bin:$PATH"
  if [ -d "$HOME/.stack/programs/x86_64-osx/ghc-9.10.2/bin" ]; then
    PATH="$HOME/.stack/programs/x86_64-osx/ghc-9.10.2/bin:$PATH"
  fi
  export PATH
}

rcr_stack() {
  command -v stack >/dev/null 2>&1 || rcr_die "stack is required"
  stack "$@"
}

rcr_record_base_metadata() {
  # reproduce-all.sh runs the per-figure scripts on one run dir; record the
  # environment once (a rerun on an existing run dir also keeps the original).
  [ -f "$ENV_FILE" ] && return
  {
    echo "## RCR reproduction environment"
    echo "started: $(date)"
    echo "started_utc: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "uname: $(uname -a)"
    echo "machine: $(uname -m)"
    if command -v sw_vers >/dev/null 2>&1; then sw_vers; fi
    if [ -r /etc/os-release ]; then cat /etc/os-release; fi
    if [ "$(uname -s)" = Darwin ]; then
      sysctl -n machdep.cpu.brand_string hw.ncpu hw.memsize \
        hw.perflevel0.physicalcpu hw.perflevel1.physicalcpu 2>&1 || true
      echo "load: $(sysctl -n vm.loadavg 2>/dev/null || uptime)"
    else
      command -v lscpu >/dev/null 2>&1 && lscpu
      command -v free >/dev/null 2>&1 && free -g
      echo "load: $(cat /proc/loadavg 2>/dev/null || uptime)"
    fi
    echo "stack: $(stack --version 2>&1 | head -1)"
    echo "options: smoke=$RCR_SMOKE reps=$RCR_REPS include_64k=$RCR_OPT64 x86_64=$RCR_X86 cooldown=$RCR_COOLDOWN run_dir=$RUN_DIR"
  } >> "$ENV_FILE"
}

rcr_finish_metadata() {
  # Under reproduce-all.sh only the final invocation records completion.
  [ "${RCR_DEFER_ASSESS:-0}" = 1 ] && return
  rcr_end=$(date +%s)
  {
    echo "completed: $(date)"
    echo "completed_utc: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "wall: $(( (rcr_end - RCR_START_EPOCH) / 60 )) min"
  } >> "$ENV_FILE"
}

rcr_preflight_revision_sources() {
  if command -v git >/dev/null 2>&1 && git -C "$REPO_ROOT" rev-parse --git-dir >/dev/null 2>&1; then
    return
  fi
  for rcr_name in light heavy dense; do
    [ -f "$RCR_DIR/revisions/$rcr_name.tar.gz" ] || rcr_die \
      "no Git checkout and missing revisions/$rcr_name.tar.gz; run from a GitHub clone or use the artifact zip"
  done
}

ensure_revision() {
  rcr_name=$1
  rcr_rev=$2
  rcr_dir="$RCR_DIR/.rcr-worktrees/$rcr_name"
  if command -v git >/dev/null 2>&1 && git -C "$REPO_ROOT" rev-parse --git-dir >/dev/null 2>&1; then
    rcr_expected=$(git -C "$REPO_ROOT" rev-parse "$rcr_rev^{commit}") || rcr_die "cannot resolve revision $rcr_rev"
    if [ -d "$rcr_dir" ]; then
      rcr_actual=$(git -C "$rcr_dir" rev-parse HEAD 2>/dev/null || true)
      [ "$rcr_actual" = "$rcr_expected" ] || rcr_die \
        "$rcr_dir exists at a different revision; remove it and retry"
    else
      mkdir -p "$RCR_DIR/.rcr-worktrees"
      git -C "$REPO_ROOT" worktree add --detach "$rcr_dir" "$rcr_rev" || rcr_die "cannot create worktree for $rcr_name"
    fi
  else
    rcr_tar="$RCR_DIR/revisions/$rcr_name.tar.gz"
    [ -f "$rcr_tar" ] || rcr_die \
      "missing $rcr_tar; run from a GitHub clone or use the artifact zip"
    if [ -d "$rcr_dir" ]; then
      [ -f "$rcr_dir/.rcr-revision" ] && [ "$(cat "$rcr_dir/.rcr-revision")" = "$rcr_rev" ] || rcr_die \
        "$rcr_dir exists without the expected revision marker; remove it and retry"
    else
      mkdir -p "$rcr_dir"
      tar xzf "$rcr_tar" -C "$rcr_dir" --strip-components=1 || rcr_die "cannot extract $rcr_tar"
      printf '%s\n' "$rcr_rev" > "$rcr_dir/.rcr-revision"
    fi
  fi
  REPLY_REVISION_DIR=$rcr_dir
}

rcr_revision_head() {
  rcr_dir=$1
  rcr_fallback=$2
  if command -v git >/dev/null 2>&1 && git -C "$rcr_dir" rev-parse HEAD >/dev/null 2>&1; then
    git -C "$rcr_dir" rev-parse HEAD
  else
    printf 'tarball:%s\n' "$rcr_fallback"
  fi
}

rcr_record_revision() {
  rcr_name=$1
  rcr_dir=$2
  rcr_rev=$3
  # Each per-figure script re-records the revisions it uses; keep one block per revision.
  grep -q "^## $rcr_name revision" "$ENV_FILE" 2>/dev/null && return
  rcr_stack_dir=$rcr_dir
  [ "$rcr_name" = dense ] && rcr_stack_dir="$rcr_dir/examples"
  {
    echo "## $rcr_name revision"
    echo "head: $(rcr_revision_head "$rcr_dir" "$rcr_rev")"
    echo "ghc: $(cd "$rcr_stack_dir" && stack exec --system-ghc -- ghc --version 2>&1)"
    echo "ghc_path: $(cd "$rcr_stack_dir" && stack exec --system-ghc -- which ghc 2>&1)"
  } >> "$ENV_FILE"
}

rcr_record_binary() {
  rcr_name=$1
  rcr_bin=$2
  grep -q "^$rcr_name binary: " "$ENV_FILE" 2>/dev/null && return
  {
    echo "$rcr_name binary: $rcr_bin"
    file "$rcr_bin"
  } >> "$ENV_FILE" 2>&1
}

rcr_has_series() {
  rcr_series=$1
  awk -F '\t' -v s="$rcr_series" 'NR > 1 && $1 == s { found=1; exit } END { exit !found }' "$RAW_TSV"
}

rcr_run_light_series() {
  rcr_wt=$1
  rcr_series=$2
  if rcr_has_series "$rcr_series"; then
    rcr_note "skip $rcr_series: rows already exist in $RAW_TSV"
    return
  fi
  rcr_note "run light series: $rcr_series"
  rcr_smoke_env=0
  [ "$RCR_SMOKE" -eq 1 ] && rcr_smoke_env=1
  rcr_opt_env=0
  [ "$RCR_OPT64" -eq 1 ] && rcr_opt_env=1
  (cd "$rcr_wt" && env EA_REPS="$RCR_REPS" EA_SMOKE="$rcr_smoke_env" \
    EA_OPT64="$rcr_opt_env" EA_COOLDOWN="$RCR_COOLDOWN" EA_CORES="${EA_LIGHT_CORES:-1 4 8 10 12 14}" \
    ./examples/market/run-round4.sh "$rcr_series") || rcr_die "light series failed: $rcr_series"
  rcr_source="$rcr_wt/examples/market/result/round4-raw.tsv"
  [ -f "$rcr_source" ] || rcr_die "run-round4.sh did not create $rcr_source"
  awk -F '\t' 'BEGIN { OFS="\t" } NR > 1 { print $0, ($5 != "NA" ? "ok" : "error") }' \
    "$rcr_source" >> "$RAW_TSV"
  rcr_log="$rcr_wt/examples/market/result/raw-round4-$rcr_series.log"
  [ -f "$rcr_log" ] && cp "$rcr_log" "$RUN_DIR/"
}

rcr_host_cores() {
  if [ "$(uname -s)" = Darwin ]; then
    sysctl -n hw.ncpu 2>/dev/null || echo 1
  elif command -v nproc >/dev/null 2>&1; then
    nproc
  else
    getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1
  fi
}

rcr_filter_cores() {
  rcr_requested=$1
  rcr_limit=$(rcr_host_cores)
  REPLY_CORES=""
  # shellcheck disable=SC2086
  for rcr_core in $rcr_requested; do
    case "$rcr_core" in *[!0-9]*|'') rcr_die "invalid core count: $rcr_core" ;; esac
    if [ "$rcr_core" -le "$rcr_limit" ]; then
      REPLY_CORES="$REPLY_CORES $rcr_core"
    else
      rcr_warn "skip ${rcr_core}-core point: host reports $rcr_limit available cores"
    fi
  done
  REPLY_CORES=${REPLY_CORES# }
}

rcr_make_figures() {
  # Under reproduce-all.sh the per-figure scripts must not judge (or abort on)
  # a partial run; the final invocation in reproduce-all.sh is authoritative.
  if [ "${RCR_DEFER_ASSESS:-0}" = 1 ]; then
    rcr_note "assessment deferred to reproduce-all.sh"
    return 0
  fi
  rcr_smoke_arg=""
  [ "$RCR_SMOKE" -eq 1 ] && rcr_smoke_arg=--smoke
  if [ -n "$rcr_smoke_arg" ]; then
    python3 "$RCR_DIR/make_figures.py" --replicate --raw "$RAW_TSV" \
      --dense-raw "$DENSE_TSV" --out-dir "$RUN_DIR" "$rcr_smoke_arg"
  else
    python3 "$RCR_DIR/make_figures.py" --replicate --raw "$RAW_TSV" \
      --dense-raw "$DENSE_TSV" --out-dir "$RUN_DIR"
  fi
}

rcr_common_args() {
  RCR_COMMON_ARGS=(--reps "$RCR_REPS" --run-dir "$RUN_DIR" --cooldown "$RCR_COOLDOWN")
  [ "$RCR_SMOKE" -eq 1 ] && RCR_COMMON_ARGS+=(--smoke)
  [ "$RCR_OPT64" -eq 0 ] && RCR_COMMON_ARGS+=(--no-64k)
  [ "$RCR_X86" -eq 1 ] && RCR_COMMON_ARGS+=(--x86_64)
}
