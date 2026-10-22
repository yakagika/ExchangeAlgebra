#!/usr/bin/env bash
set -euo pipefail
sample_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd "$sample_dir/../../.." && pwd)"
build_dir="$(mktemp -d "${TMPDIR:-/tmp}/ea-harness.XXXXXX")"
trap 'rm -rf "$build_dir"' EXIT
cd "$repo_dir"
# STACK_ARGS is split into shell words, as a list of Stack options, without eval.
read -r -a stack_args <<< "${STACK_ARGS:-}"
stack ${stack_args[@]+"${stack_args[@]}"} exec -- ghc \
    -Wall -Werror -package exchangealgebra -package aeson -package text \
    -package containers -package bytestring \
    -i"$sample_dir" -outputdir "$build_dir" \
    "$sample_dir/Main.hs" -o "$build_dir/harness"
"$build_dir/harness" "$sample_dir/CALLS.md" "$sample_dir/calls.schema.json"
