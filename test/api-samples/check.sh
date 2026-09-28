#!/usr/bin/env bash
set -euo pipefail

sample_root=$(cd "$(dirname "$0")/../.." && pwd)
sample_tmp=$(mktemp -d "${TMPDIR:-/tmp}/ea-api-samples.XXXXXX")
trap 'rm -rf "$sample_tmp"' EXIT
read -r -a sample_stack_args <<< "${STACK_ARGS:-}"

cd "$sample_tmp"
for sample in Bookkeeping MultiPeriod Readout Admission Network CustomBase CustomAccount; do
    stack ${sample_stack_args[@]+"${sample_stack_args[@]}"} \
        --stack-yaml "$sample_root/stack.yaml" exec -- ghc \
        -fforce-recomp -v0 -fdiagnostics-color=never -i \
        -hide-all-packages -package base -package exchangealgebra \
        -package array -package bytestring -package containers -package text -package time \
        -package hashable -package random \
        -outputdir "$sample_tmp" -o "$sample_tmp/$sample" \
        "$sample_root/test/api-samples/$sample.hs"
    "$sample_tmp/$sample"
    printf '[PASS] %s compiled and ran\n' "$sample"
done
