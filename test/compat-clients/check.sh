#!/usr/bin/env bash
set -euo pipefail

compat_root=$(cd "$(dirname "$0")/../.." && pwd)
compat_tmp=$(mktemp -d "${TMPDIR:-/tmp}/ea-compat-p2.XXXXXX")
trap 'rm -rf "$compat_tmp"' EXIT
read -r -a compat_stack_args <<< "${STACK_ARGS:-}"

cd "$compat_tmp"
for compat_client in OldPath NewPath MixedPath; do
    stack ${compat_stack_args[@]+"${compat_stack_args[@]}"} \
        --stack-yaml "$compat_root/stack.yaml" exec -- ghc \
        -fno-code -fforce-recomp -v0 -fdiagnostics-color=never -i \
        -i"$compat_root/test/compat-clients" -hide-all-packages \
        -package base -package exchangealgebra \
        -outputdir "$compat_tmp" \
        "$compat_root/test/compat-clients/$compat_client.hs"
done
printf '[PASS] old, new, and mixed clients compiled\n'
stack ${compat_stack_args[@]+"${compat_stack_args[@]}"} \
    --stack-yaml "$compat_root/stack.yaml" exec -- ghc \
    -fforce-recomp -v0 -fdiagnostics-color=never -i \
    -i"$compat_root/test/compat-clients" \
    -hide-all-packages -package base -package hashable -package exchangealgebra \
    -outputdir "$compat_tmp" -o "$compat_tmp/instance-client" \
    "$compat_root/test/compat-clients/InstanceMain.hs"
"$compat_tmp/instance-client"
