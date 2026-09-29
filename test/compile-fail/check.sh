#!/usr/bin/env bash
# From the repository root:
#   stack build --test --bench --no-run-benchmarks
#   bash test/compile-fail/check.sh
# Set STACK_ARGS to the global stack options used for that build (for example
# "--system-ghc --no-install-ghc" in CI) so the check reads the same package db.
# Compile as an external installed-package client with source lookup disabled.
# Logs and compiler artifacts live in a temporary directory removed on exit.
set -euo pipefail

admission_root=$(cd "$(dirname "$0")/../.." && pwd)
admission_tmp=$(mktemp -d "${TMPDIR:-/tmp}/ea-admission-boundary.XXXXXX")
trap 'rm -rf "$admission_tmp"' EXIT
read -r -a admission_stack_args <<< "${STACK_ARGS:-}"

compile() {
    local snippet=$1
    stack ${admission_stack_args[@]+"${admission_stack_args[@]}"} \
        --stack-yaml "$admission_root/stack.yaml" exec -- ghc \
        -fno-code -fforce-recomp -v0 -fdiagnostics-color=never -i \
        -hide-all-packages -package base -package text -package containers \
        -package exchangealgebra -outputdir "$admission_tmp" \
        "$admission_root/test/compile-fail/$snippet.hs" \
        > "$admission_tmp/$snippet.log" 2>&1
}

reject() {
    local snippet=$1
    local reason=$2
    if compile "$snippet"; then
        printf '[FAIL] %s compiled unexpectedly\n' "$snippet"
        exit 1
    fi
    if ! grep -Eq "$reason" "$admission_tmp/$snippet.log"; then
        printf '[FAIL] %s failed for an unexpected reason\n' "$snippet"
        cat "$admission_tmp/$snippet.log"
        exit 1
    fi
    printf '[PASS] %s rejected for the intended reason\n' "$snippet"
}

cd "$admission_tmp"
if ! compile PublicClient; then
    cat "$admission_tmp/PublicClient.log"
    exit 1
fi
printf '[PASS] public API client compiled\n'
reject Constructor 'Data constructor not in scope:.*Admitted|Not in scope:.*Admitted|Illegal term-level use of the type constructor.*Admitted'
reject RecordUpdate 'not a record selector|not a record field|Not in scope: record field.*admittedJournal'
reject Coerce "Couldn't match (representation|type)|Could not deduce.*Coercible"
reject RawDerivation "Couldn't match.*(Admitted|Journal)|Expected:.*Admitted"
reject HiddenModule 'hidden module'
if ! grep -Fq 'ExchangeAlgebra.IO.Input.Admission.Representation' "$admission_tmp/HiddenModule.log"; then
    printf '[FAIL] HiddenModule rejected a module other than the admission internal module\n'
    cat "$admission_tmp/HiddenModule.log"
    exit 1
fi
