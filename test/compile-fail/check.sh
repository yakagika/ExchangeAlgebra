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
    local source=${2:-"$admission_root/test/compile-fail/$snippet.hs"}
    stack ${admission_stack_args[@]+"${admission_stack_args[@]}"} \
        --stack-yaml "$admission_root/stack.yaml" exec -- ghc \
        -fno-code -fforce-recomp -v0 -Wdeprecations -fdiagnostics-color=never -i \
        -hide-all-packages -package base -package text -package containers \
        -package exchangealgebra -outputdir "$admission_tmp" \
        "$source" \
        > "$admission_tmp/$snippet.log" 2>&1
}

reject() {
    local snippet=$1
    local reason=$2
    if compile "$snippet" "${3:-}"; then
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
if ! compile PublicExplicitImportClient; then
    cat "$admission_tmp/PublicExplicitImportClient.log"
    exit 1
fi
printf '[PASS] explicit-import client compiled\n'
reject Constructor 'Data constructor not in scope:.*Admitted|Not in scope:.*Admitted|Illegal term-level use of the type constructor.*Admitted'
reject AdmissionFailureConstructor 'Data constructor not in scope:.*AdmissionFailure|Not in scope:.*AdmissionFailure|Illegal term-level use of the type constructor.*AdmissionFailure'
reject RecordUpdate 'not a record selector|not a record field|Not in scope: record field.*admittedJournal'
reject Coerce "Couldn't match (representation|type)|Could not deduce.*Coercible"
reject RawDerivation "Couldn't match.*(Admitted|Journal)|Expected:.*Admitted"
reject HiddenModule 'hidden module'
if ! grep -Fq 'ExchangeAlgebra.IO.Input.Admission.Representation' "$admission_tmp/HiddenModule.log"; then
    printf '[FAIL] HiddenModule rejected a module other than the admission internal module\n'
    cat "$admission_tmp/HiddenModule.log"
    exit 1
fi

reject AlgConstructor 'non-bidirectional pattern synonym|not bidirectional'
reject AlgRecordUpdate 'non-bidirectional pattern synonym|not a record selector|not a record field'
reject AlgHiddenConstructor 'does not export any children|does not export.*(Single|Liner)'

reject JcciMacroTitle 'not in scope:.*GovernmentService'
# GHC reports the type-only name as an illegal term-level use when it is in scope.
reject JcciHiddenConstructor 'not in scope:.*JcciTitles|Illegal term-level use of the type constructor.*JcciTitles'

# Build a case from the multiline pragma and remove one branch for the negative client.
jcci_source="$admission_root/src/ExchangeAlgebra/Accounting/Account/Jcci.hs"
awk '
    BEGIN {
        print "{-# OPTIONS_GHC -Werror=incomplete-patterns #-}"
        print "module JcciComplete where"
        print "import ExchangeAlgebra.Accounting.Account.Jcci"
        print "match :: JcciTitles -> ()"
        print "match title = case title of"
    }
    /^\{-# COMPLETE([[:space:]]|$)/ {
        in_pragma = 1
        sub(/^\{-# COMPLETE[[:space:]]*/, "")
    }
    in_pragma {
        last = sub(/::.*/, "")
        gsub(/,/, " ")
        for (i = 1; i <= NF; i++) {
            if ($i == "Cash") cash++
            print "    " $i " -> ()"
        }
        if (last) in_pragma = 0
    }
    END { if (cash != 1 || in_pragma) exit 1 }
' "$jcci_source" > "$admission_tmp/JcciComplete.hs"
if ! compile JcciComplete "$admission_tmp/JcciComplete.hs"; then
    cat "$admission_tmp/JcciComplete.log"
    exit 1
fi
printf '[PASS] JcciComplete compiled as an exhaustive case\n'
sed -e 's/^module JcciComplete /module JcciIncomplete /' -e '/^    Cash -> ()$/d' \
    "$admission_tmp/JcciComplete.hs" > "$admission_tmp/JcciIncomplete.hs"
reject JcciIncomplete 'incomplete-patterns' "$admission_tmp/JcciIncomplete.hs"
