#!/usr/bin/env bash
set -euo pipefail

repo_dir=$(cd "$(dirname "$0")/.." && pwd)
expected="$repo_dir/test/fixtures/account-semantics-050/consumer-inventory.txt"
actual=$(mktemp "${TMPDIR:-/tmp}/exchangealgebra-account-consumers.XXXXXX")
trap 'rm -f "$actual"' EXIT

cd "$repo_dir"
rg -l '\b(whatDiv|whichSide|whatPIMO|aiDivision|aiRoles|aiPostingCapability|aiDivisionSemantics|aiHomeSideSemantics|aiReportingEligibility|asem[A-Za-z]*|AccountSemantics|accountSemantics|AccountRole|PostingCapability|DivisionSemantics|HomeSideSemantics|ReportingEligibility|describeAccount|allAccountInfos|projWith[A-Za-z]*|finalStockRule|finalStockTransfer|checkedEntry|checkedJournal|AccountSpec|accountSpec|asDivision|asClosing|classifyAccountDivision|defaultSide|isContra|fixedCurrent|bsRows|plRows)\b' \
  src test examples tools \
  --glob '*.hs' \
  --glob '*.py' \
  --glob '!**/audit-eval/arms/**' \
  --glob '!**/audit-eval/metrics/**' \
  | sort > "$actual"

diff -u "$expected" "$actual"
echo "account-semantics consumer inventory: PASS"
