#!/usr/bin/env bash
set -u

RCR_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$RCR_DIR/../../.." && pwd)"
die() { printf '[rcr-zip] ERROR: %s\n' "$*" >&2; exit 2; }

if [ "${1:-}" = -h ] || [ "${1:-}" = --help ]; then
  echo "Usage: $0"
  echo "Create pinned revision tarballs and rcr-artifact-<UTC date>.zip."
  exit 0
fi
[ "$#" -eq 0 ] || die "this script takes no arguments"
command -v git >/dev/null 2>&1 || die "git is required"
command -v zip >/dev/null 2>&1 || die "zip is required"
git -C "$REPO_ROOT" rev-parse --git-dir >/dev/null 2>&1 || die "run from a Git checkout"
mkdir -p "$RCR_DIR/revisions"

archive_revision() {
  name=$1; rev=$2
  git -C "$REPO_ROOT" archive --format=tar.gz --prefix="$name/" \
    -o "$RCR_DIR/revisions/$name.tar.gz" "$rev" || die "git archive failed for $name ($rev)"
}
archive_revision light 9d0d769
archive_revision heavy dd35d80
archive_revision dense v0.4.0.0

stage=$(mktemp -d "${TMPDIR:-/tmp}/exchangealgebra-rcr.XXXXXX") || die "mktemp failed"
trap 'rm -rf "${stage:?}"' EXIT HUP INT TERM
prefix="$stage/exchangealgebra-rcr"
mkdir -p "$prefix/examples/market/rcr/revisions"
cp "$REPO_ROOT/LICENSE" "$prefix/LICENSE" || die "cannot copy LICENSE"
git -C "$REPO_ROOT" ls-files examples/market/rcr | while IFS= read -r path; do
  mkdir -p "$prefix/$(dirname "$path")"
  cp "$REPO_ROOT/$path" "$prefix/$path" || exit 1
done || die "cannot stage tracked RCR files"
cp "$RCR_DIR"/revisions/*.tar.gz "$prefix/examples/market/rcr/revisions/" || die "cannot stage revisions"

zip_path="$RCR_DIR/rcr-artifact-$(date -u +%Y%m%d).zip"
(cd "$stage" && zip -qr "$zip_path" exchangealgebra-rcr) || die "zip creation failed"
printf '%s\n' "$zip_path"
