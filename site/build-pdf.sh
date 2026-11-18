#!/bin/sh
set -eu
cd "$(dirname "$0")"
export TEXMFCACHE="${TEXMFCACHE:-$PWD/book-pdf/tex-cache}"
export TEXMFVAR="${TEXMFVAR:-$TEXMFCACHE}"
mkdir -p book-pdf "$TEXMFCACHE"
pandoc src/algebra/definitions.md --pdf-engine=lualatex -V geometry:margin=1in -o book-pdf/definitions.pdf
