#!/usr/bin/env bash
# Compile and run the manual examples against the pinned Hackage package.
set -euo pipefail
exec python3 "$(dirname "$0")/check-code.py" "$@"
