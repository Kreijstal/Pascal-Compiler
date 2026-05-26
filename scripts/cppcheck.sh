#!/usr/bin/env bash
# Run cppcheck against the meson compile_commands.json with the project
# suppression baseline. Exits 1 on any finding not in .cppcheck-suppressions
# so it can gate CI directly.
#
# Usage:
#   scripts/cppcheck.sh [BUILD_DIR]
#
# BUILD_DIR defaults to ./build. The build dir must contain a
# compile_commands.json (meson writes one by default).
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BUILD_DIR="${1:-${REPO_ROOT}/build}"
[[ $# -gt 0 ]] && shift
SUPPRESSIONS="${REPO_ROOT}/.cppcheck-suppressions"
DB="${BUILD_DIR}/compile_commands.json"

if [[ ! -f "${DB}" ]]; then
  echo "error: ${DB} not found. Run 'meson setup ${BUILD_DIR}' first." >&2
  exit 2
fi
if ! command -v cppcheck >/dev/null 2>&1; then
  echo "error: cppcheck not installed (pacman -S cppcheck / apt install cppcheck)" >&2
  exit 2
fi

cd "${REPO_ROOT}"

cppcheck \
  --project="${DB}" \
  --enable=warning,performance,portability \
  --inline-suppr \
  --suppressions-list="${SUPPRESSIONS}" \
  --suppress=missingIncludeSystem \
  --suppress='*:*/tests/test_cases/*' \
  --suppress='*:*/cparser/acutest.h' \
  --suppress='*:*/build*/*' \
  --suppress='*:*/.docs-venv/*' \
  --error-exitcode=1 \
  -j"$(nproc)" \
  --template='{file}:{line}:{column}: {severity}: {id}: {message}' \
  "$@"
