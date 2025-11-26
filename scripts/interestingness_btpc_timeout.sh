#!/usr/bin/env bash
#
# Interestingness script for the btpc.dpr regression:
# - FPC must compile the Pascal source and, when given input "1\n",
#   emit the known "Error 46: \"program\" expected ..." output.
# - Gwinn KGPC must compile the same source, and the resulting binary
#   must hang (timeout after 1 second) when run with the same input.
#
# Returns 0 only when both conditions hold, so that C-Reduce or similar
# tools keep cases exhibiting the problematic behaviour.

set -euo pipefail

# Optional arguments: [pascal-file] [stdin-file]. Default to btpc.dpr in CWD.
INPUT="${1:-btpc.dpr}"
STDIN_FILE="${2:-}"

if [[ ! -f "$INPUT" ]]; then
    echo "Input file '$INPUT' not found" >&2
    exit 2
fi

if [[ -n "$STDIN_FILE" ]]; then
    if [[ ! -f "$STDIN_FILE" ]]; then
        echo "stdin file '$STDIN_FILE' not found" >&2
        exit 2
    fi
    STDIN_CONTENT="$(cat "$STDIN_FILE")"
else
    STDIN_CONTENT=$'1\n'
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
KGPC="$REPO_ROOT/build/KGPC/kgpc"
RUNTIME_LIB="$REPO_ROOT/build/KGPC/libkgpc_runtime.a"

command -v fpc >/dev/null || { echo "Free Pascal Compiler (fpc) not found" >&2; exit 2; }
command -v timeout >/dev/null || { echo "'timeout' command not available" >&2; exit 2; }

if [[ ! -x "$KGPC" ]]; then
    echo "Gwinn compiler binary not found at $KGPC" >&2
    exit 2
fi

if [[ ! -f "$RUNTIME_LIB" ]]; then
    echo "Runtime library not found at $RUNTIME_LIB; build the project first" >&2
    exit 2
fi

TMP_DIR="$(mktemp -d)"
cleanup() {
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT

ASM_FILE="$TMP_DIR/kgpc_out.s"
BIN_KGPC="$TMP_DIR/kgpc_out.bin"
BIN_FPC="$TMP_DIR/fpc_out.bin"

ERROR_FRAGMENT='Error 46: "program" expected at line 2 at column 0'

# 1. Compile and run with FPC; output must match expectation exactly
fpc -v0 -o"$BIN_FPC" -FE"$TMP_DIR" "$INPUT" >/dev/null 2>&1 || exit 1
actual_output="$(printf "%s" "$STDIN_CONTENT" | "$BIN_FPC")" || exit 1
if [[ "$actual_output" != *"$ERROR_FRAGMENT"* ]]; then
    exit 1
fi

# 2. Compile with Gwinn KGPC
"$KGPC" "$INPUT" "$ASM_FILE" >/dev/null 2>&1 || exit 1
gcc -no-pie "$ASM_FILE" "$RUNTIME_LIB" -o "$BIN_KGPC" >/dev/null 2>&1 || exit 1

# 3. Interesting iff executing the KGPC binary times out after 1 second
set +e
printf "%s" "$STDIN_CONTENT" | timeout 1s "$BIN_KGPC" >/dev/null 2>&1
status=$?
set -e

if [[ $status -eq 124 ]]; then
    exit 0
fi

exit 1
