#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 || $# -gt 2 ]]; then
  echo "Usage: $0 <test-base-or-path> [build-dir]" >&2
  echo "Examples:" >&2
  echo "  $0 tdd_guidhelper_create_overload" >&2
  echo "  $0 tests/test_cases/tdd_guidhelper_create_overload.p" >&2
  exit 2
fi

TEST_ARG="$1"
BUILD_DIR="${2:-build}"

if [[ "$TEST_ARG" == *.p ]]; then
  TEST_FILE="$TEST_ARG"
else
  TEST_FILE="tests/test_cases/${TEST_ARG}.p"
fi

if [[ ! -f "$TEST_FILE" ]]; then
  echo "Missing test file: $TEST_FILE" >&2
  exit 2
fi

BASE_NAME="$(basename "$TEST_FILE" .p)"
INPUT_FILE="tests/test_cases/${BASE_NAME}.input"
EXPECTED_FILE="tests/test_cases/${BASE_NAME}.expected"

KGPC_BIN="${BUILD_DIR}/KGPC/kgpc"
RUNTIME_LIB="${BUILD_DIR}/KGPC/libkgpc_runtime.a"

if [[ ! -x "$KGPC_BIN" ]]; then
  echo "Missing kgpc binary: $KGPC_BIN" >&2
  exit 2
fi
if [[ ! -f "$RUNTIME_LIB" ]]; then
  echo "Missing runtime library: $RUNTIME_LIB" >&2
  exit 2
fi
if ! command -v fpc >/dev/null 2>&1; then
  echo "Missing fpc in PATH" >&2
  exit 2
fi

WORK_DIR="$(mktemp -d)"
cleanup() { rm -rf "$WORK_DIR"; }
trap cleanup EXIT

KGPC_ASM="$WORK_DIR/${BASE_NAME}.kgpc.s"
KGPC_EXE="$WORK_DIR/${BASE_NAME}.kgpc.exe"
FPC_EXE="$WORK_DIR/${BASE_NAME}.fpc.exe"
KGPC_OUT="$WORK_DIR/${BASE_NAME}.kgpc.out"
FPC_OUT="$WORK_DIR/${BASE_NAME}.fpc.out"

run_with_optional_input() {
  local exe="$1"
  local out="$2"
  if [[ -f "$INPUT_FILE" ]]; then
    "$exe" < "$INPUT_FILE" > "$out"
  else
    "$exe" > "$out"
  fi
}

normalize_file() {
  local file="$1"
  tr -d '\r' < "$file"
}

echo "[1/5] KGPC compile"
"$KGPC_BIN" "$TEST_FILE" "$KGPC_ASM" --emit-link-args >/dev/null 2>"$WORK_DIR/kgpc.compile.stderr"

LINK_ARGS=()
if grep -q '^KGPC_LINK_ARGS:' "$WORK_DIR/kgpc.compile.stderr"; then
  RAW_LINK_ARGS="$(grep '^KGPC_LINK_ARGS:' "$WORK_DIR/kgpc.compile.stderr" | sed 's/^KGPC_LINK_ARGS:[[:space:]]*//')"
  if [[ -n "$RAW_LINK_ARGS" ]]; then
    # shellcheck disable=SC2206
    LINK_ARGS=($RAW_LINK_ARGS)
  fi
fi

echo "[2/5] KGPC link/run"
gcc -O2 -no-pie -o "$KGPC_EXE" "$KGPC_ASM" "$RUNTIME_LIB" "${LINK_ARGS[@]}"
run_with_optional_input "$KGPC_EXE" "$KGPC_OUT"

echo "[3/5] FPC compile/run"
fpc -v0 -o"$FPC_EXE" "$TEST_FILE" >/dev/null
run_with_optional_input "$FPC_EXE" "$FPC_OUT"

echo "[4/5] Compare KGPC vs FPC output"
if ! diff -u <(normalize_file "$FPC_OUT") <(normalize_file "$KGPC_OUT"); then
  echo "Output mismatch between FPC and KGPC for $BASE_NAME" >&2
  exit 1
fi

echo "[5/5] Optional check against .expected"
if [[ -f "$EXPECTED_FILE" ]]; then
  if ! diff -u "$EXPECTED_FILE" <(normalize_file "$KGPC_OUT"); then
    echo "KGPC output differs from expected file: $EXPECTED_FILE" >&2
    exit 1
  fi
fi

echo "PASS: $BASE_NAME (KGPC output matches FPC${EXPECTED_FILE:+ and .expected})"
