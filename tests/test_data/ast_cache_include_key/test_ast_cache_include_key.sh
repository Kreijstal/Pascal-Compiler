#!/usr/bin/env bash
#
# Regression test for the AST cache include-path key.
#
# The AST cache (--pp-cache-dir) keys each parsed unit on its source path,
# -D defines, target ABI and compiler mtime.  It originally did NOT key on
# the -I include search paths.  A unit's parsed AST depends on which files
# its {$i <name>} directives resolve to, and that resolution is driven by
# -I.  So the same unit compiled under two different -I configurations
# collided on one cache file: the second compile silently reused the first's
# stale AST.
#
# That is exactly what broke FPC 3.2.2: rtl/unix/sysutils.pp includes
# rtl/objpas/sysutils/sysencodingh.inc, whose TEncoding class has a
# self-referential static dynamic-array field
# `FSystemEncodings: array of TEncoding; static;`.  A stale cache entry made
# that field mis-resolve, so SetLength/Length/High/Low on it failed with
# "first argument to SetLength must be a dynamic array variable" (plus a
# follow-on parser desync flagging an unrelated `if` as a for-in loop).
#
# This test reproduces the collision in miniature: myunit.pp declares its
# static field FData via {$i field.inc}, and two include dirs provide
# different definitions of field.inc (dynamic array vs plain Integer).
# Compiling against incscalar (Integer) must fail SetLength; compiling
# against incdyn (array) must then succeed even though they share the cache
# dir.  Before the fix, the second compile reused the first's AST and failed.

set -u

SRC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
KGPC="${KGPC_BIN:-}"
if [ -z "$KGPC" ]; then
  # Fall back to the build tree relative to the repo root.
  KGPC="${MESON_BUILD_ROOT:-$SRC_DIR/../../../build}/KGPC/kgpc"
fi
if [ ! -x "$KGPC" ]; then
  echo "FAIL: kgpc binary not found/executable at: $KGPC" >&2
  exit 1
fi

REPO_ROOT="$(cd "$SRC_DIR/../../.." && pwd)"
export KGPC_STDLIB="${KGPC_STDLIB:-$REPO_ROOT/KGPC/Units/system.p}"
export MESON_SOURCE_ROOT="${MESON_SOURCE_ROOT:-$REPO_ROOT}"

CACHE="$(mktemp -d)"
trap 'rm -rf "$CACHE"' EXIT

run() {
  # $1 = include dir, $2 = output .s
  "$KGPC" "$SRC_DIR/prog.pp" "$2" \
    --pp-cache-dir="$CACHE" \
    -I"$SRC_DIR/$1" -Fu"$SRC_DIR" > "$CACHE/out.log" 2>&1
  return $?
}

# Step 1: scalar variant — SetLength(Integer,...) must be rejected.
run incscalar "$CACHE/p1.s"
rc1=$?
if [ "$rc1" -eq 0 ]; then
  echo "FAIL: scalar variant unexpectedly compiled (SetLength on Integer should fail)" >&2
  cat "$CACHE/out.log" >&2
  exit 1
fi

# Step 2: dynamic-array variant in the SAME cache dir — must succeed.
# Before the include-path-key fix this reused step 1's poisoned AST and
# failed with "first argument to SetLength must be a dynamic array variable".
run incdyn "$CACHE/p2.s"
rc2=$?
if [ "$rc2" -ne 0 ]; then
  echo "FAIL: dynamic-array variant failed; stale AST reused across -I configs" >&2
  cat "$CACHE/out.log" >&2
  exit 1
fi

echo "PASS: AST cache distinguishes -I include configurations"
exit 0
