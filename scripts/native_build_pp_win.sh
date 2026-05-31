#!/usr/bin/env bash
# native_build_pp_win.sh — compile FPC's pp.pas into a native Win64 pp_win.exe
# entirely on a native Windows host (MSYS2), using KGPC.  Self-contained:
#
#   1. KGPC (native build) compiles FPCSource/compiler/pp.pas against the
#      Windows RTL + compiler include/unit dirs  ->  pp_win.s   (+ link args)
#   2. MinGW-w64 'as' assembles pp_win.s          ->  pp_win.o
#   3. the same MinGW-w64 gcc builds KGPC's C runtime
#   4. gcc links pp_win.o + runtime               ->  pp_win.exe
#
# TOOLCHAIN (critical): must be a MinGW-w64 gcc that defines _WIN64 — i.e. the
# UCRT64 / MINGW64 / CLANG64 subsystem, NOT the MSYS2 'msys' posix gcc.  The
# msys gcc is a Cygwin fork: it does not define _WIN64, so KGPC's Win64
# indirect-entry-information shim (kgpc_fpc_init_win_entry_info, guarded on
# _WIN64/__MINGW*) compiles to a no-op, _FPC_SysInstance/_FPC_TlsKey stay NULL,
# and win64/system.pp faults at startup; its Cygwin ABI also corrupts argv.
# Run e.g.  `. shell ucrt64`  before this script (so gcc/as/ar resolve to the
# UCRT64 toolchain), or set MINGW_PREFIX explicitly.  The default follows the
# active MSYS2 subsystem ($MSYSTEM_PREFIX) and finally /ucrt64 — no drive letter
# is ever hard-coded.
#
# The native KGPC (a Win64 .exe) is driven from this bash script on purpose:
# MSYS2 auto-translates POSIX path arguments (e.g. /tmp/pp_win.s) to native
# Windows paths when launching a native program from the shell.  (Driving the
# same .exe from a *native* python would skip that translation and the output
# path would resolve against the wrong drive root.)
set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO"

MINGW_PREFIX="${MINGW_PREFIX:-${MSYSTEM_PREFIX:-/ucrt64}}"
export PATH="$MINGW_PREFIX/bin:$PATH"
CC="$MINGW_PREFIX/bin/gcc"
AS="$MINGW_PREFIX/bin/as"
AR="$MINGW_PREFIX/bin/ar"

# Refuse a non-MinGW gcc: a missing _WIN64 means the entry-info shim is a no-op
# and the .exe crashes at startup (see header).
if ! "$CC" -dM -E - </dev/null 2>/dev/null | grep -q "define _WIN64"; then
  echo "ERROR: $CC does not define _WIN64 — not a MinGW-w64 compiler." >&2
  echo "       Run '. shell ucrt64' (or set MINGW_PREFIX) and retry." >&2
  exit 1
fi

KGPC="${KGPC:-$REPO/build-ucrt64/KGPC/kgpc.exe}"
if [ ! -x "$KGPC" ]; then
  echo "ERROR: native KGPC not found at $KGPC" >&2
  echo "       Build it first: meson compile -C build-ucrt64 kgpc" >&2
  exit 1
fi

WORK="${WORK:-/tmp}"
PP_S="${PP_S:-$WORK/pp_win.s}"
PP_O="${PP_O:-$WORK/pp_win.o}"
PP_EXE="${PP_EXE:-$WORK/pp_win.exe}"
WINRT_DIR="${WINRT_DIR:-$WORK/winrt_win64}"
PP_CACHE="${PP_CACHE:-$WORK/win_cache_pp}"
KGPC_LOG="${KGPC_LOG:-$WORK/kgpc_pp_win.log}"
mkdir -p "$WINRT_DIR" "$PP_CACHE"

# --- 1. KGPC: pp.pas -> pp_win.s ------------------------------------------
FPC=FPCSource
DEFINES=(
  -DCPU64 -DCPUX86_64 -Dx86_64 -DFPC -DWINDOWS -DWIN64 -DMSWINDOWS
  -DFPC_HAS_INDIRECT_ENTRY_INFORMATION -DFPC_HAS_TYPE_EXTENDED -DSUPPORT_EXTENDED
  -DFPC_BOOTSTRAP_INDIRECT_ENTRY -Sg
)
INC_DIRS=(
  rtl/objpas rtl/objpas/sysutils rtl/objpas/classes rtl/inc rtl/x86_64
  rtl/win rtl/win/wininc rtl/win64 rtl/win64/x86_64
  compiler compiler/x86 compiler/x86_64
)
UNIT_DIRS=(
  rtl/win rtl/win64 rtl/objpas rtl/inc rtl/objpas/sysutils rtl/objpas/classes
  rtl/charmaps compiler compiler/x86 compiler/x86_64 compiler/systems
)
KFLAGS=( --no-stdlib --target-windows "${DEFINES[@]}" )
for d in "${INC_DIRS[@]}";  do KFLAGS+=( "-I$FPC/$d" ); done
for d in "${UNIT_DIRS[@]}"; do KFLAGS+=( "-Fu$FPC/$d" ); done
KFLAGS+=( "--pp-cache-dir=$PP_CACHE" )

echo "[kgpc] $FPC/compiler/pp.pas -> $PP_S"
rm -f "$PP_S"
"$KGPC" "$FPC/compiler/pp.pas" "$PP_S" --emit-link-args "${KFLAGS[@]}" 2>"$KGPC_LOG" || {
  echo "ERROR: KGPC failed; tail of $KGPC_LOG:" >&2; tail -20 "$KGPC_LOG" >&2; exit 1; }
if [ ! -f "$PP_S" ]; then
  echo "ERROR: KGPC produced no $PP_S; tail of $KGPC_LOG:" >&2
  tail -20 "$KGPC_LOG" >&2; exit 1
fi
LINK_ARGS="$(grep '^KGPC_LINK_ARGS:' "$KGPC_LOG" | tail -1 | sed 's/^KGPC_LINK_ARGS://')"
echo "[kgpc] done size=$(stat -c %s "$PP_S") link_args:${LINK_ARGS}"

# --- 2. assemble ----------------------------------------------------------
echo "[as] $PP_S -> $PP_O"
"$AS" "$PP_S" -o "$PP_O"
echo "[as] done size=$(stat -c %s "$PP_O")"

# --- 3. KGPC C runtime ----------------------------------------------------
echo "[cc] building KGPC C runtime ($MINGW_PREFIX)"
RT_SOURCES=(
  KGPC/runtime.c
  KGPC/runtime_string.c
  KGPC/runtime_baseunix.c
  KGPC/runtime_fpc_pchar_to_shortstr.c
  KGPC/runtime_fpc_pchar_to_shortstr_upper.c
  KGPC/runtime_fpc_assign.c
  KGPC/runtime_fpc_init.c
)
RT_OBJS=()
for src in "${RT_SOURCES[@]}"; do
  obj="$WINRT_DIR/$(basename "$src" .c).o"
  RT_OBJS+=("$obj")
  "$CC" -O2 -c -I./KGPC -I./common "$src" -o "$obj"
done
"$AR" rcs "$WINRT_DIR/libkgpc_runtime.a" "${RT_OBJS[@]}"

# --- 4. link --------------------------------------------------------------
# --stack 16 MB: pp.pas's main frame is ~3.5 MB; Windows' 1 MB default
# overflows during startup.  -static: self-contained .exe (no msys/mingw DLLs).
echo "[link] -> $PP_EXE"
"$CC" "$PP_O" "$WINRT_DIR/libkgpc_runtime.a" -o "$PP_EXE" \
  -static -static-libgcc -Wl,-Bstatic -lpthread -Wl,-Bdynamic \
  -Wl,--stack,16777216 \
  $LINK_ARGS
echo "[link] done size=$(stat -c %s "$PP_EXE")"
echo "[ok] $PP_EXE"
