#!/usr/bin/env bash
# native_build_pp_win.sh — assemble + link the win-target pp.pas object on a
# *native* Windows host (MSYS2/win11), producing pp_win.exe.
#
# This is the native counterpart to cross_build_pp_win.sh (which links from a
# Linux host with the x86_64-w64-mingw32 cross toolchain).  It expects KGPC to
# already have emitted /tmp/pp_win.s via scripts/cross_compile_pp_win.py run
# under the native KGPC build.
#
# CRITICAL — toolchain selection:
#   On MSYS2, /usr/bin/gcc is the MSYS *POSIX* compiler (Cygwin fork): it
#   defines __MSYS__/__CYGWIN__/__unix__ and does NOT define _WIN32/_WIN64/
#   __MINGW64__.  Building KGPC's C runtime with it has two fatal effects:
#     1. KGPC/runtime_fpc_init.c's Win64 indirect-entry-information shim
#        (kgpc_fpc_init_win_entry_info) is guarded on _WIN32/_WIN64/__MINGW*,
#        so it compiles as a no-op.  _FPC_SysInstance / _FPC_TlsKey then stay
#        NULL when win64/system.pp's initialization runs, and the very first
#        `FPCSysInstance^ := getmodulehandle(nil)` (system.pp:436) faults
#        writing to address 0 — the process SIGSEGVs before any output.
#     2. The runtime objects are Cygwin-ABI, mismatched against the Win64-PE
#        pp_win.o; the resulting memory corruption garbles command-line
#        filenames ("hi.pas" -> "??.pas") and the banner.
#   The fix is to build everything with the MinGW-w64 toolchain in /mingw64,
#   which targets native Win64 PE and defines _WIN64/__MINGW64__.  This script
#   hard-codes that prefix; override with MINGW_PREFIX if your install differs.
set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO"

MINGW_PREFIX="${MINGW_PREFIX:-/mingw64}"
export PATH="$MINGW_PREFIX/bin:$PATH"
CC="$MINGW_PREFIX/bin/gcc"
AS="$MINGW_PREFIX/bin/as"
AR="$MINGW_PREFIX/bin/ar"

# Refuse to run with a non-MinGW gcc: a missing _WIN64 means the entry-info
# shim would be a no-op and the .exe would crash at startup (see header).
if ! "$CC" -dM -E - < /dev/null 2>/dev/null | grep -q "define _WIN64"; then
  echo "ERROR: $CC does not define _WIN64 — not a MinGW-w64 compiler." >&2
  echo "       Install the mingw-w64 toolchain and/or set MINGW_PREFIX." >&2
  exit 1
fi

PP_S="${PP_S:-/tmp/pp_win.s}"
PP_O="${PP_O:-/tmp/pp_win.o}"
PP_EXE="${PP_EXE:-/tmp/pp_win.exe}"
WINRT_DIR="${WINRT_DIR:-/tmp/winrt_mingw64}"
mkdir -p "$WINRT_DIR"

if [ ! -f "$PP_S" ]; then
  echo "ERROR: $PP_S not found; run scripts/cross_compile_pp_win.py first." >&2
  exit 1
fi

echo "[as] assembling $PP_S -> $PP_O"
"$AS" "$PP_S" -o "$PP_O"
echo "[as] done size=$(stat -c %s "$PP_O")"

echo "[cc] compiling KGPC C runtime (MinGW-w64)..."
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
  base=$(basename "$src" .c)
  obj="$WINRT_DIR/${base}.o"
  RT_OBJS+=("$obj")
  "$CC" -O2 -c -I./KGPC -I./common "$src" -o "$obj"
done
"$AR" rcs "$WINRT_DIR/libkgpc_runtime.a" "${RT_OBJS[@]}"

# --stack 16 MB: pp.pas's main frame is ~3.5 MB; Windows' 1 MB default
# overflows during startup.  -static: self-contained .exe (no msys/mingw DLLs).
echo "[link] linking $PP_EXE"
"$CC" "$PP_O" "$WINRT_DIR/libkgpc_runtime.a" -o "$PP_EXE" \
  -static -static-libgcc -Wl,-Bstatic -lpthread -Wl,-Bdynamic \
  -Wl,--stack,16777216 \
  -lkernel32 -luser32 -lws2_32
echo "[link] done size=$(stat -c %s "$PP_EXE")"
echo "[ok] $PP_EXE"
