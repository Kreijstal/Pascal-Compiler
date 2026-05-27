#!/usr/bin/env bash
# cross_build_pp_win.sh — Cross-build KGPC's C runtime for Windows x86_64 and
# attempt to link the win-target pp.pas object produced by
# /tmp/win_verify.py into a Windows .exe.
#
# Phase 1: Cross-compile KGPC/runtime*.c with x86_64-w64-mingw32-gcc into
# /tmp/winrt/*.o, then archive as /tmp/winrt/libkgpc_runtime.a.
#
# Phase 2 (RTL units): NOT NEEDED at this stage.  pp.pas's monolithic .o
# already embeds all needed FPC RTL functions (8859 defined T symbols in
# /tmp/pp_win.o); there is no separate libfpcrtl-win64.a to build.  All
# kgpc_*, fpc_*, atomic_*, etc. helper symbols are satisfied by either
# libkgpc_runtime.a (Phase 1 output) or by the mingw libs (-lkernel32, etc.).
#
# Phase 3: Link /tmp/pp_win.o + libkgpc_runtime.a + mingw libs into
# /tmp/pp_win.exe.  As of 2026-05-27 this fails on 9 hard relocation errors
# that are KGPC compiler bugs, not link-graph deficiencies:
#   jmp_buf.r12 / .r13 / .r14 / .r15 / .rbp / .rbx / .rip / .rsp
#     - record-type field-offset references inside FPC_LONGJMP inline asm
#       (FPCSource/rtl/x86_64/setjump.inc) are emitted verbatim instead of
#       being resolved to numeric byte offsets.
#   trulyrelocatethreadvar_li
#     - TrulyRelocateThreadvar from FPCSource/rtl/win/systhrd.inc is
#       referenced by SysRelocateThreadvar inline asm but its function body
#       is never emitted.  The function is declared via a $ifdef-controlled
#       identifier ({$ifdef FPC_HAS_SYSRELOCATETHREADVAR_ASM} TrulyRelocate
#       {$else} SysRelocate {$endif}) which KGPC's parser apparently fails
#       to bind to a code body.
# Both blockers are KGPC compiler bugs that need to be fixed in C, not
# worked around at link time.

set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO"

WINRT_DIR=/tmp/winrt
mkdir -p "$WINRT_DIR"
WINBUILD_LOG=/tmp/winbuild.log
: > "$WINBUILD_LOG"

CC=x86_64-w64-mingw32-gcc
AR=x86_64-w64-mingw32-ar

echo "[phase1] cross-compiling KGPC C runtime sources..." | tee -a "$WINBUILD_LOG"
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
  echo "  cc $src -> $obj" | tee -a "$WINBUILD_LOG"
  "$CC" -O2 -c -I./KGPC -I./common "$src" -o "$obj" 2>>"$WINBUILD_LOG"
done

echo "[phase1] archiving libkgpc_runtime.a..." | tee -a "$WINBUILD_LOG"
"$AR" rcs "$WINRT_DIR/libkgpc_runtime.a" "${RT_OBJS[@]}"
ls -la "$WINRT_DIR/libkgpc_runtime.a" | tee -a "$WINBUILD_LOG"

if [ ! -f /tmp/pp_win.o ]; then
  echo "[phase3] /tmp/pp_win.o not found; run python3 /tmp/win_verify.py then" >&2
  echo "         x86_64-w64-mingw32-as /tmp/pp_win.s -o /tmp/pp_win.o first." >&2
  exit 1
fi

echo "[phase3] linking /tmp/pp_win.o + libkgpc_runtime.a -> /tmp/pp_win.exe" | tee -a "$WINBUILD_LOG"
LINK_LOG=/tmp/link.log
# -static-libgcc + -Wl,-Bstatic for pthread: produce a self-contained .exe
# that does not require libwinpthread-1.dll alongside it.  Without this,
# wine reports c0000135 (STATUS_DLL_NOT_FOUND) at startup and exits 53.
#
# --stack 16777216,16777216: reserve and commit 16 MB stack.  pp.pas's
# main procedure has a ~3.5 MB local frame; Windows defaults to a 1 MB
# stack reservation which overflows during _FPC_EXE_Entry before any
# Pascal code runs.
set +e
"$CC" /tmp/pp_win.o "$WINRT_DIR/libkgpc_runtime.a" -o /tmp/pp_win.exe \
  -static-libgcc -Wl,-Bstatic -lpthread -Wl,-Bdynamic \
  -Wl,--stack,16777216 \
  -lkernel32 -luser32 -lws2_32 2>"$LINK_LOG"
LINK_RC=$?
set -e
echo "[phase3] link exit=$LINK_RC" | tee -a "$WINBUILD_LOG"
if [ "$LINK_RC" -ne 0 ]; then
  echo "[phase3] unique missing symbols:" | tee -a "$WINBUILD_LOG"
  grep "undefined reference" "$LINK_LOG" \
    | sed "s/.*undefined reference to \`//; s/'.*//" \
    | sort -u | tee -a "$WINBUILD_LOG"
fi
