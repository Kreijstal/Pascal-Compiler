#!/usr/bin/env bash
# fpc_bootstrap.sh — Full FPC bootstrap chain:
#   Stage 0: kgpc compiles msg2inc (if needed)
#   Stage 1: kgpc compiles pp.pas → pp_stage1 (the FPC compiler)
#   Stage 2: pp_stage1 compiles hello.pas → verify it works
#   Stage 3: pp_stage1 compiles pp.pas → pp_stage2 (FPC compiling itself!)
#   Stage 4: pp_stage2 compiles hello.pas → verify stage2 works too
#
# Usage: ./scripts/fpc_bootstrap.sh [--build-dir DIR] [--fpc-src DIR]
#
# Prerequisites: kgpc must be built (meson compile -C build)
set -euo pipefail

# ── Defaults ──────────────────────────────────────────────────────────────────
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="${PROJECT_DIR}/build"
FPC_SRC="${PROJECT_DIR}/FPCSource"
OUT_DIR="/tmp/fpc_bootstrap_$$"
CC="${CC:-cc}"

# ── Parse args ────────────────────────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
    case "$1" in
        --build-dir) BUILD_DIR="$2"; shift 2 ;;
        --fpc-src)   FPC_SRC="$2";   shift 2 ;;
        *) echo "Unknown option: $1" >&2; exit 1 ;;
    esac
done

KGPC="${BUILD_DIR}/KGPC/kgpc"
RUNTIME_LIB="${BUILD_DIR}/KGPC/libkgpc_runtime.a"

# ── Validate prerequisites ────────────────────────────────────────────────────
for f in "$KGPC" "$RUNTIME_LIB"; do
    if [[ ! -f "$f" ]]; then
        echo "ERROR: Required file not found: $f" >&2
        echo "       Run: meson compile -C build" >&2
        exit 1
    fi
done

if [[ ! -d "$FPC_SRC/compiler" ]]; then
    echo "ERROR: FPC source not found at $FPC_SRC" >&2
    exit 1
fi

mkdir -p "$OUT_DIR"
echo "=== FPC Full Bootstrap ==="
echo "  Project:  $PROJECT_DIR"
echo "  Build:    $BUILD_DIR"
echo "  FPC src:  $FPC_SRC"
echo "  Output:   $OUT_DIR"
echo ""

# ── Common KGPC flags (RTL paths) ────────────────────────────────────────────
KGPC_RTL_FLAGS=(
    --no-stdlib
    -DCPU64 -DCPUX86_64 -Dx86_64 -DFPC -DLINUX -DUNIX
    -DFPC_HAS_TYPE_EXTENDED -DSUPPORT_EXTENDED -DFPC_BOOTSTRAP_INDIRECT_ENTRY -Sg
    "-I${FPC_SRC}/rtl/objpas"
    "-I${FPC_SRC}/rtl/objpas/sysutils"
    "-I${FPC_SRC}/rtl/objpas/classes"
    "-I${FPC_SRC}/rtl/linux"
    "-I${FPC_SRC}/rtl/unix"
    "-I${FPC_SRC}/rtl/inc"
    "-I${FPC_SRC}/rtl/x86_64"
    "-I${FPC_SRC}/rtl/linux/x86_64"
    "-I${FPC_SRC}/rtl/unix/x86_64"
    "-Fu${FPC_SRC}/rtl/unix"
    "-Fu${FPC_SRC}/rtl/linux"
    "-Fu${FPC_SRC}/rtl/objpas"
    "-Fu${FPC_SRC}/rtl/inc"
    "-Fu${FPC_SRC}/rtl/objpas/sysutils"
    "-Fu${FPC_SRC}/rtl/objpas/classes"
)

# Extra flags for compiling the compiler sources (pp.pas)
PP_COMPILER_FLAGS=(
    "-I${FPC_SRC}/compiler"
    "-I${FPC_SRC}/compiler/x86"
    "-I${FPC_SRC}/compiler/x86_64"
    "-Fu${FPC_SRC}/compiler"
    "-Fu${FPC_SRC}/compiler/x86"
    "-Fu${FPC_SRC}/compiler/x86_64"
    "-Fu${FPC_SRC}/compiler/systems"
)

PP_STAGE_COMPILER_INCLUDE_FLAGS=(
    "-Fi${FPC_SRC}/compiler"
    "-Fi${FPC_SRC}/compiler/x86"
    "-Fi${FPC_SRC}/compiler/x86_64"
)

PP_STAGE_COMPILER_UNIT_FLAGS=(
    "-Fu${FPC_SRC}/compiler"
    "-Fu${FPC_SRC}/compiler/x86"
    "-Fu${FPC_SRC}/compiler/x86_64"
    "-Fu${FPC_SRC}/compiler/systems"
)

PREBUILT_UNITS_DIR="${FPC_SRC}/rtl/units/x86_64-linux"
PREBUILT_SYSTEM_PPU="${PREBUILT_UNITS_DIR}/system.ppu"
PREBUILT_ABITAG_O="${PREBUILT_UNITS_DIR}/abitag.o"
PPU_VERSION_SOURCE="${FPC_SRC}/compiler/ppu.pas"
SAME_SOURCE_FPC_BASENAME="ppcx64"

# ── Helper: compile .pas → .s with kgpc ─────────────────────────────────────
LINK_ARGS=()
compile_with_kgpc() {
    local src="$1" asm="$2"
    shift 2
    local extra_flags=("$@")
    echo "  Compiling $(basename "$src") with kgpc ..."
    local start=$SECONDS
    "$KGPC" "$src" "$asm" --emit-link-args "${KGPC_RTL_FLAGS[@]}" "${extra_flags[@]}" 2>"${asm}.log"
    local elapsed=$(( SECONDS - start ))
    echo "  Done in ${elapsed}s → $(basename "$asm") ($(wc -l < "$asm") lines)"

    # Extract KGPC_LINK_ARGS if emitted by the compiler
    LINK_ARGS=()
    while IFS= read -r line; do
        if [[ "$line" == KGPC_LINK_ARGS:* ]]; then
            read -ra LINK_ARGS <<< "${line#KGPC_LINK_ARGS:}"
        fi
    done < "${asm}.log"
}

# ── Helper: link .s + runtime → executable ───────────────────────────────────
link_executable() {
    local asm="$1" exe="$2"
    shift 2
    local extra_args=("$@")
    echo "  Linking $(basename "$exe") ..."
    local start=$SECONDS
    "$CC" -O2 -no-pie -o "$exe" "$asm" "$RUNTIME_LIB" "${extra_args[@]}" "${LINK_ARGS[@]}"
    local elapsed=$(( SECONDS - start ))
    local size
    size="$(du -h "$exe" | cut -f1)"
    echo "  Done in ${elapsed}s → $(basename "$exe") ($size)"
}

# ── Helper: write & compile hello world, then run it ─────────────────────────
test_hello_world() {
    local compiler="$1" label="$2" hello_dir="$3"
    local hello_pas="${hello_dir}/hello.pas"
    local hello_exe="${hello_dir}/hello"
    shift 3
    local extra_flags=("$@")

    cat > "$hello_pas" << PASCAL
program Hello;
begin
  WriteLn('Hello from ${label}!');
end.
PASCAL

    echo "  Compiling hello.pas with ${label} ..."
    # Keep the source file last so the shared output/search-path flag handling
    # stays identical between hello-world checks and pp.pas self-hosting.
    "$compiler" "${extra_flags[@]}" "-FE${hello_dir}" -o"$(basename "$hello_exe")" "$hello_pas" 2>&1 | tail -5
    echo "  Running hello ..."
    local output
    output="$("$hello_exe")"
    echo "  Output: $output"
    if [[ "$output" == *"Hello from ${label}!"* ]]; then
        echo "  PASS"
    else
        echo "  FAIL: unexpected output" >&2
        return 1
    fi
}

ensure_same_source_rtl_units() {
    rtl_sources_newer_than() {
        local reference_file="$1"
        local first_match
        first_match="$(
        find "${FPC_SRC}/rtl" \
            \( -path "${FPC_SRC}/rtl/units" -o -path "${FPC_SRC}/rtl/units/*" \) -prune -o \
            -type f -newer "$reference_file" -print -quit
        )"
        [[ -n "$first_match" ]]
    }

    local rtl_units_are_stale=0
    if [[ ! -f "$PREBUILT_SYSTEM_PPU" || ! -f "$PREBUILT_ABITAG_O" ]]; then
        rtl_units_are_stale=1
    elif [[ "$PREBUILT_SYSTEM_PPU" -ot "$PPU_VERSION_SOURCE" || "$PREBUILT_ABITAG_O" -ot "$PPU_VERSION_SOURCE" ]]; then
        rtl_units_are_stale=1
    elif rtl_sources_newer_than "$PREBUILT_SYSTEM_PPU"; then
        rtl_units_are_stale=1
    elif rtl_sources_newer_than "$PREBUILT_ABITAG_O"; then
        rtl_units_are_stale=1
    fi

    if [[ "$rtl_units_are_stale" -eq 0 ]]; then
        echo "[RTL] Same-source RTL units already available — skipping rebuild"
        echo ""
        return
    fi

    echo "[RTL] Building same-source compiler and RTL units"
    if [[ -d "$PREBUILT_UNITS_DIR" ]]; then
        rm -rf "$PREBUILT_UNITS_DIR"
    fi

    local make_bin
    make_bin="$(command -v make)"
    [[ -n "$make_bin" ]] || { echo "ERROR: make is required for bootstrap RTL units" >&2; exit 1; }

    local fpc_bin
    fpc_bin="$(command -v fpc)"
    [[ -n "$fpc_bin" ]] || { echo "ERROR: fpc is required for bootstrap RTL units" >&2; exit 1; }

    local same_source_fpc="${COMPILER_DIR}/${SAME_SOURCE_FPC_BASENAME}"
    local rtl_linux_dir="${FPC_SRC}/rtl/linux"

    "$make_bin" -C "$COMPILER_DIR" ppcx64 "FPC=${fpc_bin}"
    [[ -f "$same_source_fpc" ]] || { echo "ERROR: expected same-source compiler at $same_source_fpc" >&2; exit 1; }

    "$make_bin" -C "$rtl_linux_dir" all "FPC=${same_source_fpc}"
    [[ -f "$PREBUILT_SYSTEM_PPU" ]] || { echo "ERROR: missing $PREBUILT_SYSTEM_PPU after RTL build" >&2; exit 1; }
    [[ -f "$PREBUILT_ABITAG_O" ]] || { echo "ERROR: missing $PREBUILT_ABITAG_O after RTL build" >&2; exit 1; }
    echo ""
}

# ── Stage 0: Generate msgtxt.inc / msgidx.inc if needed ──────────────────────
COMPILER_DIR="${FPC_SRC}/compiler"
MSGTXT_INC="${COMPILER_DIR}/msgtxt.inc"
MSGIDX_INC="${COMPILER_DIR}/msgidx.inc"

if [[ ! -f "$MSGTXT_INC" || ! -f "$MSGIDX_INC" ]]; then
    echo "[Stage 0] Generating message includes via msg2inc ..."
    MSG2INC_PP="${COMPILER_DIR}/utils/msg2inc.pp"
    MSG2INC_ASM="${OUT_DIR}/msg2inc.s"
    MSG2INC_EXE="${OUT_DIR}/msg2inc"

    compile_with_kgpc "$MSG2INC_PP" "$MSG2INC_ASM"
    link_executable "$MSG2INC_ASM" "$MSG2INC_EXE"

    echo "  Running msg2inc to generate .inc files ..."
    (cd "$COMPILER_DIR" && "$MSG2INC_EXE" msg/errore.msg msg msg)
    echo "  Generated msgtxt.inc and msgidx.inc"
    echo ""
else
    echo "[Stage 0] msgtxt.inc and msgidx.inc already exist — skipping"
    echo ""
fi

ensure_same_source_rtl_units

# ── Stage 1: kgpc compiles pp.pas → pp_stage1 ───────────────────────────────
echo "[Stage 1] kgpc compiles pp.pas → pp_stage1"
PP_PAS="${FPC_SRC}/compiler/pp.pas"
PP_STAGE1_ASM="${OUT_DIR}/pp_stage1.s"
PP_STAGE1="${OUT_DIR}/pp_stage1"

compile_with_kgpc "$PP_PAS" "$PP_STAGE1_ASM" "${PP_COMPILER_FLAGS[@]}"
link_executable "$PP_STAGE1_ASM" "$PP_STAGE1"

echo "  Verifying pp_stage1 ..."
"$PP_STAGE1" -h 2>&1 | head -2 || true
echo ""

# ── Stage 2: pp_stage1 compiles hello world ──────────────────────────────────
echo "[Stage 2] pp_stage1 compiles hello world"
STAGE2_DIR="${OUT_DIR}/stage2"
mkdir -p "$STAGE2_DIR"
test_hello_world "$PP_STAGE1" "pp_stage1" "$STAGE2_DIR" -n "-Fu${PREBUILT_UNITS_DIR}"
echo ""

# ── Stage 3: pp_stage1 compiles pp.pas → pp_stage2 ──────────────────────────
echo "[Stage 3] pp_stage1 compiles pp.pas → pp_stage2 (self-compilation!)"
PP_STAGE2_DIR="${OUT_DIR}/pp_stage2"
PP_STAGE2_UNITS_DIR="${PP_STAGE2_DIR}/units"
PP_STAGE2="${PP_STAGE2_DIR}/pp_stage2"
mkdir -p "$PP_STAGE2_DIR" "$PP_STAGE2_UNITS_DIR"

echo "  Compiling pp.pas with pp_stage1 ..."
STAGE1_START=$SECONDS
"$PP_STAGE1" -n \
    "-FE${PP_STAGE2_DIR}" \
    "-FU${PP_STAGE2_UNITS_DIR}" \
    -o"$(basename "$PP_STAGE2")" \
    "-Fu${PREBUILT_UNITS_DIR}" \
    "${PP_STAGE_COMPILER_INCLUDE_FLAGS[@]}" \
    "${PP_STAGE_COMPILER_UNIT_FLAGS[@]}" \
    "$PP_PAS" \
    2>&1 | tail -10
STAGE1_ELAPSED=$(( SECONDS - STAGE1_START ))
echo "  Stage 3 done in ${STAGE1_ELAPSED}s"

if [[ ! -f "$PP_STAGE2" ]]; then
    echo "  FAIL: pp_stage2 was not produced" >&2
    echo ""
    echo "  Note: Stage 3 (self-compilation) requires pp_stage1 to be a fully"
    echo "  functional FPC compiler. If this failed, it likely means the kgpc-compiled"
    echo "  FPC has runtime issues (e.g. missing class methods, RTTI, etc)."
    echo ""
    echo "=== FPC Bootstrap: Stages 1-2 PASSED, Stage 3 FAILED ==="
    echo "  pp_stage1:    $PP_STAGE1 (functional — compiles simple programs)"
    echo "  Stage 3 log:  see output above"
    echo "  Output dir:   $OUT_DIR"
    exit 1
fi

echo "  Verifying pp_stage2 ..."
"$PP_STAGE2" -h 2>&1 | head -2 || true
echo ""

# ── Stage 4: pp_stage2 compiles hello world ──────────────────────────────────
echo "[Stage 4] pp_stage2 compiles hello world"
STAGE4_DIR="${OUT_DIR}/stage4"
mkdir -p "$STAGE4_DIR"
test_hello_world "$PP_STAGE2" "pp_stage2" "$STAGE4_DIR" -n "-Fu${PREBUILT_UNITS_DIR}"
echo ""

# ── Stage 5: Compare stage1 and stage2 binaries ─────────────────────────────
echo "[Stage 5] Comparing stage1 and stage2 binaries ..."
STAGE1_SHA="$(sha256sum "$PP_STAGE1" | cut -d' ' -f1)"
STAGE2_SHA="$(sha256sum "$PP_STAGE2" | cut -d' ' -f1)"
echo "  pp_stage1 sha256: $STAGE1_SHA"
echo "  pp_stage2 sha256: $STAGE2_SHA"
if [[ "$STAGE1_SHA" == "$STAGE2_SHA" ]]; then
    echo "  Binaries are IDENTICAL — perfect bootstrap!"
else
    echo "  Binaries differ (expected — stage1 was compiled by kgpc, stage2 by FPC)"
    # Could add a stage3 here: pp_stage2 compiles pp.pas → pp_stage3
    # stage2 and stage3 should be identical for a true fixed-point bootstrap
fi
echo ""

echo "=== FPC Full Bootstrap Complete ==="
echo "  pp_stage1: $PP_STAGE1  (kgpc → pp.pas)"
echo "  pp_stage2: $PP_STAGE2  (pp_stage1 → pp.pas)"
echo "  Output:    $OUT_DIR"
