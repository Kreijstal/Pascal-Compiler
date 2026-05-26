# Cache management for the KGPC test harness.
#
# Three distinct caches live here:
#   1.  FPC RTL AST cache   — stores parsed ASTs for FPC system/RTL units to
#                             avoid re-parsing them for every test (~3.5s saved).
#   2.  FPC RTL codegen cache — stores compiled .o files for FPC RTL units.
#   3.  Test result cache   — stores a sentinel file for each passing test so
#                             unchanged tests are skipped on subsequent runs.
#
# Do NOT delete the cache directories in this module.  Cache invalidation
# belongs in the key/freshness checks, not in test harness cleanup.
import hashlib
import os
import subprocess
import sys

from .env import (
    KGPC_PATH,
    FPC_RTL_MODE,
    FPC_RTL_DIR,
)

# ---------------------------------------------------------------------------
# FPC RTL AST cache
# ---------------------------------------------------------------------------

# AST cache directory for FPC RTL mode — avoids re-parsing system.pp + objpas.pp
# for every test (saves ~3.5s per test).
# The cache is automatically invalidated when the compiler binary changes.
_FPC_RTL_AST_CACHE_DIR = None
if FPC_RTL_MODE:
    _FPC_RTL_AST_CACHE_DIR = os.path.join(
        os.environ.get("MESON_BUILD_ROOT", "builddir"), "fpc_rtl_ast_cache"
    )
    # KGPC validates AST cache entries against the compiler binary mtime.
    # Keep a sentinel for diagnostics, but do not delete the cache directory:
    # stale entries are rejected by the compiler and can coexist safely.
    _cache_sentinel = os.path.join(_FPC_RTL_AST_CACHE_DIR, ".compiler_mtime")
    _compiler_mtime = ""
    if os.path.exists(KGPC_PATH):
        st = os.stat(KGPC_PATH)
        _compiler_mtime = f"{st.st_mtime_ns}:{st.st_size}"
    os.makedirs(_FPC_RTL_AST_CACHE_DIR, exist_ok=True)
    if _compiler_mtime:
        with open(_cache_sentinel, "w") as f:
            f.write(_compiler_mtime)

# ---------------------------------------------------------------------------
# FPC RTL codegen cache
# ---------------------------------------------------------------------------

# Codegen object cache: the compiler assembles full .o on cache miss,
# then uses --skip-unit-codegen on cache hit (all handled internally).
_FPC_RTL_CODEGEN_CACHE_DIR = None
if FPC_RTL_MODE:
    _FPC_RTL_CODEGEN_CACHE_DIR = os.path.join(
        os.environ.get("MESON_BUILD_ROOT", "builddir"), "fpc_rtl_codegen_cache"
    )
    # The compiler includes its own mtime in the cache key, so old entries
    # are never matched after a rebuild. Do not delete the directory here:
    # cache invalidation belongs in the key/freshness checks, not in test
    # harness cleanup.
    _cg_sentinel = os.path.join(
        os.environ.get("MESON_BUILD_ROOT", "builddir"), ".codegen_cache_mtime"
    )
    os.makedirs(_FPC_RTL_CODEGEN_CACHE_DIR, exist_ok=True)
    if FPC_RTL_MODE and _compiler_mtime:
        with open(_cg_sentinel, "w") as f:
            f.write(_compiler_mtime)


def with_fpc_rtl_ast_cache(flags):
    """Append the AST cache dir flag to a flags list if enabled."""
    flags = list(flags)
    if _FPC_RTL_AST_CACHE_DIR is not None:
        flags.append("--pp-cache-dir=" + _FPC_RTL_AST_CACHE_DIR)
    return flags


# ---------------------------------------------------------------------------
# FPC RTL flags (assembled here because they depend on cache dirs)
# ---------------------------------------------------------------------------

FPC_RTL_FLAGS = [
    "--no-stdlib",
    "-I" + os.path.join(FPC_RTL_DIR, "linux"),
    "-I" + os.path.join(FPC_RTL_DIR, "linux", "x86_64"),
    "-I" + os.path.join(FPC_RTL_DIR, "inc"),
    "-I" + os.path.join(FPC_RTL_DIR, "unix"),
    "-I" + os.path.join(FPC_RTL_DIR, "x86_64"),
    "-I" + os.path.join(FPC_RTL_DIR, "objpas", "sysutils"),
    "-I" + os.path.join(FPC_RTL_DIR, "objpas", "classes"),
    "-Fu" + os.path.join(FPC_RTL_DIR, "objpas"),
    "-Fu" + os.path.join(FPC_RTL_DIR, "inc"),
    "-Fu" + os.path.join(FPC_RTL_DIR, "linux"),
    "-Fu" + os.path.join(FPC_RTL_DIR, "linux", "x86_64"),
    "-Fu" + os.path.join(FPC_RTL_DIR, "x86_64"),
    "-Fu" + os.path.join(FPC_RTL_DIR, "unix"),
    "-I" + os.path.join(os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource"), "packages", "rtl-objpas", "src", "inc"),
    "-Fu" + os.path.join(os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource"), "packages", "rtl-objpas", "src", "inc"),
    "-I" + os.path.join(os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource"), "packages", "rtl-console", "src", "inc"),
    "-Fu" + os.path.join(os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource"), "packages", "rtl-console", "src", "inc"),
    "-Fu" + os.path.join(os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource"), "packages", "rtl-console", "src", "unix"),
]
if _FPC_RTL_AST_CACHE_DIR is not None:
    FPC_RTL_FLAGS.append("--pp-cache-dir=" + _FPC_RTL_AST_CACHE_DIR)
if _FPC_RTL_CODEGEN_CACHE_DIR is not None:
    FPC_RTL_FLAGS.append("--codegen-cache-dir=" + _FPC_RTL_CODEGEN_CACHE_DIR)

# ---------------------------------------------------------------------------
# Background AST cache primer for FPC RTL mode.
# Spawns a lightweight compiler process that compiles a minimal Pascal program
# importing the most common RTL units (SysUtils, Classes).  This populates the
# AST cache so that subsequent parallel test compilations get cache hits
# instead of all redundantly parsing system.pp.
#
# The primer runs outside the compiler semaphore — it does NOT block test
# execution.  It runs concurrently with the first batch of tests.  Tests
# that start before the primer finishes will parse from source (the AST
# cache uses atomic write-then-rename, so concurrent writes are safe).
# Tests that start after the primer finishes benefit from cached ASTs.
# ---------------------------------------------------------------------------
_CACHE_PRIMER_PROC = None
if FPC_RTL_MODE and _FPC_RTL_AST_CACHE_DIR is not None:
    # Only prime if the AST cache is empty (cold start).
    _existing_cache_files = [
        f for f in os.listdir(_FPC_RTL_AST_CACHE_DIR)
        if f.endswith(".ast_cache")
    ]
    if not _existing_cache_files:
        import tempfile as _tempfile
        _primer_src = os.path.join(
            _tempfile.gettempdir(), "kgpc_cache_primer.p"
        )
        _primer_asm = os.path.join(
            _tempfile.gettempdir(), "kgpc_cache_primer.s"
        )
        with open(_primer_src, "w") as _f:
            _f.write("program CachePrimer;\nuses SysUtils, Classes;\nbegin\nend.\n")
        _primer_cmd = [KGPC_PATH, _primer_src, _primer_asm, "--emit-link-args"] + FPC_RTL_FLAGS
        print(f"--- Launching background AST cache primer ---", file=sys.stderr)
        sys.stderr.flush()
        _CACHE_PRIMER_PROC = subprocess.Popen(
            _primer_cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
    del _existing_cache_files

# ---------------------------------------------------------------------------
# Test result cache — skip compile/link/run when nothing changed.
# Cache key = hash of: compiler binary, test .p file, .expected file,
# flags, runtime library.  Stored as empty files named by the key hash
# in _TEST_RESULT_CACHE_DIR.
# ---------------------------------------------------------------------------
_TEST_RESULT_CACHE_DIR = os.path.join(
    os.environ.get("MESON_BUILD_ROOT", "build"), "test_result_cache"
)
os.makedirs(_TEST_RESULT_CACHE_DIR, exist_ok=True)

# Pre-compute compiler identity once (mtime + size — fast, changes on every rebuild).
_COMPILER_HASH = ""
if os.path.exists(KGPC_PATH):
    _st = os.stat(KGPC_PATH)
    _COMPILER_HASH = f"{_st.st_mtime_ns}:{_st.st_size}"

# Pre-compute runtime library identity once.
_RUNTIME_LIB_PATH = os.environ.get("KGPC_RUNTIME_LIB", "")
_RUNTIME_HASH = ""
if _RUNTIME_LIB_PATH and os.path.exists(_RUNTIME_LIB_PATH):
    _st = os.stat(_RUNTIME_LIB_PATH)
    _RUNTIME_HASH = f"{_st.st_mtime_ns}:{_st.st_size}"


def _file_hash(path):
    """Return mtime:size identity string for a file, or empty string if missing."""
    if not path or not os.path.exists(path):
        return ""
    st = os.stat(path)
    return f"{st.st_mtime_ns}:{st.st_size}"


def _test_cache_key(input_file, expected_file, flags):
    """Compute a cache key for a test case."""
    h = hashlib.sha256()
    h.update(_COMPILER_HASH.encode())
    h.update(_RUNTIME_HASH.encode())
    h.update(_file_hash(input_file).encode())
    h.update(_file_hash(expected_file).encode())
    # Include hashes of any auxiliary unit files (e.g. cross_unit_inherit_base.p)
    base_name = os.path.splitext(os.path.basename(input_file))[0]
    test_dir = os.path.dirname(input_file)
    for f in sorted(os.listdir(test_dir)):
        if f.startswith(base_name + "_") and f.endswith(".p"):
            h.update(_file_hash(os.path.join(test_dir, f)).encode())
    # Include .input file if present
    input_data = os.path.join(test_dir, base_name + ".input")
    h.update(_file_hash(input_data).encode())
    h.update(repr(sorted(flags)).encode() if flags else b"")
    return h.hexdigest()


def test_cache_check(input_file, expected_file, flags):
    """Return True if this test has a cached passing result."""
    key = _test_cache_key(input_file, expected_file, flags)
    return os.path.exists(os.path.join(_TEST_RESULT_CACHE_DIR, key))


def test_cache_store(input_file, expected_file, flags):
    """Record a passing result for this test."""
    key = _test_cache_key(input_file, expected_file, flags)
    path = os.path.join(_TEST_RESULT_CACHE_DIR, key)
    with open(path, "w") as f:
        f.write("")
