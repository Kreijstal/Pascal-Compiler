# Environment and platform configuration for the KGPC test harness.
# All global constants derived from environment variables live here.
import os
import sys
import subprocess
import threading

# ---------------------------------------------------------------------------
# Platform detection
# ---------------------------------------------------------------------------

WINDOWS_ABI_PLATFORMS = ("win", "cygwin", "msys", "mingw")
PLATFORM_ID = sys.platform.lower()
IS_WINDOWS_ABI = os.name == "nt" or PLATFORM_ID.startswith(WINDOWS_ABI_PLATFORMS)

# Detect if we're running under Wine (Windows Python on Linux).
# In CI this may be signaled either by WINE* environment variables or by
# Meson's exe wrapper path when the Windows Python interpreter is launched
# through `/usr/bin/wine`.
_meson_exe_wrapper = os.environ.get("MESON_EXE_WRAPPER", "")
IS_WINE = IS_WINDOWS_ABI and (
    any(k.startswith("WINE") for k in os.environ)
    or "wine" in _meson_exe_wrapper.lower()
)
EXE_EXT = ".exe" if IS_WINDOWS_ABI else ""

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

# Path to the compiler executable
# Get the build directory from the environment variable set by Meson.
# Default to "build" for local testing.
build_dir = os.environ.get("MESON_BUILD_ROOT", "build")
KGPC_PATH = os.path.join(build_dir, "KGPC/kgpc.exe" if IS_WINDOWS_ABI else "KGPC/kgpc")
TEST_CASES_DIR = "tests/test_cases"
INPUT_DATA_DIR = TEST_CASES_DIR
TEST_OUTPUT_DIR = "tests/output"

from pathlib import Path
PROJECT_ROOT = Path(__file__).resolve().parents[2]
GOLDEN_AST_DIR = "tests/golden_ast"

# ---------------------------------------------------------------------------
# Timeouts
# ---------------------------------------------------------------------------

# Default execution timeout per compiled test program (seconds).
# Can be overridden via environment variable KGPC_TEST_TIMEOUT for slower machines.
try:
    EXEC_TIMEOUT = int(os.environ.get("KGPC_TEST_TIMEOUT", "10"))
except ValueError:
    EXEC_TIMEOUT = 10

# Timeout for compiler/linker subprocesses when running under Wine/MinGW.
# Can be overridden via environment variables for slow environments.
try:
    COMPILER_TIMEOUT = int(os.environ.get("KGPC_COMPILER_TIMEOUT", "120"))
except ValueError:
    COMPILER_TIMEOUT = 120
try:
    LINK_TIMEOUT = int(os.environ.get("KGPC_LINK_TIMEOUT", "60"))
except ValueError:
    LINK_TIMEOUT = 60

# Per-test timeout (entire test including compile, link, and run).
# Useful for CI to detect hanging tests.
try:
    TEST_CASE_TIMEOUT = int(os.environ.get("KGPC_TEST_CASE_TIMEOUT", "300"))
except ValueError:
    TEST_CASE_TIMEOUT = 300

# ---------------------------------------------------------------------------
# Parallel worker configuration
# ---------------------------------------------------------------------------

# Number of parallel workers for test execution (0 = sequential).
DEFAULT_PARALLEL_WORKERS = max(1, (os.cpu_count() or 1) * 5)
try:
    PARALLEL_WORKERS = int(
        os.environ.get("KGPC_PARALLEL_WORKERS", str(DEFAULT_PARALLEL_WORKERS))
    )
except ValueError:
    PARALLEL_WORKERS = DEFAULT_PARALLEL_WORKERS

# Cap concurrent compiler subprocesses in parallel test mode to avoid
# non-deterministic codegen failures under extreme process pressure.
DEFAULT_COMPILER_PARALLEL_LIMIT = 1
try:
    COMPILER_PARALLEL_LIMIT = int(
        os.environ.get(
            "KGPC_COMPILER_PARALLEL_LIMIT", str(DEFAULT_COMPILER_PARALLEL_LIMIT)
        )
    )
except ValueError:
    COMPILER_PARALLEL_LIMIT = DEFAULT_COMPILER_PARALLEL_LIMIT
COMPILER_PARALLEL_LIMIT = max(1, COMPILER_PARALLEL_LIMIT)
COMPILER_PARALLEL_SEMAPHORE = threading.BoundedSemaphore(COMPILER_PARALLEL_LIMIT)

DEFAULT_TAP_MAX_WORKERS = 8
try:
    TAP_MAX_WORKERS = int(
        os.environ.get("KGPC_TAP_MAX_WORKERS", str(DEFAULT_TAP_MAX_WORKERS))
    )
except ValueError:
    TAP_MAX_WORKERS = DEFAULT_TAP_MAX_WORKERS
TAP_MAX_WORKERS = max(1, TAP_MAX_WORKERS)

# ---------------------------------------------------------------------------
# Feature flags
# ---------------------------------------------------------------------------

# Meson exposes toggleable behaviour via environment variables so CI can
# selectively disable particularly slow checks such as the valgrind leak test.
RUN_VALGRIND_TESTS = os.environ.get("RUN_VALGRIND_TESTS", "false").lower() in (
    "1",
    "true",
    "yes",
)

# Check if VALGRIND environment variable is set to enable valgrind for all tests
VALGRIND_MODE = os.environ.get("VALGRIND", "false").lower() in ("1", "true", "yes")

# FPC RTL test mode: compile test cases against the Free Pascal Compiler RTL
# instead of KGPC's own runtime. Set KGPC_FPC_RTL=1 to enable.
FPC_RTL_MODE = os.environ.get("KGPC_FPC_RTL", "").lower() in ("1", "true", "yes")
FPC_RTL_DIR = os.path.join(os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource"), "rtl")

PP_BOOTSTRAP_FULL_CHAIN_TIMEOUT = 1800
PP_BOOTSTRAP_COMPILE_TIMEOUT = 1500
FPC_RTL_GENERATED_UNITS_DIRNAME = "units"

FAILURE_ARTIFACT_DIR_ENV = os.environ.get("KGPC_CI_FAILURE_DIR")
FAILURE_ARTIFACT_DIR = Path(FAILURE_ARTIFACT_DIR_ENV) if FAILURE_ARTIFACT_DIR_ENV else None

# ---------------------------------------------------------------------------
# Wine subprocess patches — must run at import time to take effect
# ---------------------------------------------------------------------------

_original_subprocess_run = subprocess.run


def _patched_subprocess_run(args, **kwargs):
    """Wrapper that resolves executable to absolute path on Wine."""
    if IS_WINE and args and isinstance(args, (list, tuple)):
        args = list(args)
        if os.path.exists(args[0]):
            args[0] = os.path.abspath(args[0])
    return _original_subprocess_run(args, **kwargs)


subprocess.run = _patched_subprocess_run

# Wine's CreateProcess sometimes returns invalid thread handles, causing
# _winapi.CloseHandle(ht) to raise OSError [WinError 6].  Patch CloseHandle
# to silently ignore "invalid handle" errors so subprocess.Popen doesn't crash.
if IS_WINE:
    import _winapi  # type: ignore
    _original_CloseHandle = _winapi.CloseHandle

    def _safe_CloseHandle(handle):
        try:
            _original_CloseHandle(handle)
        except OSError:
            pass

    _winapi.CloseHandle = _safe_CloseHandle

    if hasattr(subprocess, "Handle"):
        def _safe_handle_close(self):
            if not self.closed:
                self.closed = True
                _safe_CloseHandle(self)

        subprocess.Handle.Close = _safe_handle_close
        subprocess.Handle.__del__ = _safe_handle_close

    _original_popen_internal_poll = subprocess.Popen._internal_poll

    def _safe_popen_internal_poll(self, *args, **kwargs):
        try:
            return _original_popen_internal_poll(self, *args, **kwargs)
        except OSError:
            return self.returncode

    subprocess.Popen._internal_poll = _safe_popen_internal_poll

    _original_popen_del = subprocess.Popen.__del__

    def _safe_popen_del(self, *args, **kwargs):
        try:
            _original_popen_del(self, *args, **kwargs)
        except OSError:
            pass

    subprocess.Popen.__del__ = _safe_popen_del
