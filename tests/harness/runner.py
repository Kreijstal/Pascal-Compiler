# Compiler invocation helpers for the KGPC test harness.
import json
import os
import shutil
import subprocess
import sys
import time

from .env import (
    COMPILER_PARALLEL_SEMAPHORE,
    COMPILER_TIMEOUT,
    IS_WINDOWS_ABI,
    KGPC_PATH,
    VALGRIND_MODE,
)

# ---------------------------------------------------------------------------
# Global mutable state: collected per-run timing data and link args.
# These are populated by run_compiler() and read by TestCompiler.tearDownClass().
# ---------------------------------------------------------------------------
COMPILER_RUNS = []
LINK_ARGS_BY_ASM = {}

# ---------------------------------------------------------------------------
# Target / coverage helpers
# ---------------------------------------------------------------------------

# Flags that explicitly request a target ABI so we do not override them.
EXPLICIT_TARGET_FLAGS = {
    "--target",
    "-target",
    "--target-windows",
    "-target-windows",
    "--windows-abi",
    "--target-sysv",
    "-target-sysv",
    "--sysv-abi",
}

_COVERAGE_ENABLED_CACHE = None


def is_coverage_enabled():
    global _COVERAGE_ENABLED_CACHE
    if _COVERAGE_ENABLED_CACHE is not None:
        return _COVERAGE_ENABLED_CACHE

    build_dir = os.environ.get("MESON_BUILD_ROOT", "build")
    options_path = os.path.join(build_dir, "meson-info", "intro-buildoptions.json")
    try:
        with open(options_path, "r", encoding="utf-8") as f:
            data = json.load(f)
        _COVERAGE_ENABLED_CACHE = next(
            (
                bool(opt.get("value"))
                for opt in data
                if opt.get("name") == "b_coverage"
            ),
            False,
        )
    except (OSError, json.JSONDecodeError):
        _COVERAGE_ENABLED_CACHE = False
    return _COVERAGE_ENABLED_CACHE


def _has_explicit_target_flag(flags):
    if not flags:
        return False
    for flag in flags:
        if flag in EXPLICIT_TARGET_FLAGS:
            return True
        if flag.startswith("--target=") or flag.startswith("-target="):
            return True
    return False

# The compiler is built by Meson now, so this function is not needed.


def run_compiler(input_file, output_file, flags=None, timeout=None):
    """Runs the KGPC compiler with the given arguments."""
    if flags is None:
        flags = []
    else:
        flags = list(flags)
    if timeout is None:
        timeout = COMPILER_TIMEOUT

    # Ensure the output directory exists
    os.makedirs(os.path.dirname(output_file), exist_ok=True)

    command = [KGPC_PATH, input_file, output_file]
    if "--emit-link-args" not in flags:
        command.append("--emit-link-args")
    if IS_WINDOWS_ABI and not _has_explicit_target_flag(flags):
        command.append("--target-windows")
    command.extend(flags)

    # Use valgrind when VALGRIND mode is enabled
    if VALGRIND_MODE and shutil.which("valgrind") is not None:
        valgrind_cmd = [
            "valgrind",
            "--tool=memcheck",
            "--track-origins=yes",
            "--num-callers=50",
            "--error-exitcode=1",
        ]
        command = valgrind_cmd + command
        print(f"--- Running compiler with valgrind: {' '.join(command)} ---", file=sys.stderr)
        sys.stderr.flush()
    else:
        print(f"--- Running compiler: {' '.join(command)} ---", file=sys.stderr)
        sys.stderr.flush()

    start = time.perf_counter()
    try:
        run_kwargs = {
            "check": True,
            "capture_output": True,
            "text": True,
            "timeout": timeout,
        }
        with COMPILER_PARALLEL_SEMAPHORE:
            result = subprocess.run(command, **run_kwargs)
        duration = time.perf_counter() - start
        for line in result.stderr.splitlines():
            if line.startswith("KGPC_LINK_ARGS:"):
                raw_args = line[len("KGPC_LINK_ARGS:"):].strip()
                LINK_ARGS_BY_ASM[output_file] = raw_args.split() if raw_args else []
        COMPILER_RUNS.append(
            {
                "command": command,
                "duration": duration,
                "returncode": result.returncode,
            }
        )
        print(
            result.stderr, file=sys.stderr
        )  # The compiler prints status messages to stderr
        return result.stderr
    except subprocess.CalledProcessError as e:
        duration = time.perf_counter() - start
        COMPILER_RUNS.append(
            {
                "command": command,
                "duration": duration,
                "returncode": e.returncode,
            }
        )
        print(f"--- Compiler execution failed ---", file=sys.stderr)
        print(f"--- stdout: {e.stdout} ---", file=sys.stderr)
        print(f"--- stderr: {e.stderr} ---", file=sys.stderr)
        raise
