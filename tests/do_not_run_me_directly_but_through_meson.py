# THIS PROGRAM WILL NOT WORK IF YOU DO NOT COMPILE SOURCES FIRST WITH MESON
import argparse
import json
import os
import shutil
import shlex
import socket
import subprocess
import sys
import time
import traceback
import unittest
from pathlib import Path

WINDOWS_ABI_PLATFORMS = ("win", "cygwin", "msys", "mingw")
PLATFORM_ID = sys.platform.lower()
IS_WINDOWS_ABI = os.name == "nt" or PLATFORM_ID.startswith(WINDOWS_ABI_PLATFORMS)

# Detect if we're running under Wine (Windows Python on Linux)
# This happens during cross-compilation testing
# Check for Wine-specific environment variables
IS_WINE = IS_WINDOWS_ABI and any(k.startswith("WINE") for k in os.environ)

# Path to the compiler executable
# Get the build directory from the environment variable set by Meson.
# Default to "build" for local testing.
build_dir = os.environ.get("MESON_BUILD_ROOT", "build")
KGPC_PATH = os.path.join(build_dir, "KGPC/kgpc.exe" if IS_WINDOWS_ABI else "KGPC/kgpc")
TEST_CASES_DIR = "tests/test_cases"
TEST_OUTPUT_DIR = "tests/output"
GOLDEN_AST_DIR = "tests/golden_ast"
# Default execution timeout per compiled test program (seconds).
# Can be overridden via environment variable KGPC_TEST_TIMEOUT for slower machines.
try:
    EXEC_TIMEOUT = int(os.environ.get("KGPC_TEST_TIMEOUT", "10"))
except ValueError:
    EXEC_TIMEOUT = 10

# Meson exposes toggleable behaviour via environment variables so CI can
# selectively disable particularly slow checks such as the valgrind leak test.
RUN_VALGRIND_TESTS = os.environ.get("RUN_VALGRIND_TESTS", "false").lower() in (
    "1",
    "true",
    "yes",
)

# Check if VALGRIND environment variable is set to enable valgrind for all tests
VALGRIND_MODE = os.environ.get("VALGRIND", "false").lower() in ("1", "true", "yes")

# Track how long individual compiler invocations take so we can identify the
# slowest scenarios when the test suite finishes. The collected data is emitted
# from TestCompiler.tearDownClass() and written to stderr to keep TAP output
# intact.
COMPILER_RUNS = []

FAILURE_ARTIFACT_DIR_ENV = os.environ.get("KGPC_CI_FAILURE_DIR")
FAILURE_ARTIFACT_DIR = Path(FAILURE_ARTIFACT_DIR_ENV) if FAILURE_ARTIFACT_DIR_ENV else None


def _sanitize_test_identifier(name):
    return (
        name.replace("/", "_")
        .replace("\\", "_")
        .replace(":", "_")
        .replace(" ", "_")
    )


def _copy_artifact(path, dest_dir):
    if not path:
        return
    src = Path(path)
    if not src.exists():
        return
    try:
        shutil.copy2(src, dest_dir / src.name)
    except OSError:
        pass


def _write_artifact_text(dest_dir, filename, content):
    if content is None:
        return
    target = dest_dir / filename
    target.write_text(str(content), encoding="utf-8", errors="ignore")


def _store_failure_artifacts(
    test_id,
    base_name=None,
    *,
    input_file=None,
    asm_file=None,
    executable_file=None,
    expected_file=None,
    compiler_output=None,
    normalized_output=None,
    raw_stdout=None,
    raw_stderr=None,
    expected_output=None,
    returncode=None,
    exception_text=None,
):
    if FAILURE_ARTIFACT_DIR is None:
        return

    case_name = base_name or test_id.split(".")[-1]
    dest = FAILURE_ARTIFACT_DIR / _sanitize_test_identifier(case_name)
    dest.mkdir(parents=True, exist_ok=True)

    info_lines = [
        f"test_id={test_id}",
        f"case={case_name}",
    ]
    if returncode is not None:
        info_lines.append(f"returncode={returncode}")
    (dest / "info.txt").write_text(
        "\n".join(info_lines), encoding="utf-8", errors="ignore"
    )

    for candidate in (input_file, asm_file, executable_file, expected_file):
        _copy_artifact(candidate, dest)

    _write_artifact_text(dest, "compiler-stderr.txt", compiler_output)
    _write_artifact_text(dest, "raw-stdout.txt", raw_stdout)
    _write_artifact_text(dest, "raw-stderr.txt", raw_stderr)
    _write_artifact_text(dest, "normalized-output.txt", normalized_output)
    _write_artifact_text(dest, "expected-output.txt", expected_output)
    if exception_text:
        _write_artifact_text(dest, "exception.txt", exception_text)

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


def run_executable_with_valgrind(executable_args, **kwargs):
    """Run an executable, optionally with valgrind in CI mode."""
    command = list(executable_args)
    
    # Use valgrind when CI mode is enabled and valgrind is available
    if VALGRIND_MODE and shutil.which("valgrind") is not None:
        valgrind_cmd = [
            "valgrind",
            "--tool=memcheck",
            "--track-origins=yes",
            "--num-callers=50",
            "--error-exitcode=1",
        ]
        command = valgrind_cmd + command
        print(f"--- Running executable with valgrind: {' '.join(command)} ---", file=sys.stderr)
    
    return subprocess.run(command, **kwargs)


def run_compiler(input_file, output_file, flags=None):
    """Runs the KGPC compiler with the given arguments."""
    if flags is None:
        flags = []
    else:
        flags = list(flags)

    # Ensure the output directory exists
    os.makedirs(os.path.dirname(output_file), exist_ok=True)

    command = [KGPC_PATH, input_file, output_file]
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
    else:
        print(f"--- Running compiler: {' '.join(command)} ---", file=sys.stderr)
    
    start = time.perf_counter()
    try:
        result = subprocess.run(command, check=True, capture_output=True, text=True)
        duration = time.perf_counter() - start
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


class TAPTestResult(unittest.TestResult):
    def __init__(self, stream):
        super().__init__()
        self.stream = stream
        self._test_index = 0
        self._test_states = {}

    def _emit(self, line):
        self.stream.write(f"{line}\n")
        self.stream.flush()

    def _emit_diagnostic(self, text):
        for raw_line in text.rstrip().splitlines():
            self._emit(f"# {raw_line}")

    def _test_name(self, test):
        try:
            return test.id()
        except AttributeError:
            return str(test)

    def _subtest_name(self, subtest):
        if subtest is None:
            return ""
        params = getattr(subtest, "params", None) or {}
        msg = getattr(subtest, "message", None)
        parts = []
        if params:
            formatted = ", ".join(
                f"{key}={value!r}" for key, value in sorted(params.items())
            )
            parts.append(formatted)
        if msg:
            parts.append(str(msg))
        if parts:
            return " | ".join(parts)
        return str(subtest)

    def _get_state(self, test):
        return self._test_states.setdefault(
            test, {"reported": False, "had_failure": False}
        )

    def _emit_failure_header(self, test):
        state = self._get_state(test)
        if not state["reported"]:
            self._emit(f"not ok {self._test_index} - {self._test_name(test)}")
            state["reported"] = True
        return state

    def _mark_failure(self, test):
        state = self._get_state(test)
        state["had_failure"] = True
        return state

    def startTest(self, test):
        super().startTest(test)
        self._test_index += 1
        self._test_states[test] = {"reported": False, "had_failure": False}

    def stopTest(self, test):
        super().stopTest(test)
        self._test_states.pop(test, None)

    def addSuccess(self, test):
        super().addSuccess(test)
        state = self._test_states.get(test)
        if state and state.get("had_failure"):
            return
        self._emit(f"ok {self._test_index} - {self._test_name(test)}")
        if state:
            state["reported"] = True

    def addSkip(self, test, reason):
        super().addSkip(test, reason)
        self._emit(f"ok {self._test_index} - {self._test_name(test)} # SKIP {reason}")
        state = self._test_states.get(test)
        if state:
            state["reported"] = True

    def addExpectedFailure(self, test, err):
        super().addExpectedFailure(test, err)
        self._emit(
            f"ok {self._test_index} - {self._test_name(test)} # TODO expected failure"
        )
        state = self._test_states.get(test)
        if state:
            state["reported"] = True

    def addUnexpectedSuccess(self, test):
        super().addUnexpectedSuccess(test)
        self._emit(
            f"not ok {self._test_index} - {self._test_name(test)} # Unexpected success"
        )
        self._mark_failure(test)

    def addFailure(self, test, err):
        super().addFailure(test, err)
        self._emit_failure_header(test)
        self._emit_diagnostic("Failure:")
        self._emit_diagnostic("".join(traceback.format_exception(*err)))
        self._mark_failure(test)

    def addError(self, test, err):
        super().addError(test, err)
        self._emit_failure_header(test)
        self._emit_diagnostic("Error:")
        self._emit_diagnostic("".join(traceback.format_exception(*err)))
        self._mark_failure(test)

    def addSubTest(self, test, subtest, err):
        super().addSubTest(test, subtest, err)
        if err is None:
            return
        self._emit_failure_header(test)
        description = self._subtest_name(subtest)
        if description:
            self._emit_diagnostic(f"Subtest failed: {description}")
        else:
            self._emit_diagnostic("Subtest failed")
        self._emit_diagnostic("".join(traceback.format_exception(*err)))
        self._mark_failure(test)


class TAPTestRunner:
    def __init__(self, stream=None):
        self.stream = stream or sys.stdout

    def run(self, test):
        result = TAPTestResult(self.stream)
        test_count = test.countTestCases()
        result.startTestRun()
        try:
            result.stream.write(f"1..{test_count}\n")
            result.stream.flush()
            test(result)
        finally:
            result.stopTestRun()
        return result


def read_file_content(filepath):
    """Reads and returns the content of a file."""
    with open(filepath, "r") as f:
        return f.read()


class TestCompiler(unittest.TestCase):
    def __init__(self, methodName="runTest"):
        super().__init__(methodName)
        self._artifact_context = None

    def record_failure_context(self, **kwargs):
        if FAILURE_ARTIFACT_DIR is None:
            return
        if self._artifact_context is None:
            self._artifact_context = {}
        context = {}
        for key, value in kwargs.items():
            if value is not None:
                context[key] = value
        if not context:
            return
        existing = dict(self._artifact_context)
        existing.update(context)
        if "base_name" not in existing:
            existing["base_name"] = kwargs.get("base_name") or self.id().split(".")[-1]
        self._artifact_context = existing

    def _callTestMethod(self, method):
        try:
            super()._callTestMethod(method)
        except Exception:
            if FAILURE_ARTIFACT_DIR is not None:
                ctx = self._artifact_context or {}
                ctx = dict(ctx)
                ctx.setdefault("exception_text", traceback.format_exc())
                base_name = ctx.pop("base_name", None)
                _store_failure_artifacts(self.id(), base_name, **ctx)
            raise
        finally:
            self._artifact_context = None
    @classmethod
    def setUpClass(cls):
        cls._ensure_compiler_built()
        # Create output directories
        os.makedirs(TEST_OUTPUT_DIR, exist_ok=True)
        os.makedirs(TEST_CASES_DIR, exist_ok=True)

        cc_raw = os.environ.get("CC")
        if not cc_raw:
            # Attempt to infer CC from Meson build directory for local pytest runs.
            # This keeps Meson-driven CI behaviour (which sets CC explicitly)
            # while avoiding a hard failure when running tests directly.
            meson_private = os.path.join(build_dir, "meson-private")
            cmdline_path = os.path.join(meson_private, "cmdline.txt")
            inferred_cc = None
            try:
                if os.path.exists(cmdline_path):
                    with open(cmdline_path, "r") as f:
                        for token in f.read().split():
                            if token.startswith("-Dcc="):
                                inferred_cc = token[len("-Dcc="):]
                                break
            except OSError:
                inferred_cc = None

            if not inferred_cc:
                inferred_cc = shutil.which("cc") or shutil.which("gcc")

            if inferred_cc:
                cc_raw = inferred_cc
                os.environ["CC"] = cc_raw
            else:
                raise RuntimeError(
                    "CC environment variable must be set by Meson before running tests"
                )
        cls.c_compiler_display = cc_raw
        # Use Windows-style splitting when running on Windows to avoid mangling
        # backslashes in paths such as "E:\msys64\...".
        cls.c_compiler_cmd = shlex.split(cc_raw, posix=(os.name != "nt"))
        if not cls.c_compiler_cmd:
            raise RuntimeError("CC environment variable did not contain an executable")

        # When running under Wine (cross-compilation testing), the CC from meson
        # points to Linux wrapper scripts that Wine/Windows Python cannot execute.
        # We need to find and use the Windows-native compiler from MSYS2 instead.
        if IS_WINE:
            # Look for gcc.exe in the quasi-msys2 directory structure
            # Use relative paths from build_dir to avoid absolute Windows paths
            msys2_search_paths = [
                # Look in quasi-msys2/root/{ucrt64,mingw64}/bin/
                os.path.join(build_dir, "..", "quasi-msys2", "root", "ucrt64", "bin"),
                os.path.join(build_dir, "..", "quasi-msys2", "root", "mingw64", "bin"),
            ]
            
            wine_gcc = None
            for search_dir in msys2_search_paths:
                normalized_dir = os.path.normpath(search_dir)
                gcc_path = os.path.join(normalized_dir, "gcc.exe")
                if os.path.exists(gcc_path):
                    wine_gcc = gcc_path
                    break
            
            if wine_gcc:
                # Use the Windows-native GCC
                cls.c_compiler_cmd = [wine_gcc]
                cls.c_compiler_display = f"{cc_raw} (using Wine-compatible {wine_gcc})"
                print(f"Wine detected: Using Windows-native GCC at {wine_gcc}", file=sys.stderr)
            else:
                # Fallback: try to use gcc.exe directly from PATH
                # The quasi-msys2 environment should have added the bin dir to PATH
                cls.c_compiler_cmd = ["gcc.exe"]
                cls.c_compiler_display = f"{cc_raw} (using Wine-compatible gcc.exe from PATH)"
                print(f"Wine detected: Using gcc.exe from PATH (searched: {msys2_search_paths})", file=sys.stderr)

        cls.runtime_library = os.environ.get("KGPC_RUNTIME_LIB")
        if not cls.runtime_library:
            # Try to infer the runtime library from the Meson build tree so that
            # tests can be run via pytest without Meson explicitly setting env.
            candidate = os.path.join(build_dir, "KGPC", "libkgpc_runtime.a")
            if os.path.exists(candidate):
                cls.runtime_library = candidate
                os.environ["KGPC_RUNTIME_LIB"] = candidate
        if not cls.runtime_library:
            raise RuntimeError(
                "KGPC_RUNTIME_LIB environment variable is required to link generated code"
            )
        if not os.path.exists(cls.runtime_library):
            raise RuntimeError(
                f"Runtime library path from KGPC_RUNTIME_LIB does not exist: {cls.runtime_library}"
            )

        cls.ctypes_helper_library = os.environ.get("KGPC_CTYPES_HELPER")
        if not cls.ctypes_helper_library:
            # Fallback for local runs: look for ctypes_helper in Meson build dir
            # matching the name produced in KGPC/meson.build.
            for name in ("ctypes_helper.so", "libctypes_helper.so", "ctypes_helper.dylib", "libctypes_helper.dylib"):
                candidate = os.path.join(build_dir, "KGPC", name)
                if os.path.exists(candidate):
                    cls.ctypes_helper_library = candidate
                    os.environ["KGPC_CTYPES_HELPER"] = candidate
                    break
        if cls.ctypes_helper_library is not None and not os.path.exists(
            cls.ctypes_helper_library
        ):
            raise RuntimeError(
                "ctypes helper shared library provided by Meson does not exist: "
                f"{cls.ctypes_helper_library}"
            )

        raw_ctypes_helper_link = os.environ.get("KGPC_CTYPES_HELPER_LINK")
        if raw_ctypes_helper_link is None and cls.ctypes_helper_library is not None:
            # Default to using the helper library itself when invoked locally.
            raw_ctypes_helper_link = cls.ctypes_helper_library
            os.environ["KGPC_CTYPES_HELPER_LINK"] = raw_ctypes_helper_link
        cls.ctypes_helper_link = cls._resolve_ctypes_helper_link(
            raw_ctypes_helper_link,
            cls.ctypes_helper_library,
        )
        if (
            raw_ctypes_helper_link is not None
            and cls.ctypes_helper_link is None
            and os.path.exists(raw_ctypes_helper_link)
        ):
            # The provided path exists, so use it even if it might not be ideal for
            # linking (e.g. a DLL without an import library).
            cls.ctypes_helper_link = raw_ctypes_helper_link
        if (
            raw_ctypes_helper_link is not None
            and cls.ctypes_helper_link is None
        ):
            raise RuntimeError(
                "Unable to resolve ctypes helper import library from Meson-provided path: "
                f"{raw_ctypes_helper_link}"
            )
        cls.ctypes_helper_dir = (
            os.path.dirname(cls.ctypes_helper_library)
            if cls.ctypes_helper_library is not None
            else None
        )
        # Ensure runtime loader can find ctypes helper when running locally.
        if cls.ctypes_helper_dir:
            path_var = (
                "PATH" if IS_WINDOWS_ABI else
                ("DYLD_LIBRARY_PATH" if sys.platform == "darwin" else "LD_LIBRARY_PATH")
            )
            current = os.environ.get(path_var, "")
            if cls.ctypes_helper_dir not in current.split(os.pathsep):
                os.environ[path_var] = (
                    cls.ctypes_helper_dir + (os.pathsep + current if current else "")
                )
        cls.have_gmp = os.environ.get("KGPC_HAVE_GMP", "0") == "1"

    @classmethod
    def _ensure_compiler_built(cls):
        """Builds the compiler via Meson if the kgpc binary is missing."""
        if os.path.exists(KGPC_PATH):
            return

        meson = shutil.which("meson")
        if meson is None:
            raise RuntimeError(
                "Meson is required to build the compiler but is not available in PATH"
            )

        build_root = build_dir
        build_ninja = os.path.join(build_root, "build.ninja")

        setup_command = [meson, "setup", build_root, "-Dbuild_integration_tests=true"]
        setup_mode = "setup"

        # If the build directory already exists, reconfigure it so integration tests
        # stay enabled even if a previous configuration disabled them.
        if os.path.exists(build_ninja):
            setup_command.insert(2, "--reconfigure")
            setup_mode = "reconfigure"

        try:
            subprocess.run(
                setup_command,
                check=True,
                capture_output=True,
                text=True,
            )
        except subprocess.CalledProcessError as e:
            raise RuntimeError(
                f"Meson {setup_mode} failed:\nSTDOUT:\n{e.stdout}\nSTDERR:\n{e.stderr}"
            )

        try:
            subprocess.run(
                [meson, "compile", "-C", build_root],
                check=True,
                capture_output=True,
                text=True,
            )
        except subprocess.CalledProcessError as e:
            raise RuntimeError(
                f"Meson compile failed:\nSTDOUT:\n{e.stdout}\nSTDERR:\n{e.stderr}"
            )

        if not os.path.exists(KGPC_PATH):
            raise RuntimeError(
                f"Meson build completed but did not produce compiler at {KGPC_PATH}"
            )

    @classmethod
    def _resolve_ctypes_helper_link(cls, candidate_path, shared_library_path):
        """Determine which file should be passed to the C compiler for ctypes tests."""

        search_paths = []
        last_resort = None

        if candidate_path:
            search_paths.append(candidate_path)
        if shared_library_path and shared_library_path not in search_paths:
            search_paths.append(shared_library_path)

        for path in search_paths:
            if path is None:
                continue
            if os.path.exists(path) and not path.lower().endswith(".dll"):
                return path
            if path.lower().endswith(".dll"):
                directory = os.path.dirname(path)
                stem = os.path.splitext(os.path.basename(path))[0]
                candidates = [
                    os.path.join(directory, stem + ".dll.a"),
                    os.path.join(directory, stem + ".a"),
                    os.path.join(directory, stem + ".lib"),
                ]
                if not stem.startswith("lib"):
                    candidates.extend(
                        [
                            os.path.join(directory, "lib" + stem + ".dll.a"),
                            os.path.join(directory, "lib" + stem + ".a"),
                        ]
                    )
                else:
                    stripped = stem[3:]
                    if stripped:
                        candidates.append(os.path.join(directory, stripped + ".lib"))
                for candidate in candidates:
                    if os.path.exists(candidate):
                        return candidate
            if os.path.exists(path):
                last_resort = path

        return last_resort

    def compile_executable(
        self, asm_file, executable_file, extra_objects=None, extra_link_args=None
    ):
        if extra_objects is None:
            extra_objects = []
        if extra_link_args is None:
            extra_link_args = []
        try:
            command = list(self.c_compiler_cmd)
            command.append("-O2")
            if not IS_WINDOWS_ABI:
                command.append("-no-pie")
            else:
                # Allow user-provided shims (e.g., LoadLibrary_s) to override runtime
                # fallbacks without duplicate-definition errors under MinGW.
                command.append("-Wl,--allow-multiple-definition")
            if is_coverage_enabled():
                command.append("--coverage")
            command.extend([
                "-o",
                executable_file,
                asm_file,
                str(self.runtime_library),
            ])
            command.extend(list(extra_objects))
            command.extend(list(extra_link_args))
            if not IS_WINDOWS_ABI:
                command.append("-lm")
            subprocess.run(
                command,
                check=True,
                capture_output=True,
                text=True,
            )
        except subprocess.CalledProcessError as e:
            self.fail(f"{self.c_compiler_display} compilation failed: {e.stderr}")

    def _get_test_paths(self, name, extension="p"):
        input_file = os.path.join(TEST_CASES_DIR, f"{name}.{extension}")
        asm_file = os.path.join(TEST_OUTPUT_DIR, f"{name}.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, name)
        return input_file, asm_file, executable_file

    @classmethod
    def tearDownClass(cls):
        super().tearDownClass()
        if not COMPILER_RUNS:
            return

        total_time = sum(entry["duration"] for entry in COMPILER_RUNS)
        parse_only_runs = [
            entry for entry in COMPILER_RUNS if "-parse-only" in entry["command"]
        ]
        parse_only_time = sum(entry["duration"] for entry in parse_only_runs)
        failing_runs = [entry for entry in COMPILER_RUNS if entry["returncode"] != 0]
        failing_time = sum(entry["duration"] for entry in failing_runs)

        print("--- Compiler run timing summary ---", file=sys.stderr)
        print(
            f"Total compiler invocations: {len(COMPILER_RUNS)} in {total_time:.2f}s",
            file=sys.stderr,
        )
        print(
            f"Parse-only invocations: {len(parse_only_runs)} taking {parse_only_time:.2f}s",
            file=sys.stderr,
        )
        if failing_runs:
            print(
                f"Failing (non-zero exit) invocations: {len(failing_runs)} taking {failing_time:.2f}s",
                file=sys.stderr,
            )

        print("Slowest compiler commands:", file=sys.stderr)
        for entry in sorted(
            COMPILER_RUNS, key=lambda item: item["duration"], reverse=True
        )[:5]:
            command_str = " ".join(entry["command"])
            print(
                f"  {entry['duration']:.2f}s | rc={entry['returncode']} | {command_str}",
                file=sys.stderr,
            )

    def test_constant_folding_o1(self):
        """Tests the -O1 constant folding optimization."""
        input_file = os.path.join(TEST_CASES_DIR, "simple_expr.p")

        # --- Run without optimization ---
        unoptimized_output_file = os.path.join(
            TEST_OUTPUT_DIR, "simple_expr_unoptimized.s"
        )
        run_compiler(input_file, unoptimized_output_file)
        unoptimized_asm = read_file_content(unoptimized_output_file)

        # In the unoptimized version, we expect to see the `add` instruction.
        # The compiler might use `addl` for 32-bit integers.
        # I'll check for "addl" since the compiler seems to be generating 32-bit code.
        self.assertIn("addl", unoptimized_asm)

        # --- Run with -O1 optimization ---
        optimized_output_file = os.path.join(
            TEST_OUTPUT_DIR, "simple_expr_optimized_o1.s"
        )
        run_compiler(input_file, optimized_output_file, flags=["-O1"])
        optimized_asm = read_file_content(optimized_output_file)

        # In the optimized version, we expect the constant `5` to be moved directly.
        self.assertIn("movl\t$5", optimized_asm)
        # And we should not see the `add` instruction.
        self.assertNotIn("addl", optimized_asm)

    def test_dateutils_custom(self):
        """Tests DateUtils with custom regex verification."""
        input_file = os.path.join(TEST_CASES_DIR, "missing_dateutils.p")
        output_file = os.path.join(TEST_OUTPUT_DIR, "missing_dateutils.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "missing_dateutils")

        # Compile
        run_compiler(input_file, output_file)
        
        # Assemble and Link
        self.compile_executable(output_file, executable_file)
        
        # Run
        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT
        )
        
        # Verify output matches date format (e.g. 24-11-25 16:27:34)
        # We'll be lenient with the exact format for now, just checking for numbers and separators
        import re
        # Expecting something like: Current time: YY-MM-DD HH:MM:SS
        output = result.stdout.strip()
        # Simple regex for date-like string
        date_pattern = r"Current time: \d{1,4}-\d{1,2}-\d{1,2} \d{1,2}:\d{1,2}:\d{1,2}"
        
        if not re.search(date_pattern, output):
            self.fail(f"Output '{output}' does not match date pattern '{date_pattern}'")

    def test_dead_code_elimination_o2(self):
        """Tests the -O2 dead code elimination optimization."""
        input_file = os.path.join(TEST_CASES_DIR, "dead_code.p")

        # --- Run without optimization ---
        unoptimized_output_file = os.path.join(
            TEST_OUTPUT_DIR, "dead_code_unoptimized.s"
        )
        run_compiler(input_file, unoptimized_output_file)
        unoptimized_asm = read_file_content(unoptimized_output_file)

        # In the unoptimized version, we expect the variables x and y to be allocated.
        # Global variables are allocated in the BSS section, not on the stack.
        # We should see .comm directives for both variables.
        self.assertIn(".comm\t__kgpc_program_var_x_1", unoptimized_asm)
        self.assertIn(".comm\t__kgpc_program_var_y_2", unoptimized_asm)

        # --- Run with -O2 optimization ---
        optimized_output_file = os.path.join(
            TEST_OUTPUT_DIR, "dead_code_optimized_o2.s"
        )
        run_compiler(input_file, optimized_output_file, flags=["-O2"])
        optimized_asm = read_file_content(optimized_output_file)

        # In the optimized version, the variable `y` should be removed (dead code elimination).
        # The variable `x` might also be removed because it is assigned to but never used.
        # We check that the optimized assembly is smaller than the unoptimized one.
        self.assertLess(len(optimized_asm), len(unoptimized_asm))
        
        # Additionally, we should not see the unused variable y in the optimized version
        self.assertNotIn(".comm\t__kgpc_program_var_y_2", optimized_asm)

    def test_parser_ast_dump_matches_golden(self):
        """Ensures the AST dump matches the golden files for representative programs."""
        cases = {
            "helloworld": os.path.join(TEST_CASES_DIR, "helloworld.p"),
            "simple_expr": os.path.join(TEST_CASES_DIR, "simple_expr.p"),
        }

        for name, input_file in cases.items():
            with self.subTest(case=name):
                asm_file = os.path.join(TEST_OUTPUT_DIR, f"{name}_parse_only.s")
                ast_file = os.path.join(TEST_OUTPUT_DIR, f"{name}.ast")
                run_compiler(
                    input_file,
                    asm_file,
                    flags=["-parse-only", "--dump-ast", ast_file],
                )
                actual = read_file_content(ast_file)
                expected_path = os.path.join(GOLDEN_AST_DIR, f"{name}.ast")
                expected = read_file_content(expected_path)
                self.assertEqual(actual, expected)

    def test_bell_numbers_sample_parses(self):
        """Ensures the large BellNumbers sample that uses "+=" parses successfully."""
        if not self.have_gmp:
            self.skipTest("GMP support is not available")
        input_file = os.path.join(TEST_CASES_DIR, "bell_numbers.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "bell_numbers_parse_only.s")
        ast_file = os.path.join(TEST_OUTPUT_DIR, "bell_numbers.ast")

        run_compiler(
            input_file,
            asm_file,
            flags=["-parse-only", "--dump-ast", ast_file],
        )

        self.assertTrue(os.path.exists(ast_file))
        self.assertGreater(os.path.getsize(ast_file), 0)

    def test_mtinf_sample_parses(self):
        """The mtinf QL sample should parse in parse-only mode without errors."""
        input_file = os.path.join(TEST_CASES_DIR, "mtinf.pas")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "mtinf_parse_only.s")

        run_compiler(
            input_file,
            asm_file,
            flags=["-parse-only"],
        )

        self.assertTrue(os.path.exists(asm_file))
        self.assertGreater(os.path.getsize(asm_file), 0)

    def test_real_literal_codegen(self):
        """Compiling a real literal should succeed and materialize the IEEE-754 bits."""
        input_file = os.path.join(TEST_CASES_DIR, "real_literal.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "real_literal.s")
        run_compiler(input_file, asm_file)
        asm = read_file_content(asm_file)
        literal_bits = "4609434218613702656"
        self.assertIn(literal_bits, asm)

    def test_conditional_macros_skip_inactive_branch(self):
        """Conditional macros should skip inactive branches during preprocessing."""
        input_file, asm_file, executable_file = self._get_test_paths(
            "conditional_macros"
        )

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        result = run_executable_with_valgrind(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout, "42\n")

    def test_conditional_macros_invalid_syntax_reports_error(self):
        """Invalid conditional macro syntax should surface a preprocessing error."""
        input_file, asm_file, _ = self._get_test_paths(
            "conditional_macros_invalid_syntax"
        )

        with self.assertRaises(subprocess.CalledProcessError) as cm:
            run_compiler(input_file, asm_file)

        stderr = cm.exception.stderr or ""
        self.assertIn("Preprocessing failed", stderr)
        self.assertIn("unsupported {$IF} expression", stderr)

    def test_conditional_macros_undefined_macro_reports_error(self):
        """Referencing an undefined macro should abort preprocessing."""
        input_file, asm_file, _ = self._get_test_paths(
            "conditional_macros_undefined_macro"
        )

        with self.assertRaises(subprocess.CalledProcessError) as cm:
            run_compiler(input_file, asm_file)

        stderr = cm.exception.stderr or ""
        self.assertIn("Preprocessing failed", stderr)
        self.assertIn("undefined macro 'MISSING_SYMBOL'", stderr)

    def test_conditional_macros_malformed_block_reports_error(self):
        """Missing {$ENDIF} directives should be reported by the preprocessor."""
        input_file, asm_file, _ = self._get_test_paths(
            "conditional_macros_malformed_block"
        )

        with self.assertRaises(subprocess.CalledProcessError) as cm:
            run_compiler(input_file, asm_file)

        stderr = cm.exception.stderr or ""
        self.assertIn("unterminated conditional", stderr)

    def test_classof_nonclass_target_reports_error(self):
        """'class of Integer' must be rejected - Integer is not a class type."""
        input_file, asm_file, _ = self._get_test_paths("bug_classof_nonclass_target")

        with self.assertRaises(subprocess.CalledProcessError) as cm:
            run_compiler(input_file, asm_file)

        stderr = cm.exception.stderr or ""
        self.assertIn("class of", stderr.lower())
        self.assertIn("class type", stderr.lower())

    def test_classref_incompatible_assignment_reports_error(self):
        """Assigning TB to 'class of TA' must be rejected - unrelated classes."""
        input_file, asm_file, _ = self._get_test_paths("bug_classref_incompatible_assignment")

        with self.assertRaises(subprocess.CalledProcessError) as cm:
            run_compiler(input_file, asm_file)

        stderr = cm.exception.stderr or ""
        # Should report incompatible types
        self.assertIn("incompatible", stderr.lower())

    def test_bitwise_operations_execute(self):
        """Bitwise shifts and rotates should execute correctly and match expected output."""
        input_file, asm_file, executable_file = self._get_test_paths("bitwise_ops")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        expected_path = os.path.join(TEST_CASES_DIR, "bitwise_ops.expected")
        expected_output = read_file_content(expected_path)
        self.assertEqual(
            result.stdout.strip().splitlines(),
            expected_output.strip().splitlines(),
        )

    def test_bitshift_codegen_emits_rotate_instructions(self):
        """Code generation should emit rotate instructions for ROL and ROR expressions."""
        input_file = os.path.join(TEST_CASES_DIR, "bitshift_expr.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "bitshift_expr.s")

        run_compiler(input_file, asm_file)
        asm = read_file_content(asm_file)

        self.assertTrue(any(token in asm for token in ("\tsall\t", "\tshlq\t")))
        self.assertTrue(any(token in asm for token in ("\tsarl\t", "\tsarq\t")))
        self.assertTrue(any(token in asm for token in ("\troll\t", "\trolq\t")))
        self.assertTrue(any(token in asm for token in ("\trorl\t", "\trorq\t")))

    def test_bitshift_malformed_input_reports_error(self):
        """Malformed bitshift expressions should surface a descriptive parse error."""
        input_file, asm_file, _ = self._get_test_paths("bitshift_expr_malformed")

        with self.assertRaises(subprocess.CalledProcessError) as cm:
            run_compiler(input_file, asm_file)

        stderr = (cm.exception.stderr or "").lower()
        self.assertIn("parse error", stderr)
        self.assertIn("expected", stderr)

    def test_parse_only_has_no_leaks_under_valgrind(self):
        """Runs a small parse-only compilation under valgrind to ensure no leaks are reported."""
        if not RUN_VALGRIND_TESTS:
            self.skipTest("valgrind checks disabled via Meson option")
        if shutil.which("valgrind") is None:
            self.skipTest("valgrind is not installed")

        input_file = os.path.join(TEST_CASES_DIR, "helloworld.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "helloworld_valgrind.s")
        ast_file = os.path.join(TEST_OUTPUT_DIR, "helloworld_valgrind.ast")

        command = [
            "valgrind",
            "--leak-check=full",
            "--error-exitcode=1",
            KGPC_PATH,
            input_file,
            asm_file,
            "-parse-only",
            "--dump-ast",
            ast_file,
        ]

        result = subprocess.run(command, capture_output=True, text=True)
        if result.returncode != 0:
            self.fail(
                "Valgrind reported memory issues:\n"
                f"STDOUT:\n{result.stdout}\nSTDERR:\n{result.stderr}"
            )

        self.assertTrue(os.path.exists(ast_file))

    def test_ctypes_can_call_dynamic_library(self):
        """Builds a shared C helper and ensures Pascal code can call into it via ctypes aliases."""
        input_file = os.path.join(TEST_CASES_DIR, "ctypes_dll_demo.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "ctypes_dll_demo.s")

        run_compiler(input_file, asm_file)

        executable_file = os.path.join(TEST_OUTPUT_DIR, "ctypes_dll_demo")
        if self.ctypes_helper_link is None:
            self.fail(
                "Unable to locate ctypes helper import library; ensure Meson exposed it"
            )

        self.compile_executable(
            asm_file,
            executable_file,
            extra_objects=[self.ctypes_helper_link],
        )

        env = os.environ.copy()
        if IS_WINDOWS_ABI:
            path_var = "PATH"
        elif sys.platform == "darwin":
            path_var = "DYLD_LIBRARY_PATH"
        else:
            path_var = "LD_LIBRARY_PATH"
        existing = env.get(path_var, "")
        helper_dir = self.ctypes_helper_dir
        if helper_dir is None:
            self.fail("KGPC_CTYPES_HELPER must be provided to run ctypes demo")
        env[path_var] = helper_dir + (os.pathsep + existing if existing else "")

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            env=env,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout.strip(), "42")

    def test_ctypes_pointer_aliases(self):
        """Ensures pointer helper aliases in ctypes behave like regular Pascal pointers."""
        input_file = os.path.join(TEST_CASES_DIR, "ctypes_pointer_demo.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "ctypes_pointer_demo.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "ctypes_pointer_demo")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout, "42\n7\n1\n")

    def test_pointer_operators_program(self):
        """Compiling pointer operators program should produce the expected output."""
        input_file = os.path.join(TEST_CASES_DIR, "pointer_operators.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "pointer_operators.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "pointer_operators")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout, "42\n42\n")

    def test_pointer_simple_program(self):
        """Compiles and runs a program that assigns NIL to a typed pointer."""
        input_file = os.path.join(TEST_CASES_DIR, "pointer_simple.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "pointer_simple.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "pointer_simple")

        run_compiler(input_file, asm_file)
        self.assertTrue(os.path.exists(asm_file))
        self.assertGreater(os.path.getsize(asm_file), 0)

        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout, "")
        self.assertEqual(result.stderr, "")

    def test_pointer_dereference_minimal_program(self):
        """Compiles and runs a program that dereferences a typed pointer to a record."""
        input_file = os.path.join(TEST_CASES_DIR, "test_dereference_minimal.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "test_dereference_minimal.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "test_dereference_minimal")

        run_compiler(input_file, asm_file)
        self.assertTrue(os.path.exists(asm_file))
        self.assertGreater(os.path.getsize(asm_file), 0)

        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout, "42\n")
        self.assertEqual(result.stderr, "")

    def test_type_alias_parameters_accept_new_categories(self):
        """Type aliases used in parameter lists should accept char/pointer/set/enum/file arguments."""
        input_file = os.path.join(TEST_CASES_DIR, "type_alias_parameter_calls.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "type_alias_parameter_calls.s")

        run_compiler(input_file, asm_file)

        self.assertTrue(os.path.exists(asm_file))
        self.assertGreater(os.path.getsize(asm_file), 0)

    def test_runtime_features(self):
        """Verifies string helpers, Inc, and dynamic arrays on NativeUInt values."""
        input_file = os.path.join(TEST_CASES_DIR, "runtime_features.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "runtime_features.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "runtime_features")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout, "10\nCompiler\n8\n3\n")

    def test_sign_function(self):
        """Tests the sign function with positive, negative, and zero inputs."""
        input_file = "KGPC/TestPrograms/sign_test.p"
        asm_file = os.path.join(TEST_OUTPUT_DIR, "sign_test.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "sign_test")

        # Compile the pascal program to assembly
        run_compiler(input_file, asm_file)

        # Compile the assembly to an executable
        self.compile_executable(asm_file, executable_file)

        # Test with different inputs
        test_cases = {
            "10": "1\n",
            "-10": "-1\n",
            "0": "0\n",
        }

        for input_str, expected_output in test_cases.items():
            with self.subTest(input=input_str):
                try:
                    process = subprocess.run(
                        [executable_file],
                        input=input_str,
                        capture_output=True,
                        text=True,
                        timeout=EXEC_TIMEOUT,  # Add a timeout to prevent hanging
                    )
                    # Compare trimmed output to tolerate the runtime's trailing whitespace
                    self.assertEqual(process.stdout.strip(), expected_output.strip())
                    self.assertEqual(process.returncode, 0)
                except subprocess.TimeoutExpired:
                    self.fail("Test execution timed out.")

    def test_helloworld(self):
        """Tests that the helloworld program prints 'Hello, World!'."""
        input_file = os.path.join(TEST_CASES_DIR, "helloworld.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "helloworld.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "helloworld")

        # Compile the pascal program to assembly
        run_compiler(input_file, asm_file)

        # Compile the assembly to an executable
        self.compile_executable(asm_file, executable_file)

        # Run the executable and check the output
        try:
            process = run_executable_with_valgrind(
                [executable_file], capture_output=True, text=True, timeout=EXEC_TIMEOUT
            )
            self.assertEqual(process.stdout, "Hello, World!\n")
            self.assertEqual(process.returncode, 0)
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")

    def test_statement_extensions(self):
        """Ensure extended statements parse, compile, and execute."""
        input_file = os.path.join(TEST_CASES_DIR, "statement_extensions.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "statement_extensions.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "statement_extensions")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout, "112\n1\n3\n")

    def test_exception_flow(self):
        """Exercise raise statements with try/except/finally control flow."""
        input_file = os.path.join(TEST_CASES_DIR, "exception_flow.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "exception_flow.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "exception_flow")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        expected_output = (
            "outer-try\n"
            "inner-try\n"
            "inner-finally\n"
            "outer-except\n"
            "rethrow-setup\n"
            "inner-except\n"
            "outer-reraise\n"
            "convert-exception\n"
            "final-handler\n"
            "111\n"
        )
        self.assertEqual(result.stdout, expected_output)

    def test_real_arithmetic_program(self):
        """Compiles and executes a program exercising REAL arithmetic and IO."""
        input_file = os.path.join(TEST_CASES_DIR, "real_arithmetic.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "real_arithmetic.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "real_arithmetic")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        expected_output = "3.75\n3.375\nless\nmore\n-0.75\n1.5\n"
        self.assertEqual(result.stdout, expected_output)

    def test_text_file_roundtrip(self):
        """Exercises text file assignment, IO, EOF, and console readln."""

        input_file = os.path.join(TEST_CASES_DIR, "text_file_roundtrip.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "text_file_roundtrip.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "text_file_roundtrip")
        output_path = os.path.join(TEST_OUTPUT_DIR, "text_roundtrip.txt")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        if os.path.exists(output_path):
            os.remove(output_path)

        input_data = "Alpha\nBeta\n\n"
        result = subprocess.run(
            [executable_file],
            input=input_data,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
            check=True,
        )

        self.assertEqual(result.stdout, "FILE1:Alpha\nFILE2:Beta\n")

        with open(output_path, "r", encoding="utf-8") as handle:
            self.assertEqual(handle.read(), "Alpha\nBeta\n")

    def test_email_address_book_programs(self):
        """Ensure the email address book samples compile and manipulate text files."""

        book_input = os.path.join(TEST_CASES_DIR, "email_address_book.p")
        book_asm = os.path.join(TEST_OUTPUT_DIR, "email_address_book.s")
        book_exe = os.path.join(TEST_OUTPUT_DIR, "email_address_book")
        output_path = os.path.join(TEST_OUTPUT_DIR, "email_address_book.txt")

        run_compiler(book_input, book_asm)
        self.compile_executable(book_asm, book_exe)

        if os.path.exists(output_path):
            os.remove(output_path)

        address_input = (
            "Alice\n"
            "alice@example.com\n"
            "Bob\n"
            "bob@example.com\n"
            "Carol\n"
            "carol@example.com\n"
        )

        result = subprocess.run(
            [book_exe],
            input=address_input,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
            check=True,
        )

        expected_prompt = (
            "\033[2J\033[H"
            "Enter name 1 out of 3\n"
            "Enter that person's email.\n"
            "Enter name 2 out of 3\n"
            "Enter that person's email.\n"
            "Enter name 3 out of 3\n"
            "Enter that person's email.\n"
        )
        self.assertEqual(result.stdout, expected_prompt)

        with open(output_path, "r", encoding="utf-8") as handle:
            self.assertEqual(
                handle.read(),
                "Alice\nalice@example.com\nBob\nbob@example.com\nCarol\ncarol@example.com\n",
            )

        read_input = os.path.join(TEST_CASES_DIR, "email_address_book_read.p")
        read_asm = os.path.join(TEST_OUTPUT_DIR, "email_address_book_read.s")
        read_exe = os.path.join(TEST_OUTPUT_DIR, "email_address_book_read")

        run_compiler(read_input, read_asm)
        self.compile_executable(read_asm, read_exe)

        result_read = subprocess.run(
            [read_exe],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
            check=True,
        )

        expected_read = (
            "Name  1: Alice\n"
            "Email 1: alice@example.com\n\n"
            "Name  2: Bob\n"
            "Email 2: bob@example.com\n\n"
            "Name  3: Carol\n"
            "Email 3: carol@example.com\n\n"
        )
        self.assertEqual(result_read.stdout, expected_read)

        eof_input = os.path.join(TEST_CASES_DIR, "email_address_book_read_eof.p")
        eof_asm = os.path.join(TEST_OUTPUT_DIR, "email_address_book_read_eof.s")
        eof_exe = os.path.join(TEST_OUTPUT_DIR, "email_address_book_read_eof")

        run_compiler(eof_input, eof_asm)
        self.compile_executable(eof_asm, eof_exe)

        result_eof = subprocess.run(
            [eof_exe],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
            check=True,
        )

        expected_eof = expected_read
        self.assertEqual(result_eof.stdout, expected_eof)

    def test_repeat_type_inference(self):
        """Tests repeat-until loops and variable type inference."""
        input_file = os.path.join(TEST_CASES_DIR, "repeat_infer.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "repeat_infer.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "repeat_infer")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        process = subprocess.run(
            [executable_file],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )
        self.assertEqual(process.stdout, "5\n")
        self.assertEqual(process.returncode, 0)

    def test_array_consts(self):
        """Tests that const declarations and array indexing work together."""
        input_file = os.path.join(TEST_CASES_DIR, "array_const.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "array_const.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "array_const")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = subprocess.run(
                [executable_file],
                capture_output=True,
                text=True,
                timeout=EXEC_TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            self.fail("array_const execution timed out")

        self.assertEqual(process.returncode, 0)
        self.assertEqual(process.stdout, "5\n6\n7\n8\n")

    def test_record_type_declaration(self):
        """Tests that a program declaring a record type compiles and runs."""
        input_file = os.path.join(TEST_CASES_DIR, "record_decl_only.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "record_decl_only.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "record_decl_only")

        # Compile the pascal program to assembly. This exercises the record type
        # conversion logic added to the cparser import path.
        run_compiler(input_file, asm_file)

        # Compile the assembly to an executable
        self.compile_executable(asm_file, executable_file)

        # Run the executable and verify the output so we know the program ran.
        try:
            process = subprocess.run(
                [executable_file], capture_output=True, text=True, timeout=EXEC_TIMEOUT
            )
            self.assertEqual(process.stdout, "42\n")
            self.assertEqual(process.returncode, 0)
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")

    def test_record_reference_features(self):
        """Exercises record assignment, address-of, and var parameter support."""
        input_file = os.path.join(TEST_CASES_DIR, "record_reference_features.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "record_reference_features.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "record_reference_features")
        expected_output_file = os.path.join(
            TEST_CASES_DIR, "record_reference_features.expected"
        )

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        expected_output = read_file_content(expected_output_file)
        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(
            result.stdout.strip().splitlines(),
            expected_output.strip().splitlines(),
        )
        self.assertEqual(result.returncode, 0)

        asm_source = read_file_content(asm_file)
        self.assertIn("call\tkgpc_move", asm_source)
        self.assertIn("call\tsucc_i", asm_source)

    def test_register_spill_restores_value(self):
        """Ensures spilled registers are correctly reloaded when register pressure is high."""
        input_file, asm_file, executable_file = self._get_test_paths("register_spill_limit")

        prev_limit = os.environ.get("KGPC_FORCE_REGISTER_LIMIT")
        os.environ["KGPC_FORCE_REGISTER_LIMIT"] = "2"
        try:
            run_compiler(input_file, asm_file)
        finally:
            if prev_limit is None:
                os.environ.pop("KGPC_FORCE_REGISTER_LIMIT", None)
            else:
                os.environ["KGPC_FORCE_REGISTER_LIMIT"] = prev_limit

        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout.strip(), "36")

    def test_record_exotic_program(self):
        """Parses a program that uses packed and variant record constructs."""
        input_file = os.path.join(TEST_CASES_DIR, "record_exotic.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "record_exotic.s")
        stderr_output = run_compiler(
            input_file,
            asm_file,
            flags=["-parse-only"],
        )

        self.assertIn("Parse-only mode enabled.", stderr_output)
        self.assertNotIn("Parse error", stderr_output)

    def test_variant_record_minimal_program(self):
        """Compiles and runs a minimal variant record example."""
        input_file = os.path.join(TEST_CASES_DIR, "variant_record_minimal.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "variant_record_minimal.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "variant_record_minimal")
        expected_output_file = os.path.join(
            TEST_CASES_DIR, "variant_record_minimal.expected"
        )

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        expected_output = read_file_content(expected_output_file).strip().splitlines()
        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout.strip().splitlines(), expected_output)
        self.assertEqual(result.returncode, 0)

    def test_variant_record_function_return(self):
        """Compiles and runs a variant record function return example."""
        input_file = os.path.join(
            TEST_CASES_DIR, "variant_record_function_return.p"
        )
        asm_file = os.path.join(
            TEST_OUTPUT_DIR, "variant_record_function_return.s"
        )
        executable_file = os.path.join(
            TEST_OUTPUT_DIR, "variant_record_function_return"
        )
        expected_output_file = os.path.join(
            TEST_CASES_DIR, "variant_record_function_return.expected"
        )

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        expected_output = (
            read_file_content(expected_output_file).strip().splitlines()
        )
        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout.strip().splitlines(), expected_output)
        self.assertEqual(result.returncode, 0)

    def test_with_nested_multi_context_program(self):
        """Ensures nested and multi-context with statements compile and run."""
        input_file = os.path.join(
            TEST_CASES_DIR, "with_nested_multi_context.p"
        )
        asm_file = os.path.join(TEST_OUTPUT_DIR, "with_nested_multi_context.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "with_nested_multi_context")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout, "6\n13\n57\n")
        self.assertEqual(result.returncode, 0)

    def test_mod_operator(self):
        """Tests that the mod operator works correctly."""
        input_file = os.path.join(TEST_CASES_DIR, "mod_test.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "mod_test.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "mod_test")

        # Compile the pascal program to assembly
        run_compiler(input_file, asm_file)

        # Compile the assembly to an executable
        self.compile_executable(asm_file, executable_file)

        # Run the executable and check the output
        try:
            process = subprocess.run(
                [executable_file], capture_output=True, text=True, timeout=EXEC_TIMEOUT
            )
            self.assertEqual(process.stdout, "1\n")
            self.assertEqual(process.returncode, 0)
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")

    def test_string_concatenation(self):
        """Tests that string addition produces a concatenated result."""
        input_file = os.path.join(TEST_CASES_DIR, "string_concat_demo.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "string_concat_demo.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "string_concat_demo")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = subprocess.run(
                [executable_file],
                capture_output=True,
                text=True,
                timeout=EXEC_TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            self.fail("string_concat_demo execution timed out")
            return

        self.assertEqual(process.returncode, 0)
        self.assertEqual(process.stdout, "Hello World\n")

    def test_sysutils_unit(self):
        """Tests that the SysUtils unit links and provides basic helpers."""
        input_file = os.path.join(TEST_CASES_DIR, "sysutils_demo.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "sysutils_demo.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "sysutils_demo")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = subprocess.run(
                [executable_file],
                capture_output=True,
                text=True,
                # A bounded timeout keeps runaway ctypes demos from hanging CI forever.
                timeout=EXEC_TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")
            return

        lines = process.stdout.strip().splitlines()
        expected_lines = [
            "32",
            "1",
            "Trim=Pascal",
            "TrimLeft=Pascal  ",
            "TrimRight=  Pascal",
            "AnsiUpper=PASCAL",
            "AnsiLower=pascal",
            "CompareText=0",
            "SameText=TRUE",
        ]
        self.assertEqual(lines, expected_lines)
        self.assertEqual(process.returncode, 0)

    def test_unix_gethostname(self):
        """Ensures the Unix unit exposes GetHostName with actual hostname output."""
        input_file = os.path.join(TEST_CASES_DIR, "unix_gethostname_demo.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "unix_gethostname_demo.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "unix_gethostname_demo")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        process = subprocess.run(
            [executable_file],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        expected_hostname = socket.gethostname().strip()
        # Use case-insensitive comparison since hostname case can vary by platform
        self.assertEqual(process.stdout.strip().lower(), expected_hostname.lower())
        self.assertEqual(process.returncode, 0)

    def test_set_of_enum_typed_constant_unit(self):
        """Ensures a unit with a set-of-enum typed constant compiles and runs."""
        input_file = os.path.join(TEST_CASES_DIR, "set_of_enum_typed_constant_demo.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "set_of_enum_typed_constant_demo.s")
        executable_file = os.path.join(
            TEST_OUTPUT_DIR, "set_of_enum_typed_constant_demo"
        )

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = subprocess.run(
                [executable_file],
                capture_output=True,
                text=True,
                timeout=EXEC_TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            self.fail("set_of_enum_typed_constant_demo execution timed out")
            return

        self.assertEqual(process.returncode, 0)
        lines = process.stdout.strip().splitlines()
        self.assertEqual(
            lines,
            [
                "readonly",
                "visible",
                "system",
            ],
        )

    def test_ord_builtin(self):
        """Ensures the Ord builtin converts characters to their ordinal values."""
        input_file = os.path.join(TEST_CASES_DIR, "ord_builtin.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "ord_builtin.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "ord_builtin")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = subprocess.run(
                [executable_file],
                capture_output=True,
                text=True,
                timeout=EXEC_TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            self.fail("Ord builtin execution timed out.")
            return

        self.assertEqual(process.returncode, 0)
        lines = process.stdout.strip().splitlines()
        self.assertGreaterEqual(len(lines), 3)
        self.assertEqual(lines[0].strip(), "55")
        self.assertEqual(lines[1].strip(), "48")
        self.assertEqual(lines[2].strip(), "5")

    def test_typed_const_array_lowering(self):
        """Ensures typed constant arrays are lowered into runtime initializers."""
        input_file = os.path.join(TEST_CASES_DIR, "typed_const_array_demo.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "typed_const_array_demo.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "typed_const_array_demo")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = subprocess.run(
                [executable_file],
                capture_output=True,
                text=True,
                timeout=EXEC_TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            self.fail("typed_const_array_demo execution timed out.")
            return

        self.assertEqual(process.returncode, 0)
        self.assertEqual(
            process.stdout.strip().splitlines(),
            ["1", "1", "2", "3", "5"],
        )

    def test_typed_const_array_persists_between_calls(self):
        """Typed constant arrays should not be reinitialized on each invocation."""
        input_file = os.path.join(TEST_CASES_DIR, "typed_const_array_persistent_demo.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "typed_const_array_persistent_demo.s")
        executable_file = os.path.join(
            TEST_OUTPUT_DIR, "typed_const_array_persistent_demo"
        )

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = subprocess.run(
                [executable_file],
                capture_output=True,
                text=True,
                timeout=EXEC_TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            self.fail("typed_const_array_persistent_demo execution timed out.")
            return

        self.assertEqual(process.returncode, 0)
        self.assertEqual(
            process.stdout.strip().splitlines(),
            ["10", "20", "30", "11", "21", "31"],
        )

    def test_unsupported_expression_reports_tag_name(self):
        """Address-of operator (@) is now supported and should compile successfully."""
        input_file = os.path.join(TEST_CASES_DIR, "unsupported_addr_expr.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "unsupported_addr_expr.s")

        # ADDR operator is now supported, so compilation should succeed
        run_compiler(input_file, asm_file)

        # Verify the assembly file was generated
        self.assertTrue(os.path.exists(asm_file))
        self.assertGreater(os.path.getsize(asm_file), 0)

    def test_ctypes_unit(self):
        """Ensures the ctypes unit exposes C compatible aliases."""
        input_file = os.path.join(TEST_CASES_DIR, "ctypes_demo.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "ctypes_demo.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "ctypes_demo")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = subprocess.run(
                [executable_file],
                capture_output=True,
                text=True,
                timeout=EXEC_TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")
            return

        lines = process.stdout.strip().splitlines()
        self.assertEqual(lines, ["-42", "7", "1024", "ctypes"])
        self.assertEqual(process.returncode, 0)

    def test_zahlen_program_compiles(self):
        """Ensures the zahlen classification demo compiles successfully."""
        input_file = os.path.join(TEST_CASES_DIR, "zahlen.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "zahlen.s")

        # Compile without parse-only flag
        try:
            run_compiler(input_file, asm_file)
            self.assertTrue(os.path.exists(asm_file))
            # Check that it's not in parse-only mode
            content = read_file_content(asm_file)
            self.assertNotIn("parse-only mode", content)
            # Check that it contains assembly code
            self.assertIn(".text", content)
        except subprocess.CalledProcessError as e:
            self.fail(f"zahlen.p compilation failed: {e}")

    def test_zahlen_program_runs(self):
        """Ensures the zahlen classification demo compiles and executes with dynamic arrays."""
        input_file = os.path.join(TEST_CASES_DIR, "zahlen.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "zahlen_run.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "zahlen_run")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        zahlen_input = "5\n12\n-7\n0\n3\n4\n"
        process = subprocess.run(
            [executable_file],
            input=zahlen_input,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        # Verify that both even and odd buckets are populated correctly.
        expected_output_lines = [
            "Schreib wie viele Zahlen wollen sie eintippen, danach schreiben Sie die Zahlen.\n",
            "         gerade       ungerade       Positive       Negative\n",
            "              4              3              4             -7\n",
            "              0             -7              3               \n",
            "             12                             0               \n",
            "                                           12               \n",
            "Gerade Zahlen\n",
            "4 0 12 \n",
            "Ungerade Zahlen\n",
            "3 -7 \n",
            "Positive Zahlen\n",
            "4 3 0 12 \n",
            "Negative Zahlen\n",
            "-7 \n",
        ]
        expected_output = "".join(expected_output_lines)

        self.assertEqual(process.stdout, expected_output)
        self.assertEqual(process.returncode, 0)

    def test_for_program(self):
        """Tests the for program, including edge cases."""
        input_file = "KGPC/TestPrograms/CodeGeneration/for.p"
        asm_file = os.path.join(TEST_OUTPUT_DIR, "for.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "for")

        # Compile the pascal program to assembly
        run_compiler(input_file, asm_file)

        # Compile the assembly to an executable
        self.compile_executable(asm_file, executable_file)

        test_cases = [
            {"input": "3", "expected_output": "123456", "desc": "normal positive bound"},
            {"input": "0", "expected_output": "", "desc": "zero bound"},
            {"input": "-5", "expected_output": "", "desc": "negative bound"},
        ]

        for case in test_cases:
            with self.subTest(msg=case["desc"], input=case["input"]):
                try:
                    process = subprocess.run(
                        [executable_file],
                        input=case["input"],
                        capture_output=True,
                        text=True,
                        timeout=EXEC_TIMEOUT,
                    )
                    self.assertEqual(process.stdout.strip(), case["expected_output"])
                    self.assertEqual(process.returncode, 0)
                except subprocess.TimeoutExpired:
                    self.fail("Test execution timed out.")


def _discover_and_add_auto_tests():
    """
    Auto-discover test cases based on .p files with corresponding .expected files.
    Dynamically adds test methods to TestCompiler class.
    """
    if not os.path.isdir(TEST_CASES_DIR):
        return
    
    # Find all .expected files
    expected_files = []
    for filename in os.listdir(TEST_CASES_DIR):
        if filename.endswith('.expected'):
            base_name = filename[:-9]  # Remove '.expected'
            pascal_file = os.path.join(TEST_CASES_DIR, base_name + '.p')
            if os.path.isfile(pascal_file):
                expected_files.append(base_name)
    
    # Sort to ensure consistent test ordering
    expected_files.sort()
    
    # For each discovered test, create a test method
    for base_name in expected_files:
        # Create a safe method name (replace hyphens and other chars with underscores)
        method_name = 'test_auto_' + base_name.replace('-', '_').replace(' ', '_')
        
        # Skip if this test is already manually defined
        if hasattr(TestCompiler, method_name):
            continue
        
        # Create the test method using a closure to capture base_name
        def make_test_method(test_base_name):
            def test_method(self):
                """Auto-discovered test case."""
                # No platform skips here; all discovered tests must run.
                # Skip Unix fork-dependent tests on MinGW (which lacks POSIX fork)
                # Cygwin and MSYS have fork, pure MinGW does not
                if test_base_name == "unix_wait_helpers_demo":
                    # Check if we're targeting MinGW (not Cygwin/MSYS)
                    # MinGW defines _WIN32 but not __CYGWIN__
                    # We can detect this by checking if the C compiler is MinGW
                    if IS_WINDOWS_ABI and not IS_WINE:
                        # Running natively on Windows - could be MinGW or Cygwin
                        # Skip for now as we can't easily detect Cygwin vs MinGW at runtime
                        self.skipTest("Unix fork() test requires POSIX fork support (Cygwin/MSYS/Unix)")
                    elif IS_WINE:
                        # Cross-compiling with Wine - definitely MinGW, no fork support
                        self.skipTest("Unix fork() test not supported on MinGW (requires Cygwin/MSYS for fork)")
                
                input_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.p")
                asm_file = os.path.join(TEST_OUTPUT_DIR, f"{test_base_name}.s")
                executable_file = os.path.join(TEST_OUTPUT_DIR, test_base_name)
                expected_output_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.expected")
                input_data_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.input")

                compiler_output = None
                actual_output = None
                raw_stdout = None
                raw_stderr = None
                expected_output = None
                process_returncode = None

                self.record_failure_context(
                    base_name=test_base_name,
                    input_file=input_file,
                    asm_file=asm_file,
                    executable_file=executable_file,
                    expected_file=expected_output_file,
                )

                try:
                    compiler_output = run_compiler(input_file, asm_file)

                    self.compile_executable(asm_file, executable_file)

                    stdin_input = None
                    if os.path.exists(input_data_file):
                        stdin_input = read_file_content(input_data_file)

                    if stdin_input is not None:
                        process = run_executable_with_valgrind(
                            [executable_file],
                            capture_output=True,
                            text=True,
                            timeout=EXEC_TIMEOUT,
                            input=stdin_input,
                        )
                    else:
                        process = run_executable_with_valgrind(
                            [executable_file],
                            capture_output=True,
                            text=True,
                            timeout=EXEC_TIMEOUT,
                        )
                    raw_stdout = process.stdout
                    raw_stderr = process.stderr
                    process_returncode = process.returncode

                    expected_output = read_file_content(expected_output_file)
                    text = raw_stdout
                    if text is None:
                        text = ""
                    while "\r\r\n" in text:
                        text = text.replace("\r\r\n", "\r\n")
                    actual_output = text.replace("\r\n", "\n").replace("\r", "")

                    self.record_failure_context(
                        base_name=test_base_name,
                        compiler_output=compiler_output,
                        normalized_output=actual_output,
                        raw_stdout=raw_stdout,
                        raw_stderr=raw_stderr,
                        expected_output=expected_output,
                        returncode=process_returncode,
                    )

                    self.assertEqual(actual_output, expected_output)
                    self.assertEqual(process_returncode, 0)
                except subprocess.TimeoutExpired:
                    self.record_failure_context(
                        base_name=test_base_name,
                        compiler_output=compiler_output,
                        normalized_output=actual_output,
                        raw_stdout=raw_stdout,
                        raw_stderr=raw_stderr,
                        expected_output=expected_output,
                        returncode=process_returncode,
                        exception_text=traceback.format_exc(),
                    )
                    self.fail(f"Test {test_base_name} execution timed out.")
                except Exception:
                    self.record_failure_context(
                        base_name=test_base_name,
                        compiler_output=compiler_output,
                        normalized_output=actual_output,
                        raw_stdout=raw_stdout,
                        raw_stderr=raw_stderr,
                        expected_output=expected_output,
                        returncode=process_returncode,
                    )
                    raise
            
            test_method.__name__ = method_name
            test_method.__doc__ = f"Auto-discovered test case for {test_base_name}.p"
            return test_method
        
        # Add the test method to the TestCompiler class
        setattr(TestCompiler, method_name, make_test_method(base_name))


# Auto-discover and add tests before loading the suite
_discover_and_add_auto_tests()


def _load_suite():
    return unittest.defaultTestLoader.loadTestsFromModule(sys.modules[__name__])


def main():
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("--tap", action="store_true")
    args, remaining = parser.parse_known_args()

    if args.tap or os.environ.get("KGPC_TEST_PROTOCOL", "").lower() == "tap":
        suite = _load_suite()
        runner = TAPTestRunner()
        result = runner.run(suite)
        sys.exit(0 if result.wasSuccessful() else 1)
    print(f"DEBUG: argv={[sys.argv[0]] + remaining}")

    unittest.main(argv=[sys.argv[0]] + remaining)


if __name__ == "__main__":
    main()
