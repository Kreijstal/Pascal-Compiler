# THIS PROGRAM WILL NOT WORK IF YOU DO NOT COMPILE SOURCES FIRST WITH MESON
#
# This file is the entry point kept for meson compatibility.
# Utility code has been split into tests/harness/ sub-modules.
# See tests/harness/ for: env, cache, sanitizers, runner, reporters, parallel,
# artifacts, discovery.

# Ensure the project root is in sys.path so that `tests.harness` is importable
# regardless of how this script is invoked (by meson, pytest, or directly).
import sys
import os as _os
_project_root = _os.path.dirname(_os.path.dirname(_os.path.abspath(__file__)))
if _project_root not in sys.path:
    sys.path.insert(0, _project_root)

# Standard library imports still needed by TestCompiler and discovery functions.
import argparse
import hashlib
import os
import re
import shlex
import shutil
import socket
import subprocess
import tempfile
import time
import traceback
import unittest
from pathlib import Path

# ---------------------------------------------------------------------------
# Import from harness sub-modules (replaces the first ~1576 lines of
# the old monolith).  Import order: env first (patches subprocess at
# module load time).
# ---------------------------------------------------------------------------

from tests.harness.env import (
    EXE_EXT,
    EXEC_TIMEOUT,
    COMPILER_TIMEOUT,
    LINK_TIMEOUT,
    TEST_CASE_TIMEOUT,
    PARALLEL_WORKERS,
    TAP_MAX_WORKERS,
    FPC_RTL_MODE,
    FPC_RTL_DIR,
    FPC_RTL_GENERATED_UNITS_DIRNAME,
    PP_BOOTSTRAP_FULL_CHAIN_TIMEOUT,
    IS_WINDOWS_ABI,
    IS_WINE,
    KGPC_PATH,
    TEST_CASES_DIR,
    INPUT_DATA_DIR,
    TEST_OUTPUT_DIR,
    GOLDEN_AST_DIR,
    PROJECT_ROOT,
    FAILURE_ARTIFACT_DIR,
    RUN_VALGRIND_TESTS,
    VALGRIND_MODE,
    build_dir,
)

from tests.harness.cache import (
    FPC_RTL_FLAGS,
    with_fpc_rtl_ast_cache as _with_fpc_rtl_ast_cache,
    test_cache_check as _test_cache_check,
    test_cache_store as _test_cache_store,
    _FPC_RTL_AST_CACHE_DIR,
    _FPC_RTL_CODEGEN_CACHE_DIR,
    _RUNTIME_LIB_PATH,
)

from tests.harness.sanitizers import (
    HAS_PTY,
    run_executable_with_valgrind,
    _write_helper_script,
    _run_helper_with_valgrind,
)

from tests.harness.runner import (
    COMPILER_RUNS,
    LINK_ARGS_BY_ASM,
    run_compiler,
    is_coverage_enabled,
    EXPLICIT_TARGET_FLAGS,
    _has_explicit_target_flag,
)

from tests.harness.reporters import (
    TAPTestResult,
    TAPTestRunner,
    TimingTestResult,
)

from tests.harness.parallel import (
    TAPParallelTestResult,
    TAPParallelTestRunner,
    ParallelTestResult,
    ParallelTestRunner,
    _flatten_tests,
    _prepare_parallel_class_fixtures,
    _cleanup_parallel_class_fixtures,
    _run_single_test_with_timeout,
)

from tests.harness.artifacts import (
    read_file_content,
    store_failure_artifacts as _store_failure_artifacts,
    _signal_name_suffix,
)

from tests.harness.discovery import (
    kgpc_bootstrap_flags as _kgpc_bootstrap_flags_impl,
    pp_bootstrap_compiler_flags as _pp_bootstrap_compiler_flags,
    pp_bootstrap_program_flags as _pp_bootstrap_program_flags,
    should_include_in_fpcrtl as _should_include_in_fpcrtl_impl,
    _strip_pascal_comments,
    _extract_used_units,
    _collect_pascal_unit_names,
    _get_fpc_rtl_known_units,
    _tree_contains_newer_file as _tree_contains_newer_file_impl,
)

# ---------------------------------------------------------------------------
# Local wrappers that preserve the original call signatures used in the
# test class and discovery functions below.
# ---------------------------------------------------------------------------


def _kgpc_bootstrap_flags(fpc_src, *, include_compiler_dirs):
    """Wrapper preserving original signature; injects cache dirs from cache module."""
    return _kgpc_bootstrap_flags_impl(
        fpc_src,
        include_compiler_dirs=include_compiler_dirs,
        fpc_rtl_ast_cache_dir=_FPC_RTL_AST_CACHE_DIR,
        fpc_rtl_codegen_cache_dir=_FPC_RTL_CODEGEN_CACHE_DIR,
    )


def _tree_contains_newer_file(root_dir, reference_file):
    """Wrapper preserving original 2-arg signature."""
    return _tree_contains_newer_file_impl(
        root_dir, reference_file, FPC_RTL_GENERATED_UNITS_DIRNAME
    )


def _should_include_in_fpcrtl(base_name, pascal_file):
    """Wrapper preserving original signature; passes FPC_RTL_IMPLICIT_UNIT_TESTS."""
    return _should_include_in_fpcrtl_impl(
        base_name, pascal_file, FPC_RTL_IMPLICIT_UNIT_TESTS
    )


# ---------------------------------------------------------------------------
# Constants that were in the original monolith between line ~530 and ~645
# and are referenced by TestCompiler / discovery functions below.
# ---------------------------------------------------------------------------

UNIT_ONLY_TESTS = {
    "directives_and_properties_unit",
    "dotted_alias_base_unit",
    "dotted_alias_reexport_unit",
    "fpc_import_const_alias",
    "fpc_import_const_unit",
    "fpc_interface_const_after_external",
    "fpc_qualified_const_import",
    "property_indexed_unit",
    "regr_char_const_alias_unit",
    "tdd_external_unit_var",
    "tdd_variant_shadow_record",
    "unit_cardinal_type",
    "unit_high_type_const",
    "unit_longword_type",
    "unit_low_type_const",
    "unit_pointer_deref_nil",
    "unit_sizeof_array_bounds",
    "unit_sizeof_const",
    "unit_include_init_section",
}

UNIT_ONLY_FLAGS = {
    "tdd_variant_shadow_record": ["--no-stdlib"],
}

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
        except unittest.SkipTest:
            raise
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

        # FPC RTL mode uses on-demand AST/codegen caches per testcase.
        # Do not prewarm here: an eager warm-up defeats the cache design and
        # can time out before the suite even starts.

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
            # Look for clang.exe or gcc.exe in the quasi-msys2 directory structure
            # Use relative paths from build_dir to avoid absolute Windows paths
            msys2_search_paths = [
                # Look in quasi-msys2/root/{clang64,ucrt64,mingw64}/bin/
                os.path.join(build_dir, "..", "quasi-msys2", "root", "clang64", "bin"),
                os.path.join(build_dir, "..", "quasi-msys2", "root", "ucrt64", "bin"),
                os.path.join(build_dir, "..", "quasi-msys2", "root", "mingw64", "bin"),
            ]
            
            wine_cc = None
            for search_dir in msys2_search_paths:
                normalized_dir = os.path.normpath(search_dir)
                # Prefer clang.exe, fall back to gcc.exe
                for cc_name in ["clang.exe", "gcc.exe"]:
                    cc_path = os.path.join(normalized_dir, cc_name)
                    if os.path.exists(cc_path):
                        wine_cc = cc_path
                        break
                if wine_cc:
                    break
            
            if wine_cc:
                # Use the Windows-native compiler
                cls.c_compiler_cmd = [wine_cc]
                cls.c_compiler_display = f"{cc_raw} (using Wine-compatible {wine_cc})"
                print(f"Wine detected: Using Windows-native compiler at {wine_cc}", file=sys.stderr)
            else:
                # Fallback: try to use clang.exe or gcc.exe directly from PATH
                # The quasi-msys2 environment should have added the bin dir to PATH
                cls.c_compiler_cmd = ["clang.exe"]
                cls.c_compiler_display = f"{cc_raw} (using Wine-compatible clang.exe from PATH)"
                print(f"Wine detected: Using clang.exe from PATH (searched: {msys2_search_paths})", file=sys.stderr)

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
            if FPC_RTL_MODE:
                cls.ctypes_helper_library = None  # Not needed for FPC RTL tests
            else:
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
        extra_link_args.extend(LINK_ARGS_BY_ASM.get(asm_file, []))
        try:
            command = list(self.c_compiler_cmd)
            command.append("-O2")
            if IS_WINDOWS_ABI:
                command.append("-static")
            else:
                command.append("-no-pie")
            if is_coverage_enabled():
                command.append("--coverage")
            # When the runtime archive was built with a sanitizer (e.g.
            # build-asan via -Db_sanitize=address), generated executables
            # must link with the matching -fsanitize=... flag or the link
            # fails with undefined references to __asan_report_*. Meson
            # forwards b_sanitize through KGPC_SANITIZE.
            _sanitize = os.environ.get("KGPC_SANITIZE", "none")
            if _sanitize and _sanitize != "none":
                command.append(f"-fsanitize={_sanitize}")
            command.extend([
                "-o",
                executable_file,
                asm_file,
                str(self.runtime_library),
            ])
            command.extend(list(extra_objects))
            command.extend(list(extra_link_args))
            compile_kwargs = {
                "check": True,
                "capture_output": True,
                "text": True,
                "timeout": LINK_TIMEOUT,
            }
            subprocess.run(command, **compile_kwargs)
        except subprocess.CalledProcessError as e:
            self.fail(f"{self.c_compiler_display} compilation failed: {e.stderr}")

    def _get_test_paths(self, name, extension="p"):
        input_file = os.path.join(TEST_CASES_DIR, f"{name}.{extension}")
        asm_file = os.path.join(TEST_OUTPUT_DIR, f"{name}.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"{name}{EXE_EXT}")
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

    def test_constant_folding_o1_real_and_modulus(self):
        """Tests that -O1 folds real arithmetic and integer modulus constants."""
        input_file = os.path.join(TEST_CASES_DIR, "constant_folding_real_mod.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "constant_folding_real_mod_o1.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"constant_folding_real_mod_o1{EXE_EXT}")

        run_compiler(input_file, asm_file, flags=["-O1"])
        self.record_failure_context(
            input_file=input_file, asm_file=asm_file,
            executable_file=executable_file)
        self.compile_executable(asm_file, executable_file)

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout, "4.0\n1\n")

    def test_constant_folding_typed_const_array_access_and_length_o1(self):
        """Typed-const array index and Length() should fold under -O1."""
        input_file = os.path.join(TEST_CASES_DIR, "optimizer_typed_const_array_fold.p")
        optimized_output_file = os.path.join(
            TEST_OUTPUT_DIR, "optimizer_typed_const_array_fold_optimized_o1.s"
        )
        run_compiler(input_file, optimized_output_file, flags=["-O1"])
        optimized_asm = read_file_content(optimized_output_file)

        self.assertRegex(optimized_asm, r"\bmov[lq]\s+\$20\b")
        self.assertRegex(optimized_asm, r"\bmov[lq]\s+\$3\b")

    def test_expr_tree_constant_simplify_dispatch(self):
        """Constant folding folds ADDOP/MULOP/RELOP at -O1; leaves pass through unchanged."""
        input_file = os.path.join(TEST_CASES_DIR, "tdd_expr_tree_constant_simplify_dispatch.p")
        asm_o1 = os.path.join(TEST_OUTPUT_DIR, "tdd_expr_tree_constant_simplify_dispatch_o1.s")
        run_compiler(input_file, asm_o1, flags=["-O1"])
        asm_text = read_file_content(asm_o1)
        # 2+3=5, 8-3=5
        self.assertIn("$5", asm_text)
        # 2*3=6
        self.assertIn("$6", asm_text)
        # ord(#65)=65
        self.assertIn("$65", asm_text)

    def test_forward_class_constructor_assignment_no_duplicate_self_move(self):
        """Verify that the constructor codegen emits exactly one Self-move into
        the first argument register before the constructor call, and that no
        duplicate consecutive movq instructions target the first arg register."""
        input_file, asm_file, _ = self._get_test_paths("forward_class_ctor_assign")
        run_compiler(input_file, asm_file)
        asm_lines = read_file_content(asm_file).splitlines()

        # The first argument register depends on the target ABI.
        first_arg_reg = "%rcx" if IS_WINDOWS_ABI else "%rdi"

        # 1. No duplicate consecutive movq into the first arg register anywhere in the file.
        for i in range(len(asm_lines) - 1):
            self.assertFalse(
                asm_lines[i] == asm_lines[i + 1]
                and asm_lines[i].startswith("\tmovq\t")
                and asm_lines[i].endswith(f", {first_arg_reg}"),
                f"duplicate constructor self move found at line {i + 1}: {asm_lines[i]}",
            )

        # 2. Verify the expected call sequence:
        #    allocation helper → save instance → VMT init → Self move → call constructor.
        #    There must be exactly ONE movq into the first arg register between
        #    the VMT store and the constructor call.
        call_idx = None
        for i, line in enumerate(asm_lines):
            if "\tcall\ttfoo__create_p" in line:
                call_idx = i
                break
        self.assertIsNotNone(call_idx, "constructor call not found in assembly")

        # Count movq ..., <first_arg_reg> instructions between the instance allocation
        # helper return and the constructor call.
        alloc_idx = None
        for i in range(call_idx - 1, -1, -1):
            if "\tcall\tkgpc_allocmem" in asm_lines[i] or "\tcall\tcalloc" in asm_lines[i]:
                alloc_idx = i
                break
        self.assertIsNotNone(alloc_idx, "allocation call not found before constructor")

        self_moves = [
            line for line in asm_lines[alloc_idx:call_idx]
            if line.startswith("\tmovq\t") and line.endswith(f", {first_arg_reg}")
        ]
        self.assertEqual(
            len(self_moves), 1,
            f"Expected exactly 1 Self-move into {first_arg_reg} between allocation and constructor call, "
            f"found {len(self_moves)}: {self_moves}",
        )

    def test_dateutils_custom(self):
        """Tests DateUtils with custom regex verification."""
        input_file = os.path.join(TEST_CASES_DIR, "missing_dateutils.p")
        output_file = os.path.join(TEST_OUTPUT_DIR, "missing_dateutils.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"missing_dateutils{EXE_EXT}")

        # Compile
        run_compiler(input_file, output_file)
        
        # Assemble and Link
        self.compile_executable(output_file, executable_file)
        
        # Run
        result = run_executable_with_valgrind(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
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
        import re
        self.assertRegex(unoptimized_asm, r"\.comm\t__kgpc_program_var_x_\d+")
        self.assertRegex(unoptimized_asm, r"\.comm\t__kgpc_program_var_y_\d+")

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
        self.assertIsNone(re.search(r"\.comm\t__kgpc_program_var_y_\d+", optimized_asm))

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
        input_file = os.path.join(TEST_CASES_DIR, "mtinf.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "mtinf_parse_only.s")

        run_compiler(
            input_file,
            asm_file,
            flags=["-parse-only"],
        )

        self.assertTrue(os.path.exists(asm_file))
        self.assertGreater(os.path.getsize(asm_file), 0)

    def test_nested_type_declarations(self):
        """Nested type declarations (Delphi syntax: public type inside record/class) should compile."""
        input_file = os.path.join(TEST_CASES_DIR, "nested_type_declarations_parse_only.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "nested_type_declarations.s")

        run_compiler(
            input_file,
            asm_file,
        )

        self.assertTrue(os.path.exists(asm_file))
        self.assertGreater(os.path.getsize(asm_file), 0)

    def test_hresult_const_typecast(self):
        """HRESULT const typecast expressions should compile and run correctly."""
        input_file, asm_file, exe_file = self._get_test_paths("hresult_const_typecast")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, exe_file)

        result = subprocess.run(
            [exe_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )
        output = result.stdout
        self.assertIn("PASS: VAR_OK = 0", output)
        self.assertIn("PASS: S_OK = 0", output)
        self.assertIn("PASS: S_FALSE = 1", output)
        self.assertIn("PASS: VAR_PARAMNOTFOUND is negative", output)
        self.assertIn("PASS: E_FAIL is negative", output)
        self.assertIn("HRESULT const typecast test completed", output)

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

    def test_error_reports_path(self):
        """Compiler errors should include the source path prefix, using the include file when applicable."""
        input_file, asm_file, _ = self._get_test_paths(
            "tdd_error_path_in_include"
        )

        with self.assertRaises(subprocess.CalledProcessError) as cm:
            run_compiler(input_file, asm_file)

        stderr = cm.exception.stderr or ""
        # The error is in the include file, so the error prefix should show the include file path
        include_file = input_file.replace(".p", ".inc")
        self.assertIn(f"{include_file}:", stderr)
        lower = stderr.lower()
        self.assertTrue(
            "type mismatch" in lower or "incompatible types" in lower,
            "Expected type error in compiler output.",
        )
        self.assertIn("bad", stderr)

    def test_tdd_goto_undefined_label_reports_error(self):
        """Goto to an undeclared label should fail semantic checking."""
        input_file, asm_file, _ = self._get_test_paths("tdd_goto_undefined_label")

        with self.assertRaises(subprocess.CalledProcessError) as cm:
            run_compiler(input_file, asm_file)

        stderr = cm.exception.stderr or ""
        lower = stderr.lower()
        self.assertIn("goto target label '2' not declared in scope", lower)

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

        result = run_executable_with_valgrind(
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
        self.assertTrue(any(token in asm for token in ("\tshrl\t", "\tshrq\t")))
        self.assertTrue(any(token in asm for token in ("\troll\t", "\trolq\t")))
        self.assertTrue(any(token in asm for token in ("\trorl\t", "\trorq\t")))

    def test_typed_const_array_size_per_decl(self):
        """Regression: per-declaration .comm sizing for same-named typed-const arrays.

        Two units declare `sharedarr : array[0..N] of trec2` with different N.
        Before the fix, codegen pulled the array size from a cross-unit symtab
        entry, so both units' .comm allocations ended up sized by whichever
        declaration was most-recently registered.  Each unit's .comm must
        match its OWN bound: 32 bytes for unit_a (8*4) and 16 bytes for
        unit_b (4*4)."""
        input_file = os.path.join(TEST_CASES_DIR, "typed_const_array_size_per_decl.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "typed_const_array_size_per_decl.s")

        run_compiler(input_file, asm_file)
        asm = read_file_content(asm_file)

        # Locate the .comm directive for each unit's sharedarr array.
        comm_pattern = re.compile(
            r"\.comm\s+__kgpc_tconst_array_"
            r"typed_const_array_size_per_decl_unit_([ab])_0\s*,\s*(\d+)"
        )
        sizes = {m.group(1): int(m.group(2)) for m in comm_pattern.finditer(asm)}
        self.assertIn("a", sizes, "missing unit_a typed-const .comm in asm")
        self.assertIn("b", sizes, "missing unit_b typed-const .comm in asm")
        self.assertEqual(sizes["a"], 32,
            "unit_a's sharedarr [0..7] of trec2 must be 32 bytes, got "
            f"{sizes['a']} — cross-unit symtab clobbered the bounds.")
        self.assertEqual(sizes["b"], 16,
            "unit_b's sharedarr [0..3] of trec2 must be 16 bytes, got "
            f"{sizes['b']} — cross-unit symtab clobbered the bounds.")

    def test_typed_const_cross_unit_shortstring_array(self):
        """Regression: per-element stride for cross-unit array-of-shortstring init.

        A unit declares `tok2str : array[ttinytok] of string[10]`.  Before the
        fix, the per-element init in the consuming program used stride 256
        (the generic ShortString default) instead of 11 (string[10] storage =
        N+1).  Writing element K at offset K*256 OOB-clobbered adjacent BSS.

        The init code's address computation either uses an inline scaled LEA
        (`leaq (base, idx, S), idx`) when S is 1/2/4/8, or `imulq $S, idx`
        otherwise.  For stride 11 it must use `imulq $11, ...`.  The asm
        must NOT contain a stride-256 imul for our typed-const symbol."""
        input_file = os.path.join(
            TEST_CASES_DIR, "typed_const_cross_unit_shortstring_array.p")
        asm_file = os.path.join(
            TEST_OUTPUT_DIR, "typed_const_cross_unit_shortstring_array.s")

        run_compiler(input_file, asm_file)
        asm = read_file_content(asm_file)

        # The init code must reference the unit's typed-const symbol.
        self.assertIn(
            "__kgpc_tconst_array_typed_const_cross_unit_shortstring_array_recunit",
            asm,
            "expected the cross-unit typed-const symbol in emitted asm")

        # The stride-256 imul is the bug signature.  Reject it.
        self.assertNotIn(
            "imulq\t$256,", asm,
            "stride-256 per-element imul present — the bug is back.")

        # And the correct stride 11 (string[10] storage) must appear.
        self.assertIn(
            "imulq\t$11,", asm,
            "expected stride-11 imul for string[10] elements; got none.")

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
            stderr = result.stderr or ""
            if "Fatal error at startup" in stderr and "cannot be set up" in stderr:
                self.skipTest("valgrind is not usable on this host (missing loader debug symbols)")
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

        executable_file = os.path.join(TEST_OUTPUT_DIR, f"ctypes_dll_demo{EXE_EXT}")
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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"ctypes_pointer_demo{EXE_EXT}")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        result = run_executable_with_valgrind(
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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"pointer_operators{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"pointer_simple{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"test_dereference_minimal{EXE_EXT}")

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

    def test_unit_init_after_nested_body(self):
        """Initialization should resolve procedures declared after nested function bodies."""
        input_file = os.path.join(
            TEST_CASES_DIR, "tdd_top_level_subprograms_after_nested_body.p"
        )
        asm_file = os.path.join(
            TEST_OUTPUT_DIR, "tdd_top_level_subprograms_after_nested_body.s"
        )

        run_compiler(input_file, asm_file)

        self.assertTrue(os.path.exists(asm_file))
        self.assertGreater(os.path.getsize(asm_file), 0)

    def test_runtime_features(self):
        """Verifies string helpers, Inc, and dynamic arrays on NativeUInt values."""
        input_file = os.path.join(TEST_CASES_DIR, "runtime_features.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "runtime_features.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"runtime_features{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"sign_test{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"helloworld{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"statement_extensions{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"exception_flow{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"real_arithmetic{EXE_EXT}")

        run_compiler(input_file, asm_file)
        self.record_failure_context(
            input_file=input_file, asm_file=asm_file,
            executable_file=executable_file)
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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"text_file_roundtrip{EXE_EXT}")
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

        result = run_executable_with_valgrind(
            [book_exe],
            input=address_input,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
            check=True,
        )

        expected_prompt = (
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

        result_read = run_executable_with_valgrind(
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

        result_eof = run_executable_with_valgrind(
            [eof_exe],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
            check=True,
        )

        expected_eof = expected_read
        self.assertEqual(result_eof.stdout, expected_eof)

    def test_keyboard_arrow_sequences_output(self):
        """Crt ReadKey should map arrow keys to the expected output on a TTY."""
        if not HAS_PTY:
            self.skipTest("PTY not available on this platform")

        source = os.path.join(TEST_CASES_DIR, "keyboard_arrow.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "keyboard_arrow.s")
        exe_file = os.path.join(TEST_OUTPUT_DIR, f"keyboard_arrow{EXE_EXT}")

        run_compiler(source, asm_file)
        self.compile_executable(asm_file, exe_file)

        # Up arrow, newline, Ctrl+C to complete all four reads.
        input_data = "\x1b[A\n\x03"
        kgpc_run = run_executable_with_valgrind(
            [exe_file],
            input=input_data,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
            check=True,
            use_pty_capture=True,
        )
        expected_output = "0\n72\n10\n3\n"
        actual_output = re.sub(r"\x1b\[[0-9;?]*[A-Za-z]", "", kgpc_run.stdout or "")
        self.assertEqual(actual_output, expected_output)

    def test_run_executable_with_valgrind_pty_crlf_and_echo(self):
        """PTY path should normalize CRLF, merge stderr, and avoid input echo."""
        if not HAS_PTY:
            self.skipTest("PTY not available on this platform")
        self._test_pty_crlf_and_echo_impl()

    def _test_pty_crlf_and_echo_impl(self):
        helper_body = r"""
sys.stdout.write("line1\n")
sys.stdout.write("line2\r\n")
sys.stdout.flush()
sys.stderr.write("err-line\n")
sys.stderr.flush()
data = sys.stdin.readline()
sys.stdout.write("got:" + data)
sys.stdout.flush()
"""
        with tempfile.TemporaryDirectory() as tmpdir:
            script = _write_helper_script(tmpdir, helper_body)
            input_line = "user-input\n"
            result = _run_helper_with_valgrind(
                [sys.executable, script],
                input_data=input_line,
                timeout=5,
                text=True,
                capture_output=True,
                check=True,
                use_pty_capture=True,
            )

        self.assertIn(result.stderr, (None, ""))
        stdout = result.stdout or ""
        self.assertNotIn("\r", stdout)
        self.assertIn("line1\n", stdout)
        self.assertIn("line2\n", stdout)
        self.assertIn("err-line\n", stdout)
        lines = stdout.splitlines()
        self.assertNotIn("user-input", lines)
        self.assertIn("got:user-input", lines)

    def test_run_executable_with_valgrind_pty_timeout(self):
        """PTY path should terminate processes that exceed timeout."""
        if not HAS_PTY:
            self.skipTest("PTY not available on this platform")
        self._test_pty_timeout_impl()

    def _test_pty_timeout_impl(self):
        helper_body = r"""
time.sleep(10.0)
sys.stdout.write("should-not-see-this\n")
sys.stdout.flush()
"""
        with tempfile.TemporaryDirectory() as tmpdir:
            script = _write_helper_script(tmpdir, helper_body)
            result = _run_helper_with_valgrind(
                [sys.executable, script],
                timeout=0.5,
                text=True,
                capture_output=True,
                check=False,
                use_pty_capture=True,
            )

        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("should-not-see-this", result.stdout or "")

    def test_run_executable_with_valgrind_pty_vs_non_pty_equivalence(self):
        """PTY and non-PTY paths should surface the same output and exit codes."""
        if not HAS_PTY:
            self.skipTest("PTY not available on this platform")
        self._test_pty_vs_non_pty_equivalence_impl()

    def _test_pty_vs_non_pty_equivalence_impl(self):
        helper_body = r"""
sys.stdout.write("hello\n")
sys.stderr.write("world\n")
sys.stdout.flush()
sys.stderr.flush()
sys.exit(3)
"""
        with tempfile.TemporaryDirectory() as tmpdir:
            script = _write_helper_script(tmpdir, helper_body)
            non_pty_result = _run_helper_with_valgrind(
                [sys.executable, script],
                timeout=5,
                text=True,
                capture_output=False,
                check=False,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            pty_result = _run_helper_with_valgrind(
                [sys.executable, script],
                timeout=5,
                text=True,
                capture_output=True,
                check=False,
            )

        self.assertEqual(non_pty_result.returncode, 3)
        self.assertEqual(pty_result.returncode, 3)

        combined_non_pty = (
            (non_pty_result.stdout or "") + (non_pty_result.stderr or "")
        ).replace("\r\n", "\n").replace("\r", "")
        combined_pty = (
            (pty_result.stdout or "") + (pty_result.stderr or "")
        ).replace("\r\n", "\n").replace("\r", "")

        for token in ("hello\n", "world\n"):
            self.assertIn(token, combined_non_pty)
            self.assertIn(token, combined_pty)

    def test_const_expr_operators(self):
        """Tests const expressions with bitwise ops, NOT, and shifts."""
        input_file = os.path.join(TEST_CASES_DIR, "const_expr_operators.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "const_expr_operators.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"const_expr_operators{EXE_EXT}")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        process = subprocess.run(
            [executable_file],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )
        self.assertEqual(process.returncode, 0)
        self.assertEqual(
            process.stdout,
            "41\n17\n6\n9\n-1\n-2\n1\n1073741824\n",
        )

    def test_const_expr_typecasts(self):
        """Tests integer typecasts in const expressions."""
        input_file = os.path.join(TEST_CASES_DIR, "const_expr_typecasts.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "const_expr_typecasts.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"const_expr_typecasts{EXE_EXT}")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        process = subprocess.run(
            [executable_file],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )
        self.assertEqual(process.returncode, 0)
        self.assertEqual(
            process.stdout,
            "4294967295\n4294967295\n4294967296\n-1\n4294967295\n4294967295\n",
        )

    def test_fpc_directives_and_properties(self):
        """Tests FPC-style bracket directives and interface-level properties."""
        input_file = os.path.join(TEST_CASES_DIR, "directives_and_properties.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "directives_and_properties.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"directives_and_properties{EXE_EXT}")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        process = subprocess.run(
            [executable_file],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )
        self.assertEqual(process.returncode, 0)
        self.assertEqual(process.stdout, "10\n15\n3\n")

    def test_repeat_type_inference(self):
        """Tests repeat-until loops and variable type inference."""
        input_file = os.path.join(TEST_CASES_DIR, "repeat_infer.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "repeat_infer.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"repeat_infer{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"array_const{EXE_EXT}")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = run_executable_with_valgrind(
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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"record_decl_only{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"record_reference_features{EXE_EXT}")
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
        # Note: succ() is now inlined as x+1 by semcheck_builtin_predsucc, so no call to succ_i

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

    def test_fixed_register_division_under_pressure(self):
        """Ensures div/mod lowering survives when the general register pool is constrained."""
        input_file, asm_file, executable_file = self._get_test_paths("fixed_register_div_pressure")

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

        expected_output = read_file_content(
            os.path.join(TEST_CASES_DIR, "fixed_register_div_pressure.expected")
        )
        self.assertEqual(
            result.stdout.strip().splitlines(),
            expected_output.strip().splitlines(),
        )

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"variant_record_minimal{EXE_EXT}")
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
        result = run_executable_with_valgrind(
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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"with_nested_multi_context{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"mod_test{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"string_concat_demo{EXE_EXT}")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = run_executable_with_valgrind(
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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"sysutils_demo{EXE_EXT}")

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

    def test_inline_asm_uses_pascal_const_equ(self):
        """Ensures inline asm constants are emitted from Pascal const declarations."""
        input_file = os.path.join(TEST_CASES_DIR, "asm_const_equ.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "asm_const_equ.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"asm_const_equ{EXE_EXT}")

        run_compiler(input_file, asm_file)

        with open(asm_file, "r", encoding="utf-8") as f:
            asm_source = f.read()

        self.assertIn(".equ MagicValue, 1234", asm_source)
        self.assertNotIn(".equ ErmsThreshold, 1536", asm_source)
        self.assertNotIn(".equ NtThreshold, 262144", asm_source)
        self.assertNotIn(".equ PrefetchDistance, 512", asm_source)

        self.compile_executable(asm_file, executable_file)

        process = subprocess.run(
            [executable_file],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )
        self.assertEqual(process.returncode, 0)
        self.assertEqual(process.stdout, "OK\n")

    def test_asmmode_intel_directive_controls_inline_asm(self):
        input_file = os.path.join(TEST_CASES_DIR, "asmmode_intel_explicit.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "asmmode_intel_explicit.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"asmmode_intel_explicit{EXE_EXT}")

        run_compiler(input_file, asm_file)

        with open(asm_file, "r", encoding="utf-8") as f:
            asm_source = f.read()

        self.assertIn(".intel_syntax noprefix", asm_source)
        self.assertIn("mov eax, 42", asm_source)
        self.assertIn("movl $7, %eax", asm_source)
        self.assertEqual(asm_source.count(".intel_syntax noprefix"), 1)

        self.compile_executable(asm_file, executable_file)
        process = subprocess.run(
            [executable_file],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )
        self.assertEqual(process.returncode, 0)
        self.assertEqual(process.stdout, "OK\n")

    def test_unix_gethostname(self):
        """Ensures the Unix unit exposes GetHostName with actual hostname output."""
        input_file = os.path.join(TEST_CASES_DIR, "unix_gethostname_demo.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "unix_gethostname_demo.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"unix_gethostname_demo{EXE_EXT}")

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
            process = run_executable_with_valgrind(
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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"ord_builtin{EXE_EXT}")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = run_executable_with_valgrind(
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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"typed_const_array_demo{EXE_EXT}")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        try:
            process = run_executable_with_valgrind(
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
            process = run_executable_with_valgrind(
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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"ctypes_demo{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"zahlen_run{EXE_EXT}")

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
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"for{EXE_EXT}")

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

    def test_unit_compile_only(self):
        """Compiles standalone unit sources to ensure they parse and check cleanly."""
        for base_name in sorted(UNIT_ONLY_TESTS):
            with self.subTest(unit=base_name):
                input_file = os.path.join(TEST_CASES_DIR, f"{base_name}.p")
                asm_file = os.path.join(TEST_OUTPUT_DIR, f"{base_name}.s")
                flags = UNIT_ONLY_FLAGS.get(base_name)
                run_compiler(input_file, asm_file, flags=flags)

    def test_assert_failure_exits_227(self):
        """Assert(False, 'msg') must print the message to stderr and exit with code 227."""
        input_file = os.path.join(TEST_CASES_DIR, "assert_fail.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "assert_fail.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, f"assert_fail{EXE_EXT}")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        process = subprocess.run(
            [executable_file],
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )
        # stdout should show output before the assertion
        self.assertIn("before assert", process.stdout)
        # The assertion failure message must appear on stderr
        self.assertIn("Assertion failed", process.stderr)
        self.assertIn("this assertion should fail", process.stderr)
        # Must NOT reach code after the failed assert
        self.assertNotIn("this should not be printed", process.stdout)
        # FPC-compatible exit code 227 for assertion failure
        self.assertEqual(process.returncode, 227)


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
                if test_base_name in FPC_RTL_ONLY_TESTS:
                    self.skipTest("FPC RTL-only regression test")

                # Skip Unix fork-dependent tests on MinGW (which lacks POSIX fork)
                # Cygwin and MSYS have fork, pure MinGW does not
                # Skip tests with hardcoded SysV ABI inline asm on Windows
                if test_base_name == "nostackframe_asm_regsizing" and IS_WINDOWS_ABI:
                    self.skipTest("Inline asm test uses hardcoded SysV ABI registers")

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
                asm_file = os.path.join(TEST_OUTPUT_DIR, f"{test_base_name}_auto.s")
                executable_file = os.path.join(TEST_OUTPUT_DIR, f"{test_base_name}_auto{EXE_EXT}")
                expected_output_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.expected")
                expected_stderr_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.stderr.expected")
                input_data_file = os.path.join(INPUT_DATA_DIR, f"{test_base_name}.input")

                # Check test result cache
                cache_deps = [expected_stderr_file] if os.path.exists(expected_stderr_file) else []
                if _test_cache_check(input_file, expected_output_file, cache_deps):
                    return  # cached pass — skip

                compiler_output = None
                actual_output = None
                raw_stdout = None
                raw_stderr = None
                expected_output = None
                expected_stderr = None
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

                    actual_stderr = None
                    if os.path.exists(expected_stderr_file):
                        expected_stderr = read_file_content(expected_stderr_file)
                        stderr_text = raw_stderr
                        if stderr_text is None:
                            stderr_text = ""
                        while "\r\r\n" in stderr_text:
                            stderr_text = stderr_text.replace("\r\r\n", "\r\n")
                        actual_stderr = stderr_text.replace("\r\n", "\n").replace("\r", "")

                    self.record_failure_context(
                        base_name=test_base_name,
                        compiler_output=compiler_output,
                        normalized_output=actual_output,
                        raw_stdout=raw_stdout,
                        raw_stderr=raw_stderr,
                        expected_output=expected_output,
                        expected_stderr=expected_stderr,
                        returncode=process_returncode,
                    )

                    self.assertEqual(actual_output, expected_output)
                    if expected_stderr is not None:
                        self.assertEqual(actual_stderr, expected_stderr)
                    self.assertEqual(process_returncode, 0)
                    _test_cache_store(input_file, expected_output_file, cache_deps)
                except subprocess.TimeoutExpired:
                    self.record_failure_context(
                        base_name=test_base_name,
                        compiler_output=compiler_output,
                        normalized_output=actual_output,
                        raw_stdout=raw_stdout,
                        raw_stderr=raw_stderr,
                        expected_output=expected_output,
                        expected_stderr=expected_stderr,
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
                        expected_stderr=expected_stderr,
                        returncode=process_returncode,
                    )
                    raise
            
            test_method.__name__ = method_name
            test_method.__doc__ = f"Auto-discovered test case for {test_base_name}.p"
            return test_method
        
        # Add the test method to the TestCompiler class
        setattr(TestCompiler, method_name, make_test_method(base_name))



# Tests that use KGPC-only extensions not available in FPC RTL mode.
KGPC_ONLY_TESTS = {
    'random_real_function',  # Random(Real) overload is a KGPC extension
}

# Tests that target overloads or units only present in the FPC RTL suite.
FPC_RTL_ONLY_TESTS = {
    "fpc_bootstrap_hminus_extractfilepath",
}

# Tests without explicit FPC RTL/package imports are skipped from the FPC RTL
# suite by default. Keep a small explicit allowlist for tests that intentionally
# exercise implicit System/ObjPas/FPC bootstrap behavior.
FPC_RTL_IMPLICIT_UNIT_TESTS = {
    "bitsizeof_const_expr",
    # Pins the FreeMem-bypasses-RTL fix: with --no-stdlib + FPC RTL, a regression
    # would route FreeMem(p) through libc free instead of MemoryManager.FreeMem,
    # producing a "double free or corruption" SIGABRT. Two-arg FreeMem(p, size)
    # form must also stay routed through the user-visible Pascal FreeMem.
    "dos_freemem",
    "fpc_bootstrap_absolute_record_field",
    "fpc_bootstrap_andor_complex",
    "fpc_bootstrap_ansichar_type",
    "fpc_bootstrap_array_of_rawbytestring",
    "fpc_bootstrap_assign_textrec_cast",
    "fpc_bootstrap_class_of",
    "fpc_bootstrap_class_property",
    "fpc_bootstrap_class_property_getsystemencoding",
    "fpc_bootstrap_codepage_aliases",
    "fpc_bootstrap_codepage_search",
    "fpc_bootstrap_const_array_expr_bounds",
    "fpc_bootstrap_const_default_param",
    "fpc_bootstrap_constref_param",
    "fpc_bootstrap_constructor_overload_default",
    "fpc_bootstrap_constructor_overload_metaclass",
    "fpc_bootstrap_currency_type",
    "fpc_bootstrap_default_record_reset",
    "fpc_bootstrap_deprecated_const",
    "fpc_bootstrap_external_var",
    "fpc_bootstrap_fina_shortstring_move",
    "fpc_bootstrap_free_method",
    "fpc_bootstrap_heapinc_commonheader_sizeof_repro",
    "fpc_bootstrap_high_low_open_array",
    "fpc_bootstrap_if_declared",
    "fpc_bootstrap_if_fullversion",
    "fpc_bootstrap_include_shortstring_char_assign",
    "fpc_bootstrap_interface_alias",
    "fpc_bootstrap_interlockedexchangeadd_pointer",
    "fpc_bootstrap_longbool",
    "fpc_bootstrap_module_property_write",
    "fpc_bootstrap_nested_routines_begin",
    "fpc_bootstrap_nested_type_method_impl",
    "fpc_bootstrap_nested_type_typecast",
    "fpc_bootstrap_new_multidim_char_record_array",
    "fpc_bootstrap_new_multidim_array_pointer",
    "fpc_bootstrap_objpas_tendian_unit",
    "fpc_bootstrap_operator_overload",
    "fpc_bootstrap_overload_unicode_rawbyte",
    "fpc_bootstrap_pointer_const_typecast",
    "fpc_bootstrap_pointer_overload_resolution",
    "fpc_bootstrap_pointer_typecast_complex",
    "fpc_bootstrap_packed_record_pointer_for",
    "fpc_bootstrap_proc_const_typed",
    "fpc_bootstrap_procedure_var_assign",
    "fpc_bootstrap_proctype_cast_call",
    "fpc_bootstrap_ptext",
    "fpc_bootstrap_ptruint",
    "fpc_bootstrap_public_var",
    "fpc_bootstrap_qualified_case_label",
    "fpc_bootstrap_rawbytestring",
    "fpc_bootstrap_record_classvars",
    "fpc_bootstrap_resourcestring_concat",
    "fpc_bootstrap_resourcestring_output",
    "fpc_bootstrap_result_out_param",
    "fpc_bootstrap_scoped_enum_comparison",
    "fpc_bootstrap_scoped_enum_default_param",
    "fpc_bootstrap_shortstring_truncation",
    "fpc_bootstrap_shortstring_array_copy_compare",
    "fpc_bootstrap_sizeof_array_expr_bound",
    "fpc_bootstrap_sized_string_array_concat",
    "fpc_bootstrap_sized_string_array_const",
    "fpc_bootstrap_stderr",
    "fpc_bootstrap_string_n_length",
    "fpc_bootstrap_syshelps_pos_lowercase",
    "fpc_bootstrap_syssbh_pos_alias",
    "fpc_bootstrap_system_qualified_const",
    "fpc_bootstrap_system_qualified_proccall",
    "fpc_bootstrap_tclass",
    "fpc_bootstrap_trtlcriticalsection",
    "fpc_bootstrap_typedfile",
    "fpc_bootstrap_typehelpers_castinfo",
    "fpc_bootstrap_unit_init_unit",
    "fpc_const_subrange_array",
    "fpc_const_typecast",
    "fpc_exit_value",
    "fpc_exit_value_nested",
    "fpc_flush_text",
    "fpc_has_unicodestring_define",
    "fpc_heap_status",
    "fpc_macro_type_unix",
    "fpc_maxlongint",
    "fpc_octstr",
    "fpc_packed_record",
    "fpc_pansichar_array",
    "fpc_pointer_alias_chain",
    "fpc_pointer_arithmetic",
    "fpc_pointer_indexing",
    "fpc_pointer_subtraction",
    "fpc_ppansichar",
    "fpc_procedural_type_advanced",
    "fpc_procedural_type_basic",
    "fpc_puint16_typecast",
    "fpc_runtime_bitwise_or",
    "fpc_set_constant_char",
    "fpc_settextcodepage",
    "fpc_shl_const",
    "fpc_shortstring_type",
    "fpc_sizeuint",
    "fpc_str_procedure",
    "implicitsystemimport",
    # Regression for 15c0620c: `^TDerived := @ClassInstance.BaseField` must not
    # be routed through op-overload search. Test has no `uses` clause, but we
    # want it exercised under --no-stdlib too to cover both compile paths.
    "ptr_class_assign_with_variants",
    "system_core_basics",
    "system_qualified_length_if",
    "tdd_cp_acp_paramstr_ioresult",
    # GetMem/FreeMem allocator-pairing repro: FreeMem(p) must route through the
    # user-visible Pascal FreeMem (FPC's freemem_p) — same allocator as the
    # GetMem(N) above it — not directly to the runtime kgpc_freemem helper.
    "tdd_repro_pp_freemem_bypasses_rtl",
    "tdd_system_exit_qualified",
    "tdd_system_ttypekind",
    "tdd_types_core_symbols_bootstrap",
    # Tests that pass normally but fail with FPC RTL — added for regression tracking
    "bug_parser_full_repro",
    "const_var_transition",
    "random_function",
    "random_range_function",
    "reg_sysutils_inttohex_binstr",
    "tdd_capsizeint_wsm",
    "tdd_proc_field_dispatch",
    "tdd_propinfo_kind_case",
    "tdd_record_method_no_param_return_type",
    "tdd_stmt_system_error_unit_qualifier",
    "tdd_upcase_ord",
}

_FPC_RTL_KNOWN_UNITS = None


def _collect_pascal_unit_names(root_dir):
    names = set()
    if not os.path.isdir(root_dir):
        return names

    for dirpath, _, filenames in os.walk(root_dir):
        for filename in filenames:
            lower = filename.lower()
            if lower.endswith((".pp", ".p", ".pas")):
                names.add(os.path.splitext(filename)[0].lower())
    return names


def _get_fpc_rtl_known_units():
    global _FPC_RTL_KNOWN_UNITS
    if _FPC_RTL_KNOWN_UNITS is not None:
        return _FPC_RTL_KNOWN_UNITS

    roots = [
        FPC_RTL_DIR,
        os.path.join(os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource"), "packages", "rtl-objpas", "src"),
        os.path.join(os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource"), "packages", "rtl-console", "src"),
    ]
    unit_names = set()
    for root in roots:
        unit_names.update(_collect_pascal_unit_names(root))
    _FPC_RTL_KNOWN_UNITS = unit_names
    return _FPC_RTL_KNOWN_UNITS


def _strip_pascal_comments(text):
    result = []
    i = 0
    n = len(text)
    while i < n:
        if text.startswith("//", i):
            j = text.find("\n", i)
            if j == -1:
                break
            result.append("\n")
            i = j + 1
            continue
        if text.startswith("{", i):
            j = text.find("}", i + 1)
            if j == -1:
                break
            i = j + 1
            continue
        if text.startswith("(*", i):
            j = text.find("*)", i + 2)
            if j == -1:
                break
            i = j + 2
            continue
        result.append(text[i])
        i += 1
    return "".join(result)


def _extract_used_units(source_text):
    cleaned = _strip_pascal_comments(source_text)
    lower = cleaned.lower()
    units = set()
    i = 0
    while True:
        idx = lower.find("uses", i)
        if idx == -1:
            break

        before_ok = idx == 0 or not (lower[idx - 1].isalnum() or lower[idx - 1] == "_")
        after_idx = idx + 4
        after_ok = after_idx >= len(lower) or lower[after_idx].isspace()
        if not before_ok or not after_ok:
            i = idx + 4
            continue

        semi = cleaned.find(";", after_idx)
        if semi == -1:
            break
        clause = cleaned[after_idx:semi]
        for raw_unit in clause.split(","):
            token = raw_unit.strip()
            if not token:
                continue
            token = token.split("in", 1)[0].strip()
            if not token:
                continue
            unit_name = token.split(".")[-1].strip().lower()
            if unit_name:
                units.add(unit_name)
        i = semi + 1
    return units


def _should_include_in_fpcrtl(base_name, pascal_file):
    if base_name in FPC_RTL_IMPLICIT_UNIT_TESTS:
        return True

    try:
        source_text = Path(pascal_file).read_text(encoding="utf-8", errors="replace")
    except OSError:
        return True

    used_units = _extract_used_units(source_text)
    if not used_units:
        return False

    known_units = _get_fpc_rtl_known_units()
    for unit_name in used_units:
        if unit_name in known_units:
            return True
    return False


def _discover_and_add_fpc_rtl_tests():
    """
    When KGPC_FPC_RTL=1, replace auto-discovered tests with FPC RTL variants.
    Each test compiles with --no-stdlib and FPC RTL include/unit paths.
    Tests that fail to compile are skipped (not failed).
    Tests in KGPC_ONLY_TESTS are skipped entirely.
    """
    if not FPC_RTL_MODE:
        return
    if not os.path.isdir(FPC_RTL_DIR):
        return

    if not os.path.isdir(TEST_CASES_DIR):
        return

    expected_files = []
    for filename in os.listdir(TEST_CASES_DIR):
        if filename.endswith('.expected'):
            base_name = filename[:-9]
            pascal_file = os.path.join(TEST_CASES_DIR, base_name + '.p')
            if os.path.isfile(pascal_file):
                expected_files.append(base_name)

    expected_files.sort()

    for base_name in expected_files:
        if base_name in KGPC_ONLY_TESTS:
            continue
        pascal_file = os.path.join(TEST_CASES_DIR, base_name + '.p')
        if not _should_include_in_fpcrtl(base_name, pascal_file):
            continue

        method_name = 'test_fpcrtl_' + base_name.replace('-', '_').replace(' ', '_')

        if hasattr(TestCompiler, method_name):
            continue

        def make_fpc_rtl_test(test_base_name):
            def test_method(self):
                """FPC RTL test case."""
                input_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.p")
                asm_file = os.path.join(TEST_OUTPUT_DIR, f"{test_base_name}_fpcrtl.s")
                executable_file = os.path.join(TEST_OUTPUT_DIR, f"{test_base_name}_fpcrtl{EXE_EXT}")
                expected_output_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.expected")
                input_data_file = os.path.join(INPUT_DATA_DIR, f"{test_base_name}.input")

                if _test_cache_check(input_file, expected_output_file, FPC_RTL_FLAGS):
                    return  # cached pass — skip

                try:
                    compiler_output = run_compiler(input_file, asm_file,
                                                   flags=FPC_RTL_FLAGS)
                except subprocess.CalledProcessError:
                    self.fail(f"FPC RTL compilation failed for {test_base_name}")
                    return

                try:
                    self.compile_executable(asm_file, executable_file)
                except Exception:
                    self.fail(f"FPC RTL linking failed for {test_base_name}")
                    return

                stdin_input = None
                if os.path.exists(input_data_file):
                    stdin_input = read_file_content(input_data_file)

                try:
                    if stdin_input is not None:
                        process = run_executable_with_valgrind(
                            [executable_file],
                            capture_output=True, text=True,
                            timeout=EXEC_TIMEOUT, input=stdin_input,
                        )
                    else:
                        process = run_executable_with_valgrind(
                            [executable_file],
                            capture_output=True, text=True,
                            timeout=EXEC_TIMEOUT,
                        )
                except subprocess.TimeoutExpired:
                    self.fail(f"FPC RTL test {test_base_name} timed out")
                    return

                expected_output = read_file_content(expected_output_file)
                text = process.stdout or ""
                while "\r\r\n" in text:
                    text = text.replace("\r\r\n", "\r\n")
                actual_output = text.replace("\r\n", "\n").replace("\r", "")

                self.assertEqual(actual_output, expected_output)
                self.assertEqual(process.returncode, 0)
                _test_cache_store(input_file, expected_output_file, FPC_RTL_FLAGS)

            test_method.__name__ = method_name
            test_method.__doc__ = f"FPC RTL test for {test_base_name}.p"
            return test_method

        setattr(TestCompiler, method_name, make_fpc_rtl_test(base_name))


# ---------------------------------------------------------------------------
# pp.pas bootstrap compilation test — added as a special FPC RTL test.
# This verifies that the compiler can parse and codegen the FPC compiler
# itself (pp.pas), which exercises far more of the language than any
# individual RTL test case.
# ---------------------------------------------------------------------------
def _add_pp_pas_bootstrap_test():
    """Add pp.pas compilation as a special FPC RTL test.
    Enabled in FPC RTL mode."""
    if not FPC_RTL_MODE:
        return

    fpc_src = os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource")
    pp_pas = os.path.join(fpc_src, "compiler", "pp.pas")
    if not os.path.isfile(pp_pas):
        return

    pp_flags = _kgpc_bootstrap_flags(fpc_src, include_compiler_dirs=True)

    pp_expected_file = os.path.join(TEST_CASES_DIR, "pp_pas_bootstrap.expected")

    def _strip_pp_header(output):
        """Strip version/copyright/path lines from pp.pas -h output."""
        lines = output.splitlines(True)
        return "".join(
            l for l in lines
            if not l.startswith("Free Pascal Compiler version")
            and not l.startswith("Copyright")
            and "[options] <inputfile>" not in l
        )

    def test_pp_pas_bootstrap(self):
        """pp.pas bootstrap compilation — compile the FPC compiler itself."""
        asm_file = os.path.join(TEST_OUTPUT_DIR, "pp_bootstrap.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "pp_bootstrap" + EXE_EXT)

        # Generate msgtxt.inc and msgidx.inc by compiling and running the real
        # FPC msg2inc.pp utility with KGPC (same approach as
        # scripts/profile_large_unit_graph.py ensure_bootstrap_prereqs).
        compiler_dir = os.path.join(fpc_src, "compiler")
        errore_msg = os.path.join(compiler_dir, "msg", "errore.msg")
        msgtxt_inc = os.path.join(compiler_dir, "msgtxt.inc")
        msgidx_inc = os.path.join(compiler_dir, "msgidx.inc")
        msg_includes_are_stale = (
            not os.path.isfile(msgtxt_inc) or
            not os.path.isfile(msgidx_inc) or
            os.path.getmtime(msgtxt_inc) < os.path.getmtime(errore_msg) or
            os.path.getmtime(msgidx_inc) < os.path.getmtime(errore_msg)
        )
        if msg_includes_are_stale:
            msg2inc_pp = os.path.join(fpc_src, "compiler", "utils", "msg2inc.pp")
            assert os.path.isfile(msg2inc_pp), f"msg2inc.pp not found: {msg2inc_pp}"

            msg2inc_asm = os.path.join(TEST_OUTPUT_DIR, "msg2inc.s")
            msg2inc_exe = os.path.join(TEST_OUTPUT_DIR, "msg2inc" + EXE_EXT)

            # Compile msg2inc.pp with KGPC using the same RTL flags
            msg2inc_flags = _kgpc_bootstrap_flags(
                fpc_src, include_compiler_dirs=False
            )

            try:
                run_compiler(msg2inc_pp, msg2inc_asm, flags=msg2inc_flags, timeout=120)
            except subprocess.CalledProcessError as e:
                self.fail(f"msg2inc.pp compilation failed: {e}")
                return

            # Link msg2inc
            runtime_lib = _RUNTIME_LIB_PATH
            assert runtime_lib and os.path.isfile(runtime_lib), (
                f"KGPC runtime library not found: {runtime_lib!r}"
            )
            link_cmd = list(self.c_compiler_cmd) + [
                "-O2",
                "-no-pie" if not IS_WINDOWS_ABI else "-static",
                "-o", msg2inc_exe,
                msg2inc_asm,
                runtime_lib,
            ]
            link_cmd.extend(LINK_ARGS_BY_ASM.get(msg2inc_asm, []))
            try:
                subprocess.run(link_cmd, check=True, capture_output=True, text=True,
                               timeout=60)
            except subprocess.CalledProcessError as e:
                self.fail(f"msg2inc linking failed: {e.stderr}")
                return

            # Run msg2inc to generate msgtxt.inc and msgidx.inc
            run_cmd = [os.path.abspath(msg2inc_exe), "msg/errore.msg", "msg", "msg"]
            try:
                subprocess.run(run_cmd, check=True, capture_output=True, text=True,
                               cwd=compiler_dir, timeout=30)
            except subprocess.CalledProcessError as e:
                self.fail(f"msg2inc execution failed: {e.stderr}")
                return

            # Verify and SHA256 the generated files
            assert os.path.isfile(msgtxt_inc) and os.path.getsize(msgtxt_inc) > 0, (
                f"msg2inc produced empty or missing {msgtxt_inc}"
            )
            assert os.path.isfile(msgidx_inc) and os.path.getsize(msgidx_inc) > 0, (
                f"msg2inc produced empty or missing {msgidx_inc}"
            )
            with open(msgtxt_inc, "rb") as f:
                msgtxt_sha = hashlib.sha256(f.read()).hexdigest()
            with open(msgidx_inc, "rb") as f:
                msgidx_sha = hashlib.sha256(f.read()).hexdigest()
            print(f"msgtxt.inc sha256: {msgtxt_sha}", file=sys.stderr)
            print(f"msgidx.inc sha256: {msgidx_sha}", file=sys.stderr)

        try:
            run_compiler(pp_pas, asm_file, flags=pp_flags, timeout=600)
        except subprocess.CalledProcessError as e:
            self.fail(f"pp.pas compilation failed: {e}")
            return

        try:
            self.compile_executable(asm_file, executable_file)
        except Exception as e:
            self.fail(f"pp.pas linking failed: {e}")
            return

        # Run the compiled compiler with -h and compare output
        try:
            process = subprocess.run(
                [executable_file, "-h"],
                capture_output=True, text=True, timeout=30,
            )
        except subprocess.TimeoutExpired:
            self.fail("pp.pas binary timed out running with -h")
            return

        # Check exit code first — if the binary crashed, report that instead
        # of a confusing empty-string-vs-expected diff.
        if process.returncode != 0:
            sig_name = _signal_name_suffix(process.returncode)
            stderr_snippet = (process.stderr or "")[:2000]
            self.fail(
                f"pp.pas binary exited with code {process.returncode}{sig_name}\n"
                f"stderr:\n{stderr_snippet}"
            )

        expected_output = read_file_content(pp_expected_file)
        actual_output = _strip_pp_header(process.stdout or "")
        self.assertEqual(actual_output, expected_output)

        # End-to-end regression check: use the just-built pp_bootstrap to
        # compile a tiny Pascal program and then rebuild pp.pas itself. This
        # pins the generated compiler's ordinary program path as well as the
        # full self-hosting compiler path. The pipeline is:
        #     1. KGPC compiles pp.pas         -> tests/output/pp_bootstrap
        #        (already done above).
        #     2. pp_bootstrap compiles helloworld.p with the FPC RTL units.
        #     3. pp_bootstrap compiles pp.pas -> tests/output/pp_stage2/pp_stage2
        #     4. pp_stage2 starts and prints the expected help banner.
        #
        # pp_bootstrap needs prebuilt same-source RTL .ppu files for program
        # compilation and for pp.pas self-hosting. If CI has only the FPCSource
        # checkout, build a same-source compiler first, then use it to build
        # those units. A distro FPC can build incompatible .ppu files even when
        # it is good enough to seed the compiler build.
        helloworld_p = os.path.join(TEST_CASES_DIR, "helloworld.p")
        assert os.path.isfile(helloworld_p), f"helloworld.p missing: {helloworld_p}"

        prebuilt_units_dir = os.path.join(fpc_src, "rtl", "units", "x86_64-linux")
        prebuilt_system_ppu = os.path.join(prebuilt_units_dir, "system.ppu")
        ppu_version_source = os.path.join(fpc_src, "compiler", "ppu.pas")
        # `abitag.o` is built by the loader (not units) target, so a cache
        # populated by an older `make units` invocation can have system.ppu
        # without it.  Treat the loader artifact as a co-required input so
        # we always rebuild when either piece of the RTL is missing.
        prebuilt_abitag_o = os.path.join(prebuilt_units_dir, "abitag.o")
        rtl_units_are_stale = (
            not os.path.isfile(prebuilt_system_ppu) or
            not os.path.isfile(prebuilt_abitag_o) or
            os.path.getmtime(prebuilt_system_ppu) < os.path.getmtime(ppu_version_source) or
            os.path.getmtime(prebuilt_abitag_o) < os.path.getmtime(ppu_version_source) or
            _tree_contains_newer_file(os.path.join(fpc_src, "rtl"), prebuilt_system_ppu) or
            _tree_contains_newer_file(os.path.join(fpc_src, "rtl"), prebuilt_abitag_o)
        )
        if rtl_units_are_stale:
            if os.path.isdir(prebuilt_units_dir):
                shutil.rmtree(prebuilt_units_dir)
            make_bin = shutil.which("make")
            fpc_bin = shutil.which("fpc")
            assert make_bin is not None, (
                "make is required to build FPC RTL units for pp_bootstrap"
            )
            assert fpc_bin is not None, (
                "fpc is required to build prebuilt FPC RTL units for "
                "pp_bootstrap helloworld verification"
            )
            compiler_dir = os.path.join(fpc_src, "compiler")
            rtl_linux_dir = os.path.join(fpc_src, "rtl", "linux")
            same_source_fpc = os.path.join(
                compiler_dir, "ppcx64" + (".exe" if os.name == "nt" else "")
            )
            try:
                subprocess.run(
                    [make_bin, "-C", compiler_dir, "ppcx64", "FPC=" + fpc_bin],
                    check=True, capture_output=True, text=True, timeout=600,
                )
            except subprocess.CalledProcessError as e:
                self.fail(
                    "building same-source FPC compiler for pp_bootstrap failed\n"
                    f"stdout:\n{(e.stdout or '')[:2000]}\n"
                    f"stderr:\n{(e.stderr or '')[:2000]}"
                )
                return
            except subprocess.TimeoutExpired:
                self.fail("building same-source FPC compiler for pp_bootstrap timed out")
                return
            assert os.path.isfile(same_source_fpc), (
                f"FPC compiler build did not produce {same_source_fpc}"
            )
            try:
                # `make all` (not `units`) is required: the `units` target
                # only builds .ppu/.o pairs for Pascal units, but FPC's
                # Linux startup code uses `{$L abitag.o}` to pull in an
                # assembly-built note.ABI-tag section.  abitag is declared
                # as a LOADER in the RTL Makefile and is built by the
                # `loaders` (and thus `all`) target — `units` skips it,
                # which leaves pp_bootstrap unable to link any program.
                subprocess.run(
                    [
                        make_bin,
                        "-C",
                        rtl_linux_dir,
                        "all",
                        "FPC=" + os.path.abspath(same_source_fpc),
                    ],
                    check=True, capture_output=True, text=True, timeout=600,
                )
            except subprocess.CalledProcessError as e:
                self.fail(
                    "building FPC RTL units for pp_bootstrap failed\n"
                    f"stdout:\n{(e.stdout or '')[:2000]}\n"
                    f"stderr:\n{(e.stderr or '')[:2000]}"
                )
                return
            except subprocess.TimeoutExpired:
                self.fail("building FPC RTL units for pp_bootstrap timed out")
                return
            assert os.path.isfile(prebuilt_system_ppu), (
                f"FPC RTL build did not produce {prebuilt_system_ppu}"
            )
            # Loader artifacts (built by the `loaders` target rolled into
            # `all`) live alongside the .ppu files and are pulled in via
            # `{$L abitag.o}` from the Linux startup code.  Pin their
            # presence so a future Makefile/target regression surfaces
            # here instead of as an opaque "ld: cannot find abitag.o".
            assert os.path.isfile(prebuilt_abitag_o), (
                f"FPC RTL build did not produce {prebuilt_abitag_o}; "
                "did the Makefile target switch from `all` back to `units`?"
            )

        helloworld_exe = os.path.join(
            TEST_OUTPUT_DIR, "helloworld_via_pp_bootstrap" + EXE_EXT)
        try:
            os.remove(helloworld_exe)
        except FileNotFoundError:
            pass
        # `-n` skips fpc.cfg.  `-FE<dir>` puts the produced binary in
        # TEST_OUTPUT_DIR.  `-o<name>` disambiguates the artifact from
        # any other helloworld binary that may have ended up in the
        # output tree.
        bootstrap_cmd = [
            os.path.abspath(executable_file),
        ] + _pp_bootstrap_program_flags(
            rtl_units_dir=os.path.abspath(prebuilt_units_dir),
            output_dir=os.path.abspath(TEST_OUTPUT_DIR),
            executable_name=os.path.basename(helloworld_exe),
        ) + [helloworld_p]
        # Keep the source file last: this matches normal FPC invocation and
        # lets the shared flag helpers append all output/search-path flags in
        # one place before the input file.
        try:
            compile_proc = subprocess.run(
                bootstrap_cmd, capture_output=True, text=True, timeout=120
            )
        except subprocess.TimeoutExpired:
            self.fail("pp_bootstrap timed out compiling helloworld.p")
            return

        if compile_proc.returncode != 0 or not os.path.isfile(helloworld_exe):
            sig_name = _signal_name_suffix(compile_proc.returncode)
            self.fail(
                f"pp_bootstrap failed to compile {helloworld_p} "
                f"(rc={compile_proc.returncode}{sig_name}, "
                f"binary_present={os.path.isfile(helloworld_exe)})\n"
                f"cmd: {' '.join(bootstrap_cmd)}\n"
                f"stdout:\n{(compile_proc.stdout or '')[:2000]}\n"
                f"stderr:\n{(compile_proc.stderr or '')[:2000]}"
            )

        try:
            run = subprocess.run(
                [os.path.abspath(helloworld_exe)],
                capture_output=True, text=True, timeout=10,
            )
        except subprocess.TimeoutExpired:
            self.fail("helloworld binary built by pp_bootstrap timed out")
            return
        if run.returncode != 0:
            self.fail(
                f"helloworld binary built by pp_bootstrap exited with "
                f"code {run.returncode}\n"
                f"stdout:\n{(run.stdout or '')[:2000]}\n"
                f"stderr:\n{(run.stderr or '')[:2000]}"
            )
        self.assertEqual(
            (run.stdout or "").strip().splitlines(),
            ["Hello, World!"],
            "pp_bootstrap-built helloworld printed unexpected output",
        )

        stage2_dir = os.path.join(TEST_OUTPUT_DIR, "pp_stage2")
        stage2_units_dir = os.path.join(stage2_dir, "units")
        os.makedirs(stage2_units_dir, exist_ok=True)
        stage2_executable = os.path.join(stage2_dir, "pp_stage2" + EXE_EXT)
        try:
            os.remove(stage2_executable)
        except FileNotFoundError:
            pass

        stage2_cmd = [
            os.path.abspath(executable_file),
        ] + _pp_bootstrap_compiler_flags(
            fpc_src,
            rtl_units_dir=os.path.abspath(prebuilt_units_dir),
            output_dir=os.path.abspath(stage2_dir),
            unit_output_dir=os.path.abspath(stage2_units_dir),
            executable_name=os.path.basename(stage2_executable),
        ) + [os.path.abspath(pp_pas)]
        try:
            stage2_compile = subprocess.run(
                stage2_cmd,
                capture_output=True,
                text=True,
                timeout=900,
                cwd=os.path.join(fpc_src, "compiler"),
            )
        except subprocess.TimeoutExpired:
            self.fail("pp_bootstrap timed out compiling pp.pas into pp_stage2")
            return

        if stage2_compile.returncode != 0 or not os.path.isfile(stage2_executable):
            sig_name = _signal_name_suffix(stage2_compile.returncode)
            self.fail(
                f"pp_bootstrap failed to compile {pp_pas} "
                f"(rc={stage2_compile.returncode}{sig_name}, "
                f"binary_present={os.path.isfile(stage2_executable)})\n"
                f"cmd: {' '.join(stage2_cmd)}\n"
                f"stdout:\n{(stage2_compile.stdout or '')[:4000]}\n"
                f"stderr:\n{(stage2_compile.stderr or '')[:4000]}"
            )

        try:
            stage2_help = subprocess.run(
                [os.path.abspath(stage2_executable), "-h"],
                capture_output=True,
                text=True,
                timeout=30,
            )
        except subprocess.TimeoutExpired:
            self.fail("pp_stage2 timed out running with -h")
            return

        if stage2_help.returncode != 0:
            self.fail(
                f"pp_stage2 exited with code {stage2_help.returncode}\n"
                f"stdout:\n{(stage2_help.stdout or '')[:2000]}\n"
                f"stderr:\n{(stage2_help.stderr or '')[:2000]}"
            )
        self.assertEqual(
            _strip_pp_header(stage2_help.stdout or ""),
            expected_output,
            "pp_stage2 help output differs from the expected bootstrap banner",
        )

    test_pp_pas_bootstrap.__name__ = "test_fpcrtl_pp_pas_bootstrap"
    test_pp_pas_bootstrap.__doc__ = (
        "pp.pas bootstrap — compile and link the FPC compiler, use it to "
        "compile helloworld.p, then rebuild pp.pas itself"
    )
    # pp.pas is compiled twice in this test (KGPC -> pp_bootstrap, then
    # pp_bootstrap -> pp_stage2) and may also need the same-source RTL rebuild
    # on a cold worker, so allow one long timeout budget for the full chain.
    test_pp_pas_bootstrap._timeout = PP_BOOTSTRAP_FULL_CHAIN_TIMEOUT
    setattr(TestCompiler, "test_fpcrtl_pp_pas_bootstrap", test_pp_pas_bootstrap)


# Auto-discover and add tests before loading the suite
if FPC_RTL_MODE:
    _discover_and_add_fpc_rtl_tests()
    _add_pp_pas_bootstrap_test()
else:
    _discover_and_add_auto_tests()


def _load_suite():
    if FPC_RTL_MODE:
        # Only load FPC RTL tests, skip manual tests
        suite = unittest.TestSuite()
        for name in sorted(dir(TestCompiler)):
            if name.startswith('test_fpcrtl_'):
                suite.addTest(TestCompiler(name))
        return suite
    return unittest.defaultTestLoader.loadTestsFromModule(sys.modules[__name__])


class TimingTestResult(unittest.TextTestResult):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.timings = []
        self._start_times = {}
        self._status = {}

    def startTest(self, test):
        self._start_times[test] = time.perf_counter()
        super().startTest(test)

    def addSuccess(self, test):
        self._status[test] = "OK"
        super().addSuccess(test)

    def addFailure(self, test, err):
        self._status[test] = "FAIL"
        super().addFailure(test, err)

    def addError(self, test, err):
        self._status[test] = "ERROR"
        super().addError(test, err)

    def addSkip(self, test, reason):
        self._status[test] = "SKIP"
        super().addSkip(test, reason)

    def stopTest(self, test):
        start = self._start_times.pop(test, None)
        elapsed = time.perf_counter() - start if start else 0.0
        self.timings.append((test.id(), elapsed, self._status.get(test, "UNKNOWN")))
        super().stopTest(test)


def main():
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("--tap", action="store_true")
    parser.add_argument("--timings-out", dest="timings_out")
    parser.add_argument("--parallel", "-j", type=int, default=None,
                        help="Run tests in parallel with N workers (default: from KGPC_PARALLEL_WORKERS env)")
    parser.add_argument("--test-timeout", type=int, default=None,
                        help="Per-test timeout in seconds (default: from KGPC_TEST_CASE_TIMEOUT env or 300)")
    parser.add_argument("--failfast", "-f", action="store_true",
                        help="Stop on first failure/error (TAP runners only)")
    args, remaining = parser.parse_known_args()

    # Determine parallel workers and timeout
    parallel_workers = args.parallel if args.parallel is not None else PARALLEL_WORKERS
    test_timeout = args.test_timeout if args.test_timeout is not None else TEST_CASE_TIMEOUT

    tap_enabled = args.tap or os.environ.get("KGPC_TEST_PROTOCOL", "").lower() == "tap"
    if tap_enabled and args.timings_out:
        print("ERROR: --timings-out is not supported with TAP output.", file=sys.stderr)
        sys.exit(2)

    if args.tap or os.environ.get("KGPC_TEST_PROTOCOL", "").lower() == "tap":
        if remaining:
            suite = unittest.defaultTestLoader.loadTestsFromNames(
                remaining, sys.modules[__name__]
            )
        else:
            suite = _load_suite()
        if parallel_workers > 0:
            runner = TAPParallelTestRunner(workers=parallel_workers, timeout=test_timeout,
                                           failfast=args.failfast)
        else:
            runner = TAPTestRunner(failfast=args.failfast)
        result = runner.run(suite)
        sys.exit(0 if result.wasSuccessful() else 1)

    # Parallel mode with timeouts
    if parallel_workers > 0:
        if remaining:
            suite = unittest.defaultTestLoader.loadTestsFromNames(
                remaining, sys.modules[__name__]
            )
        else:
            suite = _load_suite()
        runner = ParallelTestRunner(workers=parallel_workers, timeout=test_timeout, stream=sys.stderr)
        result = runner.run(suite)
        # Print timing summary if there were timeouts or slow tests
        slow_tests = [(tid, t, s) for tid, t, s in result.test_timings if t > 30]
        if slow_tests:
            print("\n[PARALLEL] Slow tests (>30s):", file=sys.stderr)
            for test_id, elapsed, status in sorted(slow_tests, key=lambda x: -x[1]):
                print(f"  {elapsed:8.1f}s  {status:8s}  {test_id}", file=sys.stderr)
        sys.exit(0 if result.wasSuccessful() else 1)

    if args.timings_out:
        if remaining:
            suite = unittest.defaultTestLoader.loadTestsFromNames(
                remaining, sys.modules[__name__]
            )
        else:
            suite = _load_suite()
        runner = unittest.TextTestRunner(resultclass=TimingTestResult)
        result = runner.run(suite)
        with open(args.timings_out, "w", encoding="utf-8") as handle:
            for test_id, elapsed, status in sorted(
                result.timings, key=lambda x: x[1], reverse=True
            ):
                handle.write(f"{elapsed:8.3f}s\t{status}\t{test_id}\n")
        sys.exit(0 if result.wasSuccessful() else 1)
    print(f"DEBUG: argv={[sys.argv[0]] + remaining}")

    unittest.main(argv=[sys.argv[0]] + remaining)


if __name__ == "__main__":
    main()
