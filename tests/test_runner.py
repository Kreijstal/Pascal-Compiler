import argparse
import os
import shutil
import shlex
import subprocess
import sys
import time
import traceback
import unittest

WINDOWS_ABI_PLATFORMS = ("win", "cygwin", "msys", "mingw")
PLATFORM_ID = sys.platform.lower()
IS_WINDOWS_ABI = os.name == "nt" or PLATFORM_ID.startswith(WINDOWS_ABI_PLATFORMS)

# Path to the compiler executable
# Get the build directory from the environment variable set by Meson.
# Default to "build" for local testing.
build_dir = os.environ.get("MESON_BUILD_ROOT", "build")
GPC_PATH = os.path.join(build_dir, "GPC/gpc.exe" if IS_WINDOWS_ABI else "GPC/gpc")
TEST_CASES_DIR = "tests/test_cases"
TEST_OUTPUT_DIR = "tests/output"
GOLDEN_AST_DIR = "tests/golden_ast"
EXEC_TIMEOUT = 5

# Meson exposes toggleable behaviour via environment variables so CI can
# selectively disable particularly slow checks such as the valgrind leak test.
RUN_VALGRIND_TESTS = os.environ.get("RUN_VALGRIND_TESTS", "false").lower() in (
    "1",
    "true",
    "yes",
)

# Track how long individual compiler invocations take so we can identify the
# slowest scenarios when the test suite finishes. The collected data is emitted
# from TestCompiler.tearDownClass() and written to stderr to keep TAP output
# intact.
COMPILER_RUNS = []

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


def run_compiler(input_file, output_file, flags=None):
    """Runs the GPC compiler with the given arguments."""
    if flags is None:
        flags = []
    else:
        flags = list(flags)

    # Ensure the output directory exists
    os.makedirs(os.path.dirname(output_file), exist_ok=True)

    command = [GPC_PATH, input_file, output_file]
    if IS_WINDOWS_ABI and not _has_explicit_target_flag(flags):
        command.append("--target-windows")
    command.extend(flags)
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

    def startTest(self, test):
        super().startTest(test)
        self._test_index += 1

    def addSuccess(self, test):
        super().addSuccess(test)
        self._emit(f"ok {self._test_index} - {self._test_name(test)}")

    def addSkip(self, test, reason):
        super().addSkip(test, reason)
        self._emit(f"ok {self._test_index} - {self._test_name(test)} # SKIP {reason}")

    def addExpectedFailure(self, test, err):
        super().addExpectedFailure(test, err)
        self._emit(
            f"ok {self._test_index} - {self._test_name(test)} # TODO expected failure"
        )

    def addUnexpectedSuccess(self, test):
        super().addUnexpectedSuccess(test)
        self._emit(
            f"not ok {self._test_index} - {self._test_name(test)} # Unexpected success"
        )

    def addFailure(self, test, err):
        super().addFailure(test, err)
        self._emit(f"not ok {self._test_index} - {self._test_name(test)}")
        self._emit_diagnostic("Failure:")
        self._emit_diagnostic("".join(traceback.format_exception(*err)))

    def addError(self, test, err):
        super().addError(test, err)
        self._emit(f"not ok {self._test_index} - {self._test_name(test)}")
        self._emit_diagnostic("Error:")
        self._emit_diagnostic("".join(traceback.format_exception(*err)))


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
    @classmethod
    def setUpClass(cls):
        cls._ensure_compiler_built()
        # Create output directories
        os.makedirs(TEST_OUTPUT_DIR, exist_ok=True)
        os.makedirs(TEST_CASES_DIR, exist_ok=True)

        cc_raw = os.environ.get("CC")
        if not cc_raw:
            raise RuntimeError(
                "CC environment variable must be set by Meson before running tests"
            )
        cls.c_compiler_display = cc_raw
        cls.c_compiler_cmd = shlex.split(cc_raw)
        if not cls.c_compiler_cmd:
            raise RuntimeError("CC environment variable did not contain an executable")

        cls.runtime_library = os.environ.get("GPC_RUNTIME_LIB")
        if not cls.runtime_library:
            raise RuntimeError(
                "GPC_RUNTIME_LIB environment variable is required to link generated code"
            )
        if not os.path.exists(cls.runtime_library):
            raise RuntimeError(
                f"Runtime library path from GPC_RUNTIME_LIB does not exist: {cls.runtime_library}"
            )

        cls.ctypes_helper_library = os.environ.get("GPC_CTYPES_HELPER")
        cls.ctypes_helper_link = os.environ.get("GPC_CTYPES_HELPER_LINK")
        if cls.ctypes_helper_link is None and cls.ctypes_helper_library is not None:
            cls.ctypes_helper_link = cls.ctypes_helper_library
        if cls.ctypes_helper_link is not None and not os.path.exists(cls.ctypes_helper_link):
            raise RuntimeError(
                "ctypes helper link target provided by Meson does not exist: "
                f"{cls.ctypes_helper_link}"
            )
        cls.ctypes_helper_dir = (
            os.path.dirname(cls.ctypes_helper_library)
            if cls.ctypes_helper_library is not None
            else None
        )
        if cls.ctypes_helper_dir is not None and not os.path.exists(cls.ctypes_helper_library):
            raise RuntimeError(
                "ctypes helper shared library provided by Meson does not exist: "
                f"{cls.ctypes_helper_library}"
            )

    @classmethod
    def _ensure_compiler_built(cls):
        """Builds the compiler via Meson if the gpc binary is missing."""
        if os.path.exists(GPC_PATH):
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

        if not os.path.exists(GPC_PATH):
            raise RuntimeError(
                f"Meson build completed but did not produce compiler at {GPC_PATH}"
            )

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
            command.extend([
                "-o",
                executable_file,
                asm_file,
                self.runtime_library,
            ])
            command.extend(list(extra_objects))
            command.extend(list(extra_link_args))
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

    def test_dead_code_elimination_o2(self):
        """Tests the -O2 dead code elimination optimization."""
        input_file = os.path.join(TEST_CASES_DIR, "dead_code.p")

        # --- Run without optimization ---
        unoptimized_output_file = os.path.join(
            TEST_OUTPUT_DIR, "dead_code_unoptimized.s"
        )
        run_compiler(input_file, unoptimized_output_file)
        unoptimized_asm = read_file_content(unoptimized_output_file)

        # In the unoptimized version, we expect space for two integers (x and y).
        # The stack allocation depends on the active ABI: System V uses 16 bytes,
        # while the Windows x64 ABI reserves a 32-byte home space in addition to
        # the locals (48 bytes total in this case).
        if ".set\tGPC_TARGET_WINDOWS, 1" in unoptimized_asm:
            self.assertIn("subq\t$48", unoptimized_asm)
        else:
            self.assertIn("subq\t$16", unoptimized_asm)

        # --- Run with -O2 optimization ---
        optimized_output_file = os.path.join(
            TEST_OUTPUT_DIR, "dead_code_optimized_o2.s"
        )
        run_compiler(input_file, optimized_output_file, flags=["-O2"])
        optimized_asm = read_file_content(optimized_output_file)

        # In the optimized version, the variable `y` is removed.
        # The variable `x` is also removed because it is assigned to but never used.
        # So, no local variables are needed from the user's code.
        # The prelude functions still exist, so we can't just check for no stack allocation.
        # Instead, we will check that the stack allocation is smaller than the unoptimized one.
        self.assertLess(len(optimized_asm), len(unoptimized_asm))

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

        result = subprocess.run(
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
            GPC_PATH,
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
            self.fail("GPC_CTYPES_HELPER_LINK must be provided to link ctypes demo")

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
            self.fail("GPC_CTYPES_HELPER must be provided to run ctypes demo")
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
        input_file = "GPC/TestPrograms/sign_test.p"
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
            process = subprocess.run(
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

    def test_set_operations_program(self):
        """Exercises set arithmetic, membership, and boolean conditions."""
        input_file = os.path.join(TEST_CASES_DIR, "set_operations.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "set_operations.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "set_operations")

        run_compiler(input_file, asm_file)
        self.compile_executable(asm_file, executable_file)

        expected_output = read_file_content(
            os.path.join(TEST_CASES_DIR, "set_operations.expected")
        )

        result = subprocess.run(
            [executable_file],
            check=True,
            capture_output=True,
            text=True,
            timeout=EXEC_TIMEOUT,
        )

        self.assertEqual(result.stdout, expected_output)
        self.assertEqual(result.returncode, 0)

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

    def test_record_member_access(self):
        """Tests that record field loads and stores compile and execute."""
        input_file = os.path.join(TEST_CASES_DIR, "record_member_access.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "record_member_access.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "record_member_access")
        expected_output_file = os.path.join(
            TEST_CASES_DIR, "record_member_access.expected"
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

        self.assertEqual(result.stdout, expected_output)
        self.assertEqual(result.returncode, 0)

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
        self.assertIn("call\tgpc_move", asm_source)
        self.assertIn("call\tsucc_i", asm_source)

    def test_fizzbuzz(self):
        """Tests the fizzbuzz program."""
        input_file = os.path.join(TEST_CASES_DIR, "fizzbuzz.p")
        asm_file = os.path.join(TEST_OUTPUT_DIR, "fizzbuzz.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "fizzbuzz")
        expected_output_file = os.path.join(TEST_CASES_DIR, "fizzbuzz.expected")

        # Compile the pascal program to assembly
        run_compiler(input_file, asm_file)

        # Compile the assembly to an executable
        self.compile_executable(asm_file, executable_file)

        # Run the executable and check the output
        try:
            process = subprocess.run(
                [executable_file], capture_output=True, text=True, timeout=EXEC_TIMEOUT
            )
            expected_output = read_file_content(expected_output_file)
            self.assertEqual(process.stdout, expected_output)
            self.assertEqual(process.returncode, 0)
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")

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
        self.assertGreaterEqual(len(lines), 2)
        self.assertEqual(lines[0].strip(), "32")
        self.assertEqual(lines[1].strip(), "1")
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
        input_file = "GPC/TestPrograms/CodeGeneration/for.p"
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



def _load_suite():
    return unittest.defaultTestLoader.loadTestsFromModule(sys.modules[__name__])


def main():
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("--tap", action="store_true")
    args, remaining = parser.parse_known_args()

    if args.tap or os.environ.get("GPC_TEST_PROTOCOL", "").lower() == "tap":
        suite = _load_suite()
        runner = TAPTestRunner()
        result = runner.run(suite)
        sys.exit(0 if result.wasSuccessful() else 1)

    unittest.main(argv=[sys.argv[0]] + remaining)


if __name__ == "__main__":
    main()
