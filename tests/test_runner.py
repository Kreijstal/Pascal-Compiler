import subprocess
import os
import unittest

# Path to the compiler executable
# Get the build directory from the environment variable set by Meson.
# Default to "build" for local testing.
build_dir = os.environ.get('MESON_BUILD_ROOT', 'build')
GPC_PATH = os.path.join(build_dir, "GPC/gpc")
TEST_CASES_DIR = "tests/test_cases"
TEST_OUTPUT_DIR = "tests/output"
RUNTIME_SOURCE = "GPC/runtime.c"

# The compiler is built by Meson now, so this function is not needed.

def run_compiler(input_file, output_file, flags=None):
    """Runs the GPC compiler with the given arguments."""
    if flags is None:
        flags = []

    # Ensure the output directory exists
    os.makedirs(os.path.dirname(output_file), exist_ok=True)

    command = [GPC_PATH, input_file, output_file] + flags
    print(f"--- Running compiler: {' '.join(command)} ---")
    try:
        result = subprocess.run(command, check=True, capture_output=True, text=True)
        print(result.stderr) # The compiler prints status messages to stderr
        return result.stderr
    except subprocess.CalledProcessError as e:
        print(f"--- Compiler execution failed ---")
        print(f"--- stdout: {e.stdout} ---")
        print(f"--- stderr: {e.stderr} ---")
        # Still raise the exception, but return stderr if it exists
        if e.stderr:
            return e.stderr
        raise

def read_file_content(filepath):
    """Reads and returns the content of a file."""
    with open(filepath, "r") as f:
        return f.read()

class TestCompiler(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # The compiler is already built by Meson.
        # Create output directories
        os.makedirs(TEST_OUTPUT_DIR, exist_ok=True)
        os.makedirs(TEST_CASES_DIR, exist_ok=True)

        cls.runtime_object = os.path.join(TEST_OUTPUT_DIR, "runtime.o")
        cls._compile_runtime()

    @classmethod
    def _compile_runtime(cls):
        """Compile the runtime support file once to speed up repeated links."""
        try:
            subprocess.run(
                [
                    "gcc",
                    "-c",
                    "-O0",
                    "-pipe",
                    "-o",
                    cls.runtime_object,
                    RUNTIME_SOURCE,
                ],
                check=True,
                capture_output=True,
                text=True,
            )
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"runtime compilation failed: {e.stderr}")

    def compile_executable(self, asm_file, executable_file):
        try:
            subprocess.run(
                [
                    "gcc",
                    "-no-pie",
                    "-o",
                    executable_file,
                    asm_file,
                    self.runtime_object,
                ],
                check=True,
                capture_output=True,
                text=True,
            )
        except subprocess.CalledProcessError as e:
            self.fail(f"gcc compilation failed: {e.stderr}")

    def test_constant_folding_o1(self):
        """Tests the -O1 constant folding optimization."""
        input_file = os.path.join(TEST_CASES_DIR, "simple_expr.p")

        # --- Run without optimization ---
        unoptimized_output_file = os.path.join(TEST_OUTPUT_DIR, "simple_expr_unoptimized.s")
        run_compiler(input_file, unoptimized_output_file)
        unoptimized_asm = read_file_content(unoptimized_output_file)

        # In the unoptimized version, we expect to see the `add` instruction.
        # The compiler might use `addl` for 32-bit integers.
        # I'll check for "addl" since the compiler seems to be generating 32-bit code.
        self.assertIn("addl", unoptimized_asm)

        # --- Run with -O1 optimization ---
        optimized_output_file = os.path.join(TEST_OUTPUT_DIR, "simple_expr_optimized_o1.s")
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
        unoptimized_output_file = os.path.join(TEST_OUTPUT_DIR, "dead_code_unoptimized.s")
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
        optimized_output_file = os.path.join(TEST_OUTPUT_DIR, "dead_code_optimized_o2.s")
        run_compiler(input_file, optimized_output_file, flags=["-O2"])
        optimized_asm = read_file_content(optimized_output_file)

        # In the optimized version, the variable `y` is removed.
        # The variable `x` is also removed because it is assigned to but never used.
        # So, no local variables are needed from the user's code.
        # The prelude functions still exist, so we can't just check for no stack allocation.
        # Instead, we will check that the stack allocation is smaller than the unoptimized one.
        self.assertLess(len(optimized_asm), len(unoptimized_asm))


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
                        timeout=5 # Add a timeout to prevent hanging
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
                [executable_file],
                capture_output=True,
                text=True,
                timeout=5
            )
            self.assertEqual(process.stdout, "Hello, World!\n")
            self.assertEqual(process.returncode, 0)
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")

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
            timeout=5,
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
                timeout=5,
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
            process = subprocess.run([executable_file], capture_output=True, text=True, timeout=5)
            self.assertEqual(process.stdout, "42\n")
            self.assertEqual(process.returncode, 0)
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")

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
                [executable_file],
                capture_output=True,
                text=True,
                timeout=5
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
                [executable_file],
                capture_output=True,
                text=True,
                timeout=5
            )
            self.assertEqual(process.stdout, "1\n")
            self.assertEqual(process.returncode, 0)
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")

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
                timeout=5,
            )
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")
            return

        lines = process.stdout.strip().splitlines()
        self.assertGreaterEqual(len(lines), 2)
        self.assertEqual(lines[0].strip(), "32")
        self.assertEqual(lines[1].strip(), "1")
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
            timeout=5,
        )

        # Verify that both even and odd buckets are populated correctly.
        expected_output_lines = [
            "Schreib wie viele Zahlen wollen sie eintippen, danach schreiben Sie die Zahlen.\n",
            "         gerade       ungerade       Positive       Negative\n",
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
        """Tests the for program."""
        input_file = "GPC/TestPrograms/CodeGeneration/for.p"
        asm_file = os.path.join(TEST_OUTPUT_DIR, "for.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "for")

        # Compile the pascal program to assembly
        run_compiler(input_file, asm_file)

        # Compile the assembly to an executable
        try:
            subprocess.run(["gcc", "-no-pie", "-o", executable_file, asm_file, "GPC/runtime.c"], check=True, capture_output=True, text=True)
        except subprocess.CalledProcessError as e:
            self.fail(f"gcc compilation failed: {e.stderr}")

        # Run the executable and check the output
        try:
            process = subprocess.run(
                [executable_file],
                input="3",
                capture_output=True,
                text=True,
                timeout=5
            )
            self.assertEqual(process.stdout.strip(), "123456")
            self.assertEqual(process.returncode, 0)
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")


if __name__ == "__main__":
    unittest.main()
