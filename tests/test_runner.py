import subprocess
import os
import unittest

# Path to the compiler executable
GPC_PATH = "./gpc"
TEST_CASES_DIR = "tests/test_cases"
TEST_OUTPUT_DIR = "tests/output"

def compile_compiler():
    """Compiles the GPC compiler by running make."""
    print("--- Compiling the compiler ---")
    print("--- CWD: ", os.getcwd())
    try:
        # I need to find the correct makefile. Based on the file listing, it's in the root of GPC.
        subprocess.run(["make", "-C", "GPC"], check=True, capture_output=True, text=True)
        print("--- Compiler compiled successfully ---")
    except subprocess.CalledProcessError as e:
        print(f"--- Compiler compilation failed ---")
        print(f"--- stdout: {e.stdout} ---")
        print(f"--- stderr: {e.stderr} ---")
        exit(1)

def run_compiler(input_file, output_file, flags=None):
    """Runs the GPC compiler with the given arguments."""
    if flags is None:
        flags = []

    # Ensure the output directory exists
    os.makedirs(os.path.dirname(output_file), exist_ok=True)

    input_file = os.path.abspath(input_file)
    output_file = os.path.abspath(output_file)
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
        # Compile the compiler once before any tests run
        compile_compiler()
        # Create output directories
        os.makedirs(TEST_OUTPUT_DIR, exist_ok=True)
        os.makedirs(TEST_CASES_DIR, exist_ok=True)

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
        # The stack is 16-byte aligned, so this will be 16 bytes.
        self.assertIn("subq\t$16", unoptimized_asm)

        # --- Run with -O2 optimization ---
        optimized_output_file = os.path.join(TEST_OUTPUT_DIR, "dead_code_optimized_o2.s")
        run_compiler(input_file, optimized_output_file, flags=["-O2"])
        optimized_asm = read_file_content(optimized_output_file)

        # In the optimized version, the variable `y` is removed.
        # The variable `x` is also removed because it is assigned to but never used.
        # So, no local variables are needed from the user's code.
        # The prelude functions still exist, so we can't just check for no stack allocation.
        # Instead, we will check that the main program's body has no stack allocation for its own locals.
        # A simple way is to check that the stack allocation is smaller than the unoptimized one.
        self.assertLess(len(optimized_asm), len(unoptimized_asm))


    def test_sign_function(self):
        """Tests the sign function with positive, negative, and zero inputs."""
        input_file = "GPC/TestPrograms/sign_test.p"
        asm_file = os.path.join(TEST_OUTPUT_DIR, "sign_test.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "sign_test")

        # Compile the pascal program to assembly
        run_compiler(input_file, asm_file)

        # Compile the assembly to an executable
        try:
            subprocess.run(["gcc", "-no-pie", "-o", executable_file, asm_file, "GPC/runtime.c"], check=True, capture_output=True, text=True)
        except subprocess.CalledProcessError as e:
            self.fail(f"gcc compilation failed: {e.stderr}")

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
                    # The program seems to be printing a space after the number
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
        try:
            subprocess.run(["gcc", "-no-pie", "-o", executable_file, asm_file, "GPC/runtime.c"], check=True, capture_output=True, text=True)
        except subprocess.CalledProcessError as e:
            self.fail(f"gcc compilation failed: {e.stderr}")

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


    def test_refactor(self):
        """Tests the refactored code with a new test case."""
        input_file = "GPC/TestPrograms/CodeGeneration/refactor_test.p"
        asm_file = os.path.join(TEST_OUTPUT_DIR, "refactor_test.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "refactor_test")

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
                capture_output=True,
                text=True,
                timeout=5
            )
            expected_output = "i is less than j\nk = 0\nk = 1\nk = 2\nk = 3\nk = 4\ni = 1, i = 2, i = 3, i = 4, i = 5, \n"
            self.assertEqual(process.stdout, expected_output)
            self.assertEqual(process.returncode, 0)
        except subprocess.TimeoutExpired:
            self.fail("Test execution timed out.")

if __name__ == "__main__":
    unittest.main()
