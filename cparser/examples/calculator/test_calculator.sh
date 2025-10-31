#!/bin/bash

# A simple test suite for the calculator example.

# Exit immediately if a command exits with a non-zero status.
set -e

# Find the calculator executable.
# The script is run from the build directory, so the executable should be there.
# Support both native and cross-compiled (.exe) executables.
CALCULATOR_EXEC="./calculator"

if [ ! -x "$CALCULATOR_EXEC" ]; then
    # Try .exe extension for cross-compiled Windows executables
    if [ -x "./calculator.exe" ]; then
        CALCULATOR_EXEC="./calculator.exe"
    else
        echo "Error: Calculator executable not found at $CALCULATOR_EXEC or ./calculator.exe"
        exit 1
    fi
fi

echo "Running calculator test suite..."

# Counter for tests
TEST_COUNT=0
FAIL_COUNT=0

# Helper function to check a test case
# Usage: check_case "expression" "expected_output" "test_name"
check_case() {
    TEST_COUNT=$((TEST_COUNT + 1))
    local expression="$1"
    local expected="$2"
    local name="$3"

    # Run the calculator and capture the output
    # Suppress Wine warnings by redirecting stderr when running .exe files
    # Strip Windows line endings (\r) that may be present in Wine output
    local actual=$( "$CALCULATOR_EXEC" "$expression" 2>/dev/null | tr -d '\r' )

    if [ "$actual" = "$expected" ]; then
        echo "  [PASS] $name: '$expression' -> $expected"
    else
        echo "  [FAIL] $name: '$expression' -> Expected '$expected', got '$actual'"
        FAIL_COUNT=$((FAIL_COUNT + 1))
    fi
}

# Helper function to check for expected failures
# Usage: check_fail "expression" "test_name"
check_fail() {
    TEST_COUNT=$((TEST_COUNT + 1))
    local expression="$1"
    local name="$2"

    # Run the calculator, redirecting stdout and stderr.
    # We expect it to fail (non-zero exit code).
    if ! "$CALCULATOR_EXEC" "$expression" > /dev/null 2>&1; then
        echo "  [PASS] $name: '$expression' -> (Correctly failed)"
    else
        echo "  [FAIL] $name: '$expression' -> (Expected to fail, but succeeded)"
        FAIL_COUNT=$((FAIL_COUNT + 1))
    fi
}


# --- Valid Expressions ---
echo "[1] Testing valid expressions..."
check_case "1 + 1" "2" "Simple addition"
check_case "10 - 3" "7" "Simple subtraction"
check_case "5 * 7" "35" "Simple multiplication"
check_case "100 / 10" "10" "Simple division"
check_case "1 + 2 * 3" "7" "Operator precedence (mul)"
check_case "4 * 5 + 2" "22" "Operator precedence (add)"
check_case "(1 + 2) * 3" "9" "Parentheses"
check_case "10 / (2 + 3)" "2" "Parentheses with division"
check_case "-5 + 10" "5" "Unary minus"
check_case "5 * -2" "-10" "Unary minus with multiplication"
check_case "10 + -5" "5" "Addition with negative number"

# --- Failure Cases ---
echo "[2] Testing failure cases..."
check_fail "1 + * 2" "Syntax error (double operator)"
check_fail "(1 + 2" "Syntax error (unmatched parenthesis)"
check_fail "1 / 0" "Runtime error (division by zero)"
check_fail "abc" "Syntax error (invalid characters)"


# --- Final Summary ---
echo "--------------------"
if [ "$FAIL_COUNT" -eq 0 ]; then
    echo "All $TEST_COUNT calculator tests passed!"
    exit 0
else
    echo "$FAIL_COUNT out of $TEST_COUNT tests failed."
    exit 1
fi
