# Parser Combinator Examples

This directory contains example applications that demonstrate the use of the parser combinator library.

## Examples

-   **Calculator:** A simple integer arithmetic calculator that supports addition, subtraction, multiplication, and division.
-   **JSON Parser:** A parser for the JSON data format.

## Building the Examples

To build the examples, you must enable the `BUILD_INTEGRATION_TESTS` option in your CMake configuration.

For example:

```bash
cmake -S . -B build -DBUILD_INTEGRATION_TESTS=ON
cmake --build build
```

This will build the `calculator` and `json_parser_cli` executables, as well as the `json_tests` test suite. You can then run the executables from the `build` directory.

To run the tests, use `ctest`:

```bash
cd build
ctest
```
