cmake_minimum_required(VERSION 3.10)
project(ParserCalculator C)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

enable_testing()

# --- Main Parser Library ---
add_library(parser_lib STATIC parser.c combinators.c)

# --- Unit Tests ---
# These are the core unit tests for the parser library. They are always built.
add_executable(tests tests.c)
target_link_libraries(tests parser_lib)
add_test(unit_tests tests)


# --- Examples and Integration Tests ---
option(BUILD_INTEGRATION_TESTS "Build the example applications and their tests" ON)

if(BUILD_INTEGRATION_TESTS)
    message(STATUS "Building integration tests and examples...")

    # --- Calculator Example ---
    add_library(calculator_logic_lib STATIC examples/calculator/calculator_logic.c)
    target_link_libraries(calculator_logic_lib PUBLIC parser_lib)
    target_include_directories(calculator_logic_lib PUBLIC ${CMAKE_SOURCE_DIR})

    add_executable(calculator examples/calculator/calc.c)
    target_include_directories(calculator PUBLIC ${CMAKE_SOURCE_DIR})
    target_link_libraries(calculator calculator_logic_lib)
    add_test(NAME calculator_integration_tests COMMAND ${CMAKE_SOURCE_DIR}/examples/calculator/test_calculator.sh)

    add_executable(calc_tests examples/calculator/calc_tests.c)
    target_link_libraries(calc_tests calculator_logic_lib)
    add_test(NAME calculator_unit_tests COMMAND calc_tests)

    # --- JSON Parser Example (optional, depends on libunwind) ---
    find_package(PkgConfig)
    if(PKG_CONFIG_FOUND)
        pkg_check_modules(UNWIND QUIET libunwind)
        if(UNWIND_FOUND)
            message(STATUS "Found libunwind, building JSON example.")
            add_library(json_parser_lib STATIC
                examples/json_parser/json_parser.c
                examples/json_parser/json_parser.h
            )
            target_link_libraries(json_parser_lib PUBLIC parser_lib)
            target_include_directories(json_parser_lib PUBLIC ${CMAKE_SOURCE_DIR} ${UNWIND_INCLUDE_DIRS})

            add_executable(json_parser_cli examples/json_parser/json_main.c)
            target_link_libraries(json_parser_cli json_parser_lib ${UNWIND_LIBRARIES})
            target_include_directories(json_parser_cli PUBLIC ${UNWIND_INCLUDE_DIRS})

            add_executable(json_tests examples/json_parser/json_tests.c)
            target_include_directories(json_tests PUBLIC ${CMAKE_SOURCE_DIR} ${UNWIND_INCLUDE_DIRS})
            target_link_libraries(json_tests json_parser_lib ${UNWIND_LIBRARIES})
            add_test(NAME json_tests COMMAND json_tests --verbose=3)
        else()
            message(STATUS "libunwind not found, skipping JSON example.")
        endif()
    else()
        message(STATUS "pkg-config not found, skipping JSON example.")
    endif()

    # --- Pascal Parser Example ---
    add_library(pascal_parser_lib STATIC
        examples/pascal_parser/pascal_parser.c
        examples/pascal_parser/pascal_parser.h
        examples/pascal_parser/pascal_keywords.c
        examples/pascal_parser/pascal_type.c
        examples/pascal_parser/pascal_expression.c
        examples/pascal_parser/pascal_statement.c
        examples/pascal_parser/pascal_declaration.c
    )
    target_link_libraries(pascal_parser_lib PUBLIC parser_lib)
    target_include_directories(pascal_parser_lib PUBLIC ${CMAKE_SOURCE_DIR})

    add_executable(pascal_parser_cli examples/pascal_parser/pascal_main.c)
    target_link_libraries(pascal_parser_cli pascal_parser_lib)

    add_executable(pascal_tests examples/pascal_parser/pascal_tests.c)
    target_link_libraries(pascal_tests pascal_parser_lib)
    add_test(NAME pascal_tests COMMAND pascal_tests)
endif()
