project('gpc-project', 'c')

cparser_core_sources = [
  'cparser/parser.c',
  'cparser/combinators.c',
]

pascal_parser_sources = [
  'cparser/examples/pascal_parser/pascal_parser.c',
  'cparser/examples/pascal_parser/pascal_expression.c',
  'cparser/examples/pascal_parser/pascal_statement.c',
  'cparser/examples/pascal_parser/pascal_declaration.c',
  'cparser/examples/pascal_parser/pascal_type.c',
  'cparser/examples/pascal_parser/pascal_keywords.c',
  'cparser/examples/pascal_parser/pascal_preprocessor.c',
]

parser_inc = include_directories('cparser')
pascal_inc = include_directories('cparser/examples/pascal_parser')
calculator_inc = include_directories('cparser/examples/calculator')

parser_lib = static_library('cparser_core',
  cparser_core_sources,
  include_directories: parser_inc
)

cparser_dep = declare_dependency(
  link_with: parser_lib,
  include_directories: parser_inc
)

pascal_parser_lib = static_library('pascal_parser',
  pascal_parser_sources,
  include_directories: [parser_inc, pascal_inc],
  dependencies: cparser_dep
)

pascal_parser_dep = declare_dependency(
  link_with: pascal_parser_lib,
  include_directories: [parser_inc, pascal_inc]
)

build_integration_tests = get_option('build_integration_tests')

cparser_tests = executable('cparser_tests', 'cparser/tests.c',
  dependencies: cparser_dep
)

test('cparser unit tests', cparser_tests)

if build_integration_tests
  calculator_logic_lib = static_library('calculator_logic',
    'cparser/examples/calculator/calculator_logic.c',
    include_directories: [parser_inc, calculator_inc],
    dependencies: cparser_dep
  )

  calculator_dep = declare_dependency(
    link_with: calculator_logic_lib,
    include_directories: [parser_inc, calculator_inc]
  )

  calculator = executable('calculator',
    'cparser/examples/calculator/calc.c',
    dependencies: calculator_dep
  )

  calc_tests = executable('calc_tests',
    'cparser/examples/calculator/calc_tests.c',
    dependencies: calculator_dep
  )

  test('calculator unit tests', calc_tests)

  bash = find_program('bash', required: false)
  if bash.found()
    test('calculator integration tests', bash,
      args: [meson.project_source_root() / 'cparser/examples/calculator/test_calculator.sh'],
      workdir: meson.current_build_dir(),
      depends: calculator
    )
  endif

  pascal_parser_cli = executable('pascal_parser_cli',
    'cparser/examples/pascal_parser/pascal_main.c',
    dependencies: pascal_parser_dep
  )

  pascal_tests = executable('pascal_tests',
    'cparser/examples/pascal_parser/pascal_tests.c',
    dependencies: pascal_parser_dep
  )

  test('pascal parser unit tests', pascal_tests, timeout: 120)

  pascal_benchmark = executable('pascal_benchmark',
    'cparser/examples/pascal_parser/pascal_benchmark.c',
    dependencies: pascal_parser_dep
  )
endif

subdir('GPC')
