project('kgpc-project', 'c')

cc = meson.get_compiler('c')
if cc.has_function('strndup', prefix: '#include <string.h>')
  add_project_arguments('-DHAVE_STRNDUP', language: 'c')
endif

cparser_core_sources = [
  'cparser/parser.c',
  'cparser/combinators.c',
  'cparser/ast_cache.c',
  'common/arena.c',
  'common/file_lock.c',
]

pascal_parser_sources = [
  'cparser/examples/pascal_parser/pascal_parser.c',
  'cparser/examples/pascal_parser/pascal_expression.c',
  'cparser/examples/pascal_parser/pascal_statement.c',
  'cparser/examples/pascal_parser/pascal_declaration.c',
  'cparser/examples/pascal_parser/pascal_type.c',
  'cparser/examples/pascal_parser/pascal_keywords.c',
  'cparser/examples/pascal_parser/pascal_preprocessor.c',
  'cparser/examples/pascal_parser/pascal_peek.c',
  'cparser/examples/pascal_parser/pascal_statement_keywords_hash.c',
]

parser_inc = include_directories('cparser', 'common')
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
  dependencies: cparser_dep,
)

test('cparser unit tests', cparser_tests, timeout: 1800)

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

  test('calculator unit tests', calc_tests, timeout: 1800)

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
    dependencies: pascal_parser_dep,
    c_args: '-DTEST_SRCDIR="' + meson.project_source_root() / 'cparser/examples/pascal_parser' + '"'
  )

test('pascal parser unit tests', pascal_tests, timeout: 1800)

  pascal_benchmark = executable('pascal_benchmark',
    'cparser/examples/pascal_parser/pascal_benchmark.c',
    dependencies: pascal_parser_dep
  )
endif

subdir('KGPC')

# Standalone preprocessor tool
kgpc_preprocess = executable('kgpc-preprocess',
  'KGPC/preprocess_main.c',
  dependencies: pascal_parser_dep
)

# pp.pas bootstrap compilation target
run_target('compile-pp',
  command: [kgpc_cparser_exe,
    meson.project_source_root() / 'FPCSource/compiler/pp.pas',
    '--no-stdlib',
    '-DCPU64', '-DCPUX86_64', '-Dx86_64', '-DFPC', '-DLINUX', '-DUNIX',
    '-DFPC_HAS_TYPE_EXTENDED', '-DSUPPORT_EXTENDED', '-Sg',
    '-I' + meson.project_source_root() / 'FPCSource/rtl/objpas',
    '-I' + meson.project_source_root() / 'FPCSource/rtl/objpas/sysutils',
    '-I' + meson.project_source_root() / 'FPCSource/rtl/objpas/classes',
    '-I' + meson.project_source_root() / 'FPCSource/rtl/linux',
    '-I' + meson.project_source_root() / 'FPCSource/rtl/unix',
    '-I' + meson.project_source_root() / 'FPCSource/rtl/inc',
    '-I' + meson.project_source_root() / 'FPCSource/rtl/x86_64',
    '-I' + meson.project_source_root() / 'FPCSource/rtl/linux/x86_64',
    '-I' + meson.project_source_root() / 'FPCSource/rtl/unix/x86_64',
    '-I' + meson.project_source_root() / 'FPCSource/compiler',
    '-I' + meson.project_source_root() / 'FPCSource/compiler/x86',
    '-I' + meson.project_source_root() / 'FPCSource/compiler/x86_64',
    '-Fu' + meson.project_source_root() / 'FPCSource/rtl/unix',
    '-Fu' + meson.project_source_root() / 'FPCSource/rtl/linux',
    '-Fu' + meson.project_source_root() / 'FPCSource/rtl/objpas',
    '-Fu' + meson.project_source_root() / 'FPCSource/rtl/inc',
    '-Fu' + meson.project_source_root() / 'FPCSource/rtl/objpas/sysutils',
    '-Fu' + meson.project_source_root() / 'FPCSource/rtl/objpas/classes',
    '-Fu' + meson.project_source_root() / 'FPCSource/compiler',
    '-Fu' + meson.project_source_root() / 'FPCSource/compiler/x86',
    '-Fu' + meson.project_source_root() / 'FPCSource/compiler/x86_64',
    '-Fu' + meson.project_source_root() / 'FPCSource/compiler/systems',
  ],
)
