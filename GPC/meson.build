
compiler = meson.get_compiler('c')
libunwind_dep = dependency('libunwind', required: false)
libunwind_platform = compiler.find_library('unwind-x86_64', required: false)

inc_dirs = include_directories(
  '.',
  'Parser',
  'Parser/LexAndYacc',
  'Parser/ParseTree',
  'Parser/List',
  'Parser/SemanticCheck/HashTable',
  'Parser/SemanticCheck/SymTab',
  'Parser/SemanticCheck',
  'CodeGenerator/Intel_x86-64',
  'Optimizer'
)

gpc_deps = [
  cparser_dep,
  pascal_parser_dep,
  compiler.find_library('m', required: false),
]

gpc_c_args = []

if libunwind_dep.found()
  gpc_deps += libunwind_dep
  if libunwind_platform.found()
    gpc_deps += libunwind_platform
  endif
  gpc_c_args += ['-DHAVE_LIBUNWIND=1']
else
  gpc_c_args += ['-DHAVE_LIBUNWIND=0']
endif

gpc_cparser_sources = [
  'main_cparser.c',
  'flags.c',
  'Parser/ParseTree/from_cparser.c',
  'Parser/List/List.c',
  'Parser/ParseTree/tree.c',
  'Parser/parser_error.c',
  'Parser/SemanticCheck/SemCheck.c',
  'Parser/SemanticCheck/HashTable/HashTable.c',
  'Parser/SemanticCheck/SymTab/SymTab.c',
  'Parser/SemanticCheck/NameMangling.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_stmt.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_expr.c',
  'CodeGenerator/Intel_x86-64/codegen.c',
  'CodeGenerator/Intel_x86-64/codegen_statement.c',
  'CodeGenerator/Intel_x86-64/codegen_expression.c',
  'CodeGenerator/Intel_x86-64/codegen_builtins.c',
  'CodeGenerator/Intel_x86-64/stackmng/stackmng.c',
  'CodeGenerator/Intel_x86-64/expr_tree/expr_tree.c',
  'Optimizer/optimizer.c',
  'Optimizer/pass_manager.c',
  'stacktrace.c',
]

gpc_cparser_exe = executable('gpc', gpc_cparser_sources,
  include_directories: inc_dirs,
  dependencies: gpc_deps,
  c_args: gpc_c_args,
)

python = find_program('python3')
test('Compiler tests', python,
  args: [meson.project_source_root() / 'tests/test_runner.py'],
  workdir: meson.project_source_root(),
  depends: gpc_cparser_exe,
  env: [
    'RUN_BUGGY_TEST=' + get_option('run_buggy_test').to_string(),
    'MESON_BUILD_ROOT=' + meson.build_root()
    ],
  timeout: 120,
)
