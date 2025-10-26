
compiler = meson.get_compiler('c')

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

gpc_cparser_sources = [
  'main_cparser.c',
  'flags.c',
  'Parser/ParseTree/from_cparser.c',
  'Parser/List/List.c',
  'Parser/ParseTree/tree.c',
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
  'stacktrace.c',
]

gpc_cparser_exe = executable('gpc', gpc_cparser_sources,
  include_directories: inc_dirs,
  dependencies: [
    cparser_dep,
    compiler.find_library('m', required: false),
  ]
)

python = find_program('python3')
test('Compiler tests', python,
  args: [meson.project_source_root() / 'tests/test_runner.py'],
  workdir: meson.project_source_root(),
  depends: gpc_cparser_exe,
  env: [
    'RUN_BUGGY_TEST=' + get_option('run_buggy_test').to_string(),
    'MESON_BUILD_ROOT=' + meson.build_root()
    ]
)
