
compiler = meson.get_compiler('c')
flex = find_program('flex')
bison = find_program('bison')

bison_gen = generator(bison,
  output: ['@BASENAME@.tab.c', '@BASENAME@.tab.h'],
  arguments: ['-d', '-o', '@OUTPUT0@', '--defines=@OUTPUT1@', '@INPUT@'])

flex_gen = generator(flex,
  output: '@BASENAME@.c',
  arguments: ['-o', '@OUTPUT@', '@INPUT@'])

bison_generated = bison_gen.process('Parser/LexAndYacc/Grammar.y')
flex_generated = flex_gen.process('Parser/LexAndYacc/lex.yy.l')

gpc_sources = [
  'main.c',
  'flags.c',
  'Parser/ParsePascal.c',
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
  'stacktrace.c'
]

gpc_sources += bison_generated
gpc_sources += flex_generated

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

gpc_exe = executable('gpc', gpc_sources,
  include_directories: inc_dirs,
  dependencies: [
    compiler.find_library('m', required: true),
    compiler.find_library('fl', required: true),
    compiler.find_library('unwind', required: true),
    compiler.find_library('unwind-x86_64', required: true),
    compiler.find_library('dl', required: true),
    compiler.find_library('pthread', required: true),
  ]
)

python = find_program('python3')
test('Compiler tests', python,
  args: [meson.project_source_root() / 'tests/test_runner.py'],
  workdir: meson.project_source_root(),
  depends: gpc_exe,
  env: {
    'RUN_BUGGY_TEST': get_option('run_buggy_test').to_string()
  }
)
