
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

# Original GPC sources (legacy flex/bison version) - disabled for now
# gpc_sources = [
#   'main.c',
#   'flags.c',
#   'Parser/ParsePascal.c',
#   'Parser/List/List.c',
#   'Parser/ParseTree/tree.c',
#   'Parser/SemanticCheck/SemCheck.c',
#   'Parser/SemanticCheck/HashTable/HashTable.c',
#   'Parser/SemanticCheck/SymTab/SymTab.c',
#   'Parser/SemanticCheck/NameMangling.c',
#   'Parser/SemanticCheck/SemChecks/SemCheck_stmt.c',
#   'Parser/SemanticCheck/SemChecks/SemCheck_expr.c',
#   'CodeGenerator/Intel_x86-64/codegen.c',
#   'CodeGenerator/Intel_x86-64/codegen_statement.c',
#   'CodeGenerator/Intel_x86-64/codegen_expression.c',
#   'CodeGenerator/Intel_x86-64/codegen_builtins.c',
#   'CodeGenerator/Intel_x86-64/stackmng/stackmng.c',
#   'CodeGenerator/Intel_x86-64/expr_tree/expr_tree.c',
#   'Optimizer/optimizer.c',
#   'stacktrace.c'
# ]
# 
# gpc_sources += bison_generated
# gpc_sources += flex_generated
# 
# inc_dirs = include_directories(
#   '.',
#   'Parser',
#   'Parser/LexAndYacc',
#   'Parser/ParseTree',
#   'Parser/List',
#   'Parser/SemanticCheck/HashTable',
#   'Parser/SemanticCheck/SymTab',
#   'Parser/SemanticCheck',
#   'CodeGenerator/Intel_x86-64',
#   'Optimizer'
# )
# 
# # Build legacy GPC executable (for reference/comparison)
# gpc_exe = executable('gpc-legacy', gpc_sources,
#   include_directories: inc_dirs,
#   dependencies: [
#     compiler.find_library('m', required: false),
#     compiler.find_library('fl', required: false),
#     compiler.find_library('unwind', required: false),
#     compiler.find_library('unwind-x86_64', required: false),
#     compiler.find_library('dl', required: false),
#     compiler.find_library('pthread', required: false),
#   ]
# )

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

# New GPC executable using cparser
gpc_cparser_sources = [
  'main_cparser.c',
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
