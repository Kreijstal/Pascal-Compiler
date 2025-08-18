
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
  'debug_flat_ast_printer.c',
]

gpc_sources += bison_generated
gpc_sources += flex_generated

if get_option('new_codegen')
  inc_dirs = include_directories(
    '.',
    'Parser',
    'Parser/LexAndYacc',
    'Parser/ParseTree',
    'Parser/List',
    'Parser/SemanticCheck/HashTable',
    'Parser/SemanticCheck/SymTab',
    'Parser/SemanticCheck',
    'Optimizer',
    'CodeGenerator/new_codegen'
  )
  gpc_exe = executable('gpc', gpc_sources,
    c_args: '-DNEW_CODEGEN',
    include_directories: inc_dirs,
    dependencies: [
      compiler.find_library('m', required: true),
      compiler.find_library('fl', required: true)
    ]
  )
else
  gpc_sources += [
    'CodeGenerator/Intel_x86-64/codegen.c',
    'CodeGenerator/Intel_x86-64/codegen_statement.c',
    'CodeGenerator/Intel_x86-64/codegen_expression.c',
    'CodeGenerator/Intel_x86-64/codegen_builtins.c',
    'CodeGenerator/Intel_x86-64/stackmng/stackmng.c',
    'CodeGenerator/Intel_x86-64/expr_tree/expr_tree.c'
  ]
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
endif

flat_ast_printer_sources = [
  'flat_ast_printer_main.c',
  'Parser/ParsePascal.c',
  'Parser/List/List.c',
  'Parser/ParseTree/tree.c',
  'debug_flat_ast_printer.c',
]
flat_ast_printer_sources += bison_generated
flat_ast_printer_sources += flex_generated

flat_ast_printer_exe = executable('flat_ast_printer', flat_ast_printer_sources,
  include_directories: inc_dirs,
  dependencies: [
    compiler.find_library('m', required: true),
    compiler.find_library('fl', required: true)
  ]
)

python = find_program('python3')

# Parser tests
parser_test_dir = meson.project_source_root() / 'tests/parser'
foreach p_file : run_command('find', meson.project_source_root() / 'tests/cases', '-name', '*.p').stdout().strip().split('\n')
  test_name = p_file.split('/')[-1].split('.')[0]
  expected_ast_file = parser_test_dir / test_name + '.expected_ast'
  if run_command('test', '-f', expected_ast_file).returncode() == 0
    test('Parser - ' + test_name, python,
      args: [meson.project_source_root() / 'tests/test_runner.py', 'TestParser.test_parser_' + test_name],
      workdir: meson.project_source_root(),
      depends: flat_ast_printer_exe,
      env: ['MESON_BUILD_ROOT=' + meson.build_root()]
    )
  endif
endforeach

# TODO: Add codegen tests back
# test('Compiler tests', python,
#   args: [meson.project_source_root() / 'tests/test_runner.py'],
#   workdir: meson.project_source_root(),
#   depends: gpc_exe,
#   env: [
#     'RUN_BUGGY_TEST=' + get_option('run_buggy_test').to_string(),
#     'MESON_BUILD_ROOT=' + meson.build_root()
#     ]
# )
