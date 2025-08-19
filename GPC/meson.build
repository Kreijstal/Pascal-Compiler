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
  'CodeGenerator/new_codegen/flat_ir_generator.c',
  'CodeGenerator/new_codegen/x86_64_codegen.c',
  'CodeGenerator/new_codegen/register_allocator.c',
  'runtime.c', # Add runtime for linking
  # Semantic checker sources
  'Parser/SemanticCheck/flat_sem_check.c',
  'Parser/SemanticCheck/SymTab/SymTab.c',
  'Parser/SemanticCheck/HashTable/HashTable.c',
  'Parser/SemanticCheck/mangle.c'
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

semantic_checker_sources = [
  'semantic_checker_main.c',
  'Parser/ParsePascal.c',
  'Parser/List/List.c',
  'Parser/ParseTree/tree.c',
  'Parser/SemanticCheck/flat_sem_check.c',
  'Parser/SemanticCheck/SymTab/SymTab.c',
  'Parser/SemanticCheck/HashTable/HashTable.c',
  'Parser/SemanticCheck/mangle.c'
]
semantic_checker_sources += bison_generated
semantic_checker_sources += flex_generated

semantic_checker_exe = executable('semantic_checker', semantic_checker_sources,
  include_directories: inc_dirs,
  dependencies: [
    compiler.find_library('m', required: true),
    compiler.find_library('fl', required: true)
  ]
)

py_prog = find_program('python3')
test_discover_script = find_program('../tests/discover_tests.py')

# --- Test Suites ---

# Parser tests
parser_test_cases = run_command(py_prog, test_discover_script, 'TestParser', 'tests.test_runner').stdout().strip().split('\n')
foreach test_func_name : parser_test_cases
  if test_func_name != ''
    test_name = test_func_name.split('test_parser_')[-1]
    test('Parser - ' + test_name,
      py_prog,
      args: ['-m', 'unittest', 'tests.test_runner.TestParser.' + test_func_name],
      workdir: meson.project_source_root(),
      depends: flat_ast_printer_exe,
      env: ['MESON_BUILD_ROOT=' + meson.build_root()]
    )
  endif
endforeach

# Semantic tests
semantic_test_cases = run_command(py_prog, test_discover_script, 'TestSemantic', 'tests.test_runner').stdout().strip().split('\n')
foreach test_func_name : semantic_test_cases
  if test_func_name != ''
    test_name = test_func_name.split('test_semantic_')[-1]
    test('Semantic - ' + test_name,
      py_prog,
      args: ['-m', 'unittest', 'tests.test_runner.TestSemantic.' + test_func_name],
      workdir: meson.project_source_root(),
      depends: semantic_checker_exe,
      env: ['MESON_BUILD_ROOT=' + meson.build_root()]
    )
  endif
endforeach

# Codegen tests
codegen_test_cases = run_command(py_prog, test_discover_script, 'TestCompiler', 'tests.test_runner').stdout().strip().split('\n')
foreach test_func_name : codegen_test_cases
  if test_func_name != ''
    test_name = test_func_name.split('test_')[-1]
    test('Codegen - ' + test_name,
      py_prog,
      args: ['-m', 'unittest', 'tests.test_runner.TestCompiler.' + test_func_name],
      workdir: meson.project_source_root(),
      depends: gpc_exe,
      env: ['MESON_BUILD_ROOT=' + meson.build_root()]
    )
  endif
endforeach
