
compiler = meson.get_compiler('c')
libunwind_dep = dependency('libunwind', required: false)
libunwind_platform = compiler.find_library('unwind-x86_64', required: false)

inc_dirs = include_directories(
  '.',
  'Parser',
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
  'Parser/pascal_frontend.c',
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
  'unit_paths.c',
]

gpc_cparser_exe = executable('gpc', gpc_cparser_sources,
  include_directories: inc_dirs,
  dependencies: gpc_deps,
  c_args: gpc_c_args,
)

gpc_runtime_lib = static_library('gpc_runtime',
  ['runtime.c', 'runtime_gmp.c'],
  install: false,
)

ctypes_helper = shared_library('ctypes_helper',
  meson.project_source_root() / 'tests/test_cases/ctypes_helper.c',
  install: false,
)

python = find_program('python3')
env = environment()
env.set('RUN_BUGGY_TEST', get_option('run_buggy_test').to_string())
env.set('MESON_BUILD_ROOT', meson.project_build_root())
env.set('RUN_VALGRIND_TESTS', get_option('run_valgrind_tests').to_string())
env.set('CC', ' '.join(compiler.cmd_array()))
env.set('GPC_RUNTIME_LIB', gpc_runtime_lib.full_path())
env.set('GPC_CTYPES_HELPER', ctypes_helper.full_path())

ctypes_helper_link = ctypes_helper.full_path()
if host_machine.system() == 'windows'
  env.append('PATH', meson.current_build_dir(), separator: ';')
elif host_machine.system() == 'darwin'
  env.append('DYLD_LIBRARY_PATH', meson.current_build_dir())
else
  env.append('LD_LIBRARY_PATH', meson.current_build_dir())
endif
env.set('GPC_CTYPES_HELPER_LINK', ctypes_helper_link)

test('Compiler tests', python,
  args: [
    meson.project_source_root() / 'tests/test_runner.py',
    '--tap',
  ],
  workdir: meson.project_source_root(),
  depends: [gpc_cparser_exe, gpc_runtime_lib, ctypes_helper],
  env: env,
  protocol: 'tap',
  timeout: 300,
)
