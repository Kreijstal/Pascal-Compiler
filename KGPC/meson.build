
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
  'Optimizer',
  '../common'
)

kgpc_deps = [
  cparser_dep,
  pascal_parser_dep,
  compiler.find_library('m', required: false),
]

kgpc_c_args = []

# Enable graph coloring register allocator based on user option
if get_option('use_graph_coloring_allocator')
  kgpc_c_args += ['-DUSE_GRAPH_COLORING_ALLOCATOR=1']
else
  kgpc_c_args += ['-DUSE_GRAPH_COLORING_ALLOCATOR=0']
endif

if libunwind_dep.found()
  kgpc_deps += libunwind_dep
  if libunwind_platform.found()
    kgpc_deps += libunwind_platform
  endif
  kgpc_c_args += ['-DHAVE_LIBUNWIND=1']
else
  kgpc_c_args += ['-DHAVE_LIBUNWIND=0']
endif

kgpc_cparser_sources = [
  'main_cparser.c',
  'flags.c',
  'Parser/ParseTree/from_cparser.c',
  'Parser/List/List.c',
  'Parser/ParseTree/tree.c',
  'Parser/ParseTree/KgpcType.c',
  'Parser/ParseTree/operator_registry.c',
  'Parser/ParseTree/generic_types.c',
  'Parser/parser_error.c',
  'Parser/pascal_frontend.c',
  'Parser/SemanticCheck/SemCheck.c',
  'Parser/SemanticCheck/HashTable/HashTable.c',
  'Parser/SemanticCheck/SymTab/SymTab.c',
  'Parser/SemanticCheck/NameMangling.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_stmt.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_expr.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_overload.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_sizeof.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_Expr_Builtins.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_Expr_Utils.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_Expr_Resolve.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_Expr_Record.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_Expr_Constructors.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_Context.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_Helpers.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_Expr_Ops.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_Expr_Access.c',
  'Parser/SemanticCheck/SemChecks/SemCheck_Expr_Types.c',
  'CodeGenerator/Intel_x86-64/codegen.c',
  'CodeGenerator/Intel_x86-64/codegen_statement.c',
  'CodeGenerator/Intel_x86-64/codegen_expression.c',
  'CodeGenerator/Intel_x86-64/codegen_builtins.c',
  'CodeGenerator/Intel_x86-64/stackmng/stackmng.c',
  'CodeGenerator/Intel_x86-64/graph_coloring_allocator.c',
  'CodeGenerator/Intel_x86-64/expr_tree/expr_tree.c',
  'Optimizer/optimizer.c',
  'Optimizer/pass_manager.c',
  'Optimizer/mark_used.c',
  'stacktrace.c',
  'unit_paths.c',
]

kgpc_cparser_exe = executable('kgpc', kgpc_cparser_sources,
  include_directories: inc_dirs,
  dependencies: kgpc_deps,
  c_args: kgpc_c_args,
)

gmp_dep = dependency('gmp', required: get_option('with_gmp'))

runtime_sources = ['runtime.c']
if not host_machine.system().startswith('windows')
  runtime_sources += 'runtime_unix.c'
endif
runtime_deps = []
runtime_c_args = ['-O2']

if gmp_dep.found()
  runtime_sources += ['runtime_gmp.c']
  runtime_deps += [gmp_dep]
  runtime_c_args += ['-DHAVE_GMP=1']
else
  runtime_c_args += ['-DHAVE_GMP=0']
endif

if host_machine.system() != 'windows'
  runtime_deps += [compiler.find_library('dl', required: true)]
endif

kgpc_runtime_lib = static_library('kgpc_runtime',
  runtime_sources,
  c_args: runtime_c_args,
  dependencies: runtime_deps,
  install: false,
)

ctypes_helper = shared_library('ctypes_helper',
  meson.project_source_root() / 'tests/test_cases/ctypes_helper.c',
  install: false,
)

valgrind_prog = find_program('valgrind', required: false)
run_valgrind_option = get_option('run_valgrind_tests')
valgrind_platform_ok = host_machine.system() == 'linux'
run_valgrind_tests = run_valgrind_option and valgrind_platform_ok and valgrind_prog.found()

python = find_program('python3')
env = environment()
env.set('RUN_BUGGY_TEST', get_option('run_buggy_test').to_string())
env.set('MESON_BUILD_ROOT', meson.project_build_root())
env.set('RUN_VALGRIND_TESTS', run_valgrind_tests ? 'true' : 'false')

env.set('CC', ' '.join(compiler.cmd_array()))
env.set('KGPC_RUNTIME_LIB', kgpc_runtime_lib.full_path())
env.set('KGPC_CTYPES_HELPER', ctypes_helper.full_path())
env.set('KGPC_HAVE_GMP', gmp_dep.found() ? '1' : '0')

if run_valgrind_tests
  env.set('VALGRIND_BIN', valgrind_prog.full_path())
endif

ctypes_helper_link = ctypes_helper.full_path()
if host_machine.system() == 'windows'
  env.append('PATH', meson.current_build_dir(), separator: ';')
elif host_machine.system() == 'darwin'
  env.append('DYLD_LIBRARY_PATH', meson.current_build_dir())
else
  env.append('LD_LIBRARY_PATH', meson.current_build_dir())
endif
env.set('KGPC_CTYPES_HELPER_LINK', ctypes_helper_link)

test('Compiler tests', python,
  args: [
    meson.project_source_root() / 'tests/do_not_run_me_directly_but_through_meson.py',
    '--tap',
  ],
  workdir: meson.project_source_root(),
  depends: [kgpc_cparser_exe, kgpc_runtime_lib, ctypes_helper],
  env: env,
  protocol: 'tap',
  timeout: 1800,
)
