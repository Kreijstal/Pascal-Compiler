# Auto-discovery functions for KGPC test harness.
# Dynamically adds test methods to TestCompiler based on .p/.expected files.
import hashlib
import os
import shutil
import subprocess
import sys
import traceback

from .env import (
    EXE_EXT,
    EXEC_TIMEOUT,
    PP_BOOTSTRAP_FULL_CHAIN_TIMEOUT,
    PP_BOOTSTRAP_COMPILE_TIMEOUT,
    FPC_RTL_MODE,
    FPC_RTL_DIR,
    IS_WINDOWS_ABI,
    IS_WINE,
    PLATFORM_ID,
    TEST_CASES_DIR,
    INPUT_DATA_DIR,
    TEST_OUTPUT_DIR,
)
from .cache import (
    FPC_RTL_FLAGS,
    test_cache_check as _test_cache_check,
    test_cache_store as _test_cache_store,
    _FPC_RTL_AST_CACHE_DIR,
    _FPC_RTL_CODEGEN_CACHE_DIR,
    _RUNTIME_LIB_PATH,
)
from .runner import (
    run_compiler,
    LINK_ARGS_BY_ASM,
)
from .sanitizers import (
    run_executable_with_valgrind,
)
from .artifacts import (
    read_file_content,
    _signal_name_suffix,
)
from .discovery import (
    kgpc_bootstrap_flags as _kgpc_bootstrap_flags_impl,
    pp_bootstrap_compiler_flags as _pp_bootstrap_compiler_flags,
    pp_bootstrap_program_flags as _pp_bootstrap_program_flags,
    should_include_in_fpcrtl as _should_include_in_fpcrtl_impl,
    _tree_contains_newer_file as _tree_contains_newer_file_impl,
)
from .env import FPC_RTL_GENERATED_UNITS_DIRNAME
from .test_compiler import TestCompiler

# ---------------------------------------------------------------------------
# Local wrappers that inject cache dirs into discovery helpers
# ---------------------------------------------------------------------------


def _kgpc_bootstrap_flags(fpc_src, *, include_compiler_dirs):
    """Wrapper that injects cache dirs from cache module."""
    return _kgpc_bootstrap_flags_impl(
        fpc_src,
        include_compiler_dirs=include_compiler_dirs,
        fpc_rtl_ast_cache_dir=_FPC_RTL_AST_CACHE_DIR,
        fpc_rtl_codegen_cache_dir=_FPC_RTL_CODEGEN_CACHE_DIR,
    )


def _tree_contains_newer_file(root_dir, reference_file):
    """Wrapper preserving original 2-arg signature."""
    return _tree_contains_newer_file_impl(
        root_dir, reference_file, FPC_RTL_GENERATED_UNITS_DIRNAME
    )


def _should_include_in_fpcrtl(base_name, pascal_file):
    """Wrapper that injects FPC_RTL_IMPLICIT_UNIT_TESTS (defined below)."""
    # FPC_RTL_IMPLICIT_UNIT_TESTS is defined later in this module.
    return _should_include_in_fpcrtl_impl(
        base_name, pascal_file, FPC_RTL_IMPLICIT_UNIT_TESTS
    )


# ---------------------------------------------------------------------------
# Test classification sets
# ---------------------------------------------------------------------------

def _discover_and_add_auto_tests():
    """
    Auto-discover test cases based on .p files with corresponding .expected files.
    Dynamically adds test methods to TestCompiler class.
    """
    if not os.path.isdir(TEST_CASES_DIR):
        return
    
    # Find all .expected files
    expected_files = []
    for filename in os.listdir(TEST_CASES_DIR):
        if filename.endswith('.expected'):
            base_name = filename[:-9]  # Remove '.expected'
            pascal_file = os.path.join(TEST_CASES_DIR, base_name + '.p')
            if os.path.isfile(pascal_file):
                expected_files.append(base_name)
    
    # Sort to ensure consistent test ordering
    expected_files.sort()
    
    # For each discovered test, create a test method
    for base_name in expected_files:
        # Create a safe method name (replace hyphens and other chars with underscores)
        method_name = 'test_auto_' + base_name.replace('-', '_').replace(' ', '_')
        
        # Skip if this test is already manually defined
        if hasattr(TestCompiler, method_name):
            continue
        
        # Create the test method using a closure to capture base_name
        def make_test_method(test_base_name):
            def test_method(self):
                """Auto-discovered test case."""
                if test_base_name in FPC_RTL_ONLY_TESTS:
                    self.skipTest("FPC RTL-only regression test")

                # Sets below classify tests by what platform feature they
                # need.  Cygwin/MSYS provide a POSIX-emulation runtime; the
                # pure-Windows ABIs (MinGW / UCRT / clang-mingw, either
                # native or via Wine) do not.
                if test_base_name in SYSV_ABI_ONLY_TESTS and IS_WINDOWS_ABI:
                    self.skipTest("Test uses hardcoded SysV ABI registers / calling convention")
                if test_base_name in POSIX_ONLY_TESTS and IS_WINDOWS_ABI \
                        and not PLATFORM_ID.startswith(("cygwin", "msys")):
                    self.skipTest("Test requires POSIX runtime features unavailable on this Windows ABI")
                
                input_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.p")
                asm_file = os.path.join(TEST_OUTPUT_DIR, f"{test_base_name}_auto.s")
                executable_file = os.path.join(TEST_OUTPUT_DIR, f"{test_base_name}_auto{EXE_EXT}")
                expected_output_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.expected")
                expected_stderr_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.stderr.expected")
                input_data_file = os.path.join(INPUT_DATA_DIR, f"{test_base_name}.input")

                # Check test result cache
                cache_deps = [expected_stderr_file] if os.path.exists(expected_stderr_file) else []
                if _test_cache_check(input_file, expected_output_file, cache_deps):
                    return  # cached pass — skip

                compiler_output = None
                actual_output = None
                raw_stdout = None
                raw_stderr = None
                expected_output = None
                expected_stderr = None
                process_returncode = None

                self.record_failure_context(
                    base_name=test_base_name,
                    input_file=input_file,
                    asm_file=asm_file,
                    executable_file=executable_file,
                    expected_file=expected_output_file,
                )

                try:
                    compiler_output = run_compiler(input_file, asm_file)

                    self.compile_executable(asm_file, executable_file)

                    stdin_input = None
                    if os.path.exists(input_data_file):
                        stdin_input = read_file_content(input_data_file)

                    if stdin_input is not None:
                        process = run_executable_with_valgrind(
                            [executable_file],
                            capture_output=True,
                            text=True,
                            timeout=EXEC_TIMEOUT,
                            input=stdin_input,
                        )
                    else:
                        process = run_executable_with_valgrind(
                            [executable_file],
                            capture_output=True,
                            text=True,
                            timeout=EXEC_TIMEOUT,
                        )
                    raw_stdout = process.stdout
                    raw_stderr = process.stderr
                    process_returncode = process.returncode

                    expected_output = read_file_content(expected_output_file)
                    text = raw_stdout
                    if text is None:
                        text = ""
                    while "\r\r\n" in text:
                        text = text.replace("\r\r\n", "\r\n")
                    actual_output = text.replace("\r\n", "\n").replace("\r", "")

                    actual_stderr = None
                    if os.path.exists(expected_stderr_file):
                        expected_stderr = read_file_content(expected_stderr_file)
                        stderr_text = raw_stderr
                        if stderr_text is None:
                            stderr_text = ""
                        while "\r\r\n" in stderr_text:
                            stderr_text = stderr_text.replace("\r\r\n", "\r\n")
                        actual_stderr = stderr_text.replace("\r\n", "\n").replace("\r", "")

                    self.record_failure_context(
                        base_name=test_base_name,
                        compiler_output=compiler_output,
                        normalized_output=actual_output,
                        raw_stdout=raw_stdout,
                        raw_stderr=raw_stderr,
                        expected_output=expected_output,
                        expected_stderr=expected_stderr,
                        returncode=process_returncode,
                    )

                    self.assertEqual(actual_output, expected_output)
                    if expected_stderr is not None:
                        self.assertEqual(actual_stderr, expected_stderr)
                    self.assertEqual(process_returncode, 0)
                    _test_cache_store(input_file, expected_output_file, cache_deps)
                except subprocess.TimeoutExpired:
                    self.record_failure_context(
                        base_name=test_base_name,
                        compiler_output=compiler_output,
                        normalized_output=actual_output,
                        raw_stdout=raw_stdout,
                        raw_stderr=raw_stderr,
                        expected_output=expected_output,
                        expected_stderr=expected_stderr,
                        returncode=process_returncode,
                        exception_text=traceback.format_exc(),
                    )
                    self.fail(f"Test {test_base_name} execution timed out.")
                except Exception:
                    self.record_failure_context(
                        base_name=test_base_name,
                        compiler_output=compiler_output,
                        normalized_output=actual_output,
                        raw_stdout=raw_stdout,
                        raw_stderr=raw_stderr,
                        expected_output=expected_output,
                        expected_stderr=expected_stderr,
                        returncode=process_returncode,
                    )
                    raise
            
            test_method.__name__ = method_name
            test_method.__doc__ = f"Auto-discovered test case for {test_base_name}.p"
            return test_method
        
        # Add the test method to the TestCompiler class
        setattr(TestCompiler, method_name, make_test_method(base_name))



# Tests that use KGPC-only extensions not available in FPC RTL mode.
KGPC_ONLY_TESTS = {
    'random_real_function',  # Random(Real) overload is a KGPC extension
}

# Tests that depend on POSIX runtime features (fork, sigaction, signals, ...)
# which exist on Cygwin/MSYS but not on native MinGW / UCRT / clang-mingw.
# Skipped only on those latter ABIs.
POSIX_ONLY_TESTS = {
    "tdd_baseunix_fpsigaction",   # fpsigaction → POSIX sigaction(2)
    "unix_wait_helpers_demo",     # fpFork / waitpid
    # The tests below `uses BaseUnix`/`uses Unix`.  In KGPC's own stdlib those
    # resolve to the portable shim units (KGPC/Units/baseunix.p, unix.p backed
    # by runtime_baseunix.c / runtime_unix.c), so they run on any ABI.  Under
    # --no-stdlib + the FPC RTL, `BaseUnix`/`Unix` are FPC's real rtl/unix
    # units, which only exist for POSIX targets (there is no rtl/win64
    # baseunix.pp), so they cannot compile on a native Windows ABI.
    "gap_fpread_devnull",                    # BaseUnix.fpRead
    "gap_fpwrite_devnull",                   # BaseUnix.fpWrite
    "reg_sysutils_fpread",                   # BaseUnix.fpRead
    "siginfo_shadow_runtime",                # BaseUnix/Unix sigaction
    "tdd_baseunix_fpgetcwd_decl",            # BaseUnix.fpgetcwd
    "tdd_fpexecl_rawbytestring_array_literal",  # Unix.fpexecl
}

# Tests that emit hardcoded SysV-ABI inline assembly and cannot run on any
# Windows ABI (which uses the Microsoft x64 calling convention).
SYSV_ABI_ONLY_TESTS = {
    "nostackframe_asm_regsizing",
}

# Tests that target overloads or units only present in the FPC RTL suite.
FPC_RTL_ONLY_TESTS = {
    "fpc_bootstrap_hminus_extractfilepath",
}

# Tests without explicit FPC RTL/package imports are skipped from the FPC RTL
# suite by default. Keep a small explicit allowlist for tests that intentionally
# exercise implicit System/ObjPas/FPC bootstrap behavior.
FPC_RTL_IMPLICIT_UNIT_TESTS = {
    "bitsizeof_const_expr",
    # Pin the Windows std-I/O fix: with --no-stdlib KGPC must set the RTL's
    # IsConsole=true at startup (sysinit.pp is never linked) so SysInitStdIO
    # binds the standard handles via OpenStdIO. A regression makes WriteLn(StdErr,
    # ...) leak onto stdout / a Rewrite'd text file's WriteLn leak onto stdout.
    # These programs use no explicit unit, so list them here to run under the RTL.
    "tdd_win_stderr_no_leak",
    "tdd_win_textfile_write_no_leak",
    # Pins the FreeMem-bypasses-RTL fix: with --no-stdlib + FPC RTL, a regression
    # would route FreeMem(p) through libc free instead of MemoryManager.FreeMem,
    # producing a "double free or corruption" SIGABRT. Two-arg FreeMem(p, size)
    # form must also stay routed through the user-visible Pascal FreeMem.
    "dos_freemem",
    "fpc_bootstrap_absolute_record_field",
    "fpc_bootstrap_andor_complex",
    "fpc_bootstrap_ansichar_type",
    "fpc_bootstrap_array_of_rawbytestring",
    "fpc_bootstrap_assign_textrec_cast",
    "fpc_bootstrap_class_of",
    "fpc_bootstrap_class_property",
    "fpc_bootstrap_class_property_getsystemencoding",
    "fpc_bootstrap_codepage_aliases",
    "fpc_bootstrap_codepage_search",
    "fpc_bootstrap_const_array_expr_bounds",
    "fpc_bootstrap_const_default_param",
    "fpc_bootstrap_constref_param",
    "fpc_bootstrap_constructor_overload_default",
    "fpc_bootstrap_constructor_overload_metaclass",
    "fpc_bootstrap_currency_type",
    "fpc_bootstrap_default_record_reset",
    "fpc_bootstrap_deprecated_const",
    "fpc_bootstrap_external_var",
    "fpc_bootstrap_fina_shortstring_move",
    "fpc_bootstrap_free_method",
    "fpc_bootstrap_heapinc_commonheader_sizeof_repro",
    "fpc_bootstrap_high_low_open_array",
    "fpc_bootstrap_if_declared",
    "fpc_bootstrap_if_fullversion",
    "fpc_bootstrap_include_shortstring_char_assign",
    "fpc_bootstrap_interface_alias",
    "fpc_bootstrap_interlockedexchangeadd_pointer",
    "fpc_bootstrap_longbool",
    "fpc_bootstrap_module_property_write",
    "fpc_bootstrap_nested_routines_begin",
    "fpc_bootstrap_nested_type_method_impl",
    "fpc_bootstrap_nested_type_typecast",
    "fpc_bootstrap_new_multidim_char_record_array",
    "fpc_bootstrap_new_multidim_array_pointer",
    "fpc_bootstrap_objpas_tendian_unit",
    "fpc_bootstrap_operator_overload",
    "fpc_bootstrap_overload_unicode_rawbyte",
    "fpc_bootstrap_pointer_const_typecast",
    "fpc_bootstrap_pointer_overload_resolution",
    "fpc_bootstrap_pointer_typecast_complex",
    "fpc_bootstrap_packed_record_pointer_for",
    "fpc_bootstrap_proc_const_typed",
    "fpc_bootstrap_procedure_var_assign",
    "fpc_bootstrap_proctype_cast_call",
    "fpc_bootstrap_ptext",
    "fpc_bootstrap_ptruint",
    "fpc_bootstrap_public_var",
    "fpc_bootstrap_qualified_case_label",
    "fpc_bootstrap_rawbytestring",
    "fpc_bootstrap_record_classvars",
    "fpc_bootstrap_resourcestring_concat",
    "fpc_bootstrap_resourcestring_output",
    "fpc_bootstrap_result_out_param",
    "fpc_bootstrap_scoped_enum_comparison",
    "fpc_bootstrap_scoped_enum_default_param",
    "fpc_bootstrap_shortstring_truncation",
    "fpc_bootstrap_shortstring_array_copy_compare",
    "fpc_bootstrap_sizeof_array_expr_bound",
    "fpc_bootstrap_sized_string_array_concat",
    "fpc_bootstrap_sized_string_array_const",
    "fpc_bootstrap_stderr",
    "fpc_bootstrap_string_n_length",
    "fpc_bootstrap_syshelps_pos_lowercase",
    "fpc_bootstrap_syssbh_pos_alias",
    "fpc_bootstrap_system_qualified_const",
    "fpc_bootstrap_system_qualified_proccall",
    "fpc_bootstrap_tclass",
    "fpc_bootstrap_trtlcriticalsection",
    "fpc_bootstrap_typedfile",
    "fpc_bootstrap_typehelpers_castinfo",
    "fpc_bootstrap_unit_init_unit",
    "fpc_const_subrange_array",
    "fpc_const_typecast",
    "fpc_exit_value",
    "fpc_exit_value_nested",
    "fpc_flush_text",
    "fpc_has_unicodestring_define",
    "fpc_heap_status",
    "fpc_macro_type_unix",
    "fpc_maxlongint",
    "fpc_octstr",
    "fpc_packed_record",
    "fpc_pansichar_array",
    "fpc_pointer_alias_chain",
    "fpc_pointer_arithmetic",
    "fpc_pointer_indexing",
    "fpc_pointer_subtraction",
    "fpc_ppansichar",
    "fpc_procedural_type_advanced",
    "fpc_procedural_type_basic",
    "fpc_puint16_typecast",
    "fpc_runtime_bitwise_or",
    "fpc_set_constant_char",
    "fpc_settextcodepage",
    "fpc_shl_const",
    "fpc_shortstring_type",
    "fpc_sizeuint",
    "fpc_str_procedure",
    "implicitsystemimport",
    # Regression for 15c0620c: `^TDerived := @ClassInstance.BaseField` must not
    # be routed through op-overload search. Test has no `uses` clause, but we
    # want it exercised under --no-stdlib too to cover both compile paths.
    "ptr_class_assign_with_variants",
    "system_core_basics",
    "system_qualified_length_if",
    "tdd_cp_acp_paramstr_ioresult",
    # GetMem/FreeMem allocator-pairing repro: FreeMem(p) must route through the
    # user-visible Pascal FreeMem (FPC's freemem_p) — same allocator as the
    # GetMem(N) above it — not directly to the runtime kgpc_freemem helper.
    "tdd_repro_pp_freemem_bypasses_rtl",
    "tdd_system_exit_qualified",
    "tdd_system_ttypekind",
    "tdd_types_core_symbols_bootstrap",
    # Tests that pass normally but fail with FPC RTL — added for regression tracking
    "bug_parser_full_repro",
    "const_var_transition",
    "random_function",
    "random_range_function",
    "reg_sysutils_inttohex_binstr",
    "tdd_capsizeint_wsm",
    "tdd_proc_field_dispatch",
    "tdd_propinfo_kind_case",
    "tdd_record_method_no_param_return_type",
    "tdd_stmt_system_error_unit_qualifier",
    "tdd_upcase_ord",
    # STATUS.md Stage-4 regression: AnsiString / RawByteString / UnicodeString /
    # String / WideString cross-pairings for var/out parameters must remain
    # compatible (codepage rule).  Without explicit `uses` clauses, this test
    # is opt-in for the FPC RTL run.
    "regr_varparam_string_family_codepage_compat",
}

def _discover_and_add_fpc_rtl_tests():
    """
    When KGPC_FPC_RTL=1, replace auto-discovered tests with FPC RTL variants.
    Each test compiles with --no-stdlib and FPC RTL include/unit paths.
    Tests that fail to compile are skipped (not failed).
    Tests in KGPC_ONLY_TESTS are skipped entirely.
    """
    if not FPC_RTL_MODE:
        return
    if not os.path.isdir(FPC_RTL_DIR):
        return

    if not os.path.isdir(TEST_CASES_DIR):
        return

    expected_files = []
    for filename in os.listdir(TEST_CASES_DIR):
        if filename.endswith('.expected'):
            base_name = filename[:-9]
            pascal_file = os.path.join(TEST_CASES_DIR, base_name + '.p')
            if os.path.isfile(pascal_file):
                expected_files.append(base_name)

    expected_files.sort()

    for base_name in expected_files:
        if base_name in KGPC_ONLY_TESTS:
            continue
        pascal_file = os.path.join(TEST_CASES_DIR, base_name + '.p')
        if not _should_include_in_fpcrtl(base_name, pascal_file):
            continue

        method_name = 'test_fpcrtl_' + base_name.replace('-', '_').replace(' ', '_')

        if hasattr(TestCompiler, method_name):
            continue

        def make_fpc_rtl_test(test_base_name):
            def test_method(self):
                """FPC RTL test case."""
                # Same platform classification as the non-FPC-RTL path: skip
                # SysV-ABI-only and POSIX-only tests on a native Windows ABI.
                # Under the FPC RTL these are even less runnable than under the
                # KGPC stdlib — `uses BaseUnix`/`uses Unix` map to FPC's real
                # rtl/unix units, which have no Windows variant.
                if test_base_name in SYSV_ABI_ONLY_TESTS and IS_WINDOWS_ABI:
                    self.skipTest("Test uses hardcoded SysV ABI registers / calling convention")
                if test_base_name in POSIX_ONLY_TESTS and IS_WINDOWS_ABI \
                        and not PLATFORM_ID.startswith(("cygwin", "msys")):
                    self.skipTest("Test requires POSIX runtime features unavailable on this Windows ABI")
                input_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.p")
                asm_file = os.path.join(TEST_OUTPUT_DIR, f"{test_base_name}_fpcrtl.s")
                executable_file = os.path.join(TEST_OUTPUT_DIR, f"{test_base_name}_fpcrtl{EXE_EXT}")
                expected_output_file = os.path.join(TEST_CASES_DIR, f"{test_base_name}.expected")
                input_data_file = os.path.join(INPUT_DATA_DIR, f"{test_base_name}.input")

                if _test_cache_check(input_file, expected_output_file, FPC_RTL_FLAGS):
                    return  # cached pass — skip

                try:
                    compiler_output = run_compiler(input_file, asm_file,
                                                   flags=FPC_RTL_FLAGS)
                except subprocess.CalledProcessError:
                    self.fail(f"FPC RTL compilation failed for {test_base_name}")
                    return

                try:
                    self.compile_executable(asm_file, executable_file)
                except Exception:
                    self.fail(f"FPC RTL linking failed for {test_base_name}")
                    return

                stdin_input = None
                if os.path.exists(input_data_file):
                    stdin_input = read_file_content(input_data_file)

                try:
                    if stdin_input is not None:
                        process = run_executable_with_valgrind(
                            [executable_file],
                            capture_output=True, text=True,
                            timeout=EXEC_TIMEOUT, input=stdin_input,
                        )
                    else:
                        process = run_executable_with_valgrind(
                            [executable_file],
                            capture_output=True, text=True,
                            timeout=EXEC_TIMEOUT,
                        )
                except subprocess.TimeoutExpired:
                    self.fail(f"FPC RTL test {test_base_name} timed out")
                    return

                expected_output = read_file_content(expected_output_file)
                text = process.stdout or ""
                while "\r\r\n" in text:
                    text = text.replace("\r\r\n", "\r\n")
                actual_output = text.replace("\r\n", "\n").replace("\r", "")

                self.assertEqual(actual_output, expected_output)
                self.assertEqual(process.returncode, 0)
                _test_cache_store(input_file, expected_output_file, FPC_RTL_FLAGS)

            test_method.__name__ = method_name
            test_method.__doc__ = f"FPC RTL test for {test_base_name}.p"
            return test_method

        setattr(TestCompiler, method_name, make_fpc_rtl_test(base_name))


# ---------------------------------------------------------------------------
# pp.pas bootstrap compilation test — added as a special FPC RTL test.
# This verifies that the compiler can parse and codegen the FPC compiler
# itself (pp.pas), which exercises far more of the language than any
# individual RTL test case.
# ---------------------------------------------------------------------------
def _add_pp_pas_bootstrap_test():
    """Add pp.pas compilation as a special FPC RTL test.
    Enabled in FPC RTL mode."""
    if not FPC_RTL_MODE:
        return

    fpc_src = os.environ.get("KGPC_FPC_RTL_DIR", "FPCSource")
    pp_pas = os.path.join(fpc_src, "compiler", "pp.pas")
    if not os.path.isfile(pp_pas):
        return

    pp_flags = _kgpc_bootstrap_flags(fpc_src, include_compiler_dirs=True)

    # The compiler's own -h text is target-conditional: the Windows build omits
    # Unix-only options (e.g. -ap "use pipes"), so it needs its own expected.
    pp_expected_file = os.path.join(
        TEST_CASES_DIR,
        "pp_pas_bootstrap_win.expected" if IS_WINDOWS_ABI
        else "pp_pas_bootstrap.expected",
    )

    def _strip_pp_header(output):
        """Strip version/copyright/path lines from pp.pas -h output."""
        lines = output.splitlines(True)
        return "".join(
            l for l in lines
            if not l.startswith("Free Pascal Compiler version")
            and not l.startswith("Copyright")
            and "[options] <inputfile>" not in l
        )

    def test_pp_pas_bootstrap(self):
        """pp.pas bootstrap compilation — compile the FPC compiler itself."""
        asm_file = os.path.join(TEST_OUTPUT_DIR, "pp_bootstrap.s")
        executable_file = os.path.join(TEST_OUTPUT_DIR, "pp_bootstrap" + EXE_EXT)

        # Generate msgtxt.inc and msgidx.inc by compiling and running the real
        # FPC msg2inc.pp utility with KGPC (same approach as
        # scripts/profile_large_unit_graph.py ensure_bootstrap_prereqs).
        compiler_dir = os.path.join(fpc_src, "compiler")
        errore_msg = os.path.join(compiler_dir, "msg", "errore.msg")
        msgtxt_inc = os.path.join(compiler_dir, "msgtxt.inc")
        msgidx_inc = os.path.join(compiler_dir, "msgidx.inc")
        msg_includes_are_stale = (
            not os.path.isfile(msgtxt_inc) or
            not os.path.isfile(msgidx_inc) or
            os.path.getmtime(msgtxt_inc) < os.path.getmtime(errore_msg) or
            os.path.getmtime(msgidx_inc) < os.path.getmtime(errore_msg)
        )
        if msg_includes_are_stale:
            msg2inc_pp = os.path.join(fpc_src, "compiler", "utils", "msg2inc.pp")
            assert os.path.isfile(msg2inc_pp), f"msg2inc.pp not found: {msg2inc_pp}"

            msg2inc_asm = os.path.join(TEST_OUTPUT_DIR, "msg2inc.s")
            msg2inc_exe = os.path.join(TEST_OUTPUT_DIR, "msg2inc" + EXE_EXT)

            # Compile msg2inc.pp with KGPC using the same RTL flags
            msg2inc_flags = _kgpc_bootstrap_flags(
                fpc_src, include_compiler_dirs=False
            )

            try:
                run_compiler(msg2inc_pp, msg2inc_asm, flags=msg2inc_flags, timeout=120)
            except subprocess.CalledProcessError as e:
                self.fail(f"msg2inc.pp compilation failed: {e}")
                return

            # Link msg2inc
            runtime_lib = _RUNTIME_LIB_PATH
            assert runtime_lib and os.path.isfile(runtime_lib), (
                f"KGPC runtime library not found: {runtime_lib!r}"
            )
            link_cmd = list(self.c_compiler_cmd) + [
                "-O2",
                "-no-pie" if not IS_WINDOWS_ABI else "-static",
                "-o", msg2inc_exe,
                msg2inc_asm,
                runtime_lib,
            ]
            link_cmd.extend(LINK_ARGS_BY_ASM.get(msg2inc_asm, []))
            try:
                subprocess.run(link_cmd, check=True, capture_output=True, text=True,
                               timeout=60)
            except subprocess.CalledProcessError as e:
                self.fail(f"msg2inc linking failed: {e.stderr}")
                return

            # Run msg2inc to generate msgtxt.inc and msgidx.inc
            run_cmd = [os.path.abspath(msg2inc_exe), "msg/errore.msg", "msg", "msg"]
            try:
                subprocess.run(run_cmd, check=True, capture_output=True, text=True,
                               cwd=compiler_dir, timeout=30)
            except subprocess.CalledProcessError as e:
                self.fail(f"msg2inc execution failed: {e.stderr}")
                return

            # Verify and SHA256 the generated files
            assert os.path.isfile(msgtxt_inc) and os.path.getsize(msgtxt_inc) > 0, (
                f"msg2inc produced empty or missing {msgtxt_inc}"
            )
            assert os.path.isfile(msgidx_inc) and os.path.getsize(msgidx_inc) > 0, (
                f"msg2inc produced empty or missing {msgidx_inc}"
            )
            with open(msgtxt_inc, "rb") as f:
                msgtxt_sha = hashlib.sha256(f.read()).hexdigest()
            with open(msgidx_inc, "rb") as f:
                msgidx_sha = hashlib.sha256(f.read()).hexdigest()
            print(f"msgtxt.inc sha256: {msgtxt_sha}", file=sys.stderr)
            print(f"msgidx.inc sha256: {msgidx_sha}", file=sys.stderr)

        try:
            run_compiler(pp_pas, asm_file, flags=pp_flags, timeout=PP_BOOTSTRAP_COMPILE_TIMEOUT)
        except subprocess.CalledProcessError as e:
            self.fail(f"pp.pas compilation failed: {e}")
            return

        try:
            self.compile_executable(asm_file, executable_file)
        except Exception as e:
            self.fail(f"pp.pas linking failed: {e}")
            return

        # Run the compiled compiler with -h and compare output
        try:
            process = subprocess.run(
                [executable_file, "-h"],
                capture_output=True, text=True, timeout=30,
            )
        except subprocess.TimeoutExpired:
            self.fail("pp.pas binary timed out running with -h")
            return

        # Check exit code first — if the binary crashed, report that instead
        # of a confusing empty-string-vs-expected diff.
        if process.returncode != 0:
            sig_name = _signal_name_suffix(process.returncode)
            stderr_snippet = (process.stderr or "")[:2000]
            self.fail(
                f"pp.pas binary exited with code {process.returncode}{sig_name}\n"
                f"stderr:\n{stderr_snippet}"
            )

        expected_output = read_file_content(pp_expected_file)
        actual_output = _strip_pp_header(process.stdout or "")
        self.assertEqual(actual_output, expected_output)

        # End-to-end regression check: use the just-built pp_bootstrap to
        # compile a tiny Pascal program and then rebuild pp.pas itself. This
        # pins the generated compiler's ordinary program path as well as the
        # full self-hosting compiler path. The pipeline is:
        #     1. KGPC compiles pp.pas         -> tests/output/pp_bootstrap
        #        (already done above).
        #     2. pp_bootstrap compiles helloworld.p with the FPC RTL units.
        #     3. pp_bootstrap compiles pp.pas -> tests/output/pp_stage2/pp_stage2
        #     4. pp_stage2 starts and prints the expected help banner.
        #
        # pp_bootstrap needs the RTL .ppu files for program compilation and for
        # pp.pas self-hosting.  These are built by pp_bootstrap ITSELF from the
        # FPCSource checkout — a genuine self-host, with no host Pascal compiler
        # involved.  (This previously seeded a same-source `ppcx64` from a distro
        # `fpc` and built the RTL with that, which made the "bootstrap" lean on
        # an external compiler and hid every RTL-from-source codegen bug behind
        # host-built .ppu.  Building the RTL with pp_bootstrap surfaces those
        # bugs here instead.)
        helloworld_p = os.path.join(TEST_CASES_DIR, "helloworld.p")
        assert os.path.isfile(helloworld_p), f"helloworld.p missing: {helloworld_p}"

        prebuilt_units_dir = os.path.join(fpc_src, "rtl", "units", "x86_64-linux")
        prebuilt_system_ppu = os.path.join(prebuilt_units_dir, "system.ppu")
        ppu_version_source = os.path.join(fpc_src, "compiler", "ppu.pas")
        # `abitag.o` is built by the loader (not units) target, so a cache
        # populated by an older `make units` invocation can have system.ppu
        # without it.  Treat the loader artifact as a co-required input so
        # we always rebuild when either piece of the RTL is missing.
        prebuilt_abitag_o = os.path.join(prebuilt_units_dir, "abitag.o")
        rtl_units_are_stale = (
            not os.path.isfile(prebuilt_system_ppu) or
            not os.path.isfile(prebuilt_abitag_o) or
            os.path.getmtime(prebuilt_system_ppu) < os.path.getmtime(ppu_version_source) or
            os.path.getmtime(prebuilt_abitag_o) < os.path.getmtime(ppu_version_source) or
            _tree_contains_newer_file(os.path.join(fpc_src, "rtl"), prebuilt_system_ppu) or
            _tree_contains_newer_file(os.path.join(fpc_src, "rtl"), prebuilt_abitag_o)
        )
        if rtl_units_are_stale:
            if os.path.isdir(prebuilt_units_dir):
                shutil.rmtree(prebuilt_units_dir)
            make_bin = shutil.which("make")
            assert make_bin is not None, (
                "make is required to build FPC RTL units for pp_bootstrap"
            )
            rtl_linux_dir = os.path.join(fpc_src, "rtl", "linux")
            # Self-host: build the RTL .ppu with the just-built pp_bootstrap
            # itself (no host fpc).  Any KGPC codegen bug in an RTL unit now
            # fails the build here rather than being papered over by .ppu a
            # distro compiler produced.
            bootstrap_fpc = os.path.abspath(executable_file)
            try:
                # `make all` (not `units`) is required: the `units` target
                # only builds .ppu/.o pairs for Pascal units, but FPC's
                # Linux startup code uses `{$L abitag.o}` to pull in an
                # assembly-built note.ABI-tag section.  abitag is declared
                # as a LOADER in the RTL Makefile and is built by the
                # `loaders` (and thus `all`) target — `units` skips it,
                # which leaves pp_bootstrap unable to link any program.
                subprocess.run(
                    [
                        make_bin,
                        "-C",
                        rtl_linux_dir,
                        "all",
                        "FPC=" + bootstrap_fpc,
                    ],
                    check=True, capture_output=True, text=True, timeout=900,
                )
            except subprocess.CalledProcessError as e:
                self.fail(
                    "building FPC RTL units from source with pp_bootstrap "
                    "failed (self-host RTL compile)\n"
                    f"stdout:\n{(e.stdout or '')[:2000]}\n"
                    f"stderr:\n{(e.stderr or '')[:2000]}"
                )
                return
            except subprocess.TimeoutExpired:
                self.fail("building FPC RTL units with pp_bootstrap timed out")
                return
            assert os.path.isfile(prebuilt_system_ppu), (
                f"FPC RTL build did not produce {prebuilt_system_ppu}"
            )
            # Loader artifacts (built by the `loaders` target rolled into
            # `all`) live alongside the .ppu files and are pulled in via
            # `{$L abitag.o}` from the Linux startup code.  Pin their
            # presence so a future Makefile/target regression surfaces
            # here instead of as an opaque "ld: cannot find abitag.o".
            assert os.path.isfile(prebuilt_abitag_o), (
                f"FPC RTL build did not produce {prebuilt_abitag_o}; "
                "did the Makefile target switch from `all` back to `units`?"
            )

        helloworld_exe = os.path.join(
            TEST_OUTPUT_DIR, "helloworld_via_pp_bootstrap" + EXE_EXT)
        try:
            os.remove(helloworld_exe)
        except FileNotFoundError:
            pass
        # `-n` skips fpc.cfg.  `-FE<dir>` puts the produced binary in
        # TEST_OUTPUT_DIR.  `-o<name>` disambiguates the artifact from
        # any other helloworld binary that may have ended up in the
        # output tree.
        bootstrap_cmd = [
            os.path.abspath(executable_file),
        ] + _pp_bootstrap_program_flags(
            rtl_units_dir=os.path.abspath(prebuilt_units_dir),
            output_dir=os.path.abspath(TEST_OUTPUT_DIR),
            executable_name=os.path.basename(helloworld_exe),
        ) + [helloworld_p]
        # Keep the source file last: this matches normal FPC invocation and
        # lets the shared flag helpers append all output/search-path flags in
        # one place before the input file.
        try:
            compile_proc = subprocess.run(
                bootstrap_cmd, capture_output=True, text=True, timeout=120
            )
        except subprocess.TimeoutExpired:
            self.fail("pp_bootstrap timed out compiling helloworld.p")
            return

        if compile_proc.returncode != 0 or not os.path.isfile(helloworld_exe):
            sig_name = _signal_name_suffix(compile_proc.returncode)
            self.fail(
                f"pp_bootstrap failed to compile {helloworld_p} "
                f"(rc={compile_proc.returncode}{sig_name}, "
                f"binary_present={os.path.isfile(helloworld_exe)})\n"
                f"cmd: {' '.join(bootstrap_cmd)}\n"
                f"stdout:\n{(compile_proc.stdout or '')[:2000]}\n"
                f"stderr:\n{(compile_proc.stderr or '')[:2000]}"
            )

        try:
            run = subprocess.run(
                [os.path.abspath(helloworld_exe)],
                capture_output=True, text=True, timeout=10,
            )
        except subprocess.TimeoutExpired:
            self.fail("helloworld binary built by pp_bootstrap timed out")
            return
        if run.returncode != 0:
            self.fail(
                f"helloworld binary built by pp_bootstrap exited with "
                f"code {run.returncode}\n"
                f"stdout:\n{(run.stdout or '')[:2000]}\n"
                f"stderr:\n{(run.stderr or '')[:2000]}"
            )
        self.assertEqual(
            (run.stdout or "").strip().splitlines(),
            ["Hello, World!"],
            "pp_bootstrap-built helloworld printed unexpected output",
        )

        stage2_dir = os.path.join(TEST_OUTPUT_DIR, "pp_stage2")
        stage2_units_dir = os.path.join(stage2_dir, "units")
        os.makedirs(stage2_units_dir, exist_ok=True)
        stage2_executable = os.path.join(stage2_dir, "pp_stage2" + EXE_EXT)
        try:
            os.remove(stage2_executable)
        except FileNotFoundError:
            pass

        stage2_cmd = [
            os.path.abspath(executable_file),
        ] + _pp_bootstrap_compiler_flags(
            fpc_src,
            rtl_units_dir=os.path.abspath(prebuilt_units_dir),
            output_dir=os.path.abspath(stage2_dir),
            unit_output_dir=os.path.abspath(stage2_units_dir),
            executable_name=os.path.basename(stage2_executable),
        ) + [os.path.abspath(pp_pas)]
        try:
            stage2_compile = subprocess.run(
                stage2_cmd,
                capture_output=True,
                text=True,
                timeout=900,
                cwd=os.path.join(fpc_src, "compiler"),
            )
        except subprocess.TimeoutExpired:
            self.fail("pp_bootstrap timed out compiling pp.pas into pp_stage2")
            return

        if stage2_compile.returncode != 0 or not os.path.isfile(stage2_executable):
            sig_name = _signal_name_suffix(stage2_compile.returncode)
            self.fail(
                f"pp_bootstrap failed to compile {pp_pas} "
                f"(rc={stage2_compile.returncode}{sig_name}, "
                f"binary_present={os.path.isfile(stage2_executable)})\n"
                f"cmd: {' '.join(stage2_cmd)}\n"
                f"stdout:\n{(stage2_compile.stdout or '')[:4000]}\n"
                f"stderr:\n{(stage2_compile.stderr or '')[:4000]}"
            )

        try:
            stage2_help = subprocess.run(
                [os.path.abspath(stage2_executable), "-h"],
                capture_output=True,
                text=True,
                timeout=30,
            )
        except subprocess.TimeoutExpired:
            self.fail("pp_stage2 timed out running with -h")
            return

        if stage2_help.returncode != 0:
            self.fail(
                f"pp_stage2 exited with code {stage2_help.returncode}\n"
                f"stdout:\n{(stage2_help.stdout or '')[:2000]}\n"
                f"stderr:\n{(stage2_help.stderr or '')[:2000]}"
            )
        self.assertEqual(
            _strip_pp_header(stage2_help.stdout or ""),
            expected_output,
            "pp_stage2 help output differs from the expected bootstrap banner",
        )

    test_pp_pas_bootstrap.__name__ = "test_fpcrtl_pp_pas_bootstrap"
    test_pp_pas_bootstrap.__doc__ = (
        "pp.pas bootstrap — compile and link the FPC compiler, use it to "
        "compile helloworld.p, then rebuild pp.pas itself"
    )
    # pp.pas is compiled twice in this test (KGPC -> pp_bootstrap, then
    # pp_bootstrap -> pp_stage2) and may also need the same-source RTL rebuild
    # on a cold worker, so allow one long timeout budget for the full chain.
    test_pp_pas_bootstrap._timeout = PP_BOOTSTRAP_FULL_CHAIN_TIMEOUT
    setattr(TestCompiler, "test_fpcrtl_pp_pas_bootstrap", test_pp_pas_bootstrap)


# Auto-discover and add tests before loading the suite
if FPC_RTL_MODE:
    _discover_and_add_fpc_rtl_tests()
    _add_pp_pas_bootstrap_test()
else:
    _discover_and_add_auto_tests()
