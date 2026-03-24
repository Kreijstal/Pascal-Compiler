/* Abstract method stubs for FPC RTL compatibility.
 *
 * Interface method stubs are now emitted by the compiler in codegen.c
 * during VMT generation — only when there is no concrete
 * implementation in the program.  This avoids multiple-definition
 * linker errors when a program provides a real implementation.
 *
 * The stubs below are for non-interface symbols that the compiler
 * cannot yet emit structurally. */
.text

/* TMarshaller.TDeferBase.Done is virtual;abstract in an old-style Pascal
 * object.  KGPC generates a direct call instead of VMT dispatch for
 * old-style objects, so define the stub here to prevent linker errors.
 * The stub will crash if ever called (it should not be — the call is dead
 * code in tests that never instantiate TDeferBase). */
.globl tmarshaller.tdeferbase__done_u_tmarshaller_tdeferbase
tmarshaller.tdeferbase__done_u_tmarshaller_tdeferbase:
    jmp __kgpc_abstract_method_error

/* TMarshal.UnfixArray<TPtrWrapper> — generic method specialization that
 * the compiler cannot yet emit because nested generic specialization
 * chains (method-generic → record-generic → method-generic) are not
 * resolved by the pre-semcheck generic instantiation pass.
 * The function body is Finalize(TArray<TPtrWrapper>(ArrPtr)) which is a
 * no-op for TPtrWrapper (a simple record with no managed fields). */
.globl tmarshal__unfixarray_u_tptrwrapper
tmarshal__unfixarray_u_tptrwrapper:
    ret

/* SysBeep — system procedure that produces a beep.
 * Not meaningful in a CLI/batch context; provide as no-op. */
.globl SysBeep
SysBeep:
    ret

#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",@progbits
#endif

