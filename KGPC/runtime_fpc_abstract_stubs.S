/* This file is intentionally empty.
 *
 * Interface abstract method stubs are now emitted by the codegen in
 * codegen_vmt() — no static stubs are needed here.
 *
 * SysBeep is provided by runtime.c.
 *
 * Old-style object abstract methods (TMarshaller.TDeferBase.Done) and
 * generic specialisation stubs (TMarshal.UnfixArray<TPtrWrapper>) are
 * handled by the codegen's forward-declaration abstract stub pass in
 * codegen_subprograms().
 */
.text

/* Mark this object as not requiring an executable stack */
#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",@progbits
#endif

