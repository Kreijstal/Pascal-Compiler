/* Abstract method stubs for FPC RTL compatibility.
 * These symbols are referenced by FPC RTL compiled code but have no
 * concrete implementation (virtual;abstract).  They redirect to the
 * KGPC runtime abstract-method error handler so that programs link even
 * when the code path is dead (never executed at runtime). */
.text

.globl iinterfacelist__add_p_u_iunknown
iinterfacelist__add_p_u_iunknown:
    jmp __kgpc_abstract_method_error

.globl iinterfacecomponentreference__getcomponent_p
iinterfacecomponentreference__getcomponent_p:
    jmp __kgpc_abstract_method_error

.globl ieditlinkobserver__getisreadonly_p
ieditlinkobserver__getisreadonly_p:
    jmp __kgpc_abstract_method_error

.globl ifpobserver__fpoobservedchanged_p_u_tobject_e_p
ifpobserver__fpoobservedchanged_p_u_tobject_e_p:
    jmp __kgpc_abstract_method_error

.globl iobserver__getactive_p
iobserver__getactive_p:
    jmp __kgpc_abstract_method_error

.globl isequentialstream__read_p_p_li_p
isequentialstream__read_p_p_li_p:
    jmp __kgpc_abstract_method_error

.globl isequentialstream__write_p_p_li_p
isequentialstream__write_p_p_li_p:
    jmp __kgpc_abstract_method_error

.globl ivclcomobject__safecallexception_p_u_tobject_p
ivclcomobject__safecallexception_p_u_tobject_p:
    jmp __kgpc_abstract_method_error

/* TMarshaller.TDeferBase.Done is virtual;abstract in an old-style Pascal
 * object.  KGPC generates a direct call instead of VMT dispatch for
 * old-style objects, so define the stub here to prevent linker errors.
 * The stub will crash if ever called (it should not be — the call is dead
 * code in tests that never instantiate TDeferBase). */
.globl tmarshaller.tdeferbase__done_u_tmarshaller_tdeferbase
tmarshaller.tdeferbase__done_u_tmarshaller_tdeferbase:
    jmp __kgpc_abstract_method_error

/* FPC RTL mathu.inc implements arctan2, tan, cotan, log2 as nostackframe
 * assembler routines using FPC's x86-64 Extended ABI (argument at 8(%rsp)).
 * KGPC passes floats in xmm registers (System V double convention), so the
 * FPC bodies are completely incompatible.  The code generator suppresses
 * emitting those bodies so these symbols remain external references; the
 * strong definitions here redirect to KGPC runtime functions that use the
 * correct xmm convention. */
.globl arctan2_r_r
arctan2_r_r:
    cvtss2sd %xmm0, %xmm0
    cvtss2sd %xmm1, %xmm1
    call kgpc_arctan2
    cvtsd2ss %xmm0, %xmm0
    ret

.globl tan_r
tan_r:
    cvtss2sd %xmm0, %xmm0
    call kgpc_tan
    cvtsd2ss %xmm0, %xmm0
    ret

.globl cotan_r
cotan_r:
    cvtss2sd %xmm0, %xmm0
    call kgpc_cot
    cvtsd2ss %xmm0, %xmm0
    ret

/* log2_r: redirect to C library log2() which uses xmm0/xmm0 (double) convention.
 * The caller passes a Single in xmm0, so convert to double first. */
.globl log2_r
log2_r:
    cvtss2sd %xmm0, %xmm0
    call log2@PLT
    cvtsd2ss %xmm0, %xmm0
    ret

/* copysign_r_r: FPC's body treats floats as 10-byte Extended (sign at offset+8)
 * but KGPC passes System V doubles (8 bytes, xmm0/xmm1).  The FPC body
 * corrupts the caller's saved rbp by writing to rbp+0.  Provide a correct
 * Single-precision copysign: promote to double, do copysign, demote back. */
.globl copysign_r_r
copysign_r_r:
    cvtss2sd %xmm0, %xmm0
    cvtss2sd %xmm1, %xmm1
    movq    %xmm0, %rax
    movq    %xmm1, %rcx
    movabsq $0x7fffffffffffffff, %rdx
    andq    %rdx,  %rax        /* rax = |x| (sign bit cleared) */
    notq    %rdx               /* rdx = 0x8000000000000000 */
    andq    %rdx,  %rcx        /* rcx = sign bit of y */
    orq     %rcx,  %rax        /* combine magnitude and sign */
    movq    %rax,  %xmm0
    cvtsd2ss %xmm0, %xmm0
    ret
