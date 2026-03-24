.text

/* arctan2_r_r, tan_r, cotan_r, log2_r are provided by runtime_fpc_abstract_stubs.S
 * which is always included. Do not duplicate them here. */


/* FindComponentClass / ReadDeltaStream: FPC TReader/TComponent class methods that
 * are referenced but rarely executed.  Redirect to the abstract-method error
 * handler so that programs which don't exercise these paths can still link. */
.globl FindComponentClass
FindComponentClass:
    jmp __kgpc_abstract_method_error

.globl ReadDeltaStream
ReadDeltaStream:
    jmp __kgpc_abstract_method_error

/* Interface GUID/TYPEINFO/GUIDREF symbols and bare interface name labels
 * have been removed from this file.  The compiler (codegen) now emits all
 * these symbols directly:
 *   - __kgpc_guid_<Name>   (16-byte GUID constant, .globl)
 *   - __kgpc_guidref_<Name> (pointer to GUID constant, .globl)
 *   - <Name>_TYPEINFO       (RTTI structure)
 * Codegen emits GUID data for all interfaces with GUIDs in the symbol table,
 * not just those implemented by local classes.  The codegen emits
 * `leaq __kgpc_guid_<Name>(%rip)` directly for interface GUID arguments
 * (e.g. Supports(Obj, IObserver, I)), so no bare interface labels are needed. */

.data
.align 8

.globl FAction
FAction:
    .quad 0
.globl FCount
FCount:
    .quad 0
.globl FData
FData:
    .quad 0
.globl FRootAncestor
FRootAncestor:
    .quad 0
.globl FitemClass_TYPEINFO
FitemClass_TYPEINFO:
    .quad 0
.globl PPropInfo
PPropInfo:
    .quad 0
.globl SetStatus
SetStatus:
    .quad 0

#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",@progbits
#endif
