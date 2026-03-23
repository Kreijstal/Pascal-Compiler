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

/* Interface TYPEINFO indirection table.
 * These globals are referenced by FPC RTL compiled code for interface IS/AS checks.
 * The ClassName_TYPEINFO symbols are emitted by the KGPC compiler when it compiles
 * the FPC RTL interfaces.  This object is only pulled into the link when FPC RTL
 * compiled code references PopCnt or filecreate_rbs_i — which only happens in
 * FPC RTL builds where all ClassName_TYPEINFO symbols are available. */
.data
.align 8
.globl IInterface
IInterface:
    .quad IInterface_TYPEINFO
.globl IUnknown
IUnknown:
    .quad IUnknown_TYPEINFO
.globl IObserver
IObserver:
    .quad IObserver_TYPEINFO
.globl ISingleCastObserver
ISingleCastObserver:
    .quad ISingleCastObserver_TYPEINFO
.globl IMultiCastObserver
IMultiCastObserver:
    .quad IMultiCastObserver_TYPEINFO
.globl IEditLinkObserver
IEditLinkObserver:
    .quad IEditLinkObserver_TYPEINFO
.globl IInterfaceComponentReference
IInterfaceComponentReference:
    .quad IInterfaceComponentReference_TYPEINFO

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

/* Mark this object as not requiring an executable stack */
#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",@progbits
#endif
