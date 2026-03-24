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
 * the FPC RTL interfaces.  Provide weak fallback zero values for TYPEINFO symbols
 * that may not be compiled if only a subset of interfaces are used. */
.data
.align 8

/* Weak fallback TYPEINFO symbols — overridden by compiler-emitted versions when
 * the corresponding interface is actually compiled in the program. */
.weak IInterface_TYPEINFO
.weak IUnknown_TYPEINFO
.weak IObserver_TYPEINFO
.weak ISingleCastObserver_TYPEINFO
.weak IMultiCastObserver_TYPEINFO
.weak IEditLinkObserver_TYPEINFO
.weak IInterfaceComponentReference_TYPEINFO
ISingleCastObserver_TYPEINFO:
IMultiCastObserver_TYPEINFO:
IEditLinkObserver_TYPEINFO:
IInterfaceComponentReference_TYPEINFO:
    .quad 0

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
