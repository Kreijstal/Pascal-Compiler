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

/* Interface GUID fallback labels.
 * The compiler emits __kgpc_guid_<Name> (16-byte GUID data) and
 * __kgpc_guidref_<Name> (pointer indirection) as weak symbols for every
 * interface used in a class's interface table.  The bare interface name
 * labels (IObserver, etc.) are weak aliases pointing to the GUID data,
 * so that `leaq IObserver(%rip)` in Supports() resolves to GUID bytes.
 * These fallback zeros are overridden by compiler-emitted definitions. */
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

/* Fallback GUID constants (16-byte zero GUIDs) — overridden by compiler. */
.weak __kgpc_guid_IInterface
__kgpc_guid_IInterface:
    .zero 16
.weak __kgpc_guid_IUnknown
__kgpc_guid_IUnknown:
    .zero 16
.weak __kgpc_guid_IObserver
__kgpc_guid_IObserver:
    .zero 16
.weak __kgpc_guid_ISingleCastObserver
__kgpc_guid_ISingleCastObserver:
    .zero 16
.weak __kgpc_guid_IMultiCastObserver
__kgpc_guid_IMultiCastObserver:
    .zero 16
.weak __kgpc_guid_IEditLinkObserver
__kgpc_guid_IEditLinkObserver:
    .zero 16
.weak __kgpc_guid_IInterfaceComponentReference
__kgpc_guid_IInterfaceComponentReference:
    .zero 16

/* Fallback GUID-ref pointers (pguid indirection) — overridden by compiler. */
.weak __kgpc_guidref_IInterface
__kgpc_guidref_IInterface:
    .quad __kgpc_guid_IInterface
.weak __kgpc_guidref_IUnknown
__kgpc_guidref_IUnknown:
    .quad __kgpc_guid_IUnknown
.weak __kgpc_guidref_IObserver
__kgpc_guidref_IObserver:
    .quad __kgpc_guid_IObserver
.weak __kgpc_guidref_ISingleCastObserver
__kgpc_guidref_ISingleCastObserver:
    .quad __kgpc_guid_ISingleCastObserver
.weak __kgpc_guidref_IMultiCastObserver
__kgpc_guidref_IMultiCastObserver:
    .quad __kgpc_guid_IMultiCastObserver
.weak __kgpc_guidref_IEditLinkObserver
__kgpc_guidref_IEditLinkObserver:
    .quad __kgpc_guid_IEditLinkObserver
.weak __kgpc_guidref_IInterfaceComponentReference
__kgpc_guidref_IInterfaceComponentReference:
    .quad __kgpc_guid_IInterfaceComponentReference

/* Bare interface name labels — these are no longer needed because the codegen
 * now emits `leaq __kgpc_guid_<Name>(%rip)` directly when passing an interface
 * type as a GUID argument.  Keep weak zero-filled labels so that any remaining
 * references (e.g. from older codegen paths) still link. */
.weak IInterface
IInterface:
    .zero 16
.weak IUnknown
IUnknown:
    .zero 16
.weak IObserver
IObserver:
    .zero 16
.weak ISingleCastObserver
ISingleCastObserver:
    .zero 16
.weak IMultiCastObserver
IMultiCastObserver:
    .zero 16
.weak IEditLinkObserver
IEditLinkObserver:
    .zero 16
.weak IInterfaceComponentReference
IInterfaceComponentReference:
    .zero 16

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
