.text

.globl PopCnt
PopCnt:
    jmp popcnt_i64

.globl filecreate_rbs_i
filecreate_rbs_i:
    jmp filecreate_rbs

.globl filecreate_rbs_i_i
filecreate_rbs_i_i:
    jmp filecreate_rbs

.globl FindComponentClass
FindComponentClass:
    jmp treader__findcomponentclass_p_rbs_rbs_rbs

.globl ReadDeltaStream
ReadDeltaStream:
    jmp tcomponent__readdeltastream_p_u_tstream

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

/* TMarshaller.TDeferBase.Done is virtual;abstract in old-style Pascal objects.
 * KGPC generates a direct call instead of VMT dispatch for old-style objects,
 * so we define the stub here to prevent linker errors. */
.globl tmarshaller.tdeferbase__done_u_tmarshaller_tdeferbase
tmarshaller.tdeferbase__done_u_tmarshaller_tdeferbase:
    jmp __kgpc_abstract_method_error

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
