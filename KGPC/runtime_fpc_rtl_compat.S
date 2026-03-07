.text

/* FPC RTL mathu.inc provides arctan2, tan, cotan, log2 as nostackframe
 * assembler routines that expect Extended parameters passed on the stack
 * (FPC's x86-64 Extended ABI: argument at 8(%rsp)).  KGPC passes all
 * floats in xmm registers (System-V double convention), so those bodies
 * are completely incompatible.  The generated .s uses .weak for these
 * symbols, so strong definitions here override them and redirect to the
 * KGPC runtime equivalents (which use the correct xmm convention). */

.globl arctan2_r_r
arctan2_r_r:
    jmp kgpc_arctan2

.globl tan_r
tan_r:
    jmp kgpc_tan

.globl cotan_r
cotan_r:
    jmp kgpc_cot

/* log2_r: FPC's log2(x: float) — redirect to C library log2().
 * C's log2(double) uses the same xmm0/xmm0 calling convention. */
.globl log2_r
log2_r:
    jmp log2@PLT

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
