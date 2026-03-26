	.file	"tests/test_single.p"
	.section	.rodata
	.text
	.set	KGPC_TARGET_WINDOWS, 0
	.section	.rodata
.format_str_s:
.string "%s"
.format_str_d:
.string "%d"
.format_str_c:
.string "%c"
.format_str_lld:
.string "%ld"
.format_str_sn:
.string "%s\n"
.format_str_dn:
.string "%d\n"
.format_str_n:
.string "\n"
.text
.equ DefaultFileSystemCodePage, 65001
.equ MaxInt64, 9223372036854775807
.equ DefaultSystemCodePage, 65001
.equ MaxShortint, 127

# Class RTTI metadata and Virtual Method Tables (VMT)
	.section	.rodata

# RTTI for class TObject
	.align 8
.globl TObject_TYPEINFO
TObject_TYPEINFO:
	.quad	0
	.quad	__kgpc_typeinfo_name_TObject
	.quad	TObject_VMT
	.quad	0
	.quad	0
__kgpc_typeinfo_name_TObject:
	.string "TObject"
__kgpc_vmt_classname_TObject:
	.byte	7
	.ascii	"TObject"

# VMT for class TObject
	.align 8
.globl TObject_VMT
TObject_VMT:
	.quad	16
	.quad	-16
	.quad	0
	.quad	__kgpc_vmt_classname_TObject
	.quad	0
	.quad	0
	.quad	0
	.quad	TObject_TYPEINFO
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	tobject__destroy_p
	.quad	tobject__classname_p
	.quad	tobject__classparent_p
	.quad	tobject__classinfo_p
	.quad	tobject__classtype_p
	.quad	tobject__inheritsfrom_p_p
	.quad	tobject__freeinstance_p
	.quad	tobject__tostring_p

# Class variables for TObject
	.data
	.align 8
.globl TObject_CLASSVAR
TObject_CLASSVAR:
	.zero	8
	.zero	8
	.section	.rodata

# Class var storage for record TDoubleRec
	.data
	.align 8
.globl TDoubleRec_CLASSVAR
.globl	TDoubleRec
TDoubleRec:
TDoubleRec_CLASSVAR:
	.zero	8
	.section	.rodata
	.align 8
.globl TDoubleRec_TYPEINFO
TDoubleRec_TYPEINFO:
	.quad	0
.globl TDoubleRec_VMT
TDoubleRec_VMT:
	.quad	TDoubleRec_TYPEINFO

# Class var storage for record TSingleRec
	.data
	.align 8
.globl TSingleRec_CLASSVAR
.globl	TSingleRec
TSingleRec:
TSingleRec_CLASSVAR:
	.zero	8
	.section	.rodata
	.align 8
.globl TSingleRec_TYPEINFO
TSingleRec_TYPEINFO:
	.quad	0
.globl TSingleRec_VMT
TSingleRec_VMT:
	.quad	TSingleRec_TYPEINFO

# Class var storage for record TPtrWrapper
	.data
	.align 8
.globl TPtrWrapper_CLASSVAR
.globl	TPtrWrapper
TPtrWrapper:
TPtrWrapper_CLASSVAR:
	.zero	8
	.section	.rodata
	.align 8
.globl TPtrWrapper_TYPEINFO
TPtrWrapper_TYPEINFO:
	.quad	0
.globl TPtrWrapper_VMT
TPtrWrapper_VMT:
	.quad	TPtrWrapper_TYPEINFO

# RTTI for class IInterface
	.align 8
.globl IInterface_TYPEINFO
IInterface_TYPEINFO:
	.quad	0
	.quad	__kgpc_typeinfo_name_IInterface
	.quad	IInterface_VMT
	.quad	0
	.quad	0
__kgpc_typeinfo_name_IInterface:
	.string "IInterface"
__kgpc_vmt_classname_IInterface:
	.byte	10
	.ascii	"IInterface"

# VMT for class IInterface
	.align 8
.globl IInterface_VMT
IInterface_VMT:
	.quad	8
	.quad	-8
	.quad	0
	.quad	__kgpc_vmt_classname_IInterface
	.quad	0
	.quad	0
	.quad	0
	.quad	IInterface_TYPEINFO
	.quad	0
	.quad	0
	.quad	0
	.quad	0

# Class variables for IInterface
	.data
	.align 8
.globl IInterface_CLASSVAR
IInterface_CLASSVAR:
	.zero	8
	.section	.rodata

# GUID constant for interface IInterface
	.data
	.align 8
.globl __kgpc_guid_IInterface
__kgpc_guid_IInterface:
	.long	0x00000000
	.short	0x0000
	.short	0x0000
	.byte	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	.align 8
.globl __kgpc_guidref_IInterface
__kgpc_guidref_IInterface:
	.quad	__kgpc_guid_IInterface
	.section	.rodata

# Interface table (tinterfacetable) for class TInterfacedObject
	.data
	.align 8
TInterfacedObject_INTFTABLE:
	.quad	1	# EntryCount
	# Entry for IInterface (40 bytes = tinterfaceentry)
	.quad	__kgpc_guidref_IInterface
	.quad	TInterfacedObject_INTF_IInterface_VTABLE
	.quad	32
	.quad	0
	.long	0
	.zero	4
	.section	.rodata

# Interface vtable for TInterfacedObject implementing IInterface
	.data
	.align 8
TInterfacedObject_INTF_IInterface_VTABLE:
	.text
TInterfacedObject_INTF_IInterface_THUNK_QueryInterface:
	movq	(%rdi), %r11
	movq	(%r11), %rax
	addq	8(%r11), %rax
	jnz	.LTInterfacedObject_INTF_IInterface_THUNK_QueryInterface_adj
	jmp	tinterfacedobject__queryinterface_p_u_tguid_u
.LTInterfacedObject_INTF_IInterface_THUNK_QueryInterface_adj:
	subq	$32, %rdi
	jmp	tinterfacedobject__queryinterface_p_u_tguid_u
	.data
	.quad	TInterfacedObject_INTF_IInterface_THUNK_QueryInterface	# QueryInterface
	.text
TInterfacedObject_INTF_IInterface_THUNK__AddRef:
	movq	(%rdi), %r11
	movq	(%r11), %rax
	addq	8(%r11), %rax
	jnz	.LTInterfacedObject_INTF_IInterface_THUNK__AddRef_adj
	jmp	tinterfacedobject___addref_p
.LTInterfacedObject_INTF_IInterface_THUNK__AddRef_adj:
	subq	$32, %rdi
	jmp	tinterfacedobject___addref_p
	.data
	.quad	TInterfacedObject_INTF_IInterface_THUNK__AddRef	# _AddRef
	.text
TInterfacedObject_INTF_IInterface_THUNK__Release:
	movq	(%rdi), %r11
	movq	(%r11), %rax
	addq	8(%r11), %rax
	jnz	.LTInterfacedObject_INTF_IInterface_THUNK__Release_adj
	jmp	tinterfacedobject___release_p
.LTInterfacedObject_INTF_IInterface_THUNK__Release_adj:
	subq	$32, %rdi
	jmp	tinterfacedobject___release_p
	.data
	.quad	TInterfacedObject_INTF_IInterface_THUNK__Release	# _Release
	.section	.rodata

# RTTI for class TInterfacedObject
	.align 8
.globl TInterfacedObject_TYPEINFO
TInterfacedObject_TYPEINFO:
	.quad	TObject_TYPEINFO
	.quad	__kgpc_typeinfo_name_TInterfacedObject
	.quad	TInterfacedObject_VMT
	.quad	TInterfacedObject_INTFTABLE
	.quad	1
__kgpc_typeinfo_name_TInterfacedObject:
	.string "TInterfacedObject"
__kgpc_vmt_classname_TInterfacedObject:
	.byte	17
	.ascii	"TInterfacedObject"
	.align 8
__kgpc_vmt_parentref_TInterfacedObject:
	.quad	TObject_VMT

# VMT for class TInterfacedObject
	.align 8
.globl TInterfacedObject_VMT
TInterfacedObject_VMT:
	.quad	40
	.quad	-40
	.quad	__kgpc_vmt_parentref_TInterfacedObject
	.quad	__kgpc_vmt_classname_TInterfacedObject
	.quad	0
	.quad	0
	.quad	0
	.quad	TInterfacedObject_TYPEINFO
	.quad	0
	.quad	0
	.quad	TInterfacedObject_INTFTABLE
	.quad	0
	.quad	tobject__destroy_p
	.quad	tobject__classname_p
	.quad	tobject__classparent_p
	.quad	tobject__classinfo_p
	.quad	tobject__classtype_p
	.quad	tobject__inheritsfrom_p_p
	.quad	tobject__freeinstance_p
	.quad	tobject__tostring_p

# Interface dispatch: IInterface.QueryInterface -> tinterfacedobject__queryinterface_p_u_tguid_u
	.text
.globl iinterface__queryinterface_p_u_tguid_u
iinterface__queryinterface_p_u_tguid_u:
	jmp	tinterfacedobject__queryinterface_p_u_tguid_u

# Interface dispatch: IInterface._AddRef -> tinterfacedobject___addref_p
	.text
.globl iinterface___addref_p
iinterface___addref_p:
	jmp	tinterfacedobject___addref_p

# Interface dispatch: IInterface._Release -> tinterfacedobject___release_p
	.text
.globl iinterface___release_p
iinterface___release_p:
	jmp	tinterfacedobject___release_p

# Class variables for TInterfacedObject
	.data
	.align 8
.globl TInterfacedObject_CLASSVAR
TInterfacedObject_CLASSVAR:
	.zero	8
	.section	.rodata

# TYPEINFO alias: TClass = TObject
.globl	TClass_TYPEINFO
	.set	TClass_TYPEINFO, TObject_TYPEINFO
.globl	TClass_VMT
	.set	TClass_VMT, TObject_VMT

# TYPEINFO alias: PInterface = IInterface
.globl	PInterface_TYPEINFO
	.set	PInterface_TYPEINFO, IInterface_TYPEINFO
.globl	PInterface_VMT
	.set	PInterface_VMT, IInterface_VMT

# TYPEINFO alias: IUnknown = IInterface
.globl	IUnknown_TYPEINFO
	.set	IUnknown_TYPEINFO, IInterface_TYPEINFO
.globl	IUnknown_VMT
	.set	IUnknown_VMT, IInterface_VMT
.text
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_DirectorySeparator_1
__kgpc_program_var_DirectorySeparator_1:
	.zero	1
.globl	DirectorySeparator
	.set	DirectorySeparator, __kgpc_program_var_DirectorySeparator_1
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_DriveSeparator_2
__kgpc_program_var_DriveSeparator_2:
	.zero	1
.globl	DriveSeparator
	.set	DriveSeparator, __kgpc_program_var_DriveSeparator_2
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_PathSeparator_3
__kgpc_program_var_PathSeparator_3:
	.zero	1
.globl	PathSeparator
	.set	PathSeparator, __kgpc_program_var_PathSeparator_3
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_ExtensionSeparator_4
__kgpc_program_var_ExtensionSeparator_4:
	.zero	1
.globl	ExtensionSeparator
	.set	ExtensionSeparator, __kgpc_program_var_ExtensionSeparator_4
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_IsMultiThread_5
__kgpc_program_var_IsMultiThread_5:
	.zero	4
.globl	IsMultiThread
	.set	IsMultiThread, __kgpc_program_var_IsMultiThread_5
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_IsLibrary_6
__kgpc_program_var_IsLibrary_6:
	.zero	4
.globl	IsLibrary
	.set	IsLibrary, __kgpc_program_var_IsLibrary_6
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_InOutRes_7
__kgpc_program_var_InOutRes_7:
	.zero	2
.globl	InOutRes
	.set	InOutRes, __kgpc_program_var_InOutRes_7
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_FileMode_8
__kgpc_program_var_FileMode_8:
	.zero	1
.globl	FileMode
	.set	FileMode, __kgpc_program_var_FileMode_8
	.popsection
	.pushsection .bss
	.align	8
.globl	__kgpc_program_var_ExitProc_9
__kgpc_program_var_ExitProc_9:
	.zero	8
.globl	ExitProc
	.set	ExitProc, __kgpc_program_var_ExitProc_9
	.popsection
	.pushsection .bss
	.align	8
.globl	__kgpc_program_var_ErrorAddr_10
__kgpc_program_var_ErrorAddr_10:
	.zero	8
.globl	ErrorAddr
	.set	ErrorAddr, __kgpc_program_var_ErrorAddr_10
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_ExitCode_11
__kgpc_program_var_ExitCode_11:
	.zero	4
.globl	ExitCode
	.set	ExitCode, __kgpc_program_var_ExitCode_11
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_FirstDotAtFileNameStartIsExtension_12
__kgpc_program_var_FirstDotAtFileNameStartIsExtension_12:
	.zero	4
.globl	FirstDotAtFileNameStartIsExtension
	.set	FirstDotAtFileNameStartIsExtension, __kgpc_program_var_FirstDotAtFileNameStartIsExtension_12
	.popsection
	.pushsection .bss
	.align	16
.globl	__kgpc_program_var_widestringmanager_13
__kgpc_program_var_widestringmanager_13:
	.zero	200
.globl	widestringmanager
	.set	widestringmanager, __kgpc_program_var_widestringmanager_13
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_DefaultTextLineBreakStyle_14
__kgpc_program_var_DefaultTextLineBreakStyle_14:
	.zero	4
.globl	DefaultTextLineBreakStyle
	.set	DefaultTextLineBreakStyle, __kgpc_program_var_DefaultTextLineBreakStyle_14
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_DefaultSystemCodePage_15
__kgpc_program_var_DefaultSystemCodePage_15:
	.zero	2
.globl	DefaultSystemCodePage
	.set	DefaultSystemCodePage, __kgpc_program_var_DefaultSystemCodePage_15
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_DefaultUnicodeCodePage_16
__kgpc_program_var_DefaultUnicodeCodePage_16:
	.zero	2
.globl	DefaultUnicodeCodePage
	.set	DefaultUnicodeCodePage, __kgpc_program_var_DefaultUnicodeCodePage_16
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_DefaultFileSystemCodePage_17
__kgpc_program_var_DefaultFileSystemCodePage_17:
	.zero	2
.globl	DefaultFileSystemCodePage
	.set	DefaultFileSystemCodePage, __kgpc_program_var_DefaultFileSystemCodePage_17
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_DefaultRTLFileSystemCodePage_18
__kgpc_program_var_DefaultRTLFileSystemCodePage_18:
	.zero	2
.globl	DefaultRTLFileSystemCodePage
	.set	DefaultRTLFileSystemCodePage, __kgpc_program_var_DefaultRTLFileSystemCodePage_18
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_UTF8CompareLocale_19
__kgpc_program_var_UTF8CompareLocale_19:
	.zero	2
.globl	UTF8CompareLocale
	.set	UTF8CompareLocale, __kgpc_program_var_UTF8CompareLocale_19
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_StdInputHandle_20
__kgpc_program_var_StdInputHandle_20:
	.zero	4
.globl	StdInputHandle
	.set	StdInputHandle, __kgpc_program_var_StdInputHandle_20
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_StdOutputHandle_21
__kgpc_program_var_StdOutputHandle_21:
	.zero	4
.globl	StdOutputHandle
	.set	StdOutputHandle, __kgpc_program_var_StdOutputHandle_21
	.popsection
	.pushsection .bss
	.align	8
.globl	__kgpc_program_var_MemoryManager_22
__kgpc_program_var_MemoryManager_22:
	.zero	96
.globl	MemoryManager
	.set	MemoryManager, __kgpc_program_var_MemoryManager_22
	.popsection
	.pushsection .bss
	.align	4
.globl	__kgpc_program_var_StdErrorHandle_23
__kgpc_program_var_StdErrorHandle_23:
	.zero	4
.globl	StdErrorHandle
	.set	StdErrorHandle, __kgpc_program_var_StdErrorHandle_23
	.popsection
	.pushsection .bss
	.align	16
.globl	__kgpc_program_var_Input_24
__kgpc_program_var_Input_24:
	.zero	672
.globl	Input
	.set	Input, __kgpc_program_var_Input_24
	.popsection
	.pushsection .bss
	.align	16
.globl	__kgpc_program_var_Output_25
__kgpc_program_var_Output_25:
	.zero	672
.globl	Output
	.set	Output, __kgpc_program_var_Output_25
	.popsection
	.pushsection .bss
	.align	16
.globl	__kgpc_program_var_StdOut_26
__kgpc_program_var_StdOut_26:
	.zero	672
.globl	StdOut
	.set	StdOut, __kgpc_program_var_StdOut_26
	.popsection
	.pushsection .bss
	.align	16
.globl	__kgpc_program_var_StdErr_27
__kgpc_program_var_StdErr_27:
	.zero	672
.globl	StdErr
	.set	StdErr, __kgpc_program_var_StdErr_27
	.popsection
	.pushsection .bss
	.align	16
.globl	__kgpc_program_var_ErrOutput_28
__kgpc_program_var_ErrOutput_28:
	.zero	672
.globl	ErrOutput
	.set	ErrOutput, __kgpc_program_var_ErrOutput_28
	.popsection
	.pushsection .bss
	.align	8
.globl	__kgpc_program_var_CurrentTM_29
__kgpc_program_var_CurrentTM_29:
	.zero	280
.globl	CurrentTM
	.set	CurrentTM, __kgpc_program_var_CurrentTM_29
	.popsection
	.pushsection .bss
	.align	8
.globl	__kgpc_program_var_NoThreadManager_30
__kgpc_program_var_NoThreadManager_30:
	.zero	280
.globl	NoThreadManager
	.set	NoThreadManager, __kgpc_program_var_NoThreadManager_30
	.popsection
	.pushsection .bss
	.align	8
.globl	__kgpc_program_var_LazyInitThreadingProcList_31
__kgpc_program_var_LazyInitThreadingProcList_31:
	.zero	8
.globl	LazyInitThreadingProcList
	.set	LazyInitThreadingProcList, __kgpc_program_var_LazyInitThreadingProcList_31
	.popsection
	.pushsection .bss
	.align	8
.globl	__kgpc_program_var_CurrentDLM_32
__kgpc_program_var_CurrentDLM_32:
	.zero	48
.globl	CurrentDLM
	.set	CurrentDLM, __kgpc_program_var_CurrentDLM_32
	.popsection
	.comm	__kgpc_program_var_s_33,8,8
.equ TDoubleRec__Bias, 1023
.equ TSingleRec__Bias, 127
.equ CP_ACP, 0
.equ CP_OEMCP, 1
.equ CP_UTF16, 1200
.equ CP_UTF16BE, 1201
.equ CP_UTF7, 65000
.equ CP_UTF8, 65001
.equ CP_ASCII, 20127
.equ CP_NONE, 65535
.equ vtInteger, 0
.equ vtBoolean, 1
.equ vtChar, 2
.equ vtExtended, 3
.equ vtString, 4
.equ vtPointer, 5
.equ vtPChar, 6
.equ vtObject, 7
.equ vtClass, 8
.equ vtWideChar, 9
.equ vtPWideChar, 10
.equ vtAnsiString, 11
.equ vtCurrency, 12
.equ vtVariant, 13
.equ vtInterface, 14
.equ vtWideString, 15
.equ vtInt64, 16
.equ vtQWord, 17
.equ vtUnicodeString, 18
.equ TextRecNameLength, 256
.equ TextRecBufSize, 256
.equ LineEnding, 10
.equ MaxPathLen, 4096
.equ AllFilesMask, 42
.equ FileNameCaseSensitive, 1
.equ FileNameCasePreserving, 1
.equ fmClosed, 55216
.equ fmInput, 55217
.equ fmOutput, 55218
.equ fmInOut, 55219
.equ ARG_MAX, 131072
.equ NAME_MAX, 255
.equ PATH_MAX, 4095
.equ SYS_NMLN, 65
.equ SIG_MAXSIG, 128
.equ PRIO_PROCESS, 0
.equ PRIO_PGRP, 1
.equ PRIO_USER, 2
.equ UTSNAME_LENGTH, 65
.equ RTL_SIGINT, 0
.equ RTL_SIGFPE, 1
.equ RTL_SIGSEGV, 2
.equ RTL_SIGILL, 3
.equ RTL_SIGBUS, 4
.equ RTL_SIGQUIT, 5
.equ RTL_SIGDEFAULT, -1
.globl	interlocked_exchange_add_i32_impl_li_li_li
interlocked_exchange_add_i32_impl_li_li_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$8, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -32(%rbp)
	movq	%r12, -40(%rbp)
	movq	%r13, -48(%rbp)
	movq	%r14, -56(%rbp)
	movq	%r15, -64(%rbp)
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	%rdx, -20(%rbp)
call kgpc_interlocked_exchange_add_i32

	movq	-32(%rbp), %rbx
	movq	-40(%rbp), %r12
	movq	-48(%rbp), %r13
	movq	-56(%rbp), %r14
	movq	-64(%rbp), %r15
	nop
	leave
	ret
.globl	interlocked_exchange_add_i32_int_impl_li_li_li
interlocked_exchange_add_i32_int_impl_li_li_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$8, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -32(%rbp)
	movq	%r12, -40(%rbp)
	movq	%r13, -48(%rbp)
	movq	%r14, -56(%rbp)
	movq	%r15, -64(%rbp)
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	%rdx, -20(%rbp)
call kgpc_interlocked_exchange_add_i32

	movq	-32(%rbp), %rbx
	movq	-40(%rbp), %r12
	movq	-48(%rbp), %r13
	movq	-56(%rbp), %r14
	movq	-64(%rbp), %r15
	nop
	leave
	ret
.globl	interlocked_exchange_add_i32_u32_impl_li_li_li
interlocked_exchange_add_i32_u32_impl_li_li_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$8, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -32(%rbp)
	movq	%r12, -40(%rbp)
	movq	%r13, -48(%rbp)
	movq	%r14, -56(%rbp)
	movq	%r15, -64(%rbp)
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	%rdx, -20(%rbp)
call kgpc_interlocked_exchange_add_i32

	movq	-32(%rbp), %rbx
	movq	-40(%rbp), %r12
	movq	-48(%rbp), %r13
	movq	-56(%rbp), %r14
	movq	-64(%rbp), %r15
	nop
	leave
	ret
.globl	interlocked_exchange_add_i64_impl_i64_i64_i64
interlocked_exchange_add_i64_impl_i64_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$8, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -32(%rbp)
	movq	%r12, -40(%rbp)
	movq	%r13, -48(%rbp)
	movq	%r14, -56(%rbp)
	movq	%r15, -64(%rbp)
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
call kgpc_interlocked_exchange_add_i64

	movq	-32(%rbp), %rbx
	movq	-40(%rbp), %r12
	movq	-48(%rbp), %r13
	movq	-56(%rbp), %r14
	movq	-64(%rbp), %r15
	nop
	leave
	ret
.globl	interlocked_exchange_add_ptr_impl_p_p_p
interlocked_exchange_add_ptr_impl_p_p_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$8, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -32(%rbp)
	movq	%r12, -40(%rbp)
	movq	%r13, -48(%rbp)
	movq	%r14, -56(%rbp)
	movq	%r15, -64(%rbp)
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
call kgpc_interlocked_exchange_add_ptr

	movq	-32(%rbp), %rbx
	movq	-40(%rbp), %r12
	movq	-48(%rbp), %r13
	movq	-56(%rbp), %r14
	movq	-64(%rbp), %r15
	nop
	leave
	ret
.globl	interlockedexchangeadd_li_li
interlockedexchangeadd_li_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$112, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$14, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -40(%rbp)
	movq	%r12, -48(%rbp)
	movq	%r13, -56(%rbp)
	movq	%r14, -64(%rbp)
	movq	%r15, -72(%rbp)
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	$0, %ebx
	movslq	%ebx, %rbx
	movq	%rbx, -28(%rbp)
	movq	-8(%rbp), %rbx
	movq	%rbx, -84(%rbp)
	movl	-12(%rbp), %ebx
	movq	%rbx, -92(%rbp)
	leaq	-28(%rbp), %rbx
	movq	%rbx, -100(%rbp)
	movq	-100(%rbp), %rdx
	movq	-92(%rbp), %rsi
	movq	-84(%rbp), %rdi
	movl	$0, %eax
	call	interlocked_exchange_add_i32_u32_impl_li_li_li
	movl	-28(%rbp), %ebx
	movl	%ebx, %ebx
	movq	%rbx, -20(%rbp)
	movl	-20(%rbp), %eax
	movq	-40(%rbp), %rbx
	movq	-48(%rbp), %r12
	movq	-56(%rbp), %r13
	movq	-64(%rbp), %r14
	movq	-72(%rbp), %r15
	nop
	leave
	ret
.globl	interlockedexchangeadd_i64_i64
interlockedexchangeadd_i64_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$96, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$12, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -40(%rbp)
	movq	%r12, -48(%rbp)
	movq	%r13, -56(%rbp)
	movq	%r14, -64(%rbp)
	movq	%r15, -72(%rbp)
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$0, %ebx
	movslq	%ebx, %rbx
	movq	%rbx, -32(%rbp)
	movq	-8(%rbp), %rbx
	movq	%rbx, -80(%rbp)
	movq	-16(%rbp), %rbx
	movq	%rbx, -88(%rbp)
	leaq	-32(%rbp), %rbx
	movq	%rbx, -96(%rbp)
	movq	-96(%rbp), %rdx
	movq	-88(%rbp), %rsi
	movq	-80(%rbp), %rdi
	movl	$0, %eax
	call	interlocked_exchange_add_i64_impl_i64_i64_i64
	movq	-32(%rbp), %rbx
	movq	%rbx, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	-40(%rbp), %rbx
	movq	-48(%rbp), %r12
	movq	-56(%rbp), %r13
	movq	-64(%rbp), %r14
	movq	-72(%rbp), %r15
	nop
	leave
	ret
.globl	interlockedexchangeadd_p_p
interlockedexchangeadd_p_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$96, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$12, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -40(%rbp)
	movq	%r12, -48(%rbp)
	movq	%r13, -56(%rbp)
	movq	%r14, -64(%rbp)
	movq	%r15, -72(%rbp)
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$0, %rbx
	movq	%rbx, -32(%rbp)
	movq	-8(%rbp), %rbx
	movq	%rbx, -80(%rbp)
	movq	-16(%rbp), %rbx
	movq	%rbx, -88(%rbp)
	leaq	-32(%rbp), %rbx
	movq	%rbx, -96(%rbp)
	movq	-96(%rbp), %rdx
	movq	-88(%rbp), %rsi
	movq	-80(%rbp), %rdi
	movl	$0, %eax
	call	interlocked_exchange_add_ptr_impl_p_p_p
	# EXIT statement
	movq	-32(%rbp), %rbx
	movq	%rbx, %rax
	movq	-40(%rbp), %rbx
	movq	-48(%rbp), %r12
	movq	-56(%rbp), %r13
	movq	-64(%rbp), %r14
	movq	-72(%rbp), %r15
	leave
	ret
	movq	-24(%rbp), %rax
	movq	-40(%rbp), %rbx
	movq	-48(%rbp), %r12
	movq	-56(%rbp), %r13
	movq	-64(%rbp), %r14
	movq	-72(%rbp), %r15
	nop
	leave
	ret
.globl	interlockedincrement_li
interlockedincrement_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$80, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$10, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %r12
	movq	%r12, -64(%rbp)
	movl	$1, %r12d
	movq	%r12, -72(%rbp)
	movq	-72(%rbp), %rsi
	movq	-64(%rbp), %rdi
	movl	$0, %eax
	movq	%rbx, -80(%rbp)
	call	interlockedexchangeadd_li_li
	movq	-80(%rbp), %rbx
	movq	%rax, %rbx
	incl	%ebx
	movl	%ebx, %ebx
	movq	%rbx, -16(%rbp)
	movl	-16(%rbp), %eax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	interlockedincrement_i64
interlockedincrement_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$80, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$10, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %r12
	movq	%r12, -64(%rbp)
	movl	$1, %r12d
	movq	%r12, -72(%rbp)
	movq	-72(%rbp), %rsi
	movq	-64(%rbp), %rdi
	movl	$0, %eax
	movq	%rbx, -80(%rbp)
	call	interlockedexchangeadd_i64_i64
	movq	-80(%rbp), %rbx
	movq	%rax, %rbx
	incq	%rbx
	movq	%rbx, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	interlockeddecrement_li
interlockeddecrement_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$80, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$10, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %r12
	movq	%r12, -64(%rbp)
	movl	$1, %r12d
	negl	%r12d
	movslq	%r12d, %r12
	movq	%r12, -72(%rbp)
	movq	-72(%rbp), %rsi
	movq	-64(%rbp), %rdi
	movl	$0, %eax
	movq	%rbx, -80(%rbp)
	call	interlockedexchangeadd_li_li
	movq	-80(%rbp), %rbx
	movq	%rax, %rbx
	subl	$1, %ebx
	movl	%ebx, %ebx
	movq	%rbx, -16(%rbp)
	movl	-16(%rbp), %eax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	interlockeddecrement_i64
interlockeddecrement_i64:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$80, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$10, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %r12
	movq	%r12, -64(%rbp)
	movl	$1, %r12d
	negl	%r12d
	movslq	%r12d, %r12
	movq	%r12, -72(%rbp)
	movq	-72(%rbp), %rsi
	movq	-64(%rbp), %rdi
	movl	$0, %eax
	movq	%rbx, -80(%rbp)
	call	interlockedexchangeadd_i64_i64
	movq	-80(%rbp), %rbx
	movq	%rax, %rbx
	subq	$1, %rbx
	movq	%rbx, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	paramstr_li
paramstr_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$80, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$10, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %r12d
	movq	%r12, -68(%rbp)
	movq	-68(%rbp), %rdi
	movl	$0, %eax
	movq	%rbx, -80(%rbp)
	call	kgpc_param_str
	movq	-80(%rbp), %rbx
	movq	%rax, %rbx
	leaq	-12(%rbp), %r12
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movl	$0, %eax
	call	kgpc_string_assign
	movq	-12(%rbp), %rax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	tobject__create_p
tobject__create_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$8, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %r12
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	tobject__classname_p
tobject__classname_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$368, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$46, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -288(%rbp)
	movq	%r12, -296(%rbp)
	movq	%r13, -304(%rbp)
	movq	%r14, -312(%rbp)
	movq	%r15, -320(%rbp)
	movq	%rdi, -272(%rbp)
	movq	%rsi, -8(%rbp)
	movq	-8(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	%rbx, -328(%rbp)
	movq	-328(%rbp), %rdi
	movl	$0, %eax
	movq	%r12, -336(%rbp)
	call	kgpc_class_name
	movq	-336(%rbp), %r12
	movq	%rax, %r12
	movq	%r12, -280(%rbp)
	movq	-280(%rbp), %r12
	movq	%r12, -344(%rbp)
	movq	$0, %r12
	movq	-344(%rbp), %rbx
	cmpq	%r12, %rbx
	jne	.L2
	.section	.rodata
.LC1:
	.string ""
	.text
	leaq	.LC1(%rip), %rbx
	leaq	-264(%rbp), %r12
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	$256, %rdx
	movl	$0, %eax
	call	kgpc_string_to_shortstring
	jmp	.L3
.L2:
	leaq	-264(%rbp), %r12
	movq	%r12, -352(%rbp)
	movq	-280(%rbp), %r12
	movq	%r12, -360(%rbp)
	movl	$256, %r12d
	movq	%r12, -368(%rbp)
	movq	-368(%rbp), %rdx
	movq	-360(%rbp), %rsi
	movq	-352(%rbp), %rdi
	movl	$0, %eax
	call	kgpc_string_to_shortstring
.L3:
	movq	-272(%rbp), %r12
	leaq	-264(%rbp), %rbx
	movq	$256, %r13
	movq	%r13, %rdx
	movq	%rbx, %rsi
	movq	%r12, %rdi
	movl	$0, %eax
	call	kgpc_move
	movq	-272(%rbp), %rax
	movq	-288(%rbp), %rbx
	movq	-296(%rbp), %r12
	movq	-304(%rbp), %r13
	movq	-312(%rbp), %r14
	movq	-320(%rbp), %r15
	nop
	leave
	ret
.globl	tobject__classparent_p
tobject__classparent_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$80, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$10, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %r13
	movq	-8(%rbp), %rbx
	movq	%rbx, -64(%rbp)
	movq	-64(%rbp), %rdi
	movl	$0, %eax
	movq	%r13, -72(%rbp)
	call	kgpc_class_parent
	movq	-72(%rbp), %r13
	movq	%rax, %r13
	movq	%r13, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	tobject__destroy_p
tobject__destroy_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$6, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -16(%rbp)
	movq	%r12, -24(%rbp)
	movq	%r13, -32(%rbp)
	movq	%r14, -40(%rbp)
	movq	%r15, -48(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %r13
	movq	-16(%rbp), %rbx
	movq	-24(%rbp), %r12
	movq	-32(%rbp), %r13
	movq	-40(%rbp), %r14
	movq	-48(%rbp), %r15
	nop
	leave
	ret
.globl	tobject__getinterface_p_u_tguid_u
tobject__getinterface_p_u_tguid_u:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$144, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$18, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -72(%rbp)
	movq	%r12, -80(%rbp)
	movq	%r13, -88(%rbp)
	movq	%r14, -96(%rbp)
	movq	%r15, -104(%rbp)
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %r13
	movq	%r13, -32(%rbp)
	movq	-24(%rbp), %r13
	movq	%r13, -40(%rbp)
	movq	-16(%rbp), %r13
	movq	$16, %rbx
	movq	%r13, %rsi
	leaq	-56(%rbp), %rdi
	movq	%rbx, %rdx
	movl	$0, %eax
	call	kgpc_move
	movq	-32(%rbp), %r13
	movq	-32(%rbp), %rbx
	movq	%rbx, -112(%rbp)
	leaq	-56(%rbp), %rbx
	movq	%rbx, -120(%rbp)
	movq	-40(%rbp), %rbx
	movq	%rbx, -128(%rbp)
	movq	-128(%rbp), %rdx
	movq	-120(%rbp), %rsi
	movq	-112(%rbp), %rdi
	movl	$0, %eax
	movq	%r13, -136(%rbp)
	call	kgpc_get_interface
	movq	-136(%rbp), %r13
	movq	%rax, %r13
	cmpl	$0, %r13d
	setne	%r13b
	movzbl	%r13b, %r13d
	movl	%r13d, %r13d
	movq	%r13, -64(%rbp)
	movl	-64(%rbp), %eax
	movq	-72(%rbp), %rbx
	movq	-80(%rbp), %r12
	movq	-88(%rbp), %r13
	movq	-96(%rbp), %r14
	movq	-104(%rbp), %r15
	nop
	leave
	ret
.globl	tinterfacedobject__queryinterface_p_u_tguid_u
tinterfacedobject__queryinterface_p_u_tguid_u:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$160, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$20, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -72(%rbp)
	movq	%r12, -80(%rbp)
	movq	%r13, -88(%rbp)
	movq	%r14, -96(%rbp)
	movq	%r15, -104(%rbp)
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %r13
	movq	%r13, -32(%rbp)
	movq	-24(%rbp), %r13
	movq	%r13, -40(%rbp)
	movq	-16(%rbp), %r13
	movq	$16, %rbx
	movq	%r13, %rsi
	leaq	-56(%rbp), %rdi
	movq	%rbx, %rdx
	movl	$0, %eax
	call	kgpc_move
	movq	-32(%rbp), %r13
	movq	-32(%rbp), %rbx
	movq	%rbx, -112(%rbp)
	leaq	-56(%rbp), %rbx
	movq	%rbx, %rsi
	leaq	-128(%rbp), %rdi
	movq	$16, %rdx
	movl	$0, %eax
	call	kgpc_move
	leaq	-128(%rbp), %rbx
	movq	%rbx, -136(%rbp)
	movq	-40(%rbp), %rbx
	movq	%rbx, -144(%rbp)
	movq	-144(%rbp), %rdx
	movq	-136(%rbp), %rsi
	movq	-112(%rbp), %rdi
	movl	$0, %eax
	movq	%r13, -152(%rbp)
	call	tobject__getinterface_p_u_tguid_u
	movq	-152(%rbp), %r13
	movq	%rax, %r13
	testl	%r13d, %r13d
	je	.L4
	movl	$0, %r13d
	movslq	%r13d, %r13
	movq	%r13, -64(%rbp)
	jmp	.L5
.L4:
	movq	$2147500034, %r13
	movslq	%r13d, %r13
	movq	%r13, -64(%rbp)
.L5:
	movl	-64(%rbp), %eax
	movq	-72(%rbp), %rbx
	movq	-80(%rbp), %r12
	movq	-88(%rbp), %r13
	movq	-96(%rbp), %r14
	movq	-104(%rbp), %r15
	nop
	leave
	ret
.globl	tinterfacedobject___addref_p
tinterfacedobject___addref_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$96, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$12, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %r13
	leaq	-8(%rbp), %rbx
	movq	(%rbx), %rbx
	addq	$16, %rbx
	movl	(%rbx), %r13d
	movl	%r13d, -64(%rbp)
	movl	$0, %r13d
	movl	-64(%rbp), %ebx
	cmpl	%r13d, %ebx
	jne	.L6
	leaq	-8(%rbp), %rbx
	movq	(%rbx), %rbx
	addq	$16, %rbx
	movq	%rbx, -72(%rbp)
	movl	$1, %ebx
	movq	-72(%rbp), %r13
	movl	%ebx, (%r13)
	# EXIT statement
	leaq	-8(%rbp), %rbx
	movq	(%rbx), %rbx
	addq	$16, %rbx
	movl	(%rbx), %r13d
	movl	%r13d, %eax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	leave
	ret
.L6:
	leaq	-8(%rbp), %rbx
	movq	(%rbx), %rbx
	addq	$16, %rbx
	movl	(%rbx), %r13d
	movl	%r13d, -80(%rbp)
	movl	$0, %r13d
	movl	-80(%rbp), %ebx
	cmpl	%r13d, %ebx
	jle	.L8
	leaq	-8(%rbp), %r13
	movq	(%r13), %r13
	addq	$16, %r13
	movq	%r13, -88(%rbp)
	movq	-88(%rbp), %rdi
	movl	$0, %eax
	movq	%rbx, -96(%rbp)
	call	interlockedincrement_li
	movq	-96(%rbp), %rbx
	movq	%rax, %rbx
	movslq	%ebx, %rbx
	movq	%rbx, -16(%rbp)
	jmp	.L9
.L8:
	leaq	-8(%rbp), %r13
	movq	(%r13), %r13
	addq	$16, %r13
	movl	(%r13), %ebx
	movslq	%ebx, %rbx
	movq	%rbx, -16(%rbp)
.L9:
	movl	-16(%rbp), %eax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	tinterfacedobject___release_p
tinterfacedobject___release_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$112, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$14, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rbx
	leaq	-8(%rbp), %r13
	movq	(%r13), %r13
	addq	$16, %r13
	movl	(%r13), %ebx
	movl	%ebx, -64(%rbp)
	movl	$1, %ebx
	movl	-64(%rbp), %r13d
	cmpl	%ebx, %r13d
	jne	.L10
	movl	$0, %r13d
	movslq	%r13d, %r13
	movq	%r13, -16(%rbp)
	jmp	.L11
.L10:
	leaq	-8(%rbp), %rbx
	movq	(%rbx), %rbx
	addq	$16, %rbx
	movl	(%rbx), %r13d
	movl	%r13d, -72(%rbp)
	movl	$0, %r13d
	movl	-72(%rbp), %ebx
	cmpl	%r13d, %ebx
	jle	.L12
	leaq	-8(%rbp), %r13
	movq	(%r13), %r13
	addq	$16, %r13
	movq	%r13, -80(%rbp)
	movq	-80(%rbp), %rdi
	movl	$0, %eax
	movq	%rbx, -88(%rbp)
	call	interlockeddecrement_li
	movq	-88(%rbp), %rbx
	movq	%rax, %rbx
	movslq	%ebx, %rbx
	movq	%rbx, -16(%rbp)
	jmp	.L13
.L12:
	# EXIT statement
	leaq	-8(%rbp), %r13
	movq	(%r13), %r13
	addq	$16, %r13
	movl	(%r13), %ebx
	movl	%ebx, %eax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	leave
	ret
.L13:
.L11:
	movl	-16(%rbp), %ebx
	movl	%ebx, -96(%rbp)
	movl	$0, %ebx
	movl	-96(%rbp), %r13d
	cmpl	%ebx, %r13d
	jg	.L14
	leaq	-8(%rbp), %r13
	movq	(%r13), %r13
	addq	$16, %r13
	movq	%r13, -104(%rbp)
	movl	$1, %r13d
	negl	%r13d
	movslq	%r13d, %r13
	movq	-104(%rbp), %rbx
	movl	%r13d, (%rbx)
	movq	-8(%rbp), %rbx
	movq	%rbx, -112(%rbp)
	movq	-112(%rbp), %rdi
	movl	$0, %eax
	movq	%rdi, %r11
	movq	(%r11), %r11
	movq	96(%r11), %r11
	call	*%r11
.L14:
	movl	-16(%rbp), %eax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	tobject__classinfo_p
tobject__classinfo_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$96, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$12, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rbx
	movq	-8(%rbp), %r12
	movq	%r12, -64(%rbp)
	movq	-64(%rbp), %rdi
	movl	$0, %eax
	movq	%rdi, %r11
	movq	(%r11), %r11
	movq	128(%r11), %r11
	movq	%r13, -72(%rbp)
	movq	%rbx, -80(%rbp)
	call	*%r11
	movq	-72(%rbp), %r13
	movq	-80(%rbp), %rbx
	movq	%rax, %r13
	movq	%r13, -88(%rbp)
	movq	-88(%rbp), %rdi
	movl	$0, %eax
	movq	%rdi, %r11
	movq	(%r11), %r11
	movq	120(%r11), %r11
	movq	%rbx, -96(%rbp)
	call	*%r11
	movq	-96(%rbp), %rbx
	movq	%rax, %rbx
	movq	%rbx, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	tobject__classtype_p
tobject__classtype_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$8, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -24(%rbp)
	movq	%r12, -32(%rbp)
	movq	%r13, -40(%rbp)
	movq	%r14, -48(%rbp)
	movq	%r15, -56(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rbx
	movq	-8(%rbp), %r13
	movq	(%r13), %rbx
	movq	%rbx, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rbx
	movq	-32(%rbp), %r12
	movq	-40(%rbp), %r13
	movq	-48(%rbp), %r14
	movq	-56(%rbp), %r15
	nop
	leave
	ret
.globl	tobject__inheritsfrom_p_p
tobject__inheritsfrom_p_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$112, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$14, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -32(%rbp)
	movq	%r12, -40(%rbp)
	movq	%r13, -48(%rbp)
	movq	%r14, -56(%rbp)
	movq	%r15, -64(%rbp)
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rbx
	movq	-8(%rbp), %r12
	movq	%r12, -72(%rbp)
	movq	-72(%rbp), %rdi
	movl	$0, %eax
	movq	%rdi, %r11
	movq	(%r11), %r11
	movq	128(%r11), %r11
	movq	%r13, -80(%rbp)
	movq	%rbx, -88(%rbp)
	call	*%r11
	movq	-80(%rbp), %r13
	movq	-88(%rbp), %rbx
	movq	%rax, %r13
	movq	%r13, -96(%rbp)
	movq	-16(%rbp), %r13
	movq	%r13, -104(%rbp)
	movq	-104(%rbp), %rsi
	movq	-96(%rbp), %rdi
	movl	$0, %eax
	movq	%rdi, %r11
	movq	(%r11), %r11
	movq	136(%r11), %r11
	movq	%rbx, -112(%rbp)
	call	*%r11
	movq	-112(%rbp), %rbx
	movq	%rax, %rbx
	movl	%ebx, %ebx
	movq	%rbx, -24(%rbp)
	movl	-24(%rbp), %eax
	movq	-32(%rbp), %rbx
	movq	-40(%rbp), %r12
	movq	-48(%rbp), %r13
	movq	-56(%rbp), %r14
	movq	-64(%rbp), %r15
	nop
	leave
	ret
.globl	tobject__freeinstance_p
tobject__freeinstance_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$6, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -16(%rbp)
	movq	%r12, -24(%rbp)
	movq	%r13, -32(%rbp)
	movq	%r14, -40(%rbp)
	movq	%r15, -48(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rbx
	movq	-16(%rbp), %rbx
	movq	-24(%rbp), %r12
	movq	-32(%rbp), %r13
	movq	-40(%rbp), %r14
	movq	-48(%rbp), %r15
	nop
	leave
	ret
.globl	tobject__tostring_p
tobject__tostring_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$96, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$12, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -32(%rbp)
	movq	%r12, -40(%rbp)
	movq	%r13, -48(%rbp)
	movq	%r14, -56(%rbp)
	movq	%r15, -64(%rbp)
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rbx
	movq	-8(%rbp), %r13
	movq	%r13, -72(%rbp)
	movq	-72(%rbp), %rdi
	movl	$0, %eax
	movq	%rbx, -80(%rbp)
	call	kgpc_class_name
	movq	-80(%rbp), %rbx
	movq	%rax, %rbx
	movq	%rbx, -24(%rbp)
	movq	-24(%rbp), %rbx
	movq	%rbx, -88(%rbp)
	movq	$0, %rbx
	movq	-88(%rbp), %r13
	cmpq	%rbx, %r13
	jne	.L16
	.section	.rodata
.LC2:
	.string ""
	.text
	leaq	.LC2(%rip), %r13
	leaq	-16(%rbp), %rbx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movl	$0, %eax
	call	kgpc_string_assign
	jmp	.L17
.L16:
	movq	-24(%rbp), %rbx
	leaq	-16(%rbp), %r13
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movl	$0, %eax
	call	kgpc_string_assign
.L17:
	movq	-16(%rbp), %rax
	movq	-32(%rbp), %rbx
	movq	-40(%rbp), %r12
	movq	-48(%rbp), %r13
	movq	-56(%rbp), %r14
	movq	-64(%rbp), %r15
	nop
	leave
	ret
.globl	TestSingle
TestSingle:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$8, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rbx, -8(%rbp)
	movq	%r12, -16(%rbp)
	movq	%r13, -24(%rbp)
	movq	%r14, -32(%rbp)
	movq	%r15, -40(%rbp)
	movl	$47, %r13d
	movl	%r13d, __kgpc_program_var_DirectorySeparator_1(%rip)
	movl	$0, %r13d
	movl	%r13d, __kgpc_program_var_DriveSeparator_2(%rip)
	movl	$58, %r13d
	movl	%r13d, __kgpc_program_var_PathSeparator_3(%rip)
	movl	$46, %r13d
	movl	%r13d, __kgpc_program_var_ExtensionSeparator_4(%rip)
	movl	$0, %r13d
	movl	%r13d, __kgpc_program_var_IsMultiThread_5(%rip)
	movl	$0, %r13d
	movl	%r13d, __kgpc_program_var_IsLibrary_6(%rip)
	movl	$2, %r13d
	movl	%r13d, __kgpc_program_var_FileMode_8(%rip)
	movq	$0, %r13
	movq	%r13, __kgpc_program_var_ExitProc_9(%rip)
	movq	$0, %r13
	movq	%r13, __kgpc_program_var_ErrorAddr_10(%rip)
	movl	$0, %r13d
	movl	%r13d, __kgpc_program_var_ExitCode_11(%rip)
	movl	$0, %ebx
	movq	%rbx, -48(%rbp)
	movslq	-48(%rbp), %rdi
	movl	$0, %eax
	movq	%r13, -56(%rbp)
	call	paramstr_li
	movq	-56(%rbp), %r13
	movq	%rax, %r13
	leaq	__kgpc_program_var_s_33(%rip), %rbx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movl	$0, %eax
	call	kgpc_string_assign
	movq	-8(%rbp), %rbx
	movq	-16(%rbp), %r12
	movq	-24(%rbp), %r13
	movq	-32(%rbp), %r14
	movq	-40(%rbp), %r15
	nop
	leave
	ret

.data
.globl	INITFINAL
INITFINAL:
	.long	0
.globl	FPC_RESOURCESTRINGTABLES
FPC_RESOURCESTRINGTABLES:
	.quad	0
	.section	.text
	.globl	main
.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	call	kgpc_init_args
	call	TestSingle
	xor	%edi, %edi
	call	exit
	nop
	leave
	ret
	.section	.comment
	.string	"KGPC: 0.0.0"
	.section	.note.GNU-stack,"",@progbits
