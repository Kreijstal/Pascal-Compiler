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

