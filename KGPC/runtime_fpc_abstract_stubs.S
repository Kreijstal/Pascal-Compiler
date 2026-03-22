/* Residual abstract method stubs for FPC RTL compatibility.
 *
 * Most interface abstract method stubs are now emitted dynamically by the
 * codegen (weak .set aliases to __kgpc_abstract_method_error) when it
 * processes interface type declarations that have method_templates.
 *
 * The symbols below remain because the compiler does not yet handle their
 * root causes:
 *   - IObserver / IInterfaceList: interface types whose method_templates
 *     are not populated by the parser (only the type shell is captured).
 *   - TMarshaller.TDeferBase.Done: abstract method in an old-style Pascal
 *     object; KGPC generates direct calls instead of VMT dispatch.
 *   - TMarshal.UnfixArray<TPtrWrapper>: nested generic specialisation chain
 *     that the pre-semcheck instantiation pass cannot resolve.
 *
 * Each stub should be removed once its root cause is fixed in the compiler.
 */
.text

/* IObserver.GetActive — interface method_templates not populated by parser */
.globl iobserver__getactive_p
iobserver__getactive_p:
    jmp __kgpc_abstract_method_error

/* IInterfaceList.Add — interface method_templates not populated by parser */
.globl iinterfacelist__add_p_u_iunknown
iinterfacelist__add_p_u_iunknown:
    jmp __kgpc_abstract_method_error

/* TMarshaller.TDeferBase.Done — old-style object abstract method, direct call */
.globl tmarshaller.tdeferbase__done_u_tmarshaller_tdeferbase
tmarshaller.tdeferbase__done_u_tmarshaller_tdeferbase:
    jmp __kgpc_abstract_method_error

/* TMarshal.UnfixArray<TPtrWrapper> — unresolved nested generic specialisation */
.globl tmarshal__unfixarray_u_tptrwrapper
tmarshal__unfixarray_u_tptrwrapper:
    ret

