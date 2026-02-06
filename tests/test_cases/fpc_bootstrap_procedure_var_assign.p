program fpc_bootstrap_procedure_var_assign;
{ Test case for procedure variable assignment bug.
  FPC accepts this code, KGPC incorrectly rejects it with type mismatch.
  Related to OnBeep := @SysBeep in sysutils.pp }
var
  OnShowException: procedure(Obj: Pointer; Addr: Pointer);
procedure MyHandler(Obj: Pointer; Addr: Pointer);
begin
end;
begin
  OnShowException := @MyHandler;
  writeln('OK');
end.
