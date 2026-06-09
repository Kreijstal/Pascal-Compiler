{ Regression: assigning through a typecast over an untyped `var` parameter
  (as in RTL FreeAndNil's `pointer(obj):=nil`) must emit a full 64-bit store,
  not a 32-bit one. A truncated store clears only the low 32 bits, leaving the
  high 32 bits stale -- invisible on Linux (heap high bits = 0) but fatal on
  win64 (heap at 0x1xx00000000), which crashed the FPC 3.2.2 self-cycle in
  texportlibwin.Destroy (double-free of a half-niled list pointer). }
{$mode objfpc}{$H+}
program fpc_bootstrap_untyped_var_pointer_store;
procedure ClearIt(var obj);
begin
  Pointer(obj) := nil;
end;
var
  x: Pointer;
begin
  x := Pointer(PtrUInt($1234567812345678));
  ClearIt(x);
  if x = nil then
    WriteLn('niled')
  else
    WriteLn('truncated');
end.
