{$mode objfpc}
{ Test case for FPC bootstrap compatibility: external name variable modifiers }
program TestExternalVar;

{ Declare an external variable that is defined elsewhere (simulated) }
var
  test_value: longint; external name 'kgpc_test_external_value';

begin
  WriteLn('test_value address defined elsewhere');
end.
