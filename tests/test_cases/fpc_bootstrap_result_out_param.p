program fpc_bootstrap_result_out_param;
{$mode objfpc}
{ Test: Using Result as out parameter in function }
{ FPC: Should compile and output 42 }
{ KGPC: Fails with "no return statement declared in function body" }
{ This pattern is used in FPC's sysutils GUID functions }

procedure SetValue(out x: Integer);
begin
  x := 42;
end;

function GetValue: Integer;
begin
  SetValue(Result);  { Result used as out parameter }
end;

begin
  writeln(GetValue);
end.
