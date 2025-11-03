program minimal_fpc_test;

type
  TProc = procedure(x: Integer);

procedure MyProc(x: Integer);
begin
  WriteLn('MyProc called with: ', x);
end;

var
  p: TProc;

begin
  p := MyProc;
  p(42);
  WriteLn('Test completed');
end.
