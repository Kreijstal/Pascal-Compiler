program TestAssignPAnsiChar;
{ Test that Assign procedure works with PAnsiChar parameter.
  This is required for FPC bootstrap - objpas.pp uses this overload. }
var
  t: Text;
  p: PAnsiChar;
begin
  p := 'test_output.txt';
  Assign(t, p);
  Rewrite(t);
  WriteLn(t, 'PAnsiChar assign works');
  Close(t);
  WriteLn('OK');
end.
