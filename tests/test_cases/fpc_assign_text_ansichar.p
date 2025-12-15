program TestAssignAnsiChar;
{ Test that Assign procedure works with AnsiChar (single character) parameter.
  This is required for FPC bootstrap - objpas.pp uses this overload.
  Note: This test exposes a KGPC bug where adding an Assign(Text, AnsiChar)
  overload breaks resolution for Assign(Text, String). }
var
  t: Text;
  c: AnsiChar;
begin
  c := 'x';
  Assign(t, c);
  WriteLn('OK');
end.
