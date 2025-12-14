{ Test AnsiChar built-in type support }
{ Required for FPC bootstrap - objpas.pp uses AnsiChar }
program fpc_bootstrap_ansichar_type;

var
  c: AnsiChar;
begin
  c := 'A';
  WriteLn('AnsiChar = ', c);
end.
