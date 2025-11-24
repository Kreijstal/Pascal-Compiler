program missing_pchar_conv;
uses SysUtils;
var
  p: PAnsiChar;
  s: AnsiString;
begin
  p := 'hello';
  s := p;
  writeln(s);
end.
