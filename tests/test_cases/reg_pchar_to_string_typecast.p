program reg_pchar_to_string_typecast;

{$mode objfpc}
{$H+}

var
  p: PChar;
  s: String;
begin
  p := PChar('hi');
  s := String(p);
  writeln(s);
end.
