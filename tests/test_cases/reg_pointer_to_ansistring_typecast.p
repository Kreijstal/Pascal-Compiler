program reg_pointer_to_ansistring_typecast;

{$mode objfpc}
{$H+}

var
  p: Pointer;
  s: AnsiString;
begin
  s := 'hi';
  p := Pointer(s);
  s := AnsiString(p);
  writeln(s);
end.
