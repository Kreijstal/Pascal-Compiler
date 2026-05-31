program tdd_char_typecast_string_concat; {$mode objfpc}{$H+}
var
  s: ansistring;
  x: byte;
begin
  x := 66;
  s := char(65) + #0;
  writeln(length(s), ' ', s[1]);
  s := char(byte(x)) + #0;
  writeln(length(s), ' ', s[1]);
  s := char(67) + 'X';
  writeln(length(s), ' ', s);
end.
