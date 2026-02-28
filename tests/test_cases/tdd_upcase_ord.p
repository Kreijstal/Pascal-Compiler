program tdd_upcase_ord;
var
  s: string;
  b: Byte;
begin
  s := 'hello';
  b := Ord(UpCase(s[1])) - 64;
  writeln(b);
end.
