program OrdCharComprehensive;
var
  c1, c2, c3, c4: char;
  x1, x2, x3, x4: integer;
begin
  c1 := 'a';  x1 := ord(c1); writeln(x1);
  c2 := 'A';  x2 := ord(c2); writeln(x2);
  c3 := #0;   x3 := ord(c3); writeln(x3);
  c4 := #255; x4 := ord(c4); writeln(x4);
end.