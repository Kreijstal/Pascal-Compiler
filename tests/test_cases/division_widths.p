Program division_widths;
var
  a, b, c: Integer;
  la, lb, q: Longint;
begin
  a := 42;
  b := -7;
  c := a div b;
  writeln(c);

  la := 9223372036854775800;
  lb := -3;
  q := la div lb;
  writeln(q);
end.
