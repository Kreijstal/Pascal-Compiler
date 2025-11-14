program pointer_basic;
var
  p: ^Integer;
  a: Integer;
  c: Char;
  pc: ^Char;
begin
  a := 7;
  p := @a;
  writeln(p^);
  Inc(p);
  Dec(p);
  writeln(p^);
  c := 'A';
  pc := @c;
  writeln(pc^);
  Inc(pc);
end.
