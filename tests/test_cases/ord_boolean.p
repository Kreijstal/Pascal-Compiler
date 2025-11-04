program OrdBoolean;
var
  x, y, z: integer;
  b: boolean;
begin
  x := ord(true);
  y := ord(false);
  b := true;
  z := ord(b);
  write(x);
  write(y);
  writeln(z);
end.
