{$mode objfpc}
program qualified_type_reference_tendian;

var
  E: ObjPas.TEndian;

begin
  E := ObjPas.TEndian(0);
  writeln(ord(E));
end.
