{ Test unit-qualified type names like unitname.typename }
program unit_qualified_type_name;

uses ctypes;

var
  { Test using unit-qualified type name }
  x: ctypes.cint;
  y: cint;
  
begin
  x := 42;
  y := x + 10;
  writeln('x = ', x);
  writeln('y = ', y);
  writeln('Unit qualified type names work!');
end.
