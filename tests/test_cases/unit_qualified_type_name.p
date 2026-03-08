{ Test unit-qualified type names like unitname.typename }
program unit_qualified_type_name;

uses SysUtils;

var
  { Test using unit-qualified type name }
  x: SysUtils.TBytes;
  y: SysUtils.TBytes;
  
begin
  SetLength(x, 2);
  SetLength(y, 2);
  x[0] := 42;
  x[1] := 0;
  y := x;
  y[1] := x[0] + 10;
  writeln('x = ', x[0]);
  writeln('y = ', y[1]);
  writeln('Unit qualified type names work!');
end.
