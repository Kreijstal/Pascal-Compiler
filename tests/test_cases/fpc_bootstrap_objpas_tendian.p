program fpc_bootstrap_objpas_tendian;

{$mode objfpc}

uses fpc_bootstrap_objpas_tendian_unit;

var
  e: ObjPas.TEndian;

begin
  e := ObjPas.TEndian.Little;
  writeln('unit: ', EndianToString(e));
  if e = ObjPas.TEndian.Little then
    writeln('little')
  else
    writeln('big');
end.
