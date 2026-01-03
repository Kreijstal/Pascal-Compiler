{ Test TEndian type from ObjPas unit via {$mode objfpc} }
program fpc_bootstrap_objpas_tendian;

{$mode objfpc}

uses fpc_bootstrap_objpas_tendian_unit;

var
  e: TEndian;

begin
  e := Little;
  writeln('unit: ', EndianToString(e));
  if e = Little then
    writeln('little')
  else
    writeln('big');
end.
