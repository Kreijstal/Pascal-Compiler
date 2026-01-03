unit fpc_bootstrap_objpas_tendian_unit;

{$mode objfpc}

interface

type
  TLocalEndian = ObjPas.TEndian;

function EndianToString(e: ObjPas.TEndian): string;

implementation

function EndianToString(e: ObjPas.TEndian): string;
begin
  if e = ObjPas.TEndian.Little then
    EndianToString := 'little'
  else
    EndianToString := 'big';
end;

end.
