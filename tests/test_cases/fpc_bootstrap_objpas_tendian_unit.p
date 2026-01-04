{ Helper unit for TEndian test }
unit fpc_bootstrap_objpas_tendian_unit;

{$mode objfpc}

interface

type
  TLocalEndian = TEndian;

function EndianToString(e: TEndian): string;

implementation

function EndianToString(e: TEndian): string;
begin
  if e = Little then
    EndianToString := 'little'
  else
    EndianToString := 'big';
end;

end.
