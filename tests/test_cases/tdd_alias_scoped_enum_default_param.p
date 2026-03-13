program tdd_alias_scoped_enum_default_param;

{$mode objfpc}

uses objpas;

type
  TLocalEndian = ObjPas.TEndian;

const
  CLocalEndian = TLocalEndian.Little;

function EndianName(E: TLocalEndian = CLocalEndian): string;
begin
  if E = TLocalEndian.Little then
    EndianName := 'little'
  else
    EndianName := 'big';
end;

begin
  writeln(EndianName);
  writeln(EndianName(TLocalEndian.Big));
end.
