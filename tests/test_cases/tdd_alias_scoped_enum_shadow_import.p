program tdd_alias_scoped_enum_shadow_import;

{$mode objfpc}

uses tdd_alias_scoped_enum_shadow_import_unit, objpas;

type
  TEndian = ObjPas.TEndian;

const
  CLocalEndian = TEndian.Little;

begin
  if CLocalEndian = TEndian.Little then
    writeln('little')
  else
    writeln('big');
end.
