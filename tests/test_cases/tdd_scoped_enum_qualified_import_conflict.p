program tdd_scoped_enum_qualified_import_conflict;

{$mode objfpc}

uses tdd_alias_scoped_enum_shadow_import_unit, objpas;

const
  CLocalEndian = ObjPas.TEndian.Little;

begin
  if CLocalEndian = ObjPas.TEndian.Little then
    writeln('little')
  else
    writeln('big');
end.
