{$mode objfpc}
program tdd_alias_scoped_enum_literal;

type
  TLocalEndian = ObjPas.TEndian;

const
  CEndian = TLocalEndian.Little;

var
  E: TLocalEndian;

begin
  E := CEndian;
  if E = TLocalEndian.Little then
    writeln('little')
  else
    writeln('big');
end.
