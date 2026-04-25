program regr_set_typed_const_in_unit;

{$mode objfpc}

uses unit_regr_set_typed_const;

begin
  if HasRed then
    writeln('OK')
  else
    writeln('BUG');
end.
