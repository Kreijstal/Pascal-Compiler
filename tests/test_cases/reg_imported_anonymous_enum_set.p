{$mode objfpc}
program reg_imported_anonymous_enum_set;

uses reg_imported_anonymous_enum_set_unit;

begin
  if HasIgnoreCase([rfIgnoreCase]) then
    WriteLn('OK');
end.
