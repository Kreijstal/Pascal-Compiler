{ Test: Anonymous enum in set type declaration }
{ BUG: TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase) not parsed }
{$mode objfpc}
program set_anonymous_enum;

type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

var
  F: TReplaceFlags;
begin
  F := [rfReplaceAll];
  if rfReplaceAll in F then
    WriteLn('OK');
end.
