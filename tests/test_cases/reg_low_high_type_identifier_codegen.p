program reg_low_high_type_identifier_codegen;
{$mode objfpc}

type
  TKind = (ka, kb, kc);

var
  kinds: set of TKind;
begin
  kinds := [Low(TKind)..High(TKind)];
  if (ka in kinds) and (kc in kinds) then
    WriteLn('ok');
end.
