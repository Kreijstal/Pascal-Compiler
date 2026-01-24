{$mode objfpc}
program tdd_bool_eq_arg;

type
  TUseBoolStrs = (tbsFalse, tbsTrue);

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
begin
  if B then
    Result := 'T'
  else
    Result := 'F';
  if UseBoolStrs then
    Result := Result + '1'
  else
    Result := Result + '0';
end;

var
  UseBoolStrs: TUseBoolStrs;
begin
  UseBoolStrs := tbsTrue;
  WriteLn(BoolToStr(True, UseBoolStrs = tbsTrue));
end.
