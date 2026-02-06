{$mode objfpc}
program tdd_set_const_union;

function IsAlphaNum(C: Char): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNum = Alpha + ['0'..'9'];
begin
  Result := C in AlphaNum;
end;

begin
  if IsAlphaNum('9') then
    WriteLn('digit=ok')
  else
    WriteLn('digit=bad');

  if IsAlphaNum('!') then
    WriteLn('punct=bad')
  else
    WriteLn('punct=ok');
end.
