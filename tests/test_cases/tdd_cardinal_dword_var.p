{$mode objfpc}
program TestCardinalDWordVar;
{
  Test that Cardinal and DWord are treated as the same type.
  Both should be unsigned 32-bit integers (LongWord).
  This is crucial for VAR/OUT parameter matching in overload resolution.
  
  NOTE: This test uses the system unit's type definitions - no local overrides.
}

function TryStrToDWord(const s: string; var C: DWord): Boolean;
begin
  Result := True;
  C := 123;
end;

function TryStrToUInt(const s: string; out C: Cardinal): Boolean;
begin
  { This should work because Cardinal and DWord are both LongWord }
  TryStrToUInt := TryStrToDWord(s, C);
end;

var
  C: Cardinal;
  res: Boolean;
begin
  res := TryStrToUInt('123', C);
  if res then
    WriteLn('PASS: ', C)
  else
    WriteLn('FAIL');
end.
