{$mode objfpc}
{ TDD test: ShortString should be implicitly compatible with string parameter }
program tdd_shortstring_to_string;

type
  ShortString255 = string[255];

function StrToIntWrapper(const S: string): Integer;
begin
  if S = '42' then
    Result := 42
  else
    Result := 0;
end;

var
  SS: ShortString255;
  I: Integer;
begin
  SS := '42';
  I := StrToIntWrapper(SS);
  WriteLn(I);
end.
