{$mode objfpc}
{ TDD test: ShortString should be implicitly compatible with string parameter }
program tdd_shortstring_to_string;

type
  ShortString255 = string[255];

function TakeString(const S: string): Integer;
begin
  { Just return a fixed value to test that the call succeeds }
  Result := 42;
end;

var
  SS: ShortString255;
  I: Integer;
begin
  SS := 'hello';
  I := TakeString(SS);
  WriteLn(I);
end.
