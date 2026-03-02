{$mode objfpc}
program tdd_ord_upcase_char_result;

type
  PathStr = string[255];

function GetDriveIDFromLetter(const ADrive: PathStr): Byte;
begin
  if Length(ADrive) = 0 then
    Result := 0
  else
    Result := Ord(UpCase(ADrive[1])) - 64;
end;

function CIComp(a, b: PAnsiChar): SizeInt;
begin
  CIComp := Ord(UpCase(a^)) - Ord(UpCase(b^));
end;

var
  c1, c2: AnsiString;
begin
  c1 := 'c';
  c2 := 'D';
  writeln(GetDriveIDFromLetter(c1));
  writeln(CIComp(PAnsiChar(c1), PAnsiChar(c2)));
end.
