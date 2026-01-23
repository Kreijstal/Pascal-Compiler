program reg_overload_numeric_cast_forward;

{$mode objfpc}
{$H+}

type
  TBytes = array of Byte;

function Make(A: Cardinal; B: Word; C: Word;
  D, E, F, G, H, I, J, K: Byte): Integer; overload;
begin
  Result := 2;
end;

function Make(A: Integer; B: SmallInt; C: SmallInt; const D: TBytes): Integer; overload;
begin
  if Length(D) <> 8 then
    Result := -1
  else
    Result := Make(Cardinal(A), Word(B), Word(C),
      D[0], D[1], D[2], D[3], D[4], D[5], D[6], D[7]);
end;

var
  Bytes: TBytes;
  i: Integer;
begin
  SetLength(Bytes, 8);
  for i := 0 to 7 do
    Bytes[i] := i;
  writeln(Make(1, 2, 3, Bytes));
end.
