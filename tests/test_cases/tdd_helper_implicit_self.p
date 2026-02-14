program tdd_helper_implicit_self;

{$mode objfpc}

type
  TWordHelper = type helper for Word
    procedure PutBit(Index: Integer; Bit: Boolean);
    procedure PutNibble(Index: Integer; Value: Byte);
    procedure PutByte(Index: Integer; Value: Byte);
    function BitCount: Integer;
  end;

procedure TWordHelper.PutBit(Index: Integer; Bit: Boolean);
begin
  if Bit then
    Self := Self or (Word(1) shl Index)
  else
    Self := Self and not (Word(1) shl Index);
end;

procedure TWordHelper.PutNibble(Index: Integer; Value: Byte);
var
  I: Integer;
begin
  for I := 0 to 3 do
    PutBit(Index + I, ((Value shr I) and 1) <> 0);
end;

procedure TWordHelper.PutByte(Index: Integer; Value: Byte);
var
  I: Integer;
begin
  for I := 0 to 7 do
    PutBit(Index + I, ((Value shr I) and 1) <> 0);
end;

function TWordHelper.BitCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 15 do
    if (Self and (Word(1) shl I)) <> 0 then
      Inc(Result);
end;

var
  V1: Word;
  V2: Word;
  V3: Word;
begin
  V1 := 0;
  V1.PutNibble(0, $D);
  V1.PutBit(5, True);
  Writeln('v1=', V1, ' bits=', V1.BitCount);

  V2 := 0;
  V2.PutNibble(0, $D);
  V2.PutNibble(4, $A);
  Writeln('v2=', V2, ' bits=', V2.BitCount);

  V3 := 0;
  V3.PutByte(8, $FF);
  V3.PutNibble(4, $A);
  V3.PutNibble(0, $D);
  Writeln('v3=', V3, ' bits=', V3.BitCount);
end.
