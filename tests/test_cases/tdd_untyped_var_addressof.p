{$mode objfpc}
program tdd_untyped_var_addressof;

type
  TPair = record
    A: Word;
    B: Word;
  end;

  TBytes = array[0..7] of Byte;

procedure FillBytes(var Buf; Count: Integer; Seed: Byte);
var
  P: PByte;
  I: Integer;
begin
  P := @Buf;
  for I := 0 to Count - 1 do
    P[I] := Seed + Byte(I * 3);
end;

function SumBytes(var Buf; Count: Integer): Integer;
var
  P: PByte;
  I: Integer;
  Total: Integer;
begin
  P := @Buf;
  Total := 0;
  for I := 0 to Count - 1 do
    Total := Total + P[I];
  Result := Total;
end;

var
  Pair: TPair;
  Bytes: TBytes;
  Sum1, Sum2: Integer;

begin
  FillBytes(Pair, SizeOf(Pair), 5);
  FillBytes(Bytes, SizeOf(Bytes), 11);
  Sum1 := SumBytes(Pair, SizeOf(Pair));
  Sum2 := SumBytes(Bytes, SizeOf(Bytes));
  WriteLn('pair=', Pair.A, ',', Pair.B);
  WriteLn('sum1=', Sum1);
  WriteLn('sum2=', Sum2);
end.
