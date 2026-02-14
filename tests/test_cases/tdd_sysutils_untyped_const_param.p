{$codepage utf8}
program tdd_sysutils_untyped_const_param;

{$mode objfpc}
{$H+}

uses
  SysUtils;

type
  TBlock = record
    A: LongInt;
    B: array[0..3] of Word;
    C: array[0..1] of Byte;
  end;

procedure ComputeDigest(const Data; Count: Integer; var OutSum: LongInt; var OutXor: LongInt);
var
  P: PByte;
  I: Integer;
  Sum: LongInt;
  XorValue: LongInt;
begin
  P := @Data;
  Sum := 0;
  XorValue := 0;
  for I := 0 to Count - 1 do
  begin
    Sum := Sum + P[I] * (I + 1);
    XorValue := XorValue xor (P[I] shl (I mod 8));
  end;
  OutSum := Sum;
  OutXor := XorValue;
end;

function MixBytes(const Data; Count: Integer): QWord;
var
  P: PByte;
  I: Integer;
  Acc: QWord;
begin
  P := @Data;
  Acc := $9E3779B97F4A7C15;
  for I := 0 to Count - 1 do
    Acc := (Acc xor P[I]) * $100000001B3;
  Result := Acc;
end;

procedure FillBuffer(var Buffer: array of Byte);
var
  I: Integer;
begin
  for I := Low(Buffer) to High(Buffer) do
    Buffer[I] := (I * 7 + 3) and $FF;
end;

var
  Buffer: array[0..15] of Byte;
  Block: TBlock;
  SumA: LongInt;
  XorA: LongInt;
  SumB: LongInt;
  XorB: LongInt;
  MixA: QWord;
  MixB: QWord;
begin
  FillBuffer(Buffer);

  Block.A := $12345678;
  Block.B[0] := $1111;
  Block.B[1] := $2222;
  Block.B[2] := $3333;
  Block.B[3] := $4444;
  Block.C[0] := $AA;
  Block.C[1] := $BB;

  ComputeDigest(Buffer, Length(Buffer), SumA, XorA);
  ComputeDigest(Block, SizeOf(Block), SumB, XorB);

  MixA := MixBytes(Buffer, Length(Buffer));
  MixB := MixBytes(Block, SizeOf(Block));

  Writeln('sumA=', SumA);
  Writeln('xorA=', XorA);
  Writeln('sumB=', SumB);
  Writeln('xorB=', XorB);
  Writeln('mixA=', IntToHex(MixA, 16));
  Writeln('mixB=', IntToHex(MixB, 16));
end.
