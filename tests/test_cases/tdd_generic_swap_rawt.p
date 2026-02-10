{$mode objfpc}
program tdd_generic_swap_rawt;

generic procedure Swap<T>(var lhs, rhs: T);
type
  RawT = array[0 .. sizeof(T) - 1] of byte;
var
  tmp: RawT;
begin
  tmp := RawT(lhs);
  RawT(lhs) := RawT(rhs);
  RawT(rhs) := tmp;
end;

type
  TRec = record
    A: LongInt;
    B: Word;
    C: Byte;
    D: LongInt;
  end;

var
  x, y: LongInt;
  r1, r2: TRec;
  sum: LongInt;

begin
  x := 123456;
  y := -7890;
  specialize Swap<LongInt>(x, y);

  r1.A := 10;
  r1.B := 300;
  r1.C := 5;
  r1.D := -7;

  r2.A := -1;
  r2.B := 65500;
  r2.C := 250;
  r2.D := 40000;

  specialize Swap<TRec>(r1, r2);

  sum := x + y + r1.A + r1.B + r1.C + r1.D + r2.A + r2.B + r2.C + r2.D;
  WriteLn('x=', x);
  WriteLn('y=', y);
  WriteLn('r1=', r1.A, ',', r1.B, ',', r1.C, ',', r1.D);
  WriteLn('r2=', r2.A, ',', r2.B, ',', r2.C, ',', r2.D);
  WriteLn('sum=', sum);
end.
