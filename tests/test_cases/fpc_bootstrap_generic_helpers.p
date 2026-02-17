{$mode objfpc}
{$modeswitch typehelpers}
program fpc_bootstrap_generic_helpers;

type
  TRec = record
    A: LongInt;
    B: Word;
    C: Byte;
    D: Int64;
  end;

generic function IfThen<T>(val: boolean; const iftrue: T; const iffalse: T): T; inline; overload;
begin
  if val then
    Result := ifTrue
  else
    Result := ifFalse;
end;

generic function Exchange<T>(var target: T; const newvalue: T): T;
begin
  Result := target;
  target := newvalue;
end;

generic function Extract<T>(var from: T): T;
type
  RawT = array[0 .. sizeof(T) - 1] of byte;
begin
  Finalize(Result);
  RawT(Result) := RawT(from);
  Initialize(from);
end;

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

function SumRec(const R: TRec): Int64;
begin
  Result := R.A + R.B + R.C + R.D;
end;

var
  x, y, oldx: LongInt;
  s1, s2, olds: string;
  r1, r2, r3: TRec;
  flag: boolean;
  sum: Int64;
begin
  x := 42;
  y := -17;
  flag := x > y;
  s1 := 'Alpha';
  s2 := 'Beta';

  r1.A := 10;
  r1.B := 500;
  r1.C := 7;
  r1.D := -2000;

  r2.A := -5;
  r2.B := 42;
  r2.C := 255;
  r2.D := 123456;

  oldx := specialize Exchange<LongInt>(x, 99);
  olds := specialize Exchange<string>(s1, 'Gamma');

  r3 := specialize Extract<TRec>(r1);
  specialize Swap<TRec>(r2, r3);

  sum := 0;
  sum := sum + oldx + x + y;
  sum := sum + Length(olds) + Length(s1) + Length(s2);
  sum := sum + SumRec(r1) + SumRec(r2) + SumRec(r3);
  sum := sum + Ord(specialize IfThen<boolean>(flag, True, False));
  sum := sum + Length(specialize IfThen<string>(flag, 'YES', 'NO'));

  writeln('oldx=', oldx);
  writeln('x=', x);
  writeln('s1=', s1);
  writeln('r1=', r1.A, ',', r1.B, ',', r1.C, ',', r1.D);
  writeln('r2=', r2.A, ',', r2.B, ',', r2.C, ',', r2.D);
  writeln('r3=', r3.A, ',', r3.B, ',', r3.C, ',', r3.D);
  writeln('sum=', sum);
end.
