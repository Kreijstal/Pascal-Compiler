{ Test pointer arithmetic with + and - operators }
{ NOTE: This test is automatically SKIPPED by the test runner because }
{ KGPC doesn't yet support pointer arithmetic with +/- operators. }
{ It serves as documentation of the gap and a target for future implementation. }
{ See docs/FPC_BOOTSTRAP_GAPS.md for details. }
program TestPointerArithmetic;

type
  PInteger = ^Integer;

var
  arr: array[0..4] of Integer;
  p, q: PInteger;

begin
  { Initialize array }
  arr[0] := 10;
  arr[1] := 20;
  arr[2] := 30;
  arr[3] := 40;
  arr[4] := 50;
  
  { Test pointer + integer }
  p := @arr[0];
  q := p + 2;
  writeln(q^);
  
  { Test pointer - integer }
  p := @arr[4];
  q := p - 3;
  writeln(q^);
end.
