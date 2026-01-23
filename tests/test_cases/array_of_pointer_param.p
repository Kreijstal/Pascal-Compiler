program ArrayOfPointerParam;
{ Test for issue: Array of ^T parameter handling
  This test verifies that open array parameters with pointer element types
  don't cause double-free issues during compilation. }

type
  PInteger = ^Integer;
  PWord = ^Word;

function SumPointers(arr: array of PInteger): Integer;
var
  i, total: Integer;
begin
  total := 0;
  for i := 0 to High(arr) do
    if arr[i] <> nil then
      total := total + arr[i]^;
  SumPointers := total;
end;

function CombinePointers(first: PInteger; rest: array of PWord): Integer;
var
  i, result: Integer;
begin
  if first <> nil then
    result := first^
  else
    result := 0;
  for i := 0 to High(rest) do
    if rest[i] <> nil then
      result := result + rest[i]^;
  CombinePointers := result;
end;

var
  a, b, c: Integer;
  pa, pb, pc: PInteger;
  wa, wb: Word;
  pwa, pwb: PWord;
  result: Integer;
begin
  a := 10;
  b := 20;
  c := 30;
  pa := @a;
  pb := @b;
  pc := @c;

  result := SumPointers([pa, pb, pc]);
  writeln('Sum of pointers: ', result);

  wa := 5;
  wb := 15;
  pwa := @wa;
  pwb := @wb;

  result := CombinePointers(pa, [pwa, pwb]);
  writeln('Combined result: ', result);
end.
