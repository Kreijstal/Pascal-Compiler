{ Regression test: for-in over an array whose elements are larger than a
  machine word (here a 24-byte record).  Codegen previously errored with
  "FOR-IN with large array elements not yet supported"; it now keeps the
  element address and memcpys each element into the loop variable. }
{$mode objfpc}
program forin;
type
  TRec = record a, b, c: Int64; end;
var
  arr: array[0..2] of TRec;
  r: TRec;
  i: Integer;
  sum: Int64;
begin
  for i := 0 to 2 do begin
    arr[i].a := i; arr[i].b := i*10; arr[i].c := i*100;
  end;
  sum := 0;
  for r in arr do sum := sum + r.a + r.b + r.c;
  WriteLn(sum);
end.
