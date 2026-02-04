program tdd_multidim_array_stride;

type
  TArr = array[1..2, 1..3] of longint;

var
  a: TArr;

begin
  a[1,1] := 11;
  a[1,2] := 12;
  a[1,3] := 13;
  a[2,1] := 21;
  a[2,2] := 22;
  a[2,3] := 23;
  writeln(a[1,1], ' ', a[1,2], ' ', a[1,3]);
  writeln(a[2,1], ' ', a[2,2], ' ', a[2,3]);
end.
