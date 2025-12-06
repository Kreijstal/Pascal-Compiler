{ Test pointer indexing - FPC feature that KGPC doesn't support }
program TestPointerIndex;

type
  PInteger = ^Integer;

var
  arr: array[0..4] of Integer;
  p: PInteger;
  i: Integer;

begin
  { Initialize array }
  arr[0] := 10;
  arr[1] := 20;
  arr[2] := 30;
  arr[3] := 40;
  arr[4] := 50;
  
  { Test pointer indexing }
  p := @arr[0];
  
  { This should work in FPC but fails in KGPC }
  for i := 0 to 4 do
    writeln(p[i]);
end.
