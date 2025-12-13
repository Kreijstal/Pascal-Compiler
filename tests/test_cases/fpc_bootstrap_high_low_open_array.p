{ Test High/Low on open array parameters }
{ Required for FPC bootstrap: unixutil.pp uses High(s)-Low(s) }
{ on open array parameters like 'array of RawByteString' }
program fpc_bootstrap_high_low_open_array;

{$mode objfpc}

function ArrayRange(const arr: array of Integer): Integer;
begin
  { High and Low on open array parameter }
  Result := High(arr) - Low(arr);
end;

function SumArray(const arr: array of Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Low(arr) to High(arr) do
    Result := Result + arr[i];
end;

var
  a: array[0..4] of Integer;
  b: array[1..3] of Integer;
begin
  a[0] := 10; a[1] := 20; a[2] := 30; a[3] := 40; a[4] := 50;
  b[1] := 100; b[2] := 200; b[3] := 300;
  
  WriteLn('ArrayRange(a): ', ArrayRange(a));
  WriteLn('ArrayRange(b): ', ArrayRange(b));
  WriteLn('SumArray(a): ', SumArray(a));
  WriteLn('SumArray(b): ', SumArray(b));
end.
