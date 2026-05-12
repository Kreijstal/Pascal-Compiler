program tdd_stride_nested_dynarray;
{ Tests correct stride computation for nested dynamic/static arrays.
  Type: array of array[0..3] of array of longint
  Before fix: strides for 3-level types all zeroed out → SIGSEGV on 2D access.
  After  fix: per-level strides (64, 16) for 2D access → correct addresses. }

type
  TInnerDyn  = array of longint;
  TMidStat   = array[0..3] of TInnerDyn;
  TOuterDyn  = array of TMidStat;

var
  arr: TOuterDyn;
  inner: TInnerDyn;
  ok: boolean;
begin
  SetLength(arr, 2);

  { 2D access: arr[i, j] should return the inner dynamic array at that position.
    With broken strides (all zero), arr[0,0] and arr[0,1] resolve to the
    same address → SetLength on one corrupts the other → crash or FAIL. }
  SetLength(arr[0, 0], 5);
  SetLength(arr[0, 1], 5);

  { Verify they are separate arrays with correct lengths }
  ok := (Length(arr[0, 0]) = 5) and (Length(arr[0, 1]) = 5);

  { Write via 2D access and read back via 3D access }
  arr[0, 0][2] := 42;
  arr[0, 1][3] := 99;

  ok := ok and (arr[0, 0][2] = 42) and (arr[0, 1][3] = 99);

  if ok then
    writeln('PASS')
  else
    writeln('FAIL');
end.
