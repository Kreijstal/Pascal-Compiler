program array_neg_large_range;
{*
  Test array with large negative lower bound and positive upper bound.
  This ensures the compiler correctly handles large ranges like array[-100..100].
*}
var
  arr: array[-100..100] of integer;
begin
  { Test boundary elements }
  arr[-100] := 1;
  arr[-50] := 2;
  arr[0] := 3;
  arr[50] := 4;
  arr[100] := 5;
  
  { Verify }
  if arr[-100] <> 1 then
    writeln('FAIL: arr[-100] = ', arr[-100], ' expected 1')
  else if arr[-50] <> 2 then
    writeln('FAIL: arr[-50] = ', arr[-50], ' expected 2')
  else if arr[0] <> 3 then
    writeln('FAIL: arr[0] = ', arr[0], ' expected 3')
  else if arr[50] <> 4 then
    writeln('FAIL: arr[50] = ', arr[50], ' expected 4')
  else if arr[100] <> 5 then
    writeln('FAIL: arr[100] = ', arr[100], ' expected 5')
  else
    writeln('PASS');
end.
