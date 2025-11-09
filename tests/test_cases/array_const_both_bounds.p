program array_const_both_bounds;
{* 
  Test array with both bounds being constants.
  This tests that arrays like array[MIN..MAX] where both are consts
  are correctly handled.
*}
const
  MIN = 5;
  MAX = 15;
var
  arr: array[MIN..MAX] of integer;
begin
  { Initialize boundary elements }
  arr[MIN] := MIN;
  arr[MAX] := MAX;
  arr[10] := 10;
  
  { Verify values }
  if arr[5] <> 5 then
    writeln('FAIL: arr[5] = ', arr[5], ' expected 5')
  else if arr[10] <> 10 then
    writeln('FAIL: arr[10] = ', arr[10], ' expected 10')
  else if arr[15] <> 15 then
    writeln('FAIL: arr[15] = ', arr[15], ' expected 15')
  else
    writeln('PASS');
end.
