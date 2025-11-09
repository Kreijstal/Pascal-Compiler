program array_neg_to_zero;
{*
  Test array with negative lower bound and zero upper bound.
  This ensures the compiler correctly handles ranges like array[-10..0].
*}
var
  arr: array[-10..0] of integer;
  sum: integer;
begin
  { Initialize boundary elements }
  arr[-10] := -10;
  arr[-5] := -5;
  arr[0] := 0;
  
  { Calculate sum }
  sum := arr[-10] + arr[-5] + arr[0];
  
  { Verify }
  if sum <> -15 then
    writeln('FAIL: sum = ', sum, ' expected -15')
  else
    writeln('PASS');
end.
