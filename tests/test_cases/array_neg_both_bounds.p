program array_neg_both_bounds;
{*
  Test array with both negative lower and upper bounds.
  This ensures the compiler correctly handles arrays like array[-5..-1].
*}
var
  arr: array[-5..-1] of integer;
  i: integer;
begin
  { Initialize all elements }
  arr[-5] := -50;
  arr[-4] := -40;
  arr[-3] := -30;
  arr[-2] := -20;
  arr[-1] := -10;
  
  { Verify all values }
  if arr[-5] <> -50 then
    writeln('FAIL: arr[-5] = ', arr[-5], ' expected -50')
  else if arr[-4] <> -40 then
    writeln('FAIL: arr[-4] = ', arr[-4], ' expected -40')
  else if arr[-3] <> -30 then
    writeln('FAIL: arr[-3] = ', arr[-3], ' expected -30')
  else if arr[-2] <> -20 then
    writeln('FAIL: arr[-2] = ', arr[-2], ' expected -20')
  else if arr[-1] <> -10 then
    writeln('FAIL: arr[-1] = ', arr[-1], ' expected -10')
  else
    writeln('PASS');
end.
