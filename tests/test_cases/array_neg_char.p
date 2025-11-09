program array_neg_char;
{*
  Test array of char with negative bounds.
  This ensures negative indices work with different element types.
*}
var
  arr: array[-3..3] of char;
begin
  arr[-3] := 'a';
  arr[-2] := 'b';
  arr[-1] := 'c';
  arr[0] := 'd';
  arr[1] := 'e';
  arr[2] := 'f';
  arr[3] := 'g';
  
  if arr[-3] <> 'a' then
    writeln('FAIL: arr[-3] = ', arr[-3], ' expected a')
  else if arr[0] <> 'd' then
    writeln('FAIL: arr[0] = ', arr[0], ' expected d')
  else if arr[3] <> 'g' then
    writeln('FAIL: arr[3] = ', arr[3], ' expected g')
  else
    writeln('PASS');
end.
