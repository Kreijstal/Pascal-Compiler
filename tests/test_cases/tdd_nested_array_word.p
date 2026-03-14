{ Test nested array of Word - element stores must use 2-byte writes }
program tdd_nested_array_word;
{$mode objfpc}
type
  TWordArray = array[0..11] of Word;
  TNestedArray = array[0..1] of TWordArray;
var
  arr: TNestedArray;
  i, j: Integer;
begin
  { Fill with known values }
  for i := 0 to 1 do
    for j := 0 to 11 do
      arr[i][j] := Word(i * 12 + j);

  { Check values }
  WriteLn(arr[0][0]);
  WriteLn(arr[0][11]);
  WriteLn(arr[1][0]);
  WriteLn(arr[1][11]);
end.
