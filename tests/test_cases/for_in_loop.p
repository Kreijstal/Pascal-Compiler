program for_in_loop;

type
  TIntArray = array[0..2] of Integer;

var
  Values: TIntArray;
  Item, Total: Integer;
begin
  Values[0] := 1;
  Values[1] := 2;
  Values[2] := 3;

  Total := 0;
  for Item in Values do
    Total := Total + Item;

  WriteLn('for-in total = ', Total);
end.
