program CharArrayCharAssign;

var
  chars: array[1..5] of char;
  ch: char;
  i: integer;

begin
  chars[1] := 'a';
  chars[2] := 'b';
  chars[3] := 'c';
  chars[4] := 'd';
  chars[5] := 'e';

  ch := 'Z';
  chars := ch;

  for i := 1 to 5 do
    writeln(i, ':', chars[i]);
end.
