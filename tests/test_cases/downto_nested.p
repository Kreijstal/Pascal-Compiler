program PascalTriangleDownto;

{ Test downto loops with a more complex example }

var
  i, j, sum: Integer;

begin
  { Count down from 5 to 1 and accumulate }
  sum := 0;
  for i := 5 downto 1 do
    sum := sum + i;
  WriteLn('Sum 5 downto 1: ', sum);
  
  { Nested downto loops }
  for i := 3 downto 1 do
  begin
    Write('Row ', i, ': ');
    for j := 3 downto 1 do
      Write(i * j, ' ');
    WriteLn;
  end;
end.
