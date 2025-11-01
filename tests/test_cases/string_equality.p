program string_equality;

var
  Input: string;
  Position: Integer;
  Slice: string;

begin
  Input := 'test';
  Position := 1;
  Slice := Copy(Input, Position, 4);

  if Slice = 'true' then
    WriteLn('Found true')
  else
    WriteLn('Not true');

  if Input[Position] <> '"' then
    WriteLn('Not a quote')
  else
    WriteLn('Is a quote');

  if Slice = Input then
    WriteLn('Slice equals input')
  else
    WriteLn('Slice differs from input');

  WriteLn('Done');
end.
