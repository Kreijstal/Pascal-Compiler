program string_numeric_scan;
var
  Input: string;
  Position, Start: Integer;
  NumStr: string;
begin
  Input := '123.45';
  Position := 1;
  Start := Position;

  while (Position <= Length(Input)) and (Position in [1..6]) do
    Inc(Position);

  NumStr := '';
  while Start < Position do
  begin
    NumStr := NumStr + Input[Start];
    Inc(Start);
  end;

  WriteLn('Number string: ', NumStr);
end.
