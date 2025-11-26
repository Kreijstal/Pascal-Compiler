program missing_continue;
var
  i: integer;
begin
  for i := 1 to 5 do
  begin
    if i = 3 then
      Continue;
    writeln(i);
  end;
end.
