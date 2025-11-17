var
  x: Integer;
begin
  try
    x := 111;
  finally
    WriteLn('Test = ', x);
  end;
end.
