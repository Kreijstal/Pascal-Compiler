program tdd_nested_inc_result;

function Outer: Integer;
  procedure Bump;
  begin
    Inc(Result);
  end;
begin
  Result := 0;
  Bump;
end;

begin
  Writeln(Outer);
end.
