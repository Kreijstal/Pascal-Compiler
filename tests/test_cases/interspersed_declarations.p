program TestInterspersed;

procedure Outer;
  procedure Nested;
  begin
    { Nested procedure body }
  end;

  const
    MyConst = 10;

  var
    MyVar: integer;

begin
  { Outer procedure body }
end;

begin
  { Main program body }
end.
