program TestInterspersedDeclarations;

procedure Outer;
  procedure Nested;
  begin
    writeln('This is the nested procedure.');
  end;

  const
    MyConst = 10;

  var
    MyVar: integer;

begin
  MyVar := MyConst;
  Nested;
  writeln('MyVar is ', MyVar);
end;

begin
  Outer;
end.
