program StaticLinkWithArgs;
var
  total: integer;

procedure Outer(base: integer);

  function Multiply(factor: integer): integer;
  begin
    Multiply := base * factor;
  end;

  procedure Inner(offset: integer);
  begin
    total := total + base + offset + Multiply(offset);
  end;

  procedure Caller;
  begin
    Inner(base);
  end;

begin
  Inner(base + 1);
  Caller;
end;

begin
  total := 0;
  Outer(5);
  writeln(total);
end.
