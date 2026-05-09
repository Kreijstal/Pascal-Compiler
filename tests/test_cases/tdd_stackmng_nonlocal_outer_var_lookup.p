program tdd_stackmng_nonlocal_outer_var_lookup;

procedure Outer;
var
  outerValue: integer;

  procedure Inner;
  begin
    outerValue := outerValue + 7;
    writeln('Inner sees: ', outerValue);
  end;

begin
  outerValue := 35;
  Inner;
  writeln('Outer sees: ', outerValue);
end;

begin
  Outer;
end.
