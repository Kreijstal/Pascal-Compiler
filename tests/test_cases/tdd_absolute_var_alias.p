program tdd_absolute_var_alias;

var
  Base: Longint;
  Alias: Longint absolute Base;

begin
  Base := 1;
  Alias := 5;
  writeln(Base);
end.
