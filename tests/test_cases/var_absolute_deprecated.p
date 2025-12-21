program var_absolute_deprecated;

{$mode objfpc}

var
  Base: Integer;
  Alias: Integer absolute Base deprecated;

begin
  Base := 5;
  writeln(Base);
  Alias := 11;
  writeln(Base);
end.
