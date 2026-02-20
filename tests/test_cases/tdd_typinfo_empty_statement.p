program tdd_typinfo_empty_statement;

var
  value: Integer;

begin
  value := 1;
  ;
  value := value + 1;
  writeln(value);
end.
