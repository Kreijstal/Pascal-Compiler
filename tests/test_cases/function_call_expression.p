program TestFunctionReturnValue;
var
  assignedValue: integer;
function AddTen(val: integer): integer;
begin
  AddTen := val + 10;
end;
begin
  assignedValue := AddTen(5);
  writeln('Assigned value: ', assignedValue);
  writeln('Arithmetic expression: ', 100 + AddTen(20));
  writeln('Direct parameter: ', AddTen(32));
end.
