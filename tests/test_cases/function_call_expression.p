program TestFunctionReturnValue;
var
  assignedValue: integer;

function GetFive: integer;
begin
  GetFive := 5;
end;

function AddTen(val: integer): integer;
begin
  AddTen := val + 10;
end;
begin
  assignedValue := AddTen(5);
  writeln('Assigned value: ', assignedValue);
  writeln('Arithmetic expression: ', 100 + AddTen(20));
  writeln('Direct parameter: ', AddTen(32));

  assignedValue := GetFive;
  writeln('Parameterless assigned: ', assignedValue);
  writeln('Parameterless expression: ', 100 + GetFive);
  writeln('Parameterless direct: ', GetFive);
end.
