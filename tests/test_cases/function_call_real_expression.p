program TestFunctionCallRealExpression;
function Half(x: real): real;
begin
  Half := x / 2.0;
end;
var
  resultValue: real;
begin
  resultValue := Half(10.0) + 1.5;
  writeln(resultValue:0:1);
end.
