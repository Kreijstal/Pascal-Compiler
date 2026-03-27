program reg_user_low_function_not_builtin;

function Low(Value: LongInt): LongInt;
begin
  Low := Value + 7;
end;

begin
  writeln(Low(70));
end.
