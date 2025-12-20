program fpc_exit_value_nested;

function Outer(x: LongInt): LongInt;
  function Inner(y: LongInt): LongInt;
  begin
    if y < 0 then
      Exit(-1)
    else
      Exit(y + 1);
  end;
begin
  Outer := Inner(x);
end;

begin
  writeln(Outer(2));
  writeln(Outer(-5));
end.
