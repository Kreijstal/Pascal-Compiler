program tdd_typecast_vs_call;

function Foo(I: Integer): string;
begin
  Foo := 'ab';
end;

begin
  writeln(Foo(1)[1]);
end.
