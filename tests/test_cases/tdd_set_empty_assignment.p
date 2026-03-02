program tdd_set_empty_assignment;

{$mode objfpc}

type
  TSet = set of (a, b, c);

function Foo: TSet;
begin
  Result := [];
end;

begin
  if Foo = [] then
    writeln('empty')
  else
    writeln('nonempty');
end.
