program tdd_empty_array_literal;

{$mode objfpc}

type
  TBytes = array of Byte;

function Foo: TBytes;
var
  A: TBytes;
begin
  A := [];
  Result := A;
end;

begin
  if Length(Foo) = 0 then
    writeln('empty')
  else
    writeln('nonempty');
end.
