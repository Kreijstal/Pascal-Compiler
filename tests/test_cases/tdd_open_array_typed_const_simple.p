{ TDD: Simple open array (dynamic array) typed constant with scalar elements }
program tdd_open_array_typed_const_simple;

const
  Values: array of Integer = (10, 20, 30, 40, 50);
  Names: array of string = ('Hello', 'World');

var
  I: Integer;
begin
  WriteLn(Length(Values));
  for I := 0 to High(Values) do
    Write(Values[I], ' ');
  WriteLn;
  WriteLn(Length(Names));
  for I := 0 to High(Names) do
    WriteLn(Names[I]);
end.
