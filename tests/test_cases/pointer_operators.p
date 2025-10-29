program pointer_operators;

type
  PInteger = ^integer;

var
  value: integer;
  pointer_value, alias: PInteger;
begin
  value := 40;
  pointer_value := @value;
  pointer_value^ := pointer_value^ + 2;
  writeln(value);

  alias := pointer_value;
  alias^ := alias^ + 1;
  writeln(pointer_value^);
end.
