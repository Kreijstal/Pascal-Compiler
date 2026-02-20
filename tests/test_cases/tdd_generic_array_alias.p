program tdd_generic_array_alias;

{$mode objfpc}

type
  generic TArray<T> = array of T;

var
  values: specialize TArray<Integer>;

begin
  SetLength(values, 1);
  values[0] := 42;
  writeln(values[0]);
end.
