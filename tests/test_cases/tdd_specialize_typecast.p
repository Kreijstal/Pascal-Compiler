program tdd_specialize_typecast;

{$mode objfpc}

type
  generic TArray<T> = array of T;

var
  a: specialize TArray<Integer>;
  b: specialize TArray<Integer>;

begin
  specialize TArray<Integer>(a) := b;
  writeln('ok');
end.
