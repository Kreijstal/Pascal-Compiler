program TestSimpleGeneric;

type
  TFoo<T> = class
    FData: T;
  end;

var
  F: TFoo<Integer>;

begin
  writeln('Simple generic test');
end.