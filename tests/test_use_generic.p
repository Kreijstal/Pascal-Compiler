program TestUseGeneric;

type
  TFoo<T> = class
    FData: T;
  end;

var
  F: TFoo<Integer>;

begin
  F := nil;
  writeln('Test');
end.
