program tdd_specialize_typecast_record;

{$mode objfpc}

type
  TPtrWrapper = record
    Value: Pointer;
  end;
  generic TArray<T> = array of T;

function FixArray<T>(const Arr: specialize TArray<T>): TPtrWrapper;
begin
  Result.Value := nil;
  specialize TArray<T>(Result) := Arr;
end;

begin
  writeln('ok');
end.
