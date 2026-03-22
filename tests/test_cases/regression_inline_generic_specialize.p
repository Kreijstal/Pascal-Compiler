{ Regression test: inline generic specialization in expressions
  Verifies that "specialize GenericRecord<ConcreteType>.Method(args)"
  works correctly — the generic is instantiated inline and the static
  method is called directly in expression context.

  This pattern is used in heaptrc.pp (HashListTemplated).
}
{$mode objfpc}
program regression_inline_generic_specialize;

type
  generic TMyGeneric<T> = record
    class function Identity(X: T): T; static;
  end;

class function TMyGeneric.Identity(X: T): T;
begin
  Result := X;
end;

var
  A: LongInt;
  B: String;
begin
  A := specialize TMyGeneric<LongInt>.Identity(42);
  WriteLn('A=', A);

  B := specialize TMyGeneric<String>.Identity('hello');
  WriteLn('B=', B);
end.
