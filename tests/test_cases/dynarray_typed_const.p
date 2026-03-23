{$mode objfpc}
{ Regression test: dynamic array typed constant with single-element
  initializer must not produce "initializer type mismatch" error.
  See: https://github.com/Kreijstal/Pascal-Compiler/issues/476 }
program dynarray_typed_const;
type
  TIntArray = array of Integer;
const
  single_elem: TIntArray = (42);
  multi_elem: TIntArray = (10, 20, 30);
begin
  { Only test compilation — dynamic array const codegen is not yet
    fully implemented, so we just verify the program compiles. }
  WriteLn('OK');
end.
