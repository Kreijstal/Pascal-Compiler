program ConstExprTest;
const
  MaxValue = (1 + 2) * 3;
  TypedConst: array[0..1] of Integer = (1, 2);
  Derived = MaxValue - TypedConst[0];
begin
end.
