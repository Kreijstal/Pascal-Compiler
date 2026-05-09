program optimizer_typed_const_array_fold;

const
  Values: array[2..4] of Integer = (10, 20, 30);

var
  x: Integer;
  y: Integer;

begin
  x := Values[1 + 2];
  y := Length(Values);

  WriteLn(x);
  WriteLn(y);
end.
