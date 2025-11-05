program TestConstArrayBoundsSimple;

{ Test that array bounds using constants are properly resolved }

const MaxVal = 5;

type TArray = array[1..MaxVal] of integer;

var 
  x: TArray;

begin
  x[1] := 42;
  WriteLn('x[1] = ', x[1]:1);
end.
