program ExtendedArithmeticFlow;

function AddScaled(x: Extended): Extended;
begin
  AddScaled := (x + 1.25) * 2.0;
end;

var
  A, B, C: Extended;
begin
  A := 1.5;
  B := 2.25;
  C := A + B;
  WriteLn(Trunc(C));
  C := AddScaled(B);
  WriteLn(Trunc(C));
  C := -(A + B);
  WriteLn(Trunc(C));
end.
