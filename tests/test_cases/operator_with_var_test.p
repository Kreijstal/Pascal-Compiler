program OperatorWithVarTest;

type
  TVector = record
    X, Y: Integer;
  end;

class operator TVector.+(const A, B: TVector): TVector;
var
  Temp: TVector;
begin
  Temp.X := A.X + B.X;
  Temp.Y := A.Y + B.Y;
end;

begin
  WriteLn('Operator with var section parsed successfully!');
end.
