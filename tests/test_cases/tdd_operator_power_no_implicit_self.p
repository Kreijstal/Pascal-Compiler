program TddOperatorPowerNoImplicitSelf;

type
  TVec = record
    X, Y: Integer;
    class operator **(const A, B: TVec): Integer;
  end;

class operator TVec.**(const A, B: TVec): Integer;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

var
  A, B: TVec;
begin
  A.X := 1;
  A.Y := 2;
  B.X := 3;
  B.Y := 4;
  Writeln(A ** B);
end.
