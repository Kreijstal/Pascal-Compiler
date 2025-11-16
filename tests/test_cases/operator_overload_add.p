{$mode objfpc}
{$modeswitch advancedrecords}

type
  TPoint = record
    X, Y: Integer;
    class operator+ (const A, B: TPoint): TPoint;
  end;

class operator TPoint.+ (const A, B: TPoint): TPoint;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

var
  P1, P2, P3: TPoint;
begin
  P1.X := 10;
  P1.Y := 20;
  P2.X := 5;
  P2.Y := 3;
  P3 := P1 + P2;
  WriteLn('(', P1.X, ',', P1.Y, ') + (', P2.X, ',', P2.Y, ') = (', P3.X, ',', P3.Y, ')');
end.
