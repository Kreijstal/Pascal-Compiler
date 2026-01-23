{$mode objfpc}
{$modeswitch advancedrecords}
program record_static_factory;

{ Test static factory method returning the record type with parameters }

type
  TPoint = record
    X: Integer;
    Y: Integer;
    class function Create(AX, AY: Integer): TPoint; static;
  end;

class function TPoint.Create(AX, AY: Integer): TPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

var
  P: TPoint;
begin
  P := TPoint.Create(10, 20);
  WriteLn(P.X);
  WriteLn(P.Y);
end.
