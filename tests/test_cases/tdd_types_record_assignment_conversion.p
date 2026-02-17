program tdd_types_record_assignment_conversion;
{$mode objfpc}
{$modeswitch advancedrecords}

type
  TPoint = record
    X, Y: Integer;
  end;

  TPointF = record
    X, Y: Single;
    function Ceiling: TPoint;
    class operator :=(const P: TPoint): TPointF;
  end;

  TRect = record
    Left, Top, Right, Bottom: Integer;
  end;

  TRectF = record
    TopLeft: TPointF;
    BottomRight: TPointF;
    class operator :=(const R: TRect): TRectF;
    procedure InitFromRect(R: TRect; Normalize: Boolean);
    function Ceiling: TRectF;
  end;

function CeilScalar(V: Single): Integer;
var
  I: Integer;
begin
  I := System.Trunc(V);
  if V > I then
    Result := I + 1
  else
    Result := I;
end;

function TPointF.Ceiling: TPoint;
begin
  Result.X := CeilScalar(X);
  Result.Y := CeilScalar(Y);
end;

class operator TPointF.:=(const P: TPoint): TPointF;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

class operator TRectF.:=(const R: TRect): TRectF;
begin
  Result.TopLeft.X := R.Left;
  Result.TopLeft.Y := R.Top;
  Result.BottomRight.X := R.Right;
  Result.BottomRight.Y := R.Bottom;
end;

procedure TRectF.InitFromRect(R: TRect; Normalize: Boolean);
begin
  Self := R;
  if Normalize then
  begin
    if TopLeft.X > BottomRight.X then
      TopLeft.X := BottomRight.X;
    if TopLeft.Y > BottomRight.Y then
      TopLeft.Y := BottomRight.Y;
  end;
end;

function TRectF.Ceiling: TRectF;
begin
  Result.BottomRight := BottomRight.Ceiling;
  Result.TopLeft := TopLeft.Ceiling;
end;

var
  R: TRect;
  RF, RC: TRectF;
begin
  R.Left := 1;
  R.Top := 2;
  R.Right := 3;
  R.Bottom := 4;

  RF.InitFromRect(R, False);
  RF.BottomRight.X := 3.2;
  RF.BottomRight.Y := 4.8;
  RC := RF.Ceiling;

  WriteLn(System.Trunc(RC.TopLeft.X), ' ', System.Trunc(RC.TopLeft.Y), ' ',
          System.Trunc(RC.BottomRight.X), ' ', System.Trunc(RC.BottomRight.Y));
end.
