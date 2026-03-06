program tdd_variant_record_field_method;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TPoint = record
    x, y: integer;
  end;

  TPointF = record
    x, y: double;
    function Truncate: TPoint;
  end;

  TRectF = record
    function BottomRightTrunc: TPoint;
    case integer of
      0: (Left, Top, Right, Bottom: double);
      1: (TopLeft, BottomRight: TPointF);
  end;

function TPointF.Truncate: TPoint;
begin
  Result.x := Trunc(x);
  Result.y := Trunc(y);
end;

function TRectF.BottomRightTrunc: TPoint;
begin
  Result := BottomRight.Truncate;
end;

var
  r: TRectF;
  p: TPoint;

begin
  r.BottomRight.x := 12.9;
  r.BottomRight.y := -3.2;
  p := r.BottomRightTrunc;
  writeln(p.x, ' ', p.y);
end.
