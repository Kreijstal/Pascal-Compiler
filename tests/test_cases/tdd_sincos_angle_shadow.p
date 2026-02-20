program tdd_sincos_angle_shadow;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch duplicatelocals}

uses Math;

type
  TVec = record
    x, y: Real;
  public
    function Angle(const other: TVec): Real;
    function Rotate(angle: Real): TVec;
  end;

function TVec.Angle(const other: TVec): Real;
begin
  Angle := Arctan2(other.y - y, other.x - x);
end;

function TVec.Rotate(angle: Real): TVec;
var
  s, c: Real;
begin
  SinCos(angle, s, c);
  Rotate.x := x * c - y * s;
  Rotate.y := x * s + y * c;
end;

var
  v, r: TVec;
begin
  v.x := 1.0;
  v.y := 0.0;
  r := v.Rotate(Pi / 2);
  writeln(r.x:0:4);
  writeln(r.y:0:4);
end.
