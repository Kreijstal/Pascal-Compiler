program TestRecordParameters;
type
  TPoint = record x: integer; end;
var
  myPoint: TPoint;
procedure ModifyCopy(pt: TPoint);
begin
  pt.x := 999;
  writeln('Inside ModifyCopy, pt.x = ', pt.x);
end;
procedure ModifyOriginal(var pt: TPoint);
begin
  pt.x := pt.x + 1;
end;
begin
  myPoint.x := 10;
  writeln('Before calls, myPoint.x = ', myPoint.x);
  ModifyCopy(myPoint);
  writeln('After ModifyCopy, myPoint.x = ', myPoint.x);
  ModifyOriginal(myPoint);
  writeln('After ModifyOriginal, myPoint.x = ', myPoint.x);
end.
