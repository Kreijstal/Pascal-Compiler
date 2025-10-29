program RecordMemberAccess;
type
  Point = record
    x: Integer;
    y: Integer;
  end;
var
  p: Point;
begin
  p.x := 10;
  p.y := 32;
  writeln(p.x + p.y);
  p.x := p.y - 2;
  writeln(p.x);
end.
