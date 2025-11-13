program arcsech_function;
var
  r: real;
begin
  r := ArcSech(1.0);
  writeln('ArcSechOne=', r:0:4);
  r := ArcSech(0.5);
  writeln('ArcSechHalf=', r:0:4);
  r := ArcSech(0.1);
  writeln('ArcSechPointOne=', r:0:4);
end.
