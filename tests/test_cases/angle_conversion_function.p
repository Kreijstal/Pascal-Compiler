program angle_conversion_function;
var
  rad: real;
begin
  rad := DegToRad(180.0);
  writeln('DegToRad=', rad:0:6);
  writeln('RadToDeg=', RadToDeg(rad):0:2);
  writeln('DegToGrad=', DegToGrad(180.0):0:2);
  writeln('GradToDeg=', GradToDeg(200.0):0:2);
  writeln('GradToRad=', GradToRad(200.0):0:6);
  writeln('RadToGrad=', RadToGrad(rad):0:2);
  writeln('CycleToRad=', CycleToRad(0.5):0:6);
  writeln('RadToCycle=', RadToCycle(rad):0:6);
end.
