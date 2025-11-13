program MathExtendedDemo;

uses
    Math;

var
    SampleValues: array [0..3] of Longint;
    sinVal, cosVal: Real;
begin
    SampleValues[0] := 1;
    SampleValues[1] := -2;
    SampleValues[2] := 5;
    SampleValues[3] := 4;
    writeln('MaxIntValue=', MaxIntValue(SampleValues));
    writeln('MinIntValue=', MinIntValue(SampleValues));
    writeln('Sum=', Sum(SampleValues));
    writeln('Mean=', Mean(SampleValues):0:2);
    writeln('EnsureRange=', EnsureRange(15, 0, 10));
    writeln('EnsureRangeSwap=', EnsureRange(3, 10, 1));
    writeln('InRangeTrue=', InRange(5, 1, 10));
    writeln('InRangeFalse=', InRange(15, 1, 10));
    writeln('SignNeg=', Sign(-42));
    writeln('CeilPos=', Ceil(2.2));
    writeln('CeilNeg=', Ceil(-2.2));
    writeln('FloorPos=', Floor(2.8));
    writeln('FloorNeg=', Floor(-2.2));
    writeln('DegToRad=', DegToRad(180):0:3);
    writeln('RadToDeg=', RadToDeg(DegToRad(90)):0:1);
    writeln('GradToRad=', GradToRad(200):0:3);
    writeln('RadToGrad=', RadToGrad(DegToRad(90)):0:1);
    writeln('CycleToRad=', CycleToRad(0.5):0:3);
    writeln('RadToCycle=', RadToCycle(DegToRad(180)):0:3);
    SinCos(DegToRad(30), sinVal, cosVal);
    writeln('SinCos=', sinVal:0:3, ',', cosVal:0:3);
end.
