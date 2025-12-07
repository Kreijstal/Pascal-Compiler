program FPCUnitExports;

uses fpc_unit_exports;

var
  pt: TPoint;
  pi_value: Real;
begin
  pt.x := 10;
  pt.y := 20;
  WriteLn('X: ', GetX(pt));
  pi_value := PI;
  WriteLn('PI: ', pi_value);
  Counter := 5;
  WriteLn('Counter: ', Counter);
end.
