program FPCPasExtensionTest;

uses fpc_pas_extension_unit;

var
  color: TColor;
begin
  color := Red;
  WriteLn('Color: ', ColorToString(color));
  WriteLn('MaxValue: ', MaxValue);
  GlobalCounter := 42;
  WriteLn('GlobalCounter: ', GlobalCounter);
end.
