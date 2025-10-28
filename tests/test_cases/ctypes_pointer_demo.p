program ctypes_pointer_demo;

uses ctypes;

var
  value: cint;
  valuePtr: pcint;
  boolValue: cbool;
  boolPtr: pcbool;
  numbersPtr: pcint16;
begin
  value := 42;
  valuePtr := valuePtr;
  boolValue := 1;
  boolPtr := boolPtr;
  numbersPtr := numbersPtr;

  writeln(value);
  writeln(7);
  writeln(boolValue);
end.
