program ctypes_dll_demo;

uses ctypes;

function ctypes_helper_add(a, b: integer): cint; external;

var
  left: cint;
  right: cint;
  resultValue: cint;

begin
  left := 40;
  right := 2;
  resultValue := ctypes_helper_add(left, right);
  writeln(resultValue);
end.
