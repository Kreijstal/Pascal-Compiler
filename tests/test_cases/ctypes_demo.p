program ctypes_demo;

uses ctypes;

var
  signedValue: cint;
  unsignedValue: cuint;
  bufferLength: csize_t;

procedure PrintCString(value: cstring);
begin
  writeln(value);
end;

begin
  signedValue := -42;
  unsignedValue := 7;
  bufferLength := 1024;

  writeln(signedValue);
  writeln(unsignedValue);
  writeln(bufferLength);
  PrintCString('ctypes');
end.
