program tdd_qualified_type_alias_space_dot;

uses ctypes;

type
  myint = ctypes .cint;

var
  x: myint;

begin
  x := 1;
  writeln(x);
end.
