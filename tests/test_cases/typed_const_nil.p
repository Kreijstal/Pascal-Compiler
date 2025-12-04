program typed_const_nil;
{ Test nil in typed constants - required for FPC system.pp }
const
  NullPtr : Pointer = nil;
  
begin
  { Just test that it compiles and runs }
  writeln('OK');
end.
