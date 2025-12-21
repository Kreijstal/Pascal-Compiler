{$mode objfpc}
{ Test case for FPC bootstrap compatibility: public name variable modifiers }
program TestPublicVar;

{ Declare a public variable that is exported with a different name }
var
  my_stack_ptr: Pointer; public name '__test_stkptr';
  counter: longint; public name 'exported_counter';

begin
  counter := 42;
  WriteLn('Public variables declared: counter = ', counter);
end.
