{
  Test: Procedure with default/optional parameters
  
  FPC Behavior: Supports default parameter values in procedures/functions (in objfpc mode).
  Example: procedure Test(x: Integer; y: Integer = 10);
  
  This is used in FPC RTL units for optional parameters.
  
  CRITICAL for FPC bootstrap: Many RTL units use default parameters.
}
program ProcedureDefaultParams;

{$mode objfpc}

procedure PrintValue(x: Integer; prefix: string = 'Value: ');
begin
  WriteLn(prefix, x);
end;

procedure TestOptional(a: Integer; b: Integer = 100);
begin
  WriteLn('a=', a, ' b=', b);
end;

begin
  { Call with all parameters }
  PrintValue(42, 'Number: ');
  
  { Call with default parameter }
  PrintValue(99);
  
  { Test with integer defaults }
  TestOptional(1, 2);
  TestOptional(3);
  
  WriteLn('Default params test passed');
end.
