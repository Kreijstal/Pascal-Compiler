program AssertBuiltin;
{ Test that Assert properly evaluates conditions at runtime.
  Assert(true-condition) must continue execution.
  Assert(false-condition) must halt with exit code 227.
  This test only checks the success path - all assertions pass. }
var
  x: Integer;
  s: String;
  flag: Boolean;
begin
  x := 42;
  Assert(x = 42);
  Assert(x > 0, 'x must be positive');
  Assert(x <> 0);
  Assert(True);

  s := 'hello';
  Assert(Length(s) = 5, 'string length check');
  Assert(Length(s) > 0);

  flag := (x > 10) and (x < 100);
  Assert(flag, 'flag should be true for x=42');

  { Test with computed boolean expressions }
  Assert(x * 2 = 84);
  Assert(x div 2 = 21, 'integer division check');

  WriteLn('All assertions passed');
end.
