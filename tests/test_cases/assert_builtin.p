program AssertBuiltin;
{ Test that Assert is recognized as a builtin procedure.
  Assert(condition) and Assert(condition, message) must both compile
  and, in the current implementation, behave as no-ops at runtime
  (release-mode semantics). }
var
  x: Integer;
begin
  x := 42;
  Assert(x = 42);
  Assert(x > 0, 'x must be positive');
  Assert(True);
  WriteLn('PASS');
end.
