program AssertFail;
{ This program should fail with exit code 227 and print an assertion message.
  It is used by the test runner to verify Assert failure behavior. }
begin
  WriteLn('before assert');
  Assert(False, 'this assertion should fail');
  WriteLn('this should not be printed');
end.
