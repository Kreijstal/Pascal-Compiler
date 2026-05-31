program tdd_win_stderr_no_leak;
{ Regression: on Windows --no-stdlib, WriteLn(StdErr,...) must reach the
  process's standard error handle, not leak onto stdout.  Root cause was
  IsConsole defaulting to false (sysinit.pp never linked), so the RTL's
  SysInitStdIO AssignError'd the std files instead of binding them. }
begin
  WriteLn(StdErr, 'ERRLINE');
  WriteLn('OUTLINE');
end.
