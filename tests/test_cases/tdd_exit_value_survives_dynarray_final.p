program tdd_exit_value_survives_dynarray_final; {$mode objfpc}{$H+}
{ Regression: a function with a managed dynamic-array local AND an early
  `exit` must return the value already stored in Result.  KGPC used to load
  Result into the ABI return register (%rax) BEFORE the implicit
  kgpc_dynarray_finalize_local cleanup that the early-exit path emits; that
  call clobbers %rax, so the function returned garbage.  This is the exact
  shape of FPC's saved_xmm_reg_size, whose garbage return propagated into a
  bogus stack-teardown immediate in win64 _fin helpers and crashed every
  program at startup.  The fix materializes the return value AFTER cleanup. }
function compute(early: boolean): longint;
var arr: array of longint;
begin
  result := 0;
  SetLength(arr, 4);
  arr[0] := 99;
  if early then exit;     { result must stay 0 across arr finalization }
  result := arr[0];
end;
begin
  writeln(compute(true));   { expect 0 }
  writeln(compute(false));  { expect 99 }
end.
