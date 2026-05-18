{ Regression test for: `enum_const in var_set_param` testing bit of pointer
  instead of the dereferenced set.  When a procedure takes `var s: TFlags`
  (where TFlags is a small set <= 4 bytes) and the body checks `x in s`,
  KGPC used to emit `btl elem, set_val_reg` where set_val_reg held the
  pointer to the set, not the set value.  That tested some random bit of
  a stack address (ASLR-dependent), so the result was non-deterministic.
  Fix: dereference the var-param pointer before the btl.
  See memory/project_varparam_set_in_bug.md for the diagnosis. }
program regr_set_in_varparam;

type
    TFlag = (f0, f1, f2, f3, f4, f5, f6, f7);
    TFlags = set of TFlag;

procedure check(var flags: TFlags);
begin
    if f5 in flags then
        writeln('FAIL: f5 should not be in flags')
    else
        writeln('PASS: f5 not in flags');
    if f4 in flags then
        writeln('PASS: f4 in flags')
    else
        writeln('FAIL: f4 should be in flags');
end;

var
    myflags: TFlags;
begin
    myflags := [f4];
    check(myflags);
end.
