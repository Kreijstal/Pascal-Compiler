program set_in_pointer_deref;
{ Regression: dereferencing a pointer to a small (<=4-byte, register-resident)
  set must LOAD the set value, not keep the pointer address.  Previously the
  deref leaf treated all sets as by-address aggregates, so `e in pflags(arg)^`
  tested bits in the pointer instead of the set, and `t := p^` / `p^ = s`
  copied/compared the address.  This is also the exact node FPC's optimizer
  (might_have_sideeffects with mhs_exceptions) relies on, which broke the
  short-circuit-boolean decision in the -O2 self-build. }
type
  tflag = (f_a, f_b, f_c);
  tflags = set of tflag;
  pflags = ^tflags;

function check(arg: pointer): boolean;
begin
  check := (f_b in pflags(arg)^);
end;

function check_const(const flags: tflags): boolean;
begin
  check_const := (f_b in pflags(@flags)^);
end;

var
  flags, t: tflags;
  p: pflags;
begin
  flags := [f_b];
  p := @flags;

  { in via untyped pointer cast }
  if check(@flags) then writeln('in_ptr_ok') else writeln('in_ptr_BAD');

  { in via const value-param address (the FPC might_have_sideeffects shape) }
  if check_const(flags) then writeln('in_const_ok') else writeln('in_const_BAD');

  { in directly on a typed-pointer deref }
  if f_b in p^ then writeln('in_deref_ok') else writeln('in_deref_BAD');

  { assignment copy through deref }
  t := p^;
  if t = flags then writeln('assign_ok') else writeln('assign_BAD');

  { compare through deref }
  if p^ = flags then writeln('cmp_ok') else writeln('cmp_BAD');
end.
