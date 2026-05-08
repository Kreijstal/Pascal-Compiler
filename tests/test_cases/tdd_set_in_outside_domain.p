program tdd_set_in_outside_domain;

{ Regression test for kgpc miscompile of `n in [a..b]` when n is outside
  the set's element domain.

  Bug: codegen emitted register-form `btl elem, set_reg` and `btsl elem,
  dest_reg`, both of which wrap the bit index modulo 32.  So `33 in [1..18]`
  would test bit 33 mod 32 = bit 1, which is set, and falsely return TRUE.
  Pascal semantics require the IN test to return FALSE for elements outside
  the set's storage domain.

  Manifested in pp_bootstrap as a SIGSEGV in scanner.pas:6195's length
  guard `length(pattern) in [tokenlenmin..tokenlenmax]` (length=33,
  tokenlenmax=18) — the wrapped test wrongly reported true and the
  subsequent binary-search code dereferenced an out-of-range tokeninfo
  index. }

var
  n: byte;
begin
  { 33 mod 32 = 1, which IS in [1..18] — must still report 33 NOT in. }
  n := 33;
  if n in [1..18] then writeln('FAIL_33') else writeln('OK_33');

  { 1 IS in [1..18] — sanity check. }
  n := 1;
  if n in [1..18] then writeln('OK_1') else writeln('FAIL_1');

  { 65 mod 32 = 1, again inside [1..18] — must still report 65 NOT in. }
  n := 65;
  if n in [1..18] then writeln('FAIL_65') else writeln('OK_65');

  { 18 IS in [1..18] — boundary inclusive. }
  n := 18;
  if n in [1..18] then writeln('OK_18') else writeln('FAIL_18');

  { 19 is NOT in [1..18] — boundary exclusive. }
  n := 19;
  if n in [1..18] then writeln('FAIL_19') else writeln('OK_19');

  { 0 is NOT in [1..18] — lower boundary exclusive. }
  n := 0;
  if n in [1..18] then writeln('FAIL_0') else writeln('OK_0');
end.
