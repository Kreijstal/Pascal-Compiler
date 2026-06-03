program tdd_large_set_union_membership;
{ Regression: a runtime set-union expression (A + B) must preserve set
  elements whose ordinal is >= 32.  KGPC computed set union/intersection/
  difference in a single 32-bit register (orl), dropping every element past
  bit 31, so `x in (setA + [literal])` returned false for high-ordinal x.

  This was the root cause of the Win64 self-host crash: FPC's cgx86.pas gates
  RIP-relative addressing on `target_info.system in (systems_all_windows +
  [...])`, and system_x86_64_win64 has ordinal 37 -- the dropped bit made the
  test false, so globals were emitted with absolute [disp32] addressing that
  truncates at the 4 GB win64 image base and crashes at startup.

  TE has 50 members so its set is wider than 32 bits; setA holds a low (5) and
  a high (37) element. Expected: every member of (setA + [e44]) is found. }
type
  TE = (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15,
        e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27, e28, e29,
        e30, e31, e32, e33, e34, e35, e36, e37, e38, e39, e40, e41, e42, e43,
        e44, e45, e46, e47, e48, e49);
const
  setA = [e5, e37];
var
  x: TE;
begin
  x := e5;  if x in (setA + [e44]) then writeln('e5 in') else writeln('e5 out');
  x := e37; if x in (setA + [e44]) then writeln('e37 in') else writeln('e37 out');
  x := e44; if x in (setA + [e44]) then writeln('e44 in') else writeln('e44 out');
  x := e6;  if x in (setA + [e44]) then writeln('e6 in') else writeln('e6 out');
  x := e38; if x in (setA + [e44]) then writeln('e38 in') else writeln('e38 out');
end.
