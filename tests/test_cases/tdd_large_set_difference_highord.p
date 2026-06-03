program tdd_large_set_difference_highord;
{ Regression: runtime set difference (A - B) must preserve elements whose
  ordinal is >= 32.  KGPC computed set arithmetic in a single 32-bit register,
  dropping every element past bit 31, so high-ordinal elements survived a
  difference even when they should have been removed.

  TE has 50 members so its set is wider than 32 bits.  setA = [e5, e37];
  setA - [e37] = [e5] (the high-ordinal member e37 must be removed). }
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
  x := e5;  if x in (setA - [e37]) then writeln('e5 in') else writeln('e5 out');
  x := e37; if x in (setA - [e37]) then writeln('e37 in') else writeln('e37 out');
end.
