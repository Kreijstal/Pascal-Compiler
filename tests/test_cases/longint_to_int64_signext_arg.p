{ Regression for sign-extension of longint passed to int64 parameter. }
{ Before the fix, KGPC zero-extended the spilled 32-bit value, so       }
{ check(-2) received 4294967294 instead of -2 (mov from spill via       }
{ movq read the upper-half garbage instead of using movslq).            }
program signext_longint_to_int64;
type tcgint = int64;
procedure check(a: tcgint);
begin
  writeln('a = ', a);
end;
var l : longint;
begin
  l := 1; l := not l;
  check(l);
end.
