{ Test: Trunc with Currency type }
{ BUG: Trunc(C) where C is Currency fails with "expects a real argument" }
{$mode objfpc}
program trunc_currency;

var
  C: Currency;
  I: Int64;
begin
  C := 3.5;
  I := Trunc(C);
  WriteLn(I);
end.
