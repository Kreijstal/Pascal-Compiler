{$mode objfpc}
program tdd_round_real_expr;

{ Round() should accept real expressions including division results.
  This reproduces the "Round expects a real argument" error from math.pp
  where Round(AValue/RV) fails when AValue and RV are Double. }

function IntPower(base: Double; exponent: Integer): Double;
var
  i: Integer;
begin
  Result := 1.0;
  for i := 1 to Abs(exponent) do
    Result := Result * base;
  if exponent < 0 then
    Result := 1.0 / Result;
end;

function RoundTo(const AValue: Double; const Digits: Integer): Double;
var
  RV: Double;
begin
  RV := IntPower(10, Digits);
  Result := Round(AValue / RV) * RV;
end;

begin
  { RoundTo(3.456, -2) should round to 2 decimal places = 3.46 }
  writeln(RoundTo(3.456, -2):0:2);
  { RoundTo(2.5, 0) should round to nearest integer = 2 (banker's rounding) or 3 }
  writeln(Round(2.6));
  { Basic Round of a division }
  writeln(Round(7.0 / 2.0));
end.
