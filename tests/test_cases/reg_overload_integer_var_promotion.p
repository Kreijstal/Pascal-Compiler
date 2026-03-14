{$mode objfpc}
program reg_overload_integer_var_promotion;

{ In FPC, Integer = LongInt (both 32-bit), so they cannot be separate overloads.
  Use SmallInt (16-bit) as a truly distinct type instead. }

function Which(i: SmallInt): Integer; overload;
begin
  Which := 1;
end;

function Which(i: LongInt): Integer; overload;
begin
  Which := 2;
end;

function Which(i: Int64): Integer; overload;
begin
  Which := 3;
end;

var
  x: SmallInt;
  y: LongInt;
  z: Int64;
begin
  x := 1;
  y := 1;
  z := 1;
  Writeln(Which(x));
  Writeln(Which(y));
  Writeln(Which(z));
end.
