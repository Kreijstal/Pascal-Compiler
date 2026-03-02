{$mode objfpc}
program tdd_tsize_overload_cross_unit;

{ When a program uses a unit that defines TSize as an integer alias,
  calls to functions with TSize parameters should match integer arguments.
  This reproduces the FpRead/FpWrite overload mismatch from math.pp
  where TSize was resolving to a record type instead of QWord. }

uses tdd_tsize_overload_unit;

var
  buf: array[0..63] of Char;
  r: Int64;
  n: Integer;
begin
  n := 50;

  { Integer argument should match TSize (QWord alias) parameter }
  r := UnitRead(0, @buf[0], n);
  writeln(r);

  { Literal should match TSize parameter }
  r := UnitWrite(1, @buf[0], 77);
  writeln(r);
end.
