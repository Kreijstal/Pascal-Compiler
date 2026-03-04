{$mode objfpc}
program tdd_qualified_tsize_alias_call;

uses
  tdd_qualified_tsize_alias,
  tdd_qualified_tsize_shadow;

var
  buf: array[0..63] of Char;
  r: Int64;
begin
  r := UnitRead(0, @buf[0], 42);
  writeln(r);
end.
