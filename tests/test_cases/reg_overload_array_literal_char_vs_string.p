{$mode objfpc}
program reg_overload_array_literal_char_vs_string;

function Which(const A: array of Char): Integer; overload;
begin
  Which := 1;
end;

function Which(const A: array of String): Integer; overload;
begin
  Which := 2;
end;

begin
  Writeln(Which(['a','b']));
  Writeln(Which(['aa','bb']));
end.
