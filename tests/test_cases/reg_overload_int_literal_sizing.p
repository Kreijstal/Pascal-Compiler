{$mode objfpc}
program reg_overload_int_literal_sizing;

function Which(i: Integer): Integer; overload;
begin
  Which := 1;
end;

function Which(i: Int64): Integer; overload;
begin
  Which := 2;
end;

begin
  Writeln(Which(1));
end.
