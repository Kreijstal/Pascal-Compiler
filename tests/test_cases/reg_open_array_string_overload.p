program reg_open_array_string_overload;

{$mode objfpc}
{$H+}

function Which(const A: array of Char): Integer; overload;
begin
  Which := 1;
end;

function Which(const A: array of String): Integer; overload;
begin
  Which := 2;
end;

begin
  writeln('char=', Which(['a', 'b']));
  writeln('string=', Which(['aa', 'bb']));
end.
