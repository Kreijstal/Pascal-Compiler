{$mode objfpc}
program gap_str_real;

var
  s: string;

begin
  Str(1.25:0:2, s);
  WriteLn(s);
end.
