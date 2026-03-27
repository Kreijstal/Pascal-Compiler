program reg_prefetch_builtin_proc;
{$mode objfpc}

var
  values: array[0..3] of LongInt;
begin
  values[2] := 42;
  Prefetch(values[2]);
  WriteLn(values[2]);
end.
