program tdd_stmt_system_error_unit_qualifier;
{$mode objfpc}

begin
  if 1 = 0 then
    System.Error(reRangeError);
  writeln('ok');
end.
