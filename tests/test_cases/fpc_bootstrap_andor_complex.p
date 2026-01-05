program fpc_bootstrap_andor_complex;

{$mode objfpc}

function InvalidDay(ADay, AMonth: Integer): Boolean;
begin
  Result := (ADay > 30) and (AMonth in [4, 6, 9, 11])
    or (AMonth = 2) and (ADay > 29);
end;

begin
  writeln('case1=', InvalidDay(31, 4));
  writeln('case2=', InvalidDay(31, 2));
  writeln('case3=', InvalidDay(30, 2));
  writeln('case4=', InvalidDay(31, 1));
end.
