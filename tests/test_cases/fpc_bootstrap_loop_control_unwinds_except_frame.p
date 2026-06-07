program fpc_bootstrap_loop_control_unwinds_except_frame;

procedure LeaveWithBreak;
begin
  while true do
  begin
    try
      Break;
    except
      writeln('bad-break-handler');
    end;
  end;
end;

procedure LeaveWithContinue;
var
  i: integer;
begin
  i := 0;
  while i < 1 do
  begin
    inc(i);
    try
      Continue;
    except
      writeln('bad-continue-handler');
    end;
  end;
end;

begin
  LeaveWithBreak;
  LeaveWithContinue;
  raise 88;
end.
