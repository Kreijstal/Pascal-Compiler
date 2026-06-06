program fpc_bootstrap_exit_unwinds_except_frame;

procedure LeaveProtected;
begin
  try
    Exit;
  except
    writeln('bad-local-handler');
  end;
end;

begin
  LeaveProtected;
  raise 77;
end.
