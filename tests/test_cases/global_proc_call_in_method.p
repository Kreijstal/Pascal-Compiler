program GlobalProcCallInMethod;

procedure GlobalProc;
begin
  WriteLn('GlobalProc');
end;

type
  TRunner = class
    procedure Run;
  end;

procedure TRunner.Run;
begin
  GlobalProc;
end;

var
  Runner: TRunner;
begin
  Runner := TRunner.Create;
  Runner.Run;
end.
