program AbstractVirtualDispatchFieldReceiver;

type
  TCodeGen = class
    procedure GenLoad(Src, Dst: Integer); virtual; abstract;
  end;

  TCodeGenX86 = class(TCodeGen)
    procedure GenLoad(Src, Dst: Integer); override;
  end;

  TRunner = class
  private
    FBackend: TCodeGen;
  public
    constructor Create(ABackend: TCodeGen);
    procedure RunCodeGen;
  end;

constructor TRunner.Create(ABackend: TCodeGen);
begin
  FBackend := ABackend;
end;

procedure TRunner.RunCodeGen;
begin
  FBackend.GenLoad(10, 20);
end;

procedure TCodeGenX86.GenLoad(Src, Dst: Integer);
begin
  WriteLn('GenLoad: ', Src, ' -> ', Dst);
end;

var
  Runner: TRunner;
begin
  Runner := TRunner.Create(TCodeGenX86.Create);
  Runner.RunCodeGen;
end.
