program AbstractVirtualDispatchSelf;

{ Regression test: when a non-abstract method in a base class calls an
  abstract virtual method via implicit Self, it must go through VMT
  dispatch (not a direct call to a non-existent label). }

type
  TCodeGen = class
    procedure GenLoad(Src, Dst: Integer); virtual; abstract;
    procedure RunCodeGen;
  end;

  TCodeGenX86 = class(TCodeGen)
    procedure GenLoad(Src, Dst: Integer); override;
  end;

procedure TCodeGen.RunCodeGen;
begin
  GenLoad(10, 20);
end;

procedure TCodeGenX86.GenLoad(Src, Dst: Integer);
begin
  WriteLn('GenLoad: ', Src, ' -> ', Dst);
end;

var
  CG: TCodeGen;
begin
  CG := TCodeGenX86.Create;
  CG.RunCodeGen;
end.
