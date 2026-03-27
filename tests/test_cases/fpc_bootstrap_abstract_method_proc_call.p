{
  Regression: statement-form abstract virtual calls must resolve in semcheck.

  This is the procedure-call counterpart to fpc_bootstrap_abstract_method_call.p.
  It exercises semcheck_proccall rather than function-call expression lowering.
}
program fpc_bootstrap_abstract_method_proc_call;
{$mode objfpc}

type
  TBase = class
  protected
    procedure Touch; virtual; abstract;
  public
    procedure Run;
  end;

  TDerived = class(TBase)
  protected
    procedure Touch; override;
  end;

procedure TBase.Run;
begin
  Touch;
end;

procedure TDerived.Touch;
begin
  WriteLn('ok');
end;

var
  obj: TDerived;
begin
  obj := TDerived.Create;
  obj.Run;
  obj.Free;
end.
