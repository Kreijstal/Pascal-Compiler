{$mode objfpc}
program test_inherited_method_call;

{ Test calling inherited method with explicit method name }
{ This is a common FPC pattern that should work }

type
  TBase = class
  public
    procedure DoSomething; virtual;
  end;
  
  TDerived = class(TBase)
  public
    procedure DoSomething; override;
  end;

procedure TBase.DoSomething;
begin
  writeln('Base');
end;

procedure TDerived.DoSomething;
begin
  inherited DoSomething;  { Call parent method explicitly }
  writeln('Derived');
end;

var
  obj: TDerived;
begin
  obj := TDerived.Create;
  obj.DoSomething;
end.
