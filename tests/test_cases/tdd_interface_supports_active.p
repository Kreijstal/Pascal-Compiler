program tdd_interface_supports_active;

{$mode objfpc}

uses Classes;

type
  TObserverImpl = class(TInterfacedObject, IObserver)
  private
    FActive: Boolean;
    FOnToggle: TObserverToggleEvent;
  public
    constructor Create;
    procedure Removed;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetOnObserverToggle: TObserverToggleEvent;
    procedure SetOnObserverToggle(aEvent: TObserverToggleEvent);
  end;

constructor TObserverImpl.Create;
begin
  inherited Create;
  FActive := True;
end;

procedure TObserverImpl.Removed;
begin
end;

function TObserverImpl.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TObserverImpl.SetActive(Value: Boolean);
begin
  FActive := Value;
end;

function TObserverImpl.GetOnObserverToggle: TObserverToggleEvent;
begin
  Result := FOnToggle;
end;

procedure TObserverImpl.SetOnObserverToggle(aEvent: TObserverToggleEvent);
begin
  FOnToggle := aEvent;
end;

var
  Obj: TObject;
  Obs: IObserver;
begin
  Obj := TObserverImpl.Create;
  if not Supports(Obj, IObserver, Obs) then
    Halt(1);
  if Obs.Active then
    Writeln('OK')
  else
    Writeln('FAIL');
  Obj.Free;
end.
