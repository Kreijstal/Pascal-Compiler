program tdd_class_method_self_injection;

type
  TObservers = record
    Value: Integer;
  end;

  TLinkObservers = class
  protected
    class function CheckObserving(const aObservers: TObservers; aID: Integer): Integer;
  public
    class function GetValue(const aObservers: TObservers): Integer; static;
  end;

class function TLinkObservers.CheckObserving(const aObservers: TObservers; aID: Integer): Integer;
begin
  Result := aObservers.Value + aID;
end;

class function TLinkObservers.GetValue(const aObservers: TObservers): Integer;
var
  aId: Integer;
begin
  aId := CheckObserving(aObservers, 5);
  Result := aId;
end;

var
  Obs: TObservers;
begin
  Obs.Value := 37;
  Writeln(TLinkObservers.GetValue(Obs));
end.
