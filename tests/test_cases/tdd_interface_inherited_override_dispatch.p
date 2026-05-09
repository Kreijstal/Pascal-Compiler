{$mode objfpc}
program tdd_interface_inherited_override_dispatch;

type
  IValue = interface
    function GetValue: Integer;
  end;

  TBaseValue = class(TInterfacedObject, IValue)
  public
    function GetValue: Integer; virtual;
  end;

  TChildValue = class(TBaseValue)
  public
    function GetValue: Integer; override;
  end;

function TBaseValue.GetValue: Integer;
begin
  Result := 1;
end;

function TChildValue.GetValue: Integer;
begin
  Result := 2;
end;

var
  I: IValue;
begin
  I := TBaseValue.Create;
  WriteLn(I.GetValue);
  I := TChildValue.Create;
  WriteLn(I.GetValue);
end.
