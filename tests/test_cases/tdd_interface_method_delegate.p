{$mode objfpc}
program tdd_interface_method_delegate;

type
  IFoo = interface
    function GetValue: Integer;
  end;

  TFoo = class(TInterfacedObject, IFoo)
  public
    function GetValueImpl: Integer;
    function IFoo.GetValue=GetValueImpl;
  end;

function TFoo.GetValueImpl: Integer;
begin
  Result := 42;
end;

var
  F: TFoo;
begin
  F := TFoo.Create;
  WriteLn(F.GetValueImpl);
end.
