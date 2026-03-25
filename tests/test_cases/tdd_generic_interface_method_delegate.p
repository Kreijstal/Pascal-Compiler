{$mode objfpc}
program tdd_generic_interface_method_delegate;

type
  IFoo = interface
    function GetValue: Integer;
  end;

  generic TFoo<T> = class(TInterfacedObject, IFoo)
  public
    function GetValueImpl: Integer;
    function IFoo.GetValue=GetValueImpl;
  end;

  TFooInt = specialize TFoo<Integer>;

function TFoo.GetValueImpl: Integer;
begin
  Result := 84;
end;

var
  F: TFooInt;
  I: IFoo;
begin
  F := TFooInt.Create;
  I := F;
  WriteLn(I.GetValue);
end.
