{$mode objfpc}
program tdd_with_property_method_call;

type
  TInner = class
    function Name: string;
  end;

  TOuter = class
  private
    FData: TInner;
  public
    property Data: TInner read FData;
  end;

function TInner.Name: string;
begin
  Result := 'inner';
end;

var
  p: TOuter;
begin
  p := TOuter.Create;
  p.FData := TInner.Create;
  with p do
    WriteLn(Data.Name);
end.
